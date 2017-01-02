#!/usr/bin/env runhaskell
-- -*- coding: utf-8; mode: haskell; -*-

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Main where

import           System.Environment           (getArgs)

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS

import           Data.Map                     (Map)
import qualified Data.Map                     as Map

import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Data.Foldable                (toList)

import           Text.XML.Light
import           Text.XML.Light.Cursor        hiding (Path)

import           Data.Serialize               (Serialize)
import qualified Data.Serialize               as Cereal

import           GHC.Generics

import           Control.Monad
import           Data.Char                    (isSpace)
import           Data.Maybe
import           Data.Monoid

import           Text.PrettyPrint.ANSI.Leijen hiding (text, (<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Control.Monad.State.Lazy

import           Path
import           Path.IO

import           Safe                         (readMay)

mapInitLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapInitLast _ _ []     = []
mapInitLast _ g [x]    = [g x]
mapInitLast f g (x:xs) = f x : mapInitLast f g xs

splitOnCommas :: Text -> [Text]
splitOnCommas = T.split (== ',')

treadMay :: (Read r) => Text -> Maybe r
treadMay = T.unpack .> readMay

text :: Text -> Doc
text = PP.text . T.unpack

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixl 9 .>

(<#>) :: (Functor f) => f a -> (a -> b) -> f b
(<#>) = flip (<$>)
infixr 4 <#>

newtype PreparsedXML = PreparsedXML { toXMLChunks :: [Content] }
                     deriving (Generic)

deriving instance Generic Attr
deriving instance Generic CData
deriving instance Generic CDataKind
deriving instance Generic QName
deriving instance Generic Element
deriving instance Generic Content

instance Serialize Attr
instance Serialize CData
instance Serialize CDataKind
instance Serialize QName
instance Serialize Element
instance Serialize Content
instance Serialize PreparsedXML

-- data QXFunction
--   = QXFunction
--   {
--   } deriving (Eq, Show, Read, Generic)

data QXVersion      -- FIXME
data QXThreadSafety -- FIXME
data QXAccessLevel  -- FIXME
data QXStatus       -- FIXME
data QXVirtual      -- FIXME
data QXPageSubtype  -- FIXME
data QXFunctionType -- FIXME
data CxxType        -- FIXME
data CxxValue       -- FIXME
data CxxStubType    -- FIXME: normal / delete / default

data QXPosition
  = QXPosition
  { _lineno   :: Int
  , _filepath :: Path Abs File
  , _location :: Text
  , _href     :: Text
  }

data QXDecl
  = QXDeclClass    QXClass
  | QXDeclTypedef  QXTypedef
  | QXDeclEnum     QXEnum
  | QXDeclVariable QXVariable
  | QXDeclFunction QXFunction

--------------------------------------------------------------------------------

data QXKeyword
  = QXKeyword
  { _name  :: Text
  , _title :: Text
  }

--------------------------------------------------------------------------------

data QXContents
  = QXContents
  { _name  :: Text
  , _title :: Text
  , _level :: Int
  }

--------------------------------------------------------------------------------

data QXModule
  = QXModule

  -- Metadata

  { _name     :: Text

  , _position :: QXPosition

  , _brief    :: Text
  , _status   :: QXStatus

  , _groups   :: [Text]

  -- Qt-related

  , _title    :: Text
  , _seen     :: Bool
  , _members  :: [Text]
  }

--------------------------------------------------------------------------------

data QXPage
  = QXPage

  -- Metadata

  { _name      :: Text

  , _position  :: QXPosition

  , _brief     :: Text
  , _status    :: QXStatus
  , _since     :: QXVersion

  , _groups    :: [Text]

  -- Qt-related

  , _title     :: Text
  , _fulltitle :: Text
  , _subtitle  :: Text

  , _module    :: Text
  , _subtype   :: QXPageSubtype
  , _children  :: () -- contents, keyword
  }

--------------------------------------------------------------------------------

data QXGroup
  = QXGroup

  -- Metadata

  { _name     :: Text

  , _position :: QXPosition

  , _brief    :: Text
  , _status   :: QXStatus

  , _groups   :: [Text]

  -- Qt-related

  , _title    :: Text
  , _seen     :: Bool
  , _members  :: [Text]
  , _children :: () -- contents, keyword
  }

--------------------------------------------------------------------------------

data QXTypedef
  = QXTypedef

  -- Metadata

  { _name     :: Text
  , _fullname :: Text

  , _position :: QXPosition

  , _status   :: QXStatus
  , _since    :: QXVersion

  -- C++-related

  , _access   :: QXAccessLevel
  , _tsafety  :: QXThreadSafety
  , _enum     :: Text
  }

--------------------------------------------------------------------------------

data QXEnum
  = QXEnum

  -- Metadata

  { _name     :: Text
  , _fullname :: Text

  , _position :: QXPosition

  , _status   :: QXStatus
  , _since    :: QXVersion

  -- C++-related

  , _access   :: QXAccessLevel
  , _tsafety  :: QXThreadSafety
  , _typedef  :: Text
  , _values   :: Map Text CxxValue
  }

--------------------------------------------------------------------------------

data QXVariable
  = QXVariable

  -- Metadata

  { _name     :: Text
  , _fullname :: Text

  , _position :: QXPosition

  , _brief    :: Text
  , _status   :: QXStatus

  -- C++-related

  , _access   :: QXAccessLevel
  , _tsafety  :: QXThreadSafety
  , _static   :: Bool
  , _type     :: CxxType
  }

--------------------------------------------------------------------------------

data QXProperty
  = QXProperty

    -- Metadata

  { _name      :: Text
  , _fullname  :: Text

  , _position  :: QXPosition

  , _brief     :: Text
  , _status    :: QXStatus
  , _since     :: QXVersion

    -- Qt-related

  , _getters   :: [Text]
  , _setters   :: [Text]
  , _resetters :: [Text]
  , _notifiers :: [Text]

    -- C++-related

  , _access    :: QXAccessLevel
  , _tsafety   :: QXThreadSafety
  , _type      :: CxxType
  }

--------------------------------------------------------------------------------

data QXParameter
  = QXParameter
  { _name    :: Text
  , _type    :: CxxType
  , _default :: CxxValue
  }

--------------------------------------------------------------------------------

data QXFunction
  = QXFunction

  -- Metadata

  { _name      :: Text
  , _fullname  :: Text

  , _position  :: QXPosition

  , _brief     :: Text
  , _status    :: QXStatus
  , _since     :: QXVersion

  -- Qt-related

  , _assocProp :: Text

  -- C++-related

  , _access    :: QXAccessLevel
  , _tsafety   :: QXThreadSafety

  , _signature :: CxxType
  , _type      :: CxxType

  , _virtual   :: QXVirtual
  , _const     :: Bool
  , _final     :: Bool
  , _static    :: Bool
  , _stubType  :: CxxStubType
  , _overload  :: Maybe Int
  , _meta      :: QXFunctionType

  , _params    :: [QXParameter]
  }

--------------------------------------------------------------------------------

data QXClass
  = QXClass

  -- Metadata

  { _name     :: Text
  , _fullname :: Text

  , _position :: QXPosition

  , _module   :: Text
  , _groups   :: [Text]

  , _brief    :: Text
  , _status   :: QXStatus
  , _since    :: QXVersion

  -- C++-related

  , _access   :: QXAccessLevel
  , _tsafety  :: QXThreadSafety

  , _bases    :: Text
  , _children :: [QXDecl]
  }

--------------------------------------------------------------------------------

data QXNamespace
  = QXNamespace

  -- Metadata

  { _name     :: Text
  , _fullname :: Text

  , _position :: QXPosition

  , _module   :: Text

  , _brief    :: Text
  , _status   :: QXStatus

  -- C++-related

  , _access   :: QXAccessLevel
  , _tsafety  :: QXThreadSafety
  , _children :: [QXDecl]
  }

--------------------------------------------------------------------------------

data QXIndex
  = QXIndex
  { _project  :: Text
  , _url      :: Text
  , _version  :: Text
  , _title    :: Text
  , _children :: [QXNamespace]
  }

--------------------------------------------------------------------------------

parseQXPosition :: Element -> Maybe QXPosition
parseQXPosition el = do
  let toPath = T.unpack .> parseAbsFile
  _lineno   <- lookupAttrib "lineno"   el >>= treadMay
  _filepath <- lookupAttrib "filepath" el >>= toPath
  _location <- lookupAttrib "location" el
  _href     <- lookupAttrib "href"     el
  _children <- el |> elChildren |> mapM parseQXDecl
  pure $ QXPosition {..}

parseQXStatus :: Element -> Maybe QXStatus
parseQXStatus = undefined

parseQXSince :: Element -> Maybe QXVersion
parseQXSince = undefined

parseQXAccessLevel :: Element -> Maybe QXAccessLevel
parseQXAccessLevel = undefined

parseQXThreadSafety :: Element -> Maybe QXThreadSafety
parseQXThreadSafety = undefined

parseQXType :: Text -> Element -> Maybe CxxType
parseQXType = undefined

parseQXStubType :: Element -> Maybe CxxStubType
parseQXStubType = undefined

parseQXOverload :: Element -> Maybe (Maybe Int)
parseQXOverload = undefined

parseQXFunctionType :: Element -> Maybe QXFunctionType
parseQXFunctionType = undefined

parseQXVirtual :: Element -> Maybe QXVirtual
parseQXVirtual = undefined

parseQXParameters :: Element -> Maybe [QXParameter]
parseQXParameters = undefined
  where
    parseQXParameter :: Element -> QXParameter
    parseQXParameter = undefined

parseQXValues :: Element -> Maybe (Map Text CxxValue)
parseQXValues = undefined
  where
    parseQXValue :: Element -> (Text, CxxValue)
    parseQXValue = undefined

parseQXBool :: Text -> Element -> Maybe Bool
parseQXBool attr el = case lookupAttrib attr el
                      of Just "true"  -> pure True
                         Just "false" -> pure False
                         _            -> Nothing

parseQXClass :: Element -> Maybe QXClass
parseQXClass el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupAttrib "fullname"
  _position  <- el |> parseQXPosition
  _module    <- el |> lookupAttrib "module"
  _groups    <- el |> lookupAttrib "groups" |> fmap splitOnCommas
  _brief     <- el |> lookupAttrib "brief"
  _status    <- el |> parseQXStatus
  _since     <- el |> parseQXSince
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _bases     <- el |> lookupAttrib "bases"
  _children  <- el |> elChildren |> mapM parseQXDecl
  pure $ QXClass {..}

parseQXTypedef :: Element -> Maybe QXTypedef
parseQXTypedef el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupAttrib "fullname"
  _position  <- el |> parseQXPosition
  _status    <- el |> parseQXStatus
  _since     <- el |> parseQXSince
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _enum     <- lookupAttrib "enum" el
  _children  <- el |> elChildren |> mapM parseQXDecl
  pure $ QXTypedef {..}

parseQXEnum :: Element -> Maybe QXEnum
parseQXEnum el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupAttrib "fullname"
  _position  <- el |> parseQXPosition
  _brief     <- el |> lookupAttrib "brief"
  _status    <- el |> parseQXStatus
  _since     <- el |> parseQXSince
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _typedef   <- el |> lookupAttrib "typedef"
  _values    <- el |> parseQXValues
  pure $ QXEnum {..}

parseQXVariable :: Element -> Maybe QXVariable
parseQXVariable el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupAttrib "fullname"
  _position  <- el |> parseQXPosition
  _brief     <- el |> lookupAttrib "brief"
  _status    <- el |> parseQXStatus
  _since     <- el |> parseQXSince
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _type      <- el |> parseQXType "type"
  _static    <- el |> parseQXBool "static"
  _children  <- el |> elChildren |> mapM parseQXDecl
  pure $ QXVariable {..}

parseQXFunction :: Element -> Maybe QXFunction
parseQXFunction el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupAttrib "fullname"
  _position  <- el |> parseQXPosition
  _brief     <- el |> lookupAttrib "brief"
  _status    <- el |> parseQXStatus
  _since     <- el |> parseQXSince
  _assocProp <- el |> lookupAttrib "associated-property"
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _signature <- el |> parseQXType "signature"
  _type      <- el |> parseQXType "type"
  _virtual   <- el |> parseQXVirtual
  _const     <- el |> parseQXBool "const"
  _final     <- el |> parseQXBool "final"
  _static    <- el |> parseQXBool "static"
  _stubType  <- el |> parseQXStubType
  _overload  <- el |> parseQXOverload
  _meta      <- el |> parseQXFunctionType
  _params    <- el |> parseQXParameters
  pure $ QXFunction {..}

parseQXDecl :: Element -> Maybe QXDecl
parseQXDecl el = case el |> elName |> qName
                 of "class"    -> QXDeclClass    <$> parseQXClass    el
                    "typedef"  -> QXDeclTypedef  <$> parseQXTypedef  el
                    "enum"     -> QXDeclEnum     <$> parseQXEnum     el
                    "variable" -> QXDeclVariable <$> parseQXVariable el
                    "function" -> QXDeclFunction <$> parseQXFunction el
                    owise      -> fail $ "unknown declaration: " <> owise

parseQXNamespace :: Element -> Maybe QXNamespace
parseQXNamespace el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupAttrib "fullname"
  _position  <- el |> parseQXPosition
  _module    <- el |> lookupAttrib "module"
  _brief     <- el |> lookupAttrib "brief"
  _status    <- el |> parseQXStatus
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _children  <- el |> elChildren |> mapM parseQXDecl
  pure $ QXNamespace {..}

parseQXIndex :: [Content] -> Maybe QXIndex
parseQXIndex xml = do
  let index = xml |> onlyElems |> (!! 1)
  _project  <- index |> lookupAttrib "project"
  _url      <- index |> lookupAttrib "url"
  _version  <- index |> lookupAttrib "version"
  _title    <- index |> lookupAttrib "title"
  _children <- index |> elChildren |> mapM parseQXNamespace
  pure $ QXIndex {..}

lookupAttrib :: Text -> Element -> Maybe Text
lookupAttrib = undefined

isSpaceCData :: Content -> Bool
isSpaceCData (Text (CData { cdData = xs })) = all isSpace xs
isSpaceCData _                              = False

-- removeContent :: (Content -> Bool) -> [Content] -> [Content]
-- removeContent _    []      = []
-- removeContent pred content = let Just cur = fromForest content
--                              in toForest $ removeC cur
--   where
--     removeC cur = if pred $ toTree cur
--                   then case removeGoRight cur
--                        of Just cur' -> removeC cur'
--                           Nothing   -> case removeGoUp cur
--                                        of Just cur' -> removeC cur'
--                                           Nothing   -> cur
--                   else case nextDF cur
--                        of Just cur' -> removeC cur'
--                           Nothing   -> cur

modifyForest :: ([Content] -> [Content]) -> Cursor -> Cursor
modifyForest f = modifyContent helper
  where
    helper :: Content -> Content
    helper (Elem e) = Elem (e { elContent = (e |> elContent |> f) })
    helper owise    = owise

removeSpaceCData :: [Content] -> [Content]
removeSpaceCData content = toForest $ last iters
  where
    iters :: [Cursor]
    iters = content
            |> fromForest
            |> iterate (>>= nextDF . helper)
            |> takeWhile isJust
            |> map fromJust
    -- iters = map fromJust $ takeWhile isJust $ iterate (>>= nextDF . helper) $ fromForest content
    helper = modifyForest (filter (not . isSpaceCData))

-- removeSpaceCData :: [Content] -> [Content]
-- removeSpaceCData = removeContent isSpaceCData

type XTag = Text
type XKey = Text
type XVal = Text
type XAttrSet = Map XKey XVal

data XTree = XElement XTag XAttrSet [XTree]
           | XText    Text

summarizeXTree :: XTree -> Map XTag [XAttrSet]
summarizeXTree = \xt -> execState (helper xt) mempty
  where
    helper :: XTree -> State (Map XTag [XAttrSet]) ()
    helper (XText _) = pure ()
    helper (XElement tag attrs children) = do
      modify $ Map.insertWith (<>) tag [attrs]
      mapM_ helper children

maximalAttrs :: [XAttrSet] -> Map XKey (Set XVal)
maximalAttrs input = execState (mapM_ helper input) mempty
  where
    helper :: XAttrSet -> State (Map XKey (Set XVal)) ()
    helper xas = do
      cur <- get
      Set.toList allKeys
        |> map (\k -> (k, Map.lookup k xas |> toList |> Set.fromList))
        |> Map.fromList
        |> (`merge` cur)
        |> put

    merge :: Map XKey (Set XVal) -> Map XKey (Set XVal) -> Map XKey (Set XVal)
    merge = Map.unionWith Set.union

    allKeys :: Set Text
    allKeys = Set.fromList $ concatMap Map.keys input

structureXTree :: XTree -> Map XTag (Set XTag)
structureXTree = \xt -> execState (helper xt) mempty
  where
    helper :: XTree -> State (Map XTag (Set XTag)) ()
    helper (XText _) = pure ()
    helper (XElement tag _ children) = do
      let subtags = Set.fromList $ map (\(XElement t _ _) -> t) children
      modify $ Map.insertWith (<>) tag subtags
      mapM_ helper children

instance Pretty XTree where
  pretty (XText t) = dquotes $ text t
  pretty (XElement tag attrs children)
    = case children
      of [] -> tagD <+> attrsD <> semi
         _  -> [ tagD <+> attrsD <+> lbrace
               , hang 4 (flatAlt (text "    ") empty <> vsep childrenD)
               , rbrace
               ] |> sep
    where
      tagD = blue $ text tag
      attrsD = attrs
               |> Map.toList
               |> map (uncurry renderAttr .> (space <>))
               |> mapInitLast id (<> space)
               |> tupled
      childrenD = map pretty children
      renderAttr k v = [ bold $ green $ fill padding $ text k
                       , equals
                       , red $ dquotes $ text v
                       ] |> hsep
      padding = attrs |> Map.keys |> map T.length |> maximum

elementToXT :: Element -> XTree
elementToXT el = XElement elementName elementAttrs elementContent
  where
    elementName    = el |> elName    |> qName |> T.pack
    elementAttrs   = el |> elAttribs |> map attrToPair |> Map.fromList
    elementContent = el |> elContent |> contentsToXT

    attrToPair :: Attr -> (Text, Text)
    attrToPair (Attr (QName { qName = key }) val) = (T.pack key, T.pack val)

contentsToXT :: [Content] -> [XTree]
contentsToXT = concatMap helper
  where
    helper (Elem el) = [elementToXT el]
    helper (Text cd) = [XText $ T.pack $ cdData cd]
    helper _         = []

getPreparsed :: IO [Content]
getPreparsed = do
  Right pp <- Cereal.decode <$> BS.readFile "./debug.dat"
  pure $ toXMLChunks pp

preparseXML :: FilePath -> FilePath -> IO ()
preparseXML input output = do
  xml <- parseXML <$> T.readFile input
  BS.writeFile output $ Cereal.encode xml

simple :: IO XTree
simple = elementToXT . (!! 1) . onlyElems . parseXML
         <$> T.readFile "./simple.xml"

printSimple :: IO ()
printSimple = simple >>= pretty .> putDoc

main :: IO ()
main = getPreparsed
       >>= print

-- -- import           System.Console.Docopt

-- -- patterns :: Docopt
-- -- patterns = [docopt|
-- -- qdocxml-parser version 0.1.0
-- --
-- -- Usage:
-- --   qdocxml-parser preparse [-h] <input> <output>
-- --   qdocxml-parser help
-- --   qdocxml-parser version
-- --
-- -- Options:
-- --   -h, --help    Print help for the specified subcommand
-- -- |]

-- -- getArgOrExit = getArgOrExitWith patterns

-- -- main :: IO ()
-- -- main = do
-- --   args <- parseArgsOrExit patterns =<< getArgs
-- --
-- --   when (args `isPresent` command "preparse") $ do
-- --     input  <- args `getArgOrExit` argument "input"
-- --     output <- args `getArgOrExit` argument "output"
-- --     preparseXML input output
-- --     exitSuccess
-- --
-- --   exitFailure
