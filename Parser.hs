#!/usr/bin/env runhaskell
-- -*- coding: utf-8; mode: haskell; -*-

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Control.Applicative
import           Control.Monad
import           Data.Char                    (isSpace)
import           Data.Functor
import           Data.Maybe
import           Data.Monoid

import           Text.PrettyPrint.ANSI.Leijen hiding (empty, text, (<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Control.Monad.State.Lazy

import           Path
import           Path.IO

import           Safe                         (readMay)

import           Control.Monad.Catch

import           GHC.Stack

myError :: (HasCallStack) => String -> a
myError = error

choice :: (Alternative f) => [f a] -> f a
choice = foldr (<|>) empty

mapInitLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapInitLast _ _ []     = []
mapInitLast _ g [x]    = [g x]
mapInitLast f g (x:xs) = f x : mapInitLast f g xs

splitOnCommas :: Text -> [Text]
splitOnCommas = T.split (== ',')

treadM :: (MonadThrow m, Read r) => Text -> m r
treadM txt = case (txt |> T.unpack |> readMay)
             of Just r  -> pure r
                Nothing -> ["failed to read: ", txt]
                           |> mconcat |> T.unpack |> myError

tshow :: (Show s) => s -> Text
tshow = show .> T.pack

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

data QXVersion
  = QXVersion { _components :: [Int] }
  deriving (Eq, Show)

data QXThreadSafety
  = QXTS_NonReentrant
  | QXTS_Reentrant
  | QXTS_ThreadSafe
  | QXTS_Unspecified
  deriving (Eq, Show)

data QXAccessLevel
  = QXAL_Private
  | QXAL_Protected
  | QXAL_Public
  deriving (Eq, Show)

data QXStatus
  = QXS_Obsolete
  | QXS_Internal
  | QXS_Active
  | QXS_Preliminary
  deriving (Eq, Show)

data QXVirtual
  = QXV_NonVirtual
  | QXV_Virtual
  | QXV_Pure
  deriving (Eq, Show)

data QXPageSubtype
  = QXPS_Example
  | QXPS_ExternalPage
  | QXPS_Header
  | QXPS_Page
  deriving (Eq, Show)

data QXFunctionType
  = QXFT_Constructor
  | QXFT_Destructor
  | QXFT_CopyAssign
  | QXFT_CopyConstructor
  | QXFT_MacroWithoutParams
  | QXFT_MacroWithParams
  | QXFT_Signal
  | QXFT_Slot
  | QXFT_Plain
  deriving (Eq, Show)

data CxxType
  = CxxType { _text :: Text }
  deriving (Eq, Show)

data CxxValue
  = CxxValue { _text :: Text }
  deriving (Eq, Show)

data CxxStubType
  = CST_Normal
  | CST_Delete
  | CST_Default
  deriving (Eq, Show)

data QXPosition
  = QXPosition
  { _lineno   :: Maybe Int
  , _filepath :: Maybe (Path Abs File)
  , _location :: Maybe Text
  , _href     :: Maybe Text
  } deriving (Eq, Show)

data QXDecl
  = QXDeclClass    QXClass
  | QXDeclTypedef  QXTypedef
  | QXDeclEnum     QXEnum
  | QXDeclVariable QXVariable
  | QXDeclFunction QXFunction
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXKeyword
  = QXKeyword
  { _name  :: Text
  , _title :: Text
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXContents
  = QXContents
  { _name  :: Text
  , _title :: Text
  , _level :: Int
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXTypedef
  = QXTypedef

  -- Metadata

  { _name     :: Text
  , _fullname :: Maybe Text

  , _position :: QXPosition

  , _status   :: QXStatus
  , _since    :: QXVersion

  -- C++-related

  , _access   :: QXAccessLevel
  , _tsafety  :: QXThreadSafety
  , _enum     :: Text
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXEnum
  = QXEnum

  -- Metadata

  { _name     :: Text
  , _fullname :: Maybe Text

  , _position :: QXPosition

  , _status   :: QXStatus
  , _since    :: QXVersion

  -- C++-related

  , _access   :: QXAccessLevel
  , _tsafety  :: QXThreadSafety
  , _typedef  :: Text
  , _values   :: Map Text CxxValue
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXVariable
  = QXVariable

  -- Metadata

  { _name     :: Text
  , _fullname :: Maybe Text

  , _position :: QXPosition

  , _brief    :: Text
  , _status   :: QXStatus

  -- C++-related

  , _access   :: QXAccessLevel
  , _tsafety  :: QXThreadSafety
  , _static   :: Bool
  , _type     :: CxxType
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXProperty
  = QXProperty

    -- Metadata

  { _name      :: Text
  , _fullname  :: Maybe Text

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
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXParameter
  = QXParameter
  { _name    :: Text
  , _type    :: CxxType
  , _default :: CxxValue
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXFunction
  = QXFunction

  -- Metadata

  { _name      :: Text
  , _fullname  :: Maybe Text

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
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXClass
  = QXClass

  -- Metadata

  { _name     :: Text
  , _fullname :: Maybe Text

  , _position :: QXPosition

  , _module   :: Maybe Text
  , _groups   :: [Text]

  , _brief    :: Text
  , _status   :: QXStatus
  , _since    :: QXVersion

  -- C++-related

  , _access   :: QXAccessLevel
  , _tsafety  :: QXThreadSafety

  , _bases    :: Text
  , _children :: [QXDecl]
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXNamespace
  = QXNamespace

  -- Metadata

  { _name     :: Text
  , _fullname :: Maybe Text

  , _position :: QXPosition

  , _module   :: Maybe Text

  , _brief    :: Text
  , _status   :: QXStatus

  -- C++-related

  , _access   :: QXAccessLevel
  , _tsafety  :: QXThreadSafety
  , _children :: [QXDecl]
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data QXIndex
  = QXIndex
  { _project  :: Text
  , _url      :: Text
  , _version  :: Text
  , _title    :: Text
  , _children :: [QXNamespace]
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

parseQXBool :: (MonadThrow m, HasCallStack) => Text -> Element -> m Bool
parseQXBool attr el = do
  bl <- lookupAttrib attr el
  case bl of "true"  -> pure True
             "false" -> pure False
             owise   -> ["unknown boolean: ", owise]
                        |> mconcat |> T.unpack |> myError


parseQXPosition :: (MonadThrow m, HasCallStack) => Element -> m QXPosition
parseQXPosition el = do
  let toPath = T.unpack .> parseAbsFile
  _lineno   <- lookupOptAttrib "lineno"   el >>= fmap treadM .> sequenceA
  _filepath <- lookupOptAttrib "filepath" el >>= fmap toPath .> sequenceA
  _location <- lookupOptAttrib "location" el
  _href     <- lookupOptAttrib "href"     el
  pure $ QXPosition {..}

parseQXStatus :: (MonadThrow m, HasCallStack) => Element -> m QXStatus
parseQXStatus el = do
  status <- lookupAttrib "status" el
  case status of "obsolete"    -> pure QXS_Obsolete
                 "internal"    -> pure QXS_Internal
                 "active"      -> pure QXS_Active
                 "preliminary" -> pure QXS_Preliminary
                 owise         -> ["unknown status: ", owise]
                                  |> mconcat |> T.unpack |> myError

parseQXSince :: (MonadThrow m, HasCallStack) => Element -> m QXVersion
parseQXSince el = lookupAttrib "since" el
                  >>= T.replace "," "."
                  .>  T.filter (`elem` ('.' : ['0'..'9']))
                  .>  T.split (== '.')
                  .>  mapM treadM
                  .>  fmap QXVersion

parseQXAccessLevel :: (MonadThrow m, HasCallStack) => Element -> m QXAccessLevel
parseQXAccessLevel el = do
  ts <- lookupAttrib "access" el
  case ts of "private"   -> pure QXAL_Private
             "protected" -> pure QXAL_Protected
             "public"    -> pure QXAL_Public
             owise       -> ["unknown access: ", owise]
                            |> mconcat |> T.unpack |> myError

parseQXThreadSafety :: (MonadThrow m, HasCallStack) => Element -> m QXThreadSafety
parseQXThreadSafety el = do
  ts <- lookupAttrib "threadsafety" el
  case ts of "non-reentrant" -> pure QXTS_NonReentrant
             "reentrant"     -> pure QXTS_Reentrant
             "thread safe"   -> pure QXTS_ThreadSafe
             "unspecified"   -> pure QXTS_Unspecified
             owise           -> ["unknown threadsafety: ", owise]
                                |> mconcat |> T.unpack |> myError


parseQXType :: (MonadThrow m, HasCallStack) => Text -> Element -> m CxxType
parseQXType attr el = el |> lookupAttrib attr |> fmap CxxType

parseQXStubType :: (MonadThrow m, HasCallStack) => Element -> m CxxStubType
parseQXStubType el = case success
                     of Just s  -> pure s
                        Nothing -> ["failed to parse stub type: ", tshow el]
                                   |> mconcat |> T.unpack |> myError
  where
    success :: Maybe CxxStubType
    success = [ (do True <- parseQXBool "delete" el
                    pure CST_Delete)
              , (do True <- parseQXBool "default" el
                    pure CST_Default)
              , pure CST_Normal
              ] |> choice

parseQXOverload :: (MonadThrow m, HasCallStack) => Element -> m (Maybe Int)
parseQXOverload el = do
  ov <- parseQXBool "overload" el
  if ov
    then treadM <$> lookupAttrib "overload-number" el
    else pure Nothing

parseQXFunctionType :: (MonadThrow m, HasCallStack) => Element -> m QXFunctionType
parseQXFunctionType el = do
  ts <- lookupAttrib "meta" el
  case ts of "constructor"        -> pure QXFT_Constructor
             "destructor"         -> pure QXFT_Destructor
             "copy-assign"        -> pure QXFT_CopyAssign
             "copy-constructor"   -> pure QXFT_CopyConstructor
             "macrowithoutparams" -> pure QXFT_MacroWithoutParams
             "macrowithparams"    -> pure QXFT_MacroWithParams
             "signal"             -> pure QXFT_Signal
             "slot"               -> pure QXFT_Slot
             "plain"              -> pure QXFT_Plain
             owise                -> ["unknown function type: ", owise]
                                     |> mconcat |> T.unpack |> myError


parseQXVirtual :: (MonadThrow m, HasCallStack) => Element -> m QXVirtual
parseQXVirtual el = do
  ts <- lookupAttrib "virtual" el
  case ts of "non"     -> pure QXV_NonVirtual
             "virtual" -> pure QXV_Virtual
             "pure"    -> pure QXV_Pure
             owise     -> ["unknown virtual: ", owise]
                          |> mconcat |> T.unpack |> myError

parseQXParameters :: (MonadThrow m, HasCallStack) => Element -> m [QXParameter]
parseQXParameters el = pure mempty -- FIXME
  where
    parseQXParameter :: Element -> QXParameter
    parseQXParameter = undefined

parseQXValues :: (MonadThrow m, HasCallStack) => Element -> m (Map Text CxxValue)
parseQXValues el = pure mempty -- FIXME
  where
    parseQXValue :: Element -> (Text, CxxValue)
    parseQXValue = undefined

parseQXClass :: (MonadThrow m, HasCallStack) => Element -> m QXClass
parseQXClass el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupOptAttrib "fullname"
  _position  <- el |> parseQXPosition
  _module    <- el |> lookupOptAttrib "module"
  _groups    <- el |> lookupAttrib "groups" |> fmap splitOnCommas
  _brief     <- el |> lookupAttrib "brief"
  _status    <- el |> parseQXStatus
  _since     <- el |> parseQXSince
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _bases     <- el |> lookupAttrib "bases"
  _children  <- el |> elChildren |> mapM parseQXDecl
  pure $ QXClass {..}

parseQXTypedef :: (MonadThrow m, HasCallStack) => Element -> m QXTypedef
parseQXTypedef el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupOptAttrib "fullname"
  _position  <- el |> parseQXPosition
  _status    <- el |> parseQXStatus
  _since     <- el |> parseQXSince
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _enum      <- lookupAttrib "enum" el
  _children  <- el |> elChildren |> mapM parseQXDecl
  pure $ QXTypedef {..}

parseQXEnum :: (MonadThrow m, HasCallStack) => Element -> m QXEnum
parseQXEnum el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupOptAttrib "fullname"
  _position  <- el |> parseQXPosition
  _brief     <- el |> lookupAttrib "brief"
  _status    <- el |> parseQXStatus
  _since     <- el |> parseQXSince
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _typedef   <- el |> lookupAttrib "typedef"
  _values    <- el |> parseQXValues
  pure $ QXEnum {..}

parseQXVariable :: (MonadThrow m, HasCallStack) => Element -> m QXVariable
parseQXVariable el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupOptAttrib "fullname"
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

parseQXFunction :: (MonadThrow m, HasCallStack) => Element -> m QXFunction
parseQXFunction el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupOptAttrib "fullname"
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

parseQXDecl :: (MonadThrow m, HasCallStack) => Element -> m QXDecl
parseQXDecl el = case el |> elName |> qName
                 of "class"    -> QXDeclClass    <$> parseQXClass    el
                    "typedef"  -> QXDeclTypedef  <$> parseQXTypedef  el
                    "enum"     -> QXDeclEnum     <$> parseQXEnum     el
                    "variable" -> QXDeclVariable <$> parseQXVariable el
                    "function" -> QXDeclFunction <$> parseQXFunction el
                    owise      -> myError $ "unknown declaration: " <> owise

parseQXNamespace :: (MonadThrow m, HasCallStack) => Element -> m QXNamespace
parseQXNamespace el = do
  let ename = el |> elName |> qName
  when (ename /= "namespace")
    $ myError $ mconcat $ ["tag name is not \"namespace\": ", ename]

  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupOptAttrib "fullname"
  _position  <- el |> parseQXPosition
  _module    <- el |> lookupOptAttrib "module"
  _brief     <- el |> lookupAttrib "brief"
  _status    <- el |> parseQXStatus
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _children  <- el |> elChildren |> mapM parseQXDecl
  pure $ QXNamespace {..}

parseQXIndex :: (MonadThrow m, HasCallStack) => [Content] -> m QXIndex
parseQXIndex xml = do
  let el = xml |> onlyElems |> (!! 1)

  let ename = el |> elName |> qName
  when (ename /= "INDEX")
    $ myError $ mconcat $ ["tag name is not \"INDEX\": ", ename]

  _project  <- el |> lookupAttrib "project"
  _url      <- el |> lookupAttrib "url"
  _version  <- el |> lookupAttrib "version"
  _title    <- el |> lookupAttrib "title"
  _children <- el |> elChildren |> mapM parseQXNamespace
  pure $ QXIndex {..}

lookupAttrib :: (MonadThrow m, HasCallStack) => Text -> Element -> m Text
lookupAttrib aname (el@(Element { elAttribs })) = inspected
  where
    inspected = case filtered
                of [(_, v)] -> pure (T.pack v)
                   []       -> ["attribute not found: ", aname, " in \n\n",
                                prettyEl el]
                               |> mconcat |> T.unpack |> myError
                   owise    -> ["duplicated attributes: ", tshow owise]
                               |> mconcat |> T.unpack |> myError

    filtered = elAttribs
               |> fmap (\a -> (a |> attrKey |> qName |> T.pack, a |> attrVal))
               |> filter (fst .> (== aname))

lookupOptAttrib :: (MonadThrow m, HasCallStack) => Text -> Element -> m (Maybe Text)
lookupOptAttrib aname (el@(Element { elAttribs })) = inspected
  where
    inspected = case filtered
                of [(_, v)] -> pure (Just $ T.pack v)
                   []       -> pure Nothing
                   owise    -> ["duplicated attributes: ", tshow owise]
                               |> mconcat |> T.unpack |> myError

    filtered = elAttribs
               |> fmap (\a -> (a |> attrKey |> qName |> T.pack, a |> attrVal))
               |> filter (fst .> (== aname))

prettyEl :: Element -> Text
prettyEl = pretty .> show .> T.pack

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
               , hang 4 (flatAlt (text "    ") PP.empty <> vsep childrenD)
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

instance Pretty Element where
  pretty = elementToXT .> pretty

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

simple :: IO Element
simple = (!! 1) . onlyElems . parseXML
         <$> T.readFile "./simple.xml"

debug :: IO ()
debug = T.readFile "./simple.xml" >>= parseXML .> parseQXIndex >>= print
-- printSimple :: IO ()
-- printSimple = simple >>= pretty .> putDoc

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
