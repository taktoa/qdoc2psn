{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module QDoc.Parser where

import           QDoc.Helpers

import           System.Environment           (getArgs)

import           Data.ByteString              (ByteString)
import           Data.Text                    (Text)

import qualified Data.ByteString              as BS

import qualified Data.ByteString.Lazy         as LBS

import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.IO                 as T

import qualified Data.Text.Lazy               as LT
import qualified Data.Text.Lazy.Builder       as LT
import qualified Data.Text.Lazy.Encoding      as LT
import qualified Data.Text.Lazy.IO            as LT

import           Data.Map                     (Map)
import qualified Data.Map                     as Map

import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Data.Foldable                (asum, toList)

import           Text.XML.Light
import           Text.XML.Light.Cursor        hiding (Path)

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

import           HPath
import           HPath.IO

import           Safe                         (readMay)

import           Control.Monad.Catch

import           GHC.Stack

import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Encode.Pretty     as Aeson

import qualified Codec.Compression.GZip       as GZip
import           Data.Store                   (Store)
import qualified Data.Store                   as Store

text :: Text -> Doc
text = PP.text . T.unpack

deriving instance Generic Attr
deriving instance Generic CData
deriving instance Generic CDataKind
deriving instance Generic QName
deriving instance Generic Element
deriving instance Generic Content

instance Store Attr
instance Store CData
instance Store CDataKind
instance Store QName
instance Store Element
instance Store Content

data QXVersion
  = QXVersion { _components :: [Int] }
  deriving (Eq, Show, Generic)

data QXThreadSafety
  = QXTS_NonReentrant
  | QXTS_Reentrant
  | QXTS_ThreadSafe
  | QXTS_Unspecified
  deriving (Eq, Show, Generic)

data QXAccessLevel
  = QXAL_Private
  | QXAL_Protected
  | QXAL_Public
  deriving (Eq, Show, Generic)

data QXStatus
  = QXS_Obsolete
  | QXS_Internal
  | QXS_Active
  | QXS_Preliminary
  deriving (Eq, Show, Generic)

data QXVirtual
  = QXV_NonVirtual
  | QXV_Virtual
  | QXV_Pure
  deriving (Eq, Show, Generic)

data QXPageSubtype
  = QXPS_Example
  | QXPS_ExternalPage
  | QXPS_Header
  | QXPS_Page
  deriving (Eq, Show, Generic)

data QXFunctionType
  = QXFT_Constructor
  | QXFT_Destructor
  | QXFT_CopyAssign
  | QXFT_CopyConstructor
  | QXFT_MoveAssign
  | QXFT_MoveConstructor
  | QXFT_Macro
  | QXFT_MacroWithoutParams
  | QXFT_MacroWithParams
  | QXFT_Signal
  | QXFT_Slot
  | QXFT_Plain
  deriving (Eq, Show, Generic)

data CxxType
  = CxxType { _text :: Text }
  deriving (Eq, Show, Generic)

data CxxConstExpr
  = CxxConstExpr { _text :: Text }
  deriving (Eq, Show, Generic)

data CxxStubType
  = CST_Normal
  | CST_Delete
  | CST_Default
  deriving (Eq, Show, Generic)

data QXPosition
  = QXPosition
    { _lineno   :: Maybe Int
    , _filepath :: Maybe ByteString
    , _location :: Maybe Text
    , _href     :: Maybe Text
    } deriving (Eq, Show, Generic)

data QXDecl
  = QXDeclNamespace QXNamespace
  | QXDeclClass     QXClass
  | QXDeclTypedef   QXTypedef
  | QXDeclEnum      QXEnum
  | QXDeclVariable  QXVariable
  | QXDeclProperty  QXProperty
  | QXDeclFunction  QXFunction
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data QXTypedef
  = QXTypedef

    -- Metadata

    { _name     :: Text
    , _fullname :: Maybe Text

    , _position :: QXPosition

    , _status   :: QXStatus
    , _since    :: Maybe QXVersion

    -- C++-related

    , _access   :: QXAccessLevel
    , _tsafety  :: QXThreadSafety
    , _enum     :: Text
    } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data QXEnum
  = QXEnum

    -- Metadata

    { _name     :: Text
    , _fullname :: Maybe Text

    , _position :: QXPosition

    , _status   :: QXStatus
    , _since    :: Maybe QXVersion

    -- C++-related

    , _access   :: QXAccessLevel
    , _tsafety  :: QXThreadSafety
    , _typedef  :: Text
    , _values   :: Map Text CxxConstExpr
    } deriving (Eq, Show, Generic)

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
    } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data QXProperty
  = QXProperty

      -- Metadata

    { _name      :: Text
    , _fullname  :: Maybe Text

    , _position  :: QXPosition

    , _brief     :: Text
    , _status    :: QXStatus
    , _since     :: Maybe QXVersion

      -- Qt-related

    , _getters   :: [Text]
    , _setters   :: [Text]
    , _resetters :: [Text]
    , _notifiers :: [Text]

      -- C++-related

    , _access    :: QXAccessLevel
    , _tsafety   :: QXThreadSafety
    , _type      :: CxxType
    } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data QXParameter
  = QXParameter
    { _name    :: Text
    , _type    :: CxxType
    , _default :: CxxConstExpr
    } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data QXFunction
  = QXFunction

    -- Metadata

    { _name      :: Text
    , _fullname  :: Maybe Text

    , _position  :: QXPosition

    , _brief     :: Text
    , _status    :: QXStatus
    , _since     :: Maybe QXVersion

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
    } deriving (Eq, Show, Generic)

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
    , _since    :: Maybe QXVersion

    -- C++-related

    , _access   :: QXAccessLevel
    , _tsafety  :: QXThreadSafety

    , _bases    :: Text
    , _children :: [QXDecl]
    } deriving (Eq, Show, Generic)

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
    } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data QXIndex
  = QXIndex
    { _project  :: Text
    , _url      :: Text
    , _version  :: Text
    , _title    :: Text
    , _children :: [QXDecl]
    } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

instance Aeson.ToJSON QXPosition where
  toJSON (QXPosition {..}) = Aeson.object
                             [ "_lineno"   ~> _lineno
                             , "_filepath" ~> fpToJSON _filepath
                             , "_location" ~> _location
                             , "_href"     ~> _href
                             ]
    where
      fpToJSON = fmap T.decodeUtf8
      (~>) :: (Aeson.ToJSON v, Aeson.KeyValue kv) => Text -> v -> kv
      (~>) = (Aeson..=)

instance Aeson.ToJSON CxxConstExpr
instance Aeson.ToJSON CxxType
instance Aeson.ToJSON CxxStubType
instance Aeson.ToJSON QXVersion
instance Aeson.ToJSON QXThreadSafety
instance Aeson.ToJSON QXAccessLevel
instance Aeson.ToJSON QXStatus
instance Aeson.ToJSON QXVirtual
instance Aeson.ToJSON QXPageSubtype
instance Aeson.ToJSON QXFunctionType
instance Aeson.ToJSON QXDecl
instance Aeson.ToJSON QXTypedef
instance Aeson.ToJSON QXEnum
instance Aeson.ToJSON QXVariable
instance Aeson.ToJSON QXProperty
instance Aeson.ToJSON QXParameter
instance Aeson.ToJSON QXFunction
instance Aeson.ToJSON QXClass
instance Aeson.ToJSON QXNamespace
instance Aeson.ToJSON QXIndex

instance Store CxxConstExpr
instance Store CxxType
instance Store CxxStubType
instance Store QXVersion
instance Store QXThreadSafety
instance Store QXAccessLevel
instance Store QXStatus
instance Store QXVirtual
instance Store QXPageSubtype
instance Store QXFunctionType
instance Store QXPosition
instance Store QXDecl
instance Store QXTypedef
instance Store QXEnum
instance Store QXVariable
instance Store QXProperty
instance Store QXParameter
instance Store QXFunction
instance Store QXClass
instance Store QXNamespace
instance Store QXIndex

parseQXBool :: (MonadThrow m, HasCallStack) => Text -> Element -> m Bool
parseQXBool attr el = do
  bl <- lookupAttrib attr el
  case bl of "true"  -> pure True
             "false" -> pure False
             owise   -> ["unknown boolean: ", owise]
                        |> mconcat |> T.unpack |> myError

parseQXPosition :: (MonadThrow m, HasCallStack) => Element -> m QXPosition
parseQXPosition el = do
  let toPath = T.encodeUtf8 .> pure
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

parseQXSince :: (MonadThrow m, HasCallStack) => Element -> m (Maybe QXVersion)
parseQXSince el = do
  let allowed = Set.fromList ('.' : ['0'..'9'])
  let isAllowed x = Set.member x allowed
  ms <- lookupOptAttrib "since" el
  case ms of Just s  -> s |> T.replace "," "."
                          |> T.filter isAllowed
                          |> T.split (== '.')
                          |> mapM treadM
                          |> fmap QXVersion
                          |> pure
             Nothing -> pure Nothing

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
              ] |> asum

parseQXOverload :: (MonadThrow m, HasCallStack) => Element -> m (Maybe Int)
parseQXOverload el = do
  ov <- parseQXBool "overload" el
  if ov
    then (lookupAttrib "overload-number" el >>= treadM) |> fmap pure
    else pure Nothing

parseQXFunctionType :: (MonadThrow m, HasCallStack) => Element -> m QXFunctionType
parseQXFunctionType el = do
  ts <- lookupAttrib "meta" el
  case ts of "constructor"        -> pure QXFT_Constructor
             "destructor"         -> pure QXFT_Destructor
             "copy-assign"        -> pure QXFT_CopyAssign
             "copy-constructor"   -> pure QXFT_CopyConstructor
             "move-assign"        -> pure QXFT_MoveAssign
             "move-constructor"   -> pure QXFT_MoveConstructor
             "macro"              -> pure QXFT_Macro
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

parseQXPropertyChildren :: (MonadThrow m, HasCallStack)
                        => Text -> Element -> m [Text]
parseQXPropertyChildren expected = elChildren
                                   .> mapMaybe parseChild
                                   .> pure
  where
    parseChild :: Element -> Maybe Text
    parseChild el = do
      let ename = el |> elName |> qName |> T.pack
      guard $ ename == expected
      el |> lookupAttrib "name"

parseQXParameters :: (MonadThrow m, HasCallStack) => Element -> m [QXParameter]
parseQXParameters = elChildren .> mapMaybe parseQXParameter .> pure
  where
    parseQXParameter :: Element -> Maybe QXParameter
    parseQXParameter el = do
      let ename = el |> elName |> qName |> T.pack
      guard $ ename == "parameter"
      _name    <- el |> lookupAttrib "name"
      _type    <- el |> lookupAttrib "left"    |> fmap CxxType
      _default <- el |> lookupAttrib "default" |> fmap CxxConstExpr
      pure $ QXParameter {..}

parseQXValues :: (MonadThrow m, HasCallStack)
              => Element -> m (Map Text CxxConstExpr)
parseQXValues = elChildren .> mapMaybe parseQXValue .> Map.fromList .> pure
  where
    parseQXValue :: Element -> Maybe (Text, CxxConstExpr)
    parseQXValue el = do
      let ename = el |> elName |> qName |> T.pack
      guard $ ename == "value"
      name  <- el |> lookupAttrib "name"
      value <- el |> lookupAttrib "value"
      pure (name, CxxConstExpr value)

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
  _children  <- el |> parseQXChildren
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
  pure $ QXVariable {..}

parseQXProperty :: (MonadThrow m, HasCallStack) => Element -> m QXProperty
parseQXProperty el = do
  _name      <- el |> lookupAttrib "name"
  _fullname  <- el |> lookupOptAttrib "name"
  _position  <- el |> parseQXPosition
  _brief     <- el |> lookupAttrib "brief"
  _status    <- el |> parseQXStatus
  _since     <- el |> parseQXSince
  _getters   <- el |> parseQXPropertyChildren "getter"
  _setters   <- el |> parseQXPropertyChildren "setter"
  _resetters <- el |> parseQXPropertyChildren "resetter"
  _notifiers <- el |> parseQXPropertyChildren "notifier"
  _access    <- el |> parseQXAccessLevel
  _tsafety   <- el |> parseQXThreadSafety
  _type      <- el |> parseQXType "type"
  pure $ QXProperty {..}

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
  _children  <- el |> parseQXChildren
  pure $ QXNamespace {..}

parseQXDecl :: (MonadThrow m, HasCallStack) => Element -> m (Maybe QXDecl)
parseQXDecl el = case el |> elName |> qName of
  "class"     -> el |> parseQXClass     |> fmap (QXDeclClass     .> Just)
  "typedef"   -> el |> parseQXTypedef   |> fmap (QXDeclTypedef   .> Just)
  "enum"      -> el |> parseQXEnum      |> fmap (QXDeclEnum      .> Just)
  "variable"  -> el |> parseQXVariable  |> fmap (QXDeclVariable  .> Just)
  "property"  -> el |> parseQXProperty  |> fmap (QXDeclProperty  .> Just)
  "function"  -> el |> parseQXFunction  |> fmap (QXDeclFunction  .> Just)
  "namespace" -> el |> parseQXNamespace |> fmap (QXDeclNamespace .> Just)
  "page"      -> pure Nothing
  "group"     -> pure Nothing
  "module"    -> pure Nothing
  "target"    -> pure Nothing
  "keyword"   -> pure Nothing
  "contents"  -> pure Nothing
  owise       -> myError $ "unknown declaration: " <> owise

parseQXChildren :: (MonadThrow m, HasCallStack) => Element -> m [QXDecl]
parseQXChildren = elChildren .> mapM parseQXDecl .> fmap catMaybes

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
  _children <- el |> parseQXChildren
  pure $ QXIndex {..}

lookupAttrib :: (MonadThrow m, HasCallStack) => Text -> Element -> m Text
lookupAttrib aname (el@(Element { elAttribs })) = inspected
  where
    inspected = case filtered
                of [(_, v)] -> pure (T.pack v)
                   []       -> pure ""
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

searchIndex :: QXIndex -> Text -> [QXDecl]
searchIndex = undefined

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

recurseXML :: (Element -> Bool) -> Element -> [Element]
recurseXML predicate = helper
  where
    helper :: Element -> [Element]
    helper el = concatMap helper (elChildren el)
                |> (if predicate el then (el :) else id)

preparseXML :: FilePath -> FilePath -> IO ()
preparseXML input output = do
  xml <- parseXML <$> T.readFile input
  xml |> Store.encode |> LBS.fromStrict |> GZip.compress |> LBS.writeFile output

preparseQX :: FilePath -> FilePath -> IO ()
preparseQX input output = do
  xml <- parseXML <$> T.readFile input
  qx <- parseQXIndex xml
  qx |> Store.encode |> LBS.fromStrict |> GZip.compress |> LBS.writeFile output

getPreparsedXML :: IO [Content]
getPreparsedXML = LBS.readFile "./data/debug.dat.gz"
                  >>= GZip.decompress
                  .> LBS.toStrict
                  .> Store.decodeIO

getPreparsedQX :: IO QXIndex
getPreparsedQX = LBS.readFile "./data/debug.qx.gz"
                 >>= GZip.decompress .> LBS.toStrict .> Store.decodeIO

simple :: IO Element
simple = (!! 1) . onlyElems . parseXML
         <$> T.readFile "./data/simple.xml"

debug :: IO QXIndex
debug = T.readFile "./data/simple.xml" >>= parseXML .> parseQXIndex
-- printSimple :: IO ()
-- printSimple = simple >>= pretty .> putDoc

main :: IO ()
main = getPreparsedQX >>= print

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
