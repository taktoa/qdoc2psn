{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module QDoc.Simple where

import           QDoc.Helpers

import           Data.Text          (Text)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T

import           Data.Map           (Map)
import qualified Data.Map           as Map

import           Data.Set           (Set)
import qualified Data.Set           as Set

import           QDoc.Helpers
import qualified QDoc.Parser        as P

newtype QSTopLevel = QSTopLevel (Map QualifiedName QSDecl)
                   deriving (Eq, Show)

data QualifiedName
  = QualifiedName
    { _namespace :: [Text]
    , _name      :: Text
    } deriving (Eq, Show)


data QSMeta
  = QSMeta
    { _position :: P.QXPosition
    } deriving (Eq, Show)

data QSDecl
  = QSVariable
    { _type :: QSType
    , _meta :: QSMeta
    }
  | QSMethod
    { _type :: QSType
    , _meta :: QSMeta
    }
  | QSProcedure
    { _type :: QSType
    , _meta :: QSMeta
    }
  deriving (Eq, Show)

data QSType
  = QSTypeFunction [QSType]
  deriving (Eq, Show)
