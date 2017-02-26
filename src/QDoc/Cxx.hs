{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module QDoc.Cxx where

import           QDoc.Helpers

import           Data.Monoid

import           Data.Text                            (Text)
import qualified Data.Text                            as T

import           Language.PureScript.AST.Binders
import           Language.PureScript.AST.Declarations
import           Language.PureScript.AST.Exported
import           Language.PureScript.AST.Literals
import           Language.PureScript.AST.Operators
import           Language.PureScript.AST.SourcePos
import           Language.PureScript.AST.Traversals
import           Language.PureScript.Comments
import           Language.PureScript.Kinds
import           Language.PureScript.Names
import           Language.PureScript.Types

import           Language.PureScript.Externs

-- import Language.PureScript.CodeGen.Cpp.AST
-- import Language.PureScript.CodeGen.Cpp.Types
-- import qualified Language.PureScript.Pretty.Cpp as PP (prettyPrintCpp)

import           Text.PrettyPrint.Boxes               (Box)
import qualified Text.PrettyPrint.Boxes               as Box

import qualified Language.PureScript.Pretty.Kinds     as PP (prettyPrintKind)
import qualified Language.PureScript.Pretty.Types     as PP (prettyPrintType)
import qualified Language.PureScript.Pretty.Values    as PP (prettyPrintValue)
import qualified Language.PureScript.Pretty.Values    as PP (prettyPrintBinder)

-- prettyPrintKind :: Kind -> Box
-- prettyPrintKind = PP.prettyPrintKind .> T.unpack .> Box.text
--
-- prettyPrintType :: Type -> Box
-- prettyPrintType = PP.prettyPrintType .> Box.text
--
-- prettyPrintValue :: Expr -> Box
-- prettyPrintValue = PP.prettyPrintValue 0
--
-- nullSourceSpan :: SourceSpan
-- nullSourceSpan = SourceSpan "" (SourcePos 0 0) (SourcePos 0 0)
--
-- testModule :: Module
-- testModule = Module nullSourceSpan comments name [] Nothing -- (Just [_])
--   where
--     name = moduleNameFromString "Qt.Lmao.FooBar"
--     comments = [LineComment "ayy lmao", BlockComment "blocky"]

testEF :: ExternsFile
testEF = let efVersion      = "5.5"
             efModuleName   = moduleNameFromString "Qt.Lmao.FooBar"
             efExports      = mempty :: [DeclarationRef]
             efImports      = mempty :: [ExternsImport]
             efFixities     = mempty :: [ExternsFixity]
             efTypeFixities = mempty :: [ExternsTypeFixity]
             efDeclarations = mempty :: [ExternsDeclaration]
         in ExternsFile {..}
