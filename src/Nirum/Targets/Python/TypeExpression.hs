{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Python.TypeExpression
    ( compileTypeExpression
    , compilePrimitiveType
    ) where

import Data.Text
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.Identifier
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import Nirum.Package.Metadata
import {-# SOURCE #-} Nirum.Targets.Python ()
import Nirum.Targets.Python.CodeGen
import Nirum.TypeInstance.BoundModule

compileTypeExpression :: BoundModule Python
                      -> Maybe TypeExpression
                      -> CodeGen Code
compileTypeExpression mod' (Just (TypeIdentifier i)) =
    case lookupType i mod' of
        Missing -> fail $ "undefined identifier: " ++ toString i
        Imported _ (PrimitiveType p _) -> compilePrimitiveType p
        Imported m _ -> do
            insertThirdPartyImports [(toImportPath target' m, [toClassName i])]
            return $ toClassName i
        Local _ -> return $ toClassName i
  where
    target' :: Python
    target' = target $ metadata $ boundPackage mod'
compileTypeExpression mod' (Just (MapModifier k v)) = do
    kExpr <- compileTypeExpression mod' (Just k)
    vExpr <- compileTypeExpression mod' (Just v)
    typing <- importStandardLibrary "typing"
    return [qq|$typing.Mapping[$kExpr, $vExpr]|]
compileTypeExpression mod' (Just modifier) = do
    expr <- compileTypeExpression mod' (Just typeExpr)
    typing <- importStandardLibrary "typing"
    return [qq|$typing.$className[$expr]|]
  where
    typeExpr :: TypeExpression
    className :: Text
    (typeExpr, className) = case modifier of
        OptionModifier t' -> (t', "Optional")
        SetModifier t' -> (t', "AbstractSet")
        ListModifier t' -> (t', "Sequence")
        TypeIdentifier _ -> undefined  -- never happen!
        MapModifier _ _ -> undefined  -- never happen!
compileTypeExpression _ Nothing =
    return "None"

compilePrimitiveType :: PrimitiveTypeIdentifier -> CodeGen Code
compilePrimitiveType primitiveTypeIdentifier' = do
    pyVer <- getPythonVersion
    case (primitiveTypeIdentifier', pyVer) of
        (Bool, _) -> return "bool"
        (Bigint, _) -> return "int"
        (Decimal, _) -> do
            decimal <- importStandardLibrary "decimal"
            return [qq|$decimal.Decimal|]
        (Int32, Python2) -> do
            numbers <- importStandardLibrary "numbers"
            return [qq|$numbers.Integral|]
        (Int32, Python3) -> return "int"
        (Int64, Python2) -> do
            numbers <- importStandardLibrary "numbers"
            return [qq|$numbers.Integral|]
        (Int64, Python3) -> return "int"
        (Float32, _) -> return "float"
        (Float64, _) -> return "float"
        (Text, Python2) -> return "unicode"
        (Text, Python3) -> return "str"
        (Binary, _) -> return "bytes"
        (Date, _) -> do
            datetime <- importStandardLibrary "datetime"
            return [qq|$datetime.date|]
        (Datetime, _) -> do
            datetime <- importStandardLibrary "datetime"
            return [qq|$datetime.datetime|]
        (Uuid, _) -> do
            uuid <- importStandardLibrary "uuid"
            return [qq|$uuid.UUID|]
        (Uri, Python2) -> return "basestring"
        (Uri, Python3) -> return "str"
