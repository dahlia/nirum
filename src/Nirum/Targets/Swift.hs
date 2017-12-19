{-# LANGUAGE TypeFamilies #-}
module Nirum.Targets.Swift (Swift) where

import Data.Typeable

import Data.Map.Strict
import Data.Text
import Data.Text.Encoding

import Nirum.Package.Metadata

data Swift = Swift
    { packageName :: Text
    } deriving (Eq, Ord, Show, Typeable)

instance Target Swift where
    type CompileResult Swift = Text
    type CompileError Swift = Text

    targetName _ = "swift"
    parseTarget = parseTarget'
    compilePackage = compilePackage'
    showCompileError _ e = e
    toByteString _ = encodeUtf8

parseTarget' :: Table -> Either MetadataError Swift
parseTarget' table = do
    packageName' <- stringField "name" table
    return Swift { packageName = packageName' }

compilePackage' :: Package Swift -> Map FilePath (Either Text Text)
compilePackage' Package { modules = ms } = undefined
