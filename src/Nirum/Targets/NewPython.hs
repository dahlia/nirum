{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, OverloadedLists,
             QuasiQuotes, TypeFamilies, TypeSynonymInstances,
             MultiParamTypeClasses #-}
module Nirum.Targets.NewPython
    ( Python (Python)
    ) where

import Data.Typeable (Typeable)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.SemVer as SV
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.InterpolatedString.Perl6 (qq)

import qualified Nirum.Constructs.ModulePath as MP
import Nirum.Package (Package (Package, metadata))
import qualified Nirum.Package.Metadata as MD
import qualified Nirum.Package.ModuleSet as MS

minimumRuntime :: SV.Version
minimumRuntime = SV.version 0 3 9 [] []

data Python = Python { packageName :: T.Text
                     , minimumRuntimeVersion :: SV.Version
                     , renames :: RenameMap
                     } deriving (Eq, Ord, Show, Typeable)

type RenameMap = M.Map MP.ModulePath MP.ModulePath

type Package' = Package Python
type CompileError' = MD.CompileError Python
type CompileResult' = MD.CompileResult Python

data PythonVersion = Python2 | Python3 deriving (Eq, Ord, Show)

data InstallRequires =
    InstallRequires { dependencies :: S.Set T.Text
                    , optionalDependencies :: M.Map (Int, Int) (S.Set T.Text)
                    } deriving (Eq, Ord, Show)

manifestIn :: Code
manifestIn = [q|recursive-include src *.py
recursive-include src-py2 *.py
|]

compilePackage' :: Package'
                -> M.Map FilePath (Either CompileError' CompileResult')
compilePackage' pkg@Package { metadata = MD.Metadata { MD.target = target' } } =
    M.fromList $
        initFiles ++
        [ ( f
          , case cd of
                Left e -> Left e
                Right (_, cd') -> Right cd'
          )
        | (f, cd) <- modules'
        ] ++
        [ ("MANIFEST.in", Right manifestIn)
        -- , ("setup.py", Right $ compilePackageMetadata pkg installRequires)
        ]
  where
    toPythonFilename :: MP.ModulePath -> [FilePath]
    toPythonFilename mp = [ T.unpack (toAttributeName i)
                          | i <- toList $ renameMP target' mp
                          ] ++ ["__init__.py"]
    versions :: [PythonVersion]
    versions = [Python2, Python3]
    toFilename :: T.Text -> MP.ModulePath -> FilePath
    toFilename sourceRootDirectory mp =
        joinPath $ T.unpack sourceRootDirectory : toPythonFilename mp
    initFiles :: [(FilePath, Either CompileError' CompileResult')]
    initFiles = [ (toFilename (sourceDirectory ver) mp', Right "")
                | mp <- MS.keys (modules package)
                , mp' <- S.elems (hierarchy mp)
                , ver <- versions
                ]
    modules' :: [ (FilePath
                  , Either CompileError' (InstallRequires, CompileResult')
                  )
                ]
    modules' =
        [ ( toFilename (sourceDirectory ver) modulePath'
          , compileModule ver $ Source package boundModule
          )
        | (modulePath', _) <- MS.toAscList (modules package)
        , Just boundModule <- [resolveBoundModule modulePath' package]
        , ver <- versions
        ]
    installRequires :: InstallRequires
    installRequires = foldl unionInstallRequires
                            (InstallRequires [] [])
                            [deps | (_, Right (deps, _)) <- modules']


instance MD.Target Python where
    type CompileResult Python = T.Text
    type CompileError Python = T.Text
    targetName _ = "npython"
    parseTarget table = do
        name' <- MD.stringField "name" table
        minRuntime <- case MD.versionField "minimum_runtime" table of
            Left (MD.FieldError _) -> Right minimumRuntime
            otherwise' -> otherwise'
        renameTable <- case MD.tableField "renames" table of
            Right t -> Right t
            Left (MD.FieldError _) -> Right HM.empty
            otherwise' -> otherwise'
        renamePairs <- sequence
            [ case (MP.fromText k, v) of
                  (Just modulePath', MD.VString v') -> case MP.fromText v' of
                      Just altPath -> Right (modulePath', altPath)
                      Nothing -> Left $ MD.FieldValueError [qq|renames.$k|]
                          [qq|expected a module path, not "$v'"|]
                  (Nothing, _) -> Left $ MD.FieldValueError [qq|renams.$k|]
                      [qq|expected a module path as a key, not "$k"|]
                  _ -> Left $ MD.FieldTypeError [qq|renames.$k|] "string" $
                                                MD.fieldType v
            | (k, v) <- HM.toList renameTable
            ]
        return Python { packageName = name'
                      , minimumRuntimeVersion = max minRuntime minimumRuntime
                      , renames = M.fromList renamePairs
                      }
    compilePackage = compilePackage'
    showCompileError _ e = e
    toByteString _ = encodeUtf8
