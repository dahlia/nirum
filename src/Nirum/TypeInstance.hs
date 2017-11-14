module Nirum.TypeInstance
    ( TypeInstance (..)
    ) where

import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import Nirum.Package.Metadata
import Nirum.TypeInstance.BoundModule

data Target t => TypeInstance t
    = Instance { typename :: Name
               , type' :: Type
               , boundModule :: BoundModule t
               }
    | Optional { instance' :: TypeInstance t }
    | SetOf { elementInstance :: TypeInstance t }
    | ListOf { elementInstance :: TypeInstance t }
    | MapOf { keyInstance :: TypeInstance t
            , valueInstance :: TypeInstance t
            }
    deriving (Eq, Ord, Show)
