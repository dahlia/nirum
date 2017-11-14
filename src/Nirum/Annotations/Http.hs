module Nirum.Annotations.Http
    (
    ) where

import Data.List.NonEmpty

import Data.Map.Strict
import Data.Text (Text)
import Network.URI.Template
import Network.URI.Template.Types

import Nirum.Constructs.DeclarationSet as DeclarationSet
import Nirum.Constructs.Identifier
import Nirum.Constructs.Name
import Nirum.Constructs.Service
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression

newtype HttpResource = HttpResource
    { pathTemplate :: UriTemplate
    } deriving (Eq, Show)

data ParameterError = UnsupportedModifier Text

flattenParameters :: DeclarationSet Parameter
                  -> Either ParameterError (Map Variable (NonEmpty Identifier))
flattenParameters params =
    case sequence result of
        Left e -> Left e
        Right vars -> Right $ Data.Map.Strict.fromList vars
  where
    flatten :: TypeExpression
            -> Either ParameterError (Variable -> Variable, [Identifier])
    flatten TypeIdentifier { identifier } = undefined
    flatten OptionModifier {} =
        Left $ UnsupportedModifier "Option modifier is unsupported"
    flatten SetModifier {} =
        Left $ UnsupportedModifier "Set modifier is unsupported"
    flatten ListModifier {} =
        Left $ UnsupportedModifier "List modifier is unsupported"
    flatten MapModifier {} =
        Left $ UnsupportedModifier "Map modifier is unsupported"
    result :: [Either ParameterError (Variable, NonEmpty Identifier)]
    result =
        [ case flatten typeExpr of
              Left e -> Left e
              Right (modifyVariable, innerFieldNames) -> Right
                  ( modifyVariable $ Variable (toNormalizedString fname) Normal
                  , fname :| innerFieldNames
                  )
        | Parameter (Name fname _) typeExpr _ <- DeclarationSet.toList params
        ]

-- get-bar (bigint a, uuid b)
--
-- /foo/{a}/bar/{b}
-- /foo/([0-9]+)/bar/([a-f0-9]{32}|[a-f0-9]{9}(?:-[a-f0-9]{4}){3}-[a-f0-9]{12})
--
-- record coord (bigint x, bigint y);
-- get-foo (coord point)
--
-- /foo/{point.x}/{point.y}
-- /foo/([0-9]+)/([0-9]+)
-- 1. 인자에 대한 타입 알아내기
-- 2. 지원 안 하는 타입일 경우 오류 내기
-- 3. 디코더의 목록 반환
--    - 리터럴은 리터럴 디코더
--    - 인자는 인자 타입별 디코더
analyzeParameterTypes :: UriTemplate
                      -> DeclarationSet Parameter
                      -> Map Variable ()
analyzeParameterTypes = []
