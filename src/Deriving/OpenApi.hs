{-# OPTIONS_GHC -Wno-orphans #-}

module Deriving.OpenApi
  ( CustomOpenApi,
    DatatypeNameModifier,
    ToSchema,
    module Deriving.Aeson,
  )
where

import qualified Data.Aeson.Types as A
import Data.Kind
import Data.OpenApi
import Data.OpenApi.Internal.Schema
import Data.Proxy
import Deriving.Aeson
import GHC.Generics
import GHC.TypeLits

type CustomOpenApi = CustomJSON

instance (OpenApiOptions xs, GToSchema (Rep x), Generic x) => ToSchema (CustomJSON xs x) where
  declareNamedSchema Proxy = genericDeclareNamedSchema (openApiOptions @xs) (Proxy @x)

data DatatypeNameModifier t

instance (StringModifier f, OpenApiOptions xs) => OpenApiOptions (DatatypeNameModifier f ': xs) where
  openApiOptions = (openApiOptions @xs) {datatypeNameModifier = getStringModifier @f}

-- deriving-aeson-based instances

class OpenApiOptions (xs :: [Type]) where
  openApiOptions :: SchemaOptions

instance OpenApiOptions '[] where
  openApiOptions = defaultSchemaOptions

instance OpenApiOptions xs => OpenApiOptions (UnwrapUnaryRecords ': xs) where
  openApiOptions = (openApiOptions @xs) {unwrapUnaryRecords = True}

instance OpenApiOptions xs => OpenApiOptions (OmitNothingFields ': xs) where
  openApiOptions = openApiOptions @xs

instance OpenApiOptions xs => OpenApiOptions (RejectUnknownFields ': xs) where
  openApiOptions = openApiOptions @xs

instance (StringModifier f, OpenApiOptions xs) => OpenApiOptions (FieldLabelModifier f ': xs) where
  openApiOptions = (openApiOptions @xs) {fieldLabelModifier = getStringModifier @f}

instance (StringModifier f, OpenApiOptions xs) => OpenApiOptions (ConstructorTagModifier f ': xs) where
  openApiOptions = (openApiOptions @xs) {constructorTagModifier = getStringModifier @f}

instance
  (OpenApiOptions xs, TypeError ( 'Text "openapi3-deriving does not currently the `TagSingleConstructors` modifier.")) =>
  OpenApiOptions (TagSingleConstructors ': xs)
  where
  openApiOptions = undefined

instance OpenApiOptions xs => OpenApiOptions (NoAllNullaryToStringTag ': xs) where
  openApiOptions = (openApiOptions @xs) {allNullaryToStringTag = False}

instance (KnownSymbol t, KnownSymbol c, OpenApiOptions xs) => OpenApiOptions (SumTaggedObject t c ': xs) where
  openApiOptions = (openApiOptions @xs) {sumEncoding = A.TaggedObject (symbolVal (Proxy @t)) (symbolVal (Proxy @c))}

instance (OpenApiOptions xs) => OpenApiOptions (SumUntaggedValue ': xs) where
  openApiOptions = (openApiOptions @xs) {sumEncoding = A.UntaggedValue}

instance (OpenApiOptions xs) => OpenApiOptions (SumObjectWithSingleField ': xs) where
  openApiOptions = (openApiOptions @xs) {sumEncoding = A.ObjectWithSingleField}

instance (OpenApiOptions xs) => OpenApiOptions (SumTwoElemArray ': xs) where
  openApiOptions = (openApiOptions @xs) {sumEncoding = A.TwoElemArray}
