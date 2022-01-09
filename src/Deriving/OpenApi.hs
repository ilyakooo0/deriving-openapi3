{-# OPTIONS_GHC -Wno-orphans #-}

module Deriving.OpenApi
  ( CustomOpenApi,
    DatatypeNameModifier,
    ToSchema,
    module Deriving.Aeson,
    Extending,
  )
where

import Control.Lens
import qualified Data.Aeson.Types as A
import Data.Kind
import Data.OpenApi
import Data.OpenApi.Internal.Schema
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Deriving.Aeson
import GHC.Generics
import GHC.TypeLits
import Servant.API

type CustomOpenApi = CustomJSON

instance (OpenApiOptions xs, GToSchema (Rep x), Generic x, Typeable x, Typeable xs) => ToSchema (CustomJSON xs x) where
  declareNamedSchema Proxy =
    openApiSchemaModifier @xs <$> genericDeclareNamedSchema (openApiOptions @xs) (Proxy @x)

data DatatypeNameModifier t

instance (StringModifier f, OpenApiOptions xs) => OpenApiOptions (DatatypeNameModifier f ': xs) where
  openApiOptions = (openApiOptions @xs) {datatypeNameModifier = getStringModifier @f}

instance (AesonOptions xs) => AesonOptions (DatatypeNameModifier f ': xs) where
  aesonOptions = aesonOptions @xs

instance (AesonOptions xs) => AesonOptions (Description f ': xs) where
  aesonOptions = aesonOptions @xs

instance (OpenApiOptions xs, KnownSymbol t) => OpenApiOptions (Description t ': xs) where
  openApiOptions = openApiOptions @xs
  openApiSchemaModifier = schema . description <>~ Just (toTextLine @t)

toTextLine :: forall s. KnownSymbol s => Text
toTextLine = "\n\n" <> T.pack (symbolVal (Proxy @s))

type family (++) (x :: [k]) (y :: [k]) :: [k] where
  (++) '[] ys = ys
  (++) (x ': xs) ys = xs ++ (x ': ys)

type family Extending (c :: * -> *) (ee :: [*]) :: * -> * where
  Extending (CustomJSON a) ee = CustomJSON (a ++ ee)

-- deriving-aeson-based instances

class OpenApiOptions (xs :: [Type]) where
  openApiOptions :: SchemaOptions
  openApiSchemaModifier :: NamedSchema -> NamedSchema
  openApiSchemaModifier = id

instance OpenApiOptions '[] where
  openApiOptions = defaultSchemaOptions

instance OpenApiOptions xs => OpenApiOptions (UnwrapUnaryRecords ': xs) where
  openApiOptions = (openApiOptions @xs) {unwrapUnaryRecords = True}

instance OpenApiOptions xs => OpenApiOptions (OmitNothingFields ': xs) where
  openApiOptions = openApiOptions @xs

instance OpenApiOptions xs => OpenApiOptions (RejectUnknownFields ': xs) where
  openApiOptions = openApiOptions @xs
  openApiSchemaModifier = schema . additionalProperties .~ Just (AdditionalPropertiesAllowed False)

instance (StringModifier f, OpenApiOptions xs) => OpenApiOptions (FieldLabelModifier f ': xs) where
  openApiOptions = (openApiOptions @xs) {fieldLabelModifier = getStringModifier @f}

instance (StringModifier f, OpenApiOptions xs) => OpenApiOptions (ConstructorTagModifier f ': xs) where
  openApiOptions = (openApiOptions @xs) {constructorTagModifier = getStringModifier @f}

instance
  (OpenApiOptions xs, TypeError ('Text "openapi3-deriving does not currently the `TagSingleConstructors` modifier.")) =>
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
