{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | __Example:__
--
-- >>> :set -XDerivingStrategies -XDerivingVia -XDeriveGeneric
--
-- >>> :{
-- data User = User
--   { userFirstName :: String,
--     userAge :: Maybe Integer
--   }
--   deriving stock (Generic)
--   deriving (FromJSON, ToJSON, ToSchema)
--     via CustomJSON
--          '[FieldLabelModifier '[StripPrefix "user", CamelToSnake], RejectUnknownFields]
--          User
-- showYaml = Data.ByteString.Char8.putStr . Data.Yaml.encode
-- :}
--
-- >>> showYaml $ toSchema (Proxy :: Proxy User)
-- type: object
-- properties:
--   first_name:
--     type: string
--   age:
--     type: integer
-- required:
-- - first_name
-- additionalProperties: false
module Deriving.OpenApi
  ( CustomOpenApi,
    DatatypeNameModifier,
    ToSchema,
    module Deriving.Aeson,
  )
where

import Control.Lens
import qualified Data.Aeson.Types as A
import Data.OpenApi
import Data.OpenApi.Internal.Schema
import Data.Proxy
import Data.Typeable
import Deriving.Aeson
import GHC.Generics
import GHC.TypeLits

#ifdef SERVANT_DESCRIPTION

import Servant.API
import Data.Text (Text)
import qualified Data.Text as T

instance (AesonOptions xs) => AesonOptions (Description f ': xs) where
  aesonOptions = aesonOptions @xs

instance KnownSymbol t => OpenApiOptionModifier (Description t) where
  openApiSchemaModifier = schema . description <>~ Just (toTextLine @t)

toTextLine :: forall s. KnownSymbol s => Text
toTextLine = "\n\n" <> T.pack (symbolVal (Proxy @s))

#endif

-- $setup
-- >>> import qualified Data.Yaml
-- >>> import qualified Data.ByteString.Char8

type CustomOpenApi = CustomJSON

instance
  (OpenApiOptionModifier xs, GToSchema (Rep x), Generic x, Typeable x, Typeable xs, Typeable k) =>
  ToSchema (CustomJSON (xs :: k) x)
  where
  declareNamedSchema Proxy =
    openApiSchemaModifier @xs
      <$> genericDeclareNamedSchema (openApiOptionsModifier @xs defaultSchemaOptions) (Proxy @x)

class OpenApiOptionModifier x where
  openApiOptionsModifier :: SchemaOptions -> SchemaOptions
  openApiOptionsModifier = id
  openApiSchemaModifier :: NamedSchema -> NamedSchema
  openApiSchemaModifier = id

data DatatypeNameModifier t

instance (StringModifier f) => OpenApiOptionModifier (DatatypeNameModifier f) where
  openApiOptionsModifier o = o {datatypeNameModifier = getStringModifier @f}

instance (AesonOptions xs) => AesonOptions (DatatypeNameModifier f ': xs) where
  aesonOptions = aesonOptions @xs

-- deriving-aeson-based instances

instance OpenApiOptionModifier UnwrapUnaryRecords where
  openApiOptionsModifier o = o {unwrapUnaryRecords = True}

instance OpenApiOptionModifier OmitNothingFields

instance OpenApiOptionModifier RejectUnknownFields where
  openApiSchemaModifier = schema . additionalProperties .~ Just (AdditionalPropertiesAllowed False)

instance StringModifier f => OpenApiOptionModifier (FieldLabelModifier f) where
  openApiOptionsModifier o = o {fieldLabelModifier = getStringModifier @f}

instance StringModifier f => OpenApiOptionModifier (ConstructorTagModifier f) where
  openApiOptionsModifier o = o {constructorTagModifier = getStringModifier @f}

instance
  TypeError ('Text "deriving-openapi3 does not currently the `TagSingleConstructors` modifier.") =>
  OpenApiOptionModifier TagSingleConstructors

instance OpenApiOptionModifier NoAllNullaryToStringTag where
  openApiOptionsModifier o = o {allNullaryToStringTag = False}

instance (KnownSymbol t, KnownSymbol c) => OpenApiOptionModifier (SumTaggedObject t c) where
  openApiOptionsModifier o = o {sumEncoding = A.TaggedObject (symbolVal (Proxy @t)) (symbolVal (Proxy @c))}

instance OpenApiOptionModifier SumUntaggedValue where
  openApiOptionsModifier o = o {sumEncoding = A.UntaggedValue}

instance OpenApiOptionModifier SumObjectWithSingleField where
  openApiOptionsModifier o = o {sumEncoding = A.ObjectWithSingleField}

instance OpenApiOptionModifier SumTwoElemArray where
  openApiOptionsModifier o = o {sumEncoding = A.TwoElemArray}

instance OpenApiOptionModifier '[]

instance (OpenApiOptionModifier x, OpenApiOptionModifier xs) => OpenApiOptionModifier (x ': xs) where
  openApiOptionsModifier = openApiOptionsModifier @xs . openApiOptionsModifier @x
  openApiSchemaModifier = openApiSchemaModifier @xs . openApiSchemaModifier @x
