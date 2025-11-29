{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Types.OptionalField
  ( OptionalField (..),
    flatten,
  )
where

--------------------------------------------------------------------------------

import Servant qualified

--------------------------------------------------------------------------------

newtype OptionalField a = OptionalField {getOptionalField :: Maybe a}

instance forall a. (Servant.FromHttpApiData a) => Servant.FromHttpApiData (OptionalField a) where
  parseQueryParam "" = Right (OptionalField Nothing)
  parseQueryParam val = OptionalField . Just <$> Servant.parseQueryParam @a val

instance (Servant.ToHttpApiData a) => Servant.ToHttpApiData (OptionalField a) where
  toQueryParam (OptionalField Nothing) = mempty
  toQueryParam (OptionalField (Just val)) = Servant.toQueryParam val

flatten :: Maybe (OptionalField a) -> Maybe a
flatten opt = opt >>= getOptionalField
