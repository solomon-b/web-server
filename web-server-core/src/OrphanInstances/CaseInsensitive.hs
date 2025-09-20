{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.CaseInsensitive where

--------------------------------------------------------------------------------

import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Functor.Contravariant (contramap)
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))

--------------------------------------------------------------------------------

instance (EncodeValue a) => EncodeValue (CI a) where
  encodeValue = contramap CI.original encodeValue

instance (CI.FoldCase a, DecodeValue a) => DecodeValue (CI a) where
  decodeValue = fmap CI.mk decodeValue
