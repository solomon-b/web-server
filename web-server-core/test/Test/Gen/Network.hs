{-# LANGUAGE ApplicativeDo #-}

module Test.Gen.Network where

--------------------------------------------------------------------------------

import Data.Functor ((<&>))
import Data.IP
import Data.Word (Word32)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

genW32 :: (MonadGen m) => m Word32
genW32 = Gen.word32 (Range.linear minBound maxBound)

genIP4 :: (MonadGen m) => m IPv4
genIP4 = toIPv4w <$> genW32

genIP6 :: (MonadGen m) => m IPv6
genIP6 = do
  w1 <- genW32
  w2 <- genW32
  w3 <- genW32
  w4 <- genW32
  pure $ toIPv6w (w1, w2, w3, w4)

genIP :: (MonadGen m) => m IP
genIP = Gen.choice [IPv4 <$> genIP4, IPv6 <$> genIP6]

genNetAddrIP :: (MonadGen m) => m IPRange
genNetAddrIP =
  genIP >>= \case
    (IPv4 ip) ->
      Gen.int (Range.linear 0 32)
        <&> \mask -> IPv4Range $ makeAddrRange ip mask
    (IPv6 ip) ->
      Gen.int (Range.linear 0 128)
        <&> \mask -> IPv6Range $ makeAddrRange ip mask
