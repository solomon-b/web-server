{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Gen.Time where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Data.Time
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

futureTimeGen :: (MonadIO m, MonadGen m) => m UTCTime
futureTimeGen = do
  now <- liftIO getCurrentTime
  secondsForward :: Integer <- Gen.integral_ (Range.linear 0 1_000_000)
  let timeInFuture = secondsToNominalDiffTime $ 60 * 60 + fromIntegral secondsForward
  pure $ addUTCTime timeInFuture now

pastTimeGen :: (MonadIO m, MonadGen m) => m UTCTime
pastTimeGen = do
  now <- liftIO getCurrentTime
  secondsForward :: Integer <- Gen.integral_ (Range.linear 0 1_000_000)
  let timeInFuture = secondsToNominalDiffTime $ negate (60 * 60) - fromIntegral secondsForward
  pure $ addUTCTime timeInFuture now
