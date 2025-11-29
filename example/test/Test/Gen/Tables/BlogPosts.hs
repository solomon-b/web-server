{-# LANGUAGE NumDecimals #-}

module Test.Gen.Tables.BlogPosts where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Data.Ratio ((%))
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Effects.Database.Tables.BlogPosts qualified as UUT
import Effects.Database.Tables.Images qualified as Images
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

blogPostInsertGen :: (MonadIO m, MonadGen m) => m (UUT.Subject, UUT.Body, Bool, Maybe Images.Id)
blogPostInsertGen = do
  iTitle <- UUT.Subject <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  iContent <- UUT.Body <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  pure (iTitle, iContent, False, Nothing)

blogPostUpdateGen :: (MonadIO m, MonadGen m) => m (UUT.Subject, UUT.Body, Maybe UTCTime, Maybe Images.Id)
blogPostUpdateGen = do
  muTitle <- UUT.Subject <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  muContent <- UUT.Body <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  muPublishedAt <- Gen.maybe $ truncateToMicros <$> liftIO getCurrentTime
  pure (muTitle, muContent, muPublishedAt, Nothing)

truncateToMicros :: UTCTime -> UTCTime
truncateToMicros t =
  posixSecondsToUTCTime $
    fromRational $
      (truncate (toRational (utcTimeToPOSIXSeconds t) * 1e6) :: Integer)
        % 1e6
