module FunctorsExamples where

import Data.Time.Clock

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset = addUTCTime (offset * 24 * 3600) <$> getCurrentTime

