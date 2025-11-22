-- | Testing reports
module Test.Mutagen.Report
  ( -- * Testing reports
    Report (..)
  , isSuccess
  )
where

import Test.Mutagen.Property (Args)

{-------------------------------------------------------------------------------
-- * Testing reports
-------------------------------------------------------------------------------}

-- | Testing report
data Report
  = -- | The property passed all tests
    Success
      { numPassed :: Int
      -- ^ Number of passed tests
      , numDiscarded :: Int
      -- ^ Number of discarded tests
      , numFailed :: Int
      -- ^ Number of failed tests
      }
  | -- | The property failed for the given arguments
    Counterexample
      { numPassed :: Int
      -- ^ Number of passed tests
      , numDiscarded :: Int
      -- ^ Number of discarded tests
      , failingArgs :: Args
      -- ^ Failing arguments
      }
  | -- | The testing loop gave up before completing all tests
    GaveUp
      { numPassed :: Int
      -- ^ Number of passed tests
      , numDiscarded :: Int
      -- ^ Number of discarded tests
      , reason :: String
      -- ^ Reason for giving up
      }
  | -- | The property was expected to fail, but all tests passed
    NoExpectedFailure
      { numPassed :: Int
      -- ^ Number of passed tests
      , numDiscarded :: Int
      -- ^ Number of discarded tests
      }
  deriving (Show)

-- | Check if a report indicates a successful test run
isSuccess :: Report -> Bool
isSuccess (Success{}) = True
isSuccess _ = False
