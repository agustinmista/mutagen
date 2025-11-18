-- | Testing reports
module Test.Mutagen.Test.Report
  ( -- * Testing reports
    Report (..)
  , isSuccess
  )
where

import Test.Mutagen.Property

{-------------------------------------------------------------------------------
-- * Testing reports
-------------------------------------------------------------------------------}

-- | Testing report
data Report
  = Success
      { numPassed :: Int
      -- ^ Number of passed tests
      , numDiscarded :: Int
      -- ^ Number of discarded tests
      }
  | Counterexample
      { numPassed :: Int
      -- ^ Number of passed tests
      , numDiscarded :: Int
      -- ^ Number of discarded tests
      , failingArgs :: Args
      -- ^ Failing arguments
      }
  | GaveUp
      { numPassed :: Int
      -- ^ Number of passed tests
      , numDiscarded :: Int
      -- ^ Number of discarded tests
      , reason :: String
      -- ^ Reason for giving up
      }
  | NoExpectedFailure
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
