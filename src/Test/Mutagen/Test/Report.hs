module Test.Mutagen.Test.Report where

import Test.Mutagen.Property

----------------------------------------

-- | Testing results
data Report
  = Success
      { numPassed :: Int
      , numDiscarded :: Int
      }
  | Counterexample
      { numPassed :: Int
      , numDiscarded :: Int
      , failingArgs :: Args
      }
  | GaveUp
      { numPassed :: Int
      , numDiscarded :: Int
      , why :: String
      }
  | NoExpectedFailure
      { numPassed :: Int
      , numDiscarded :: Int
      }
  deriving (Show)

isSuccess :: Report -> Bool
isSuccess (Success{}) = True
isSuccess _ = False
