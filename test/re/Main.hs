{-# LANGUAGE TypeApplications #-}

-- | Main entry point for regex tests
module Main where

import RE.Match
  ( matches
  , optimize
  )
import RE.Types
  ( ASCII (..)
  , RE (..)
  )
import System.Exit
  ( exitFailure
  , exitSuccess
  )
import Test.Mutagen
  ( Config (..)
  , DebugMode (..)
  , EvaluationOrder (..)
  , LazyPruningMode (..)
  , Prop
  , allow
  , defaultConfig
  , example
  , expectFailure
  , isSuccess
  , mutagenWithReport
  , (==>)
  )

-- | Main entry point
-- TODO: use a Tasty driver here after it's implemented
main :: IO ()
main = do
  report <-
    mutagenWithReport config
      $ expectFailure
      $ prop_optimize
  if isSuccess report
    then exitSuccess
    else exitFailure

-- | Testing configuration
config :: Config
config =
  defaultConfig
    { maxSuccess =
        -- Number of successful tests to find before stopping
        5000
    , timeout =
        -- Stop after this many seconds
        Just 5
    , maxGenSize =
        -- Max generation size
        5
    , lazyPruning =
        -- Prune mutations affecting unevaluated subexpressions, following the
        -- same evaluation order of the property over the original test case
        LazyPruning Forward
    , useFragments =
        -- Keep a store of test case fragments to be reused
        True
    , examples =
        -- We can provide examples to initialize the fragment store
        [ example (Star (Atom (ASCII 'Y')))
        , example (Plus Eps (Atom (ASCII 'X')))
        ]
    , filterFragments =
        -- Only store fragments of the following types
        allow @ASCII
          <> allow @(RE ASCII)
    , keepGoing =
        -- Keep going after finding the first counterexample
        False
    , -- True
      saveCounterexamples =
        -- Save found counterexamples to a file
        Nothing
    , -- Just "tmp/prop_optimize_@.txt"
      chatty =
        -- Print extra info
        False
    , -- True
      debug =
        -- Disable interactive debugging mode
        NoDebug
    , tui =
        -- Use the brick-based terminal UI
        False
    }

-- | Optimizing a regex preserves the language it accepts
prop_optimize :: (RE ASCII, [ASCII]) -> Prop
prop_optimize (re, str) =
  not (null str)
    && (re `matches` str)
      ==> (optimize re `matches` str)
