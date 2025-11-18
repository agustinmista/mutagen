{-# LANGUAGE TypeApplications #-}

module Main where

import qualified RE.Spec as Spec
import RE.Types
import System.Exit
import Test.Mutagen

main :: IO ()
main = do
  let config =
        defaultConfig
          { maxGenSize = 5 -- Max generation size
          -- , debug = StopOnPassed -- Go step by step but skip discarded tests
          , useLazyPrunning = True -- Pprune mutations affecting unevaluated subexpressions
          , useFragments = True -- Keep a store of test case fragments to be reused
          , examples -- We can provide examples to initialize the fragment store
            =
              [ example (Star (Atom (ASCII 'Y')))
              , example (Plus Eps (Atom (ASCII 'X')))
              ]
          , filterFragments -- Only store fragments of the following types
            =
              Just
                [ allow @(RE ASCII)
                , allow @ASCII
                ]
          }
  -- TODO: use a Tasty driver here after it's implemented
  report <-
    mutagenWithReport config
      $ expectFailure
      $ Spec.prop_optimize
  if isSuccess report
    then exitSuccess
    else exitFailure
