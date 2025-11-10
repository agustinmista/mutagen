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
          { -- Max generation size
            maxGenSize = 5
          , -- Go step by step
            -- debug = True,

            -- The tracing backend, either Tree or Bitmap (default)
            -- traceMethod = Tree,    -- Prefix trees (Tries)
            -- traceMethod = Bitmap,  -- Edge-based bitmaps (like AFL)

            -- Use lazyness to prune mutations that affect unevaluated subexpressions
            useLazyPrunning = True
          , -- Keep a store of test case fragments to be reused
            useFragments = True
          , -- We can provide examples to initialize the fragment store
            examples =
              [ example (Star (Atom (ASCII 'Y')))
              , example (Plus Eps (Atom (ASCII 'X')))
              ]
          , -- Only store fragments of the following types (default is all types)
            filterFragments =
              Just
                [ allow @(RE ASCII)
                , allow @ASCII
                ]
          }
  let prop = expectFailure Spec.prop_optimize
  -- TODO: use a Tasty driver here after it's implemented
  report <- mutagenWithReport config prop
  if isSuccess report
    then exitSuccess
    else exitFailure
