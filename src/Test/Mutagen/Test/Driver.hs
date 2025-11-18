{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Mutagen.Test.Driver where

import Control.Monad
import Test.Mutagen.Property
import Test.Mutagen.Test.Config
import Test.Mutagen.Test.Loop
import Test.Mutagen.Test.Report
import Test.Mutagen.Test.State

----------------------------------------

-- | Test drivers, mirrored from QuickCheck ones
mutagen :: (Testable p) => p -> IO ()
mutagen = mutagenWith defaultConfig

mutagenVerbose :: (Testable p) => p -> IO ()
mutagenVerbose = mutagenWith defaultConfig{chatty = True}

mutagenVerboseReport :: (Testable p) => p -> IO Report
mutagenVerboseReport = mutagenWithReport defaultConfig{chatty = True}

mutagenReport :: (Testable p) => p -> IO Report
mutagenReport = mutagenWithReport defaultConfig

mutagenWith :: (Testable p) => Config -> p -> IO ()
mutagenWith cfg p = void (mutagenWithReport cfg p)

-- The main driver
mutagenWithReport :: (Testable p) => Config -> p -> IO Report
mutagenWithReport cfg p = do
  -- create the initial internal state
  st <- initMutagenState cfg (property p)
  -- go go go!
  loop runTestCase st
