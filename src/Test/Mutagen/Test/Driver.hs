{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Test drivers for Mutagen properties, mirrored from QuickCheck
module Test.Mutagen.Test.Driver
  ( -- * Test drivers
    mutagen
  , mutagenVerbose
  , mutagenVerboseReport
  , mutagenReport
  , mutagenWith
  , mutagenWithReport
  )
where

import Control.Monad (void)
import Test.Mutagen.Config (Config (..), defaultConfig)
import Test.Mutagen.Property (Testable (..))
import Test.Mutagen.Report (Report)
import Test.Mutagen.Test.Loop (loop)
import Test.Mutagen.Test.State (initMutagenState)

{-------------------------------------------------------------------------------
-- * Test drivers
-------------------------------------------------------------------------------}

-- | Run Mutagen with default configuration
mutagen :: (Testable p) => p -> IO ()
mutagen = mutagenWith defaultConfig

-- | Run Mutagen with default configuration in verbose mode
mutagenVerbose :: (Testable p) => p -> IO ()
mutagenVerbose = mutagenWith defaultConfig{chatty = True}

-- | Run Mutagen with default configuration in verbose mode, returning a report
mutagenVerboseReport :: (Testable p) => p -> IO Report
mutagenVerboseReport = mutagenWithReport defaultConfig{chatty = True}

-- | Run Mutagen with default configuration, returning a report
mutagenReport :: (Testable p) => p -> IO Report
mutagenReport = mutagenWithReport defaultConfig

-- | Run Mutagen with a custom configuration
mutagenWith :: (Testable p) => Config -> p -> IO ()
mutagenWith cfg p = void (mutagenWithReport cfg p)

-- | Run Mutagen with a custom configuration, returning a report
mutagenWithReport :: (Testable p) => Config -> p -> IO Report
mutagenWithReport cfg p = do
  initMutagenState cfg (property p) >>= loop
