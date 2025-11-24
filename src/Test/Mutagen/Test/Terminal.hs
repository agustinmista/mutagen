{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Terminal UI
module Test.Mutagen.Test.Terminal
  ( -- * Generic TUI interface
    TUI
  , TerminalT
  , withTerminalT
  , MonadTerminal (..)

    -- * Basic stdout TUI
  , stdoutTUI

    -- * Brick-based
  , brickTUI

    -- * Derived printers
  , printGlobalStats
  , printShortStats
  , printBatchStatus

    -- * Helpers
  , prettyPrint
  , compactPrint
  )
where

import Brick (App (..), BrickEvent, EventM, str)
import qualified Brick
import qualified Brick.Widgets.Border as Brick
import Brick.Widgets.List (List)
import qualified Brick.Widgets.List as Brick
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask, lift)
import Control.Monad.State (StateT)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as Vector
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import qualified Graphics.Vty as Vty
import Test.Mutagen.Fragment.Store (fragmentStoreSize)
import Test.Mutagen.Property (Args)
import Test.Mutagen.Test.Queue (MutationBatch (..), mutationQueueSize)
import Test.Mutagen.Test.State (MutagenState (..))
import Text.Pretty.Simple
  ( CheckColorTty (..)
  , OutputOptions (..)
  , defaultOutputOptionsDarkBg
  , pPrintOpt
  )

{-------------------------------------------------------------------------------
-- * Generic terminal interface
-------------------------------------------------------------------------------}

-- | Generic terminal UI interface
data TUI m = TUI
  { tuiMessage :: String -> m ()
  -- ^ Print a message to the terminal
  , tuiPretty :: forall a. (Show a) => a -> m ()
  -- ^ Pretty-print a value to the terminal
  }

-- | t'TerminalT' monad transformer
newtype TerminalT m a = TerminalT {runTerminalT :: ReaderT (TUI m) m a}
  deriving (Applicative, Functor, Monad, MonadIO)

-- | Run a t'TerminalT' action with the given t'TUI'
withTerminalT :: TUI m -> TerminalT m a -> m a
withTerminalT tui = flip runReaderT tui . runTerminalT

-- | Monad class for terminal interactions
class (Monad m) => MonadTerminal m where
  message :: String -> m ()
  pretty :: forall a. (Show a) => a -> m ()

instance (Monad m) => MonadTerminal (TerminalT m) where
  message msg = TerminalT $ do
    tui <- ask
    lift $ tuiMessage tui msg
  pretty val = TerminalT $ do
    tui <- ask
    lift $ tuiPretty tui val

instance (MonadIO m, MonadTerminal m) => MonadTerminal (StateT s m) where
  message msg = lift $ message msg
  pretty val = lift $ pretty val

{-------------------------------------------------------------------------------
-- * Basic stdout terminal interface
-------------------------------------------------------------------------------}

-- | A basic TUI that prints to stdout
stdoutTUI :: IO (TUI IO)
stdoutTUI =
  return
    $ TUI
      { tuiMessage = putStrLn
      , tuiPretty = compactPrint
      }

{-------------------------------------------------------------------------------
-- * Brick-based TUI implementation
-------------------------------------------------------------------------------}

-- | A TUI implementation using the Brick library
brickTUI :: IO (TUI IO)
brickTUI = do
  startTUI
  return
    $ TUI
      { tuiMessage = const (return ())
      , tuiPretty = const (return ())
      }

-- | Names for Brick widgets
data Name = TopLog | BottomLog
  deriving (Eq, Ord, Show)

-- | State of the TUI
data TUIState = TUIState
  { tuiTopLog :: List Name String
  , tuiBottomLog :: List Name String
  }

-- | Start the Brick-based TUI
startTUI :: IO ()
startTUI =
  void $ Brick.defaultMain theApp initialState
  where
    theApp :: App TUIState e Name
    theApp =
      App
        { appDraw = drawUI
        , appChooseCursor = Brick.neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = pure ()
        , appAttrMap = const theMap
        }

    initialState :: TUIState
    initialState =
      TUIState
        { tuiTopLog = Brick.list TopLog (Vector.fromList []) 1
        , tuiBottomLog = Brick.list BottomLog (Vector.fromList []) 1
        }

    theMap :: Brick.AttrMap
    theMap =
      Brick.attrMap
        Vty.defAttr
        []

    drawUI :: TUIState -> [Brick.Widget Name]
    drawUI st =
      [ Brick.vBox
          [ Brick.borderWithLabel (Brick.str "Top Log")
              $ Brick.renderList drawLine True (tuiTopLog st)
          , Brick.borderWithLabel (Brick.str "Bottom Log")
              $ Brick.renderList drawLine True (tuiBottomLog st)
          ]
      ]

    drawLine :: Bool -> String -> Brick.Widget Name
    drawLine _selected txt =
      Brick.str txt

    handleEvent :: BrickEvent Name e -> EventM Name TUIState ()
    handleEvent = \case
      Brick.VtyEvent (EvKey (KChar 'c') [MCtrl]) -> Brick.halt
      _ -> return ()

{-------------------------------------------------------------------------------
-- * Derived printers
-------------------------------------------------------------------------------}

-- | Print the status of the current mutation batch
printBatchStatus :: (MonadTerminal m) => MutationBatch Args -> m ()
printBatchStatus batch = do
  message $ "Current mutation batch:"
  -- past positions
  forM_ (reverse (mbPastPositions batch)) $ \pos ->
    pretty pos
  case mbNextPositions batch of
    [] -> return ()
    (p : ps) -> do
      message "================"
      pretty p
      message
        $ "("
          <> show (length (mbCurrBatch batch))
          <> " mutants left)"
      message "================"
      forM_ ps $ \pos ->
        pretty pos

-- | Print detailed statistics about the testing session
printGlobalStats :: (MonadIO m, MonadTerminal m) => MutagenState -> m ()
printGlobalStats st = do
  message
    "=== Statistics ==="
  message
    $ "* Ran "
      <> show (stNumPassed st + stNumDiscarded st)
      <> " tests ("
      <> show (stNumInteresting st)
      <> " interesting, "
      <> show (stNumBoring st)
      <> " boring) (last interesting was "
      <> show (stNumTestsSinceLastInteresting st)
      <> " tests ago)"
  message
    $ "* Passed "
      <> show (stNumPassed st)
      <> " tests ("
      <> show (stNumDiscarded st)
      <> " discarded, "
      <> show (stNumFailed st)
      <> " failed)"
  message
    $ "* Tests origin: "
      <> show (stNumGenerated st)
      <> " generated, "
      <> show (stNumMutatedFromPassed st)
      <> " mutated from passed, "
      <> show (stNumMutatedFromDiscarded st)
      <> " mutated from discarded"
  message
    $ "* Mutant kinds: "
      <> show (stNumPureMutants st)
      <> " pure, "
      <> show (stNumRandMutants st)
      <> " random, "
      <> show (stNumFragMutants st)
      <> " fragments"
  message
    $ "* Enqueued tests for mutation: "
      <> show (mutationQueueSize (stPassedQueue st))
      <> " passed, "
      <> show (mutationQueueSize (stDiscardedQueue st))
      <> " discarded"
  message
    $ "* Auto-reset is "
      <> maybe "off" (const "on") (stAutoResetAfter st)
      <> ", using "
      <> show (stRandomMutations st)
      <> " random mutations (after "
      <> show (stNumTraceStoreResets st)
      <> " trace store resets)"
  message
    $ "* Current generation size: "
      <> show (stCurrentGenSize st)
  message
    $ "* Fragment store size: "
      <> show (fragmentStoreSize (stFragmentStore st))
  now <- liftIO getPOSIXTime
  let elapsed = now - stStartTime st
  message
    $ "* Elapsed time: "
      <> show elapsed
      <> " seconds"
  message
    "=================="

-- | Print short statistics about the testing session
printShortStats :: (MonadTerminal m) => MutagenState -> m ()
printShortStats st = do
  let total = stNumPassed st + stNumDiscarded st + stNumFailed st
  let mutated = stNumMutatedFromPassed st + stNumMutatedFromDiscarded st
  message
    $ "Executed "
      <> show total
      <> " tests ("
      <> show (percentage (stNumPassed st) total)
      <> "% passed, "
      <> show (percentage (stNumInteresting st) total)
      <> "% interesting, "
      <> show (percentage (stNumGenerated st) total)
      <> "% generated, "
      <> show (percentage mutated total)
      <> "% mutated)"
  where
    percentage :: Int -> Int -> Int
    percentage n m = round @Double ((fromIntegral n / fromIntegral m) * 100)

{-------------------------------------------------------------------------------
-- * Helpers
-------------------------------------------------------------------------------}

-- | Pretty-print a value to stdout
prettyPrint :: (MonadIO m, Show a) => a -> m ()
prettyPrint =
  pPrintOpt
    CheckColorTty
    defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 2
      , outputOptionsPageWidth = 100
      , outputOptionsCompact = False
      , outputOptionsCompactParens = False
      }

-- | Pretty-print a value to stdout in compact form
compactPrint :: (MonadIO m, Show a) => a -> m ()
compactPrint =
  pPrintOpt
    CheckColorTty
    defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 2
      , outputOptionsPageWidth = 100
      , outputOptionsCompact = True
      , outputOptionsCompactParens = True
      }
