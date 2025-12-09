{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Terminal UI.
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

    -- * Helpers
  , renderGlobalStats
  , renderShortStats
  , renderBatch
  , prettyPrint
  , compactPrint
  )
where

import Brick (App (..), BrickEvent, EventM, str)
import qualified Brick
import qualified Brick.BChan as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Brick.Widgets.Edit as Brick
import Control.Concurrent (forkIO)
import Control.Monad (void, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask, lift)
import Control.Monad.State (StateT)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text.Zipper as TextZipper
import Data.Time.Clock.POSIX (getPOSIXTime)
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

-- | Generic terminal UI interface.
data TUI m = TUI
  { tuiMessage :: String -> m ()
  -- ^ Print a message to the terminal
  , tuiPretty :: forall a. (Show a) => a -> m ()
  -- ^ Pretty-print a value to the terminal
  , tuiCleanLog :: m ()
  -- ^ Clean the log output
  , tuiReadLine :: String -> m String
  -- ^ Get input from the user (waits for a newline)
  , tuiPrintStats :: Bool -> MutagenState -> m ()
  -- ^ Print global statistics about the testing session
  , tuiPrintBatch :: MutationBatch Args -> m ()
  -- ^ Print the status of the current mutation batch
  }

-- | 'TerminalT' monad transformer.
newtype TerminalT m a = TerminalT {runTerminalT :: ReaderT (TUI m) m a}
  deriving (Applicative, Functor, Monad, MonadIO)

-- | Run a 'TerminalT' action with the given t'TUI'.
withTerminalT :: TUI m -> TerminalT m a -> m a
withTerminalT tui = flip runReaderT tui . runTerminalT

-- | Monad class for terminal interactions.
class (Monad m) => MonadTerminal m where
  message :: String -> m ()
  pretty :: forall a. (Show a) => a -> m ()
  cleanLog :: m ()
  readLine :: String -> m String
  printStats :: Bool -> MutagenState -> m ()
  printBatch :: MutationBatch Args -> m ()

instance (Monad m) => MonadTerminal (TerminalT m) where
  message msg = TerminalT $ do
    tui <- ask
    lift $ tuiMessage tui msg
  pretty val = TerminalT $ do
    tui <- ask
    lift $ tuiPretty tui val
  cleanLog = TerminalT $ do
    tui <- ask
    lift $ tuiCleanLog tui
  readLine prompt = TerminalT $ do
    tui <- ask
    lift $ tuiReadLine tui prompt
  printStats full st = TerminalT $ do
    tui <- ask
    lift $ tuiPrintStats tui full st
  printBatch batch = TerminalT $ do
    tui <- ask
    lift $ tuiPrintBatch tui batch

instance (MonadIO m, MonadTerminal m) => MonadTerminal (StateT s m) where
  message msg = lift $ message msg
  pretty val = lift $ pretty val
  cleanLog = lift $ cleanLog
  readLine prompt = lift $ readLine prompt
  printStats full st = lift $ printStats full st
  printBatch batch = lift $ printBatch batch

{-------------------------------------------------------------------------------
-- * Basic stdout terminal interface
-------------------------------------------------------------------------------}

-- | A basic TUI that prints to stdout.
stdoutTUI :: IO (TUI IO)
stdoutTUI =
  return
    $ TUI
      { tuiMessage = putStrLn
      , tuiPretty = compactPrint
      , tuiCleanLog = return ()
      , tuiReadLine = \prompt -> do
          putStr prompt
          getLine
      , tuiPrintStats = \full st -> do
          let renderStats
                | full = renderGlobalStats
                | otherwise = renderShortStats
          stats <- renderStats st
          putStrLn stats
      , tuiPrintBatch = renderBatch >=> putStrLn
      }

{-------------------------------------------------------------------------------
-- * Brick-based TUI implementation
-------------------------------------------------------------------------------}

-- | A TUI implementation using the Brick library.
brickTUI :: IO (TUI IO)
brickTUI = do
  (inChan, outChan) <- startTUI
  return
    $ TUI
      { tuiMessage = \msg -> do
          Brick.writeBChan inChan (TUIMessage msg)
      , tuiPretty = \obj ->
          Brick.writeBChan inChan (TUIPretty obj)
      , tuiCleanLog = do
          Brick.writeBChan inChan TUICleanLog
      , tuiReadLine = \prompt -> do
          Brick.writeBChan inChan (TUIReadLine prompt)
          Brick.readBChan outChan
      , tuiPrintStats = \_ st -> do
          Brick.writeBChan inChan (TUIStats st)
      , tuiPrintBatch = \batch -> do
          Brick.writeBChan inChan (TUIBatch batch)
      }

-- | Names for Brick widgets.
data TUIName = Stats | CurrentBatch | Log | StatusLine
  deriving (Eq, Ord, Show)

-- | State mode of the TUI.
data TUIMode = TUINormal | TUIInsert
  deriving (Eq, Ord, Show)

-- | TUI event type.
data TUIEvent where
  TUIMessage :: String -> TUIEvent
  TUIPretty :: (Show a) => a -> TUIEvent
  TUICleanLog :: TUIEvent
  TUIReadLine :: String -> TUIEvent
  TUIStats :: MutagenState -> TUIEvent
  TUIBatch :: MutationBatch Args -> TUIEvent

-- | State of the TUI.
data TUIState = TUIState
  { tuiStatsWidget :: Brick.Widget TUIName
  , tuiBatchWidget :: Brick.Widget TUIName
  , tuiLogWidget :: Brick.Widget TUIName
  , tuiStatusLineWidget :: Brick.Editor String TUIName
  , tuiMode :: TUIMode
  , tuiLogLines :: Seq String
  , tuiInputBuffer :: String
  }

initialTUIState :: TUIState
initialTUIState =
  TUIState
    { tuiStatsWidget =
        Brick.str
          $ "Mutagen stats"
    , tuiBatchWidget =
        Brick.str
          $ "Current mutation batch"
    , tuiLogWidget =
        Brick.str
          $ "Log messages"
    , tuiStatusLineWidget =
        Brick.editor
          StatusLine
          (Just 1)
          ""
    , tuiMode = TUINormal
    , tuiLogLines = Seq.empty
    , tuiInputBuffer = ""
    }

-- | Start the Brick-based TUI.
startTUI :: IO (Brick.BChan TUIEvent, Brick.BChan String)
startTUI = do
  inChan <- Brick.newBChan 10
  outChan <- Brick.newBChan 10
  void $ forkIO $ void $ tuiMain inChan outChan
  return (inChan, outChan)
  where
    tuiMain
      :: Brick.BChan TUIEvent
      -> Brick.BChan String
      -> IO (TUIState, Vty.Vty)
    tuiMain inChan outChan =
      Brick.customMainWithDefaultVty
        (Just inChan)
        (mutagenApp outChan)
        initialTUIState

    mutagenApp
      :: Brick.BChan String
      -> App TUIState TUIEvent TUIName
    mutagenApp outChan =
      App
        { appDraw = drawUI
        , appChooseCursor = Brick.neverShowCursor
        , appHandleEvent = handleEvent outChan
        , appStartEvent = pure ()
        , appAttrMap = const attrMap
        }

    attrMap :: Brick.AttrMap
    attrMap =
      Brick.attrMap
        Vty.defAttr
        []

    drawUI
      :: TUIState
      -> [Brick.Widget TUIName]
    drawUI st =
      [ Brick.vBox
          [ Brick.borderWithLabel (Brick.str " Stats ")
              $ tuiStatsWidget st
          , Brick.borderWithLabel (Brick.str " CurrentBatch ")
              $ tuiBatchWidget st
          , Brick.borderWithLabel (Brick.str " Log ")
              $ Brick.vBox
                [ tuiLogWidget st
                , Brick.fill ' '
                ]
          , Brick.borderWithLabel (Brick.str " StatusLine ")
              $ Brick.renderEditor (Brick.str . concat) True
              $ tuiStatusLineWidget st
          ]
      ]

    handleEvent
      :: Brick.BChan String
      -> BrickEvent TUIName TUIEvent
      -> EventM TUIName TUIState ()
    handleEvent outChan = \case
      Brick.VtyEvent ev ->
        case ev of
          EvKey (KChar 'c') [MCtrl] -> do
            Brick.halt
          EvKey KEnter [] -> do
            st <- Brick.get
            case tuiMode st of
              TUIInsert -> do
                let input = reverse (tuiInputBuffer st)
                Brick.put
                  st
                    { tuiInputBuffer =
                        ""
                    , tuiMode =
                        TUINormal
                    }
                -- Send the input back through the output channel
                liftIO $ Brick.writeBChan outChan input
              TUINormal -> return ()
          EvKey (KChar c) [] -> do
            st <- Brick.get
            case tuiMode st of
              TUIInsert -> do
                Brick.put
                  st
                    { tuiInputBuffer =
                        c : tuiInputBuffer st
                    , tuiStatusLineWidget =
                        Brick.applyEdit
                          (TextZipper.insertChar c)
                          $ tuiStatusLineWidget st
                    }
              TUINormal -> return ()
          _ -> return ()
      Brick.AppEvent ev ->
        case ev of
          TUIMessage msg -> do
            st <- Brick.get
            let logLines = tuiLogLines st |> msg
            Brick.put
              st
                { tuiLogLines =
                    logLines
                , tuiLogWidget =
                    Brick.str
                      $ foldr (\line acc -> line <> "\n" <> acc) ""
                      $ logLines
                }
          TUIPretty obj -> do
            st <- Brick.get
            let logLines = tuiLogLines st |> show obj
            Brick.put
              st
                { tuiLogLines =
                    logLines
                , tuiLogWidget =
                    Brick.str
                      $ foldr (\line acc -> line <> "\n" <> acc) ""
                      $ logLines
                }
          TUICleanLog -> do
            st <- Brick.get
            Brick.put
              st
                { tuiLogLines =
                    Seq.empty
                , tuiLogWidget =
                    Brick.str ""
                }
          TUIReadLine prompt -> do
            st <- Brick.get
            Brick.put
              st
                { tuiStatusLineWidget =
                    Brick.applyEdit
                      (TextZipper.insertMany prompt . TextZipper.clearZipper)
                      $ tuiStatusLineWidget st
                , tuiMode =
                    TUIInsert
                }
          TUIStats mutSt -> do
            statsStr <- renderGlobalStats mutSt
            st <- Brick.get
            Brick.put
              st
                { tuiStatsWidget =
                    Brick.str statsStr
                }
          TUIBatch batch -> do
            batchStr <- renderBatch batch
            st <- Brick.get
            Brick.put
              st
                { tuiBatchWidget =
                    Brick.str batchStr
                }
      _ ->
        return ()

{-------------------------------------------------------------------------------
-- * Derived printers
-------------------------------------------------------------------------------}

-- | Print the status of the current mutation batch.
renderBatch :: (MonadIO m) => MutationBatch Args -> m String
renderBatch batch = do
  return
    $ unlines
    $ [ "Current mutation batch:"
      , "* Past positions: "
      ]
      <> [ show pos
         | pos <- reverse (mbPastPositions batch)
         ]
      <> ( case mbNextPositions batch of
             [] -> []
             (p : ps) ->
               [ "================"
               , show p
               , "("
                   <> show (length (mbCurrBatch batch))
                   <> " mutants left)"
               , "================"
               ]
                 <> [ show pos
                    | pos <- ps
                    ]
         )

-- | Render detailed statistics about the testing session.
renderGlobalStats :: (MonadIO m) => MutagenState -> m String
renderGlobalStats st = do
  now <- liftIO getPOSIXTime
  let elapsed = now - stStartTime st
  return
    $ unlines
    $ [ "* Ran "
          <> show (stNumPassed st + stNumDiscarded st)
          <> " tests ("
          <> show (stNumInteresting st)
          <> " interesting, "
          <> show (stNumBoring st)
          <> " boring) (last interesting was "
          <> show (stNumTestsSinceLastInteresting st)
          <> " tests ago)"
      , "* Passed "
          <> show (stNumPassed st)
          <> " tests ("
          <> show (stNumDiscarded st)
          <> " discarded, "
          <> show (stNumFailed st)
          <> " failed)"
      , "* Tests origin: "
          <> show (stNumGenerated st)
          <> " generated, "
          <> show (stNumMutatedFromPassed st)
          <> " mutated from passed, "
          <> show (stNumMutatedFromDiscarded st)
          <> " mutated from discarded"
      , "* Mutant kinds: "
          <> show (stNumPureMutants st)
          <> " pure, "
          <> show (stNumRandMutants st)
          <> " random, "
          <> show (stNumFragMutants st)
          <> " fragments"
      , "* Enqueued tests for mutation: "
          <> show (mutationQueueSize (stPassedQueue st))
          <> " passed, "
          <> show (mutationQueueSize (stDiscardedQueue st))
          <> " discarded"
      , "* Auto-reset is "
          <> maybe "off" (const "on") (stAutoResetAfter st)
          <> ", using "
          <> show (stRandomMutations st)
          <> " random mutations (after "
          <> show (stNumTraceStoreResets st)
          <> " trace store resets)"
      , "* Current generation size: "
          <> show (stCurrentGenSize st)
      , "* Fragment store size: "
          <> show (fragmentStoreSize (stFragmentStore st))
      , "* Elapsed time: "
          <> show elapsed
          <> " seconds"
      ]

-- | Render short statistics about the testing session.
renderShortStats :: (MonadIO m) => MutagenState -> m String
renderShortStats st = do
  let total = stNumPassed st + stNumDiscarded st + stNumFailed st
  let mutated = stNumMutatedFromPassed st + stNumMutatedFromDiscarded st
  return
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

-- | Pretty-print a value to stdout.
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

-- | Pretty-print a value to stdout in compact form.
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
