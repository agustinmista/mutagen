{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Terminal UI
module Test.Mutagen.Test.Terminal
  ( -- * Terminal UI
    startTUI

    -- * Pretty printers
  , printDot
  , printPos
  , printTestCase
  , printTrace
  , printMessage
  , printGlobalStats
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
import Control.Monad (void)
import qualified Data.Vector as Vector
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import qualified Graphics.Vty as Vty
import System.Console.ANSI (cursorUp)
import System.IO (hFlush, stdout)
import Test.Mutagen.Mutation (Pos)
import Test.Mutagen.Property (Args)
import Test.Mutagen.Test.Queue (MutationBatch (..))
import Test.Mutagen.Test.State (MutagenState (..))
import Test.Mutagen.Tracer.Trace (Trace (..))
import Text.Pretty.Simple
  ( CheckColorTty (..)
  , OutputOptions (..)
  , defaultOutputOptionsDarkBg
  , pPrintOpt
  )
import Text.Printf (printf)

{-------------------------------------------------------------------------------
-- * Terminal UI
-------------------------------------------------------------------------------}

data Name = TopLog | BottomLog
  deriving (Eq, Ord, Show)

data TUIState = TUIState
  { tuiTopLog :: List Name String
  , tuiBottomLog :: List Name String
  }

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
-- * Pretty printers
-------------------------------------------------------------------------------}

-- | Print a dot to indicate progress
printDot :: IO ()
printDot = printf "." >> hFlush stdout

-- | Print a 'Pos'
printPos :: [Pos] -> IO ()
printPos = compactPrint

-- | Print a test case
printTestCase :: Args -> IO ()
printTestCase = compactPrint

-- | Print a 'Trace'
printTrace :: Trace -> IO ()
printTrace = compactPrint . unTrace

-- | Print a message
printMessage :: String -> IO ()
printMessage = printf ">>> %s\n"

-- | Print the status of the current mutation batch
printBatchStatus :: MutationBatch Args -> IO ()
printBatchStatus mbatch = do
  printf
    ">>> Current mutation batch: %d tests enqueued, %d mutations levels left\n"
    (length (mbCurrBatch mbatch))
    (mbMaxMutationDepth mbatch)
  printf "*** Mutated positions:\n"
  mapM_
    (\pos -> putStrLn (show pos <> " *"))
    (reverse (mbPastPositions mbatch))
  case mbNextPositions mbatch of
    [] -> return ()
    (p : ps) -> do
      putStrLn (show p <> " <== current")
      mapM_ print ps

-- | Print global statistics about the testing session
printGlobalStats :: MutagenState -> IO ()
printGlobalStats st = do
  cursorUp 1
  printMessage
    $ show (stNumPassed st)
      <> " passed, "
      <> show (stNumDiscarded st)
      <> " discarded, "
      <> show (stNumGenerated st)
      <> " generated, "
      <> show (stNumMutatedFromPassed st, stNumMutatedFromDiscarded st)
      <> " mutated, "
      <> show (stNumInteresting st)
      <> " interesting, "
      <> show (stNumBoring st)
      <> " boring, "
      <> show (stNumTestsSinceLastInteresting st)
      <> " tests since last interesting, "
      <> show (stNumTraceLogResets st)
      <> " trace log resets"

-- -- | Print a message indicating that a test is running
-- printRunningTest :: IO ()
-- printRunningTest = do
--   printf ">>> Running test...\n"

-- ** Detail printers

-- -- | Print evaluated subexpressions
-- printEvaluatedSubexpressions :: [Pos] -> IO ()
-- printEvaluatedSubexpressions pos = do
--   printf ">>> Evaluated subexpressions:\n"
--   print pos
--
-- -- | Print that a test case was interesting
-- printTestCaseWasInteresting :: Int -> Int -> IO ()
-- printTestCaseWasInteresting new prio = do
--   printf ">>> Test case was interesting! (new trace nodes=%d, prio=%d)\n" new prio
--
-- -- | Print that a test case was boring
-- printTestResult :: String -> IO ()
-- printTestResult res = do
--   printf ">>> Test result: %s\n" res
--
-- -- | Print an original test case that is being mutated
-- printOriginalTestCase :: Int -> Args -> Bool -> IO ()
-- printOriginalTestCase prio args isPassed = do
--   printf
--     ">>> Mutating %s test case (prio=%d):\n"
--     (if isPassed then "passed" else "discarded")
--     prio
--   prettyPrint args
--
-- -- ** Test case printers
--
-- -- | Print a generated test case
-- printGeneratedTestCase :: Args -> IO ()
-- printGeneratedTestCase args = do
--   printf ">>> Generated new random test case:\n"
--   prettyPrint args
--
-- -- | Print a mutated test case
-- printMutatedTestCase :: Args -> IO ()
-- printMutatedTestCase args = do
--   printf ">>> Mutated test case:\n"
--   prettyPrint args
--
-- -- | Print the original trace of a test case
-- printOriginalTestCaseTrace :: Trace -> IO ()
-- printOriginalTestCaseTrace tr = do
--   printf ">>> Original trace:\n"
--   print (unTrace tr)
--
-- -- | Print the mutated trace of a test case
-- printMutatedTestCaseTrace :: Trace -> IO ()
-- printMutatedTestCaseTrace tr = do
--   printf ">>> New trace:\n"
--   print (unTrace tr)
--
-- -- ** Status printers
--
-- -- | Print the status of the current mutation batch
-- printBatchStatus :: MutationBatch Args -> IO ()
-- printBatchStatus mbatch = do
--   printf
--     ">>> Current mutation batch: %d tests enqueued, %d mutations levels left\n"
--     (length (mbCurrBatch mbatch))
--     (mbMaxMutationDepth mbatch)
--   printf ">>> Mutated positions:\n"
--   mapM_
--     (\pos -> putStrLn (show pos <> " *"))
--     (reverse (mbPastPositions mbatch))
--   printf ">>> Next mutable positions:\n"
--   case mbNextPositions mbatch of
--     [] -> return ()
--     (p : ps) -> do
--       putStrLn (show p <> " <== current")
--       mapM_ print ps
--
-- -- | Print global statistics about the testing session
-- printGlobalStats :: MutagenState -> IO ()
-- printGlobalStats st = do
--   printf ">>> Statistics:\n"
--   printf
--     "* Executed test cases: %d (%d interesting, %d boring) (last interesting was %d tests ago)\n"
--     (stNumInteresting st + stNumBoring st)
--     (stNumInteresting st)
--     (stNumBoring st)
--     (stNumTestsSinceLastInteresting st)
--   printf
--     "* Passed %d tests (%d discarded)\n"
--     (stNumPassed st)
--     (stNumDiscarded st)
--   printf
--     "* Tests origin: %d generated, %d mutated from passed, %d mutated from discarded\n"
--     (stNumGenerated st)
--     (stNumMutatedFromPassed st)
--     (stNumMutatedFromDiscarded st)
--   printf
--     "* Mutant kinds: %d pure, %d random, %d fragments\n"
--     (stNumPureMutants st)
--     (stNumRandMutants st)
--     (stNumFragMutants st)
--   printf
--     "* Enqueued tests for mutation: %d passed, %d discarded\n"
--     (mutationQueueSize (stPassedQueue st))
--     (mutationQueueSize (stDiscardedQueue st))
--   printf
--     "* Auto-reset is %s, using %d random mutations (after %d trace log resets)\n"
--     (maybe "off" (const "on") (stAutoResetAfter st))
--     (stRandomMutations st)
--     (stNumTraceLogResets st)
--   printf
--     "* Current generation size: %d\n"
--     (stCurrentGenSize st)
--   printf
--     "* Fragment store size: %s\n"
--     (show (fragmentStoreSize (stFragmentStore st)))
--   now <- round <$> getPOSIXTime
--   let elapsed = now - stStartTime st
--   printf "* Elapsed time: %d seconds (+/- 1 second)\n" elapsed
--   printf "\n"
--
-- -- * Report printers
--
-- -- | Report that testing is done
-- reportDoneTesting :: MutagenState -> IO ()
-- reportDoneTesting st = do
--   clear
--   printGlobalStats st
--   printf ">>> Done testing\n"
--
-- -- | Report that testing gave up
-- reportGaveUp :: MutagenState -> String -> IO ()
-- reportGaveUp st r = do
--   clear
--   printGlobalStats st
--   printf ">>> Gave up (%s)\n" r
--
-- -- | Report a found counterexample
-- reportCounterexample :: MutagenState -> Args -> Result -> IO ()
-- reportCounterexample st as res = do
--   clear
--   printGlobalStats st
--   printf ">>> Found counterexample!\n"
--   printf
--     "* Reason of failure: %s\n"
--     (fromMaybe "assertion failed" (resultReason res))
--   when (isJust (resultException res)) $ do
--     printf "* The exception was:\n%s\n" (show (fromJust (resultException res)))
--   printf "* Failing inputs:\n"
--   prettyPrint as
--   printf "\n"
--
-- -- | Report that no failure was found, despite expecting one
-- reportNoExpectedFailure :: MutagenState -> IO ()
-- reportNoExpectedFailure st = do
--   clear
--   printGlobalStats st
--   printf ">>> Expected a failure, but all tests passed!\n"
--
-- {-------------------------------------------------------------------------------
-- -- * Helpers
-- -------------------------------------------------------------------------------}

-- | Pretty-print a value to stdout
prettyPrint :: (Show a) => a -> IO ()
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
compactPrint :: (Show a) => a -> IO ()
compactPrint =
  pPrintOpt
    CheckColorTty
    defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 2
      , outputOptionsPageWidth = 100
      , outputOptionsCompact = True
      , outputOptionsCompactParens = True
      }
