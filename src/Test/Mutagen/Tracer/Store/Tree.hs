-- | Trace-based tree trace store
module Test.Mutagen.Tracer.Store.Tree
  ( -- * Tree trace store
    TreeTraceStore
  , newTraceStore
  , resetTraceStore
  , saveTrace
  , printTraceStore
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tree (Forest, Tree (..))
import Data.Tree.Pretty (drawVerticalForest)
import Test.Mutagen.Tracer.Trace (Trace (..), TraceNode)

{-------------------------------------------------------------------------------
-- * Tree trace store
-------------------------------------------------------------------------------}

-- | Trace-based tree trace store
newtype TreeTraceStore
  = -- | Trace tree reference
    TreeTraceStore
      (IORef TraceTree)
      -- ^ Global trace tree

-- | Create an empty trace store
newTraceStore :: IO TreeTraceStore
newTraceStore = TreeTraceStore <$> newIORef emptyTraceTree

-- | Reset a trace store
resetTraceStore :: TreeTraceStore -> IO ()
resetTraceStore (TreeTraceStore ref) = writeIORef ref emptyTraceTree

-- | Save a trace into a trace store
saveTrace :: Trace -> TreeTraceStore -> IO (Int, Int)
saveTrace trace (TreeTraceStore ref) = do
  tt <- readIORef ref
  let (tt', n, d) = insertTrace trace tt
  writeIORef ref tt'
  return (n, d)

-- | Print a trace store
printTraceStore :: TreeTraceStore -> IO ()
printTraceStore (TreeTraceStore ref) = do
  tt <- readIORef ref
  putStrLn (drawTraceTree tt)

{-------------------------------------------------------------------------------
-- * Trace trees implemented using "rose maps"
-------------------------------------------------------------------------------}

-- | Recursive partial maps indexed by trace nodes
newtype TraceTree = TraceTree (Map TraceNode TraceTree)

-- | Create an empty trace tree
emptyTraceTree :: TraceTree
emptyTraceTree = TraceTree mempty

-- | Insert a trace into a trace tree
--
-- Returns the updated tree, the number of new nodes added, and the depth at
-- which the input trace was inserted into the global trace tree.
insertTrace :: Trace -> TraceTree -> (TraceTree, Int, Int)
insertTrace (Trace entries) = go 0 entries
  where
    go d [] (TraceTree tt) = (TraceTree tt, 0, d)
    go d (e : es) (TraceTree tt) =
      case Map.lookup e tt of
        Nothing ->
          let (subTree, new) = chain es
           in (TraceTree (Map.insert e subTree tt), new + 1, d)
        Just subTree ->
          let (subTree', new, d') = go (d + 1) es subTree
           in (TraceTree (Map.insert e subTree' tt), new, d')

    chain [] = (TraceTree mempty, 0)
    chain (e : es) = (TraceTree (Map.singleton e tlog'), n + 1)
      where
        (tlog', n) = chain es

-- | Convert a trace tree to a forest for pretty printing
traceTreetoForest :: TraceTree -> Forest TraceNode
traceTreetoForest (TraceTree tt) =
  Map.elems (Map.mapWithKey (\node -> Node node . traceTreetoForest) tt)

-- | Pretty print a trace tree
drawTraceTree :: TraceTree -> String
drawTraceTree tt = drawVerticalForest (fmap show <$> toForest tt)
  where
    toForest (TraceTree tt') =
      Map.elems (Map.mapWithKey (\node -> Node node . traceTreetoForest) tt')
