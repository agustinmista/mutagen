{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Trace-based tree trace store
module Test.Mutagen.Tracer.Store.Tree
  ( -- * Tree trace store
    TraceStore
  , newTraceStore
  , resetTraceStore
  , saveTrace
  , printTraceStore
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import Data.Tree.Pretty (drawVerticalForest)
import Test.Mutagen.Tracer.Store.API (TraceStoreImpl (..))
import Test.Mutagen.Tracer.Store.Types (TraceType (Tree))
import Test.Mutagen.Tracer.Trace (Trace (..), TraceNode)

{-------------------------------------------------------------------------------
-- * Tree trace store
-------------------------------------------------------------------------------}

instance TraceStoreImpl Tree where
  newtype TraceStore Tree = TreeStore (IORef TraceTree)

  type SaveTraceResult Tree = (Int, Int)

  newTraceStore _ = do
    TreeStore <$> newIORef emptyTraceTree

  resetTraceStore (TreeStore ref) = do
    writeIORef ref emptyTraceTree

  saveTrace trace (TreeStore ref) = do
    tt <- readIORef ref
    let (tt', n, d) = insertTrace trace tt
    writeIORef ref tt'
    return (n, d)

  printTraceStore (TreeStore ref) = do
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
traceTreetoForest :: TraceTree -> Tree.Forest TraceNode
traceTreetoForest (TraceTree tt) =
  Map.elems (Map.mapWithKey (\node -> Tree.Node node . traceTreetoForest) tt)

-- | Pretty print a trace tree
drawTraceTree :: TraceTree -> String
drawTraceTree tt = drawVerticalForest (fmap show <$> toForest tt)
  where
    toForest (TraceTree tt') =
      Map.elems (Map.mapWithKey (\node -> Tree.Node node . traceTreetoForest) tt')
