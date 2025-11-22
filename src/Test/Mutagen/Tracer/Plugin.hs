{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

-- | GHC source plugin that instruments Haskell code to include tracing calls
--
-- You can enable this plugin in different ways:
--
-- * Globally, by passing the @-fplugin@ flag to GHC:
--
-- @
--  -fplugin=Test.Mutagen.Tracer.Plugin
-- @
--
-- * Per module, by adding the following pragma to the top of the module:
--
-- @
--  {-# OPTIONS_GHC -fplugin=Test.Mutagen.Tracer.Plugin #-}
-- @
--
-- * Per function, by adding the TRACE annotation pragma to the function:
--
-- @
--  {-# ANN myFunction TRACE #-}
-- @
module Test.Mutagen.Tracer.Plugin
  ( -- * GHC Plugin
    plugin
  )
where

import Control.Monad ((>=>))
import Control.Monad.Writer (MonadIO, WriterT, lift, runWriterT, tell)
import Data.Generics (Data, everywhereM, listify, mkM)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import GHC.Hs
  ( AnnDecl (..)
  , AnnProvenance (..)
  , GRHS (..)
  , GhcPs
  , HsExpr (..)
  , HsLit (..)
  , HsMatchContext (..)
  , HsModule (..)
  , HsParsedModule (..)
  , LHsExpr
  , Match (..)
  , gHsPar
  , getLocA
  , noExtField
  , noLocA
  , simpleImportDecl
  )
import qualified GHC.Hs as GHC
import GHC.Plugins hiding ((<>))
import GHC.Types.Name.Occurrence as Name
import GHC.Types.SourceText (mkIntegralLit)
import System.IO.Unsafe (unsafePerformIO)
import Test.Mutagen.Tracer.Metadata
  ( ModuleMetadata (..)
  , NodeLocation (..)
  , NodeMetadata (..)
  , NodeType (..)
  , mkSingleModuleTracerMetadata
  , saveTracerMetadata
  )
import Test.Mutagen.Tracer.Trace (TraceNode)

{-------------------------------------------------------------------------------
-- * GHC Plugin
-------------------------------------------------------------------------------}

-- | Top-level plugin
plugin :: Plugin
plugin = defaultPlugin{parsedResultAction = action}
  where
    action cli summary parsed = do
      let ParsedResult source msgs = parsed
      let L loc modAST = hpm_module source
      flags <- getDynFlags
      let modName = moduleNameString (moduleName (ms_mod summary))
      mutagenLog $ "Plugin started on module " <> modName
      (modAST', modMetadata) <- instrumentModule cli flags modAST
      let tracerMetadata = mkSingleModuleTracerMetadata modName modMetadata
      liftIO $ saveTracerMetadata tracerMetadata
      mutagenLog "Done"
      return (ParsedResult (source{hpm_module = L loc modAST'}) msgs)

{-------------------------------------------------------------------------------
-- * Instrumentation
-------------------------------------------------------------------------------}

-- | Instrument a parsed module
instrumentModule
  :: [CommandLineOption]
  -> DynFlags
  -> HsModule GhcPs
  -> Hsc (HsModule GhcPs, ModuleMetadata)
instrumentModule _cli flags modAST = do
  (modAST', nodes) <- runWriterT
    $ case traceAnnotations of
      [] -> do
        mutagenLog "Run mode: full module"
        instrumentEntireModule modAST
      bindings -> do
        mutagenLog $ "Run mode: trace only " <> showPpr flags bindings
        instrumentTopLevelBindings bindings modAST
  let metadata = ModuleMetadata nodes
  return (modAST', metadata)
  where
    -- Extract all the TRACE annotations from this module.
    traceAnnotations = extractAnn <$> listify (isAnn flags) modAST

    -- Add an import to the module exporting __trace__, so it's always in scope
    addTraceImport
      :: HsModule GhcPs
      -> WriterT [NodeMetadata] Hsc (HsModule GhcPs)
    addTraceImport m = do
      mutagenLog $ "Adding module import for" <> showPpr flags tracerModuleName
      let tracerModuleImport = noLocA (simpleImportDecl tracerModuleName)
      return m{hsmodImports = tracerModuleImport : hsmodImports m}

    -- Instrument and entire module
    instrumentEntireModule
      :: HsModule GhcPs
      -> WriterT [NodeMetadata] Hsc (HsModule GhcPs)
    instrumentEntireModule =
      addTraceImport
        >=> instrumentEverywhere

    -- Instrument only specific bindings
    instrumentTopLevelBindings
      :: [RdrName]
      -> HsModule GhcPs
      -> WriterT [NodeMetadata] Hsc (HsModule GhcPs)
    instrumentTopLevelBindings bindings =
      addTraceImport
        >=> everywhereM (mkM (instrumentTopLevelBinding bindings))

    -- Instrument top-level functions having TRACE annotation pragmas
    instrumentTopLevelBinding
      :: [RdrName]
      -> Match GhcPs (LHsExpr GhcPs)
      -> WriterT [NodeMetadata] Hsc (Match GhcPs (LHsExpr GhcPs))
    instrumentTopLevelBinding annotations match =
      case match of
        Match m_x m_ctx m_ps m_bodies
          | isFunRhs m_ctx && unLoc (mc_fun m_ctx) `elem` annotations -> do
              m_bodies' <- instrumentEverywhere m_bodies
              return (Match m_x m_ctx m_ps m_bodies')
        x -> return x

    -- Recursively instrument every sub-expression
    instrumentEverywhere
      :: (Data a)
      => a
      -> WriterT [NodeMetadata] Hsc a
    instrumentEverywhere =
      everywhereM (mkM instrumentGRHSs)
        >=> everywhereM (mkM instrumentIFs)

    -- Instrument every RHS with a tracer node.
    -- These come after function clauses, case statements, multi-way ifs, etc.
    instrumentGRHSs
      :: GRHS GhcPs (LHsExpr GhcPs)
      -> WriterT [NodeMetadata] Hsc (GRHS GhcPs (LHsExpr GhcPs))
    instrumentGRHSs (GRHS ext guards rhs) = do
      rhsNode <- liftIO freshTraceNode
      let rhsLoc = getLocA rhs
      tell [NodeMetadata rhsNode GRHSNode (srcSpanToNodeLocation rhsLoc)]
      logInstrumentedNode "RHS" rhsNode rhsLoc
      let rhs' = wrapTracer rhsNode rhs
      return (GRHS ext guards rhs')

    -- Instrument each branch of an if-then-else expression with a tracer
    instrumentIFs
      :: HsExpr GhcPs
      -> WriterT [NodeMetadata] Hsc (HsExpr GhcPs)
    instrumentIFs expr =
      case expr of
        HsIf ext cond th el -> do
          -- then branch
          thNode <- liftIO freshTraceNode
          let thLoc = getLocA th
          tell [NodeMetadata thNode ThenNode (srcSpanToNodeLocation thLoc)]
          logInstrumentedNode "then branch" thNode thLoc
          let th' = wrapTracer thNode th
          -- else branch
          elNode <- liftIO freshTraceNode
          let elLoc = getLocA el
          tell [NodeMetadata elNode ElseNode (srcSpanToNodeLocation elLoc)]
          logInstrumentedNode "else branch" elNode elLoc
          let el' = wrapTracer elNode el
          -- wrap it up again
          return (HsIf ext cond th' el')
        x -> return x

    -- Log an instrumentation message
    logInstrumentedNode
      :: String
      -> TraceNode
      -> SrcSpan
      -> WriterT [NodeMetadata] Hsc ()
    logInstrumentedNode reason node loc = do
      lift
        $ mutagenLog
        $ "Inoculating tracer #"
          <> show node
          <> " on "
          <> reason
          <> " at "
          <> showPpr flags loc

{-------------------------------------------------------------------------------
-- * Helpers
-------------------------------------------------------------------------------}

-- ** Trace node generation

-- | Global counter for trace nodes
traceNodeCounter :: IORef TraceNode
traceNodeCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE traceNodeCounter #-}

-- | Generate a fresh trace node
freshTraceNode :: IO TraceNode
freshTraceNode = atomicModifyIORef' traceNodeCounter $ \n ->
  (n + 1, n + 1)
{-# INLINE freshTraceNode #-}

-- ** Logging

{- FOURMOLU_DISABLE -}
-- | Print a message if debugging is enabled
mutagenLog :: MonadIO m => String -> m ()
mutagenLog _str =
#ifdef MUTAGEN_PLUGIN_DEBUG
  liftIO $ putStrLn $ "[MUTAGEN] " <> _str
#else
  liftIO $ return ()
#endif
{- FOURMOLU_ENABLE -}

-- ** Annotation pragmas

-- | Pattern for matching against annotation pragmas
pattern HsAnn :: RdrName -> RdrName -> AnnDecl GhcPs
pattern HsAnn lhs rhs <-
  HsAnnotation
    _
    (ValueAnnProvenance (L _ lhs))
    (L _ (HsVar _ (L _ rhs)))

-- | Check whether an annotation pragma is of the shape:
-- {-# ANN ident TRACE #-}
isAnn :: DynFlags -> AnnDecl GhcPs -> Bool
isAnn flags (HsAnn _ rhs) = showPpr flags rhs == showPpr flags tracerAnnName
isAnn _ _ = False

-- | Extract the target of an annotation pragma
extractAnn :: AnnDecl GhcPs -> RdrName
extractAnn (HsAnn target _) = target
extractAnn _ = error "this should not happen"

-- ** Source locations

-- | Turn a generic 'SrcSpan' into something more amenable to serialization
srcSpanToNodeLocation :: SrcSpan -> Maybe NodeLocation
srcSpanToNodeLocation loc =
  case loc of
    RealSrcSpan realLoc _ ->
      Just
        $ NodeLocation
          { filePath = unpackFS (srcSpanFile realLoc)
          , startLine = srcSpanStartLine realLoc
          , startCol = srcSpanStartCol realLoc
          , endLine = srcSpanEndLine realLoc
          , endCol = srcSpanEndCol realLoc
          }
    _ -> Nothing

-- ** Predicates

-- | Is this a patter matching an argument of a function binding?
isFunRhs :: HsMatchContext id -> Bool
isFunRhs (FunRhs{}) = True
isFunRhs _ = False

-- ** Constants

-- | Module name of the tracing module
tracerModuleName :: ModuleName
tracerModuleName = mkModuleName "Test.Mutagen.Tracer"

-- | Name of the tracing function
tracerFunName :: RdrName
tracerFunName = mkRdrName "__trace__"

-- | Name of the tracing annotation
tracerAnnName :: RdrName
tracerAnnName = mkRdrName "TRACE"

-- ** AST Builders

-- | Make an unqualified 'RdrName' from a string
mkRdrName :: String -> RdrName
mkRdrName str = mkUnqual Name.varName (mkFastString str)

-- | Build a variable expression from a 'RdrName'
var :: RdrName -> LHsExpr GhcPs
var v = noLocA (HsVar noExtField (noLocA v))

-- | Wrap an expression in parentheses
paren :: LHsExpr GhcPs -> LHsExpr GhcPs
paren x = noLocA (gHsPar x)

-- | Apply one expression to another
app :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
#if MIN_VERSION_ghc(9,10,1)
app x y = noLocA (HsApp GHC.noExtField x y)
#else
app x y = noLocA (HsApp GHC.noComments x y)
#endif

infixl 5 `app`

-- | Build a numeric literal expression
numLit :: Int -> LHsExpr GhcPs
#if MIN_VERSION_ghc(9,10,1)
numLit n = noLocA (HsLit GHC.noExtField (HsInt noExtField (mkIntegralLit n)))
#else
numLit n = noLocA (HsLit GHC.noComments (HsInt noExtField (mkIntegralLit n)))
#endif

-- | Wrap an expression with the tracer function
wrapTracer :: TraceNode -> LHsExpr GhcPs -> LHsExpr GhcPs
wrapTracer node expr =
  var tracerFunName
    `app` numLit node
    `app` paren expr
