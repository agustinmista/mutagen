{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.Mutagen.Tracer.Plugin (
    __trace__,
    TraceAnn (TRACE),
    plugin,
) where

import Control.Monad

import Data.Generics (Data, everywhereM, listify, mkM)
import Data.IORef

import System.IO.Unsafe (unsafePerformIO)

import GHC.Hs
import GHC.Plugins hiding ((<>))
import GHC.Types.Name.Occurrence as Name
import GHC.Types.SourceText

import Test.Mutagen.Tracer.Trace

----------------------------------------

-- | Tracing primitive
__trace__ :: Int -> a -> a
__trace__ n expr =
    unsafePerformIO $ do
        addTraceEntry n
        return expr
{-# INLINE __trace__ #-}

----------------------------------------

-- | Source plugin
data TraceAnn = TRACE
    deriving (Data)

-- IORefs

{-# NOINLINE uid #-}
uid :: IORef Int
uid = unsafePerformIO (newIORef 0)

newUID :: Hsc Int
newUID = liftIO $ atomicModifyIORef uid (\n -> (n + 1, n + 1))

-- The top-level plugin
plugin :: Plugin
plugin = defaultPlugin{parsedResultAction = tracePlugin}

-- The plugin logic
tracePlugin :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
tracePlugin _cli summary (ParsedResult source msgs) = do
    -- Initialize some stuff
    flags <- getDynFlags
    let modName = moduleName (ms_mod summary)
    -- Apply some transformations over the source code
    message $ "plugin started on module " <> showPpr flags modName
    let L loc hsMod = hpm_module source
    -- Check if there are TRACE annotations in the code.
    -- If not, transform the whole module.
    case extractAnn <$> listify (isAnn flags) hsMod of
        [] -> do
            message "run mode: full module"
            let transform =
                    addTraceImport flags modName
                        >=> everywhereM (mkM (annotateGRHS flags))
                        >=> everywhereM (mkM (annotateIfs flags))
            hsMod' <- transform hsMod
            n <- liftIO $ readIORef uid
            liftIO $ writeFile ".tracer" (show n)
            message $ "generated " <> show n <> " trace nodes"
            message "done"
            return (ParsedResult (source{hpm_module = L loc hsMod'}) msgs)
        annotations -> do
            message $ "run mode: trace only " <> showPpr flags annotations
            let transform =
                    addTraceImport flags modName
                        >=> everywhereM (mkM (annotateTopLevel flags annotations))
            hsMod' <- transform hsMod
            n <- liftIO $ readIORef uid
            liftIO $ writeFile ".tracer" (show n)
            message $ "generated " <> show n <> " trace nodes"
            message "done"
            return (ParsedResult (source{hpm_module = L loc hsMod'}) msgs)

-- Include an import to this module, so __trace__ is always in scope
addTraceImport :: DynFlags -> ModuleName -> HsModule GhcPs -> Hsc (HsModule GhcPs)
addTraceImport flags modName hsMod = do
    message $ "adding tracer import to module " <> showPpr flags (moduleNameFS modName)
    let theNewImport = noLocA (simpleImportDecl tracerModuleName)
    let hsMod' = hsMod{hsmodImports = theNewImport : hsmodImports hsMod}
    return hsMod'

-- Annotate every RHS with a tracer
-- They come after: function clauses, case statements, multi-way ifs, etc
annotateGRHS :: DynFlags -> GRHS GhcPs (LHsExpr GhcPs) -> Hsc (GRHS GhcPs (LHsExpr GhcPs))
annotateGRHS flags (GRHS ext guards body) = do
    nth <- newUID
    instrumentedMessage flags "rhs" nth (getLocA body)
    let body' = wrapTracer nth body
    return (GRHS ext guards body')

-- Annotate each branch of an if-then-else expression with a tracer
annotateIfs :: DynFlags -> HsExpr GhcPs -> Hsc (HsExpr GhcPs)
annotateIfs flags expr =
    case expr of
        HsIf ext cond th el -> do
            -- then branch
            nth <- newUID
            instrumentedMessage flags "then branch" nth (getLocA th)
            let th' = wrapTracer nth th
            -- else branch
            nel <- newUID
            instrumentedMessage flags "else branch" nel (getLocA el)
            let el' = wrapTracer nel el
            -- wrap it up again
            return (HsIf ext cond th' el')
        x -> return x

-- Annotate top level functions having TRACE annotation pragmas
annotateTopLevel :: DynFlags -> [RdrName] -> Match GhcPs (LHsExpr GhcPs) -> Hsc (Match GhcPs (LHsExpr GhcPs))
annotateTopLevel flags annotations match =
    case match of
        Match m_x m_ctx m_ps m_bodies
            | isFunRhs m_ctx && unLoc (mc_fun m_ctx) `elem` annotations -> do
                let transform =
                        everywhereM (mkM (annotateGRHS flags))
                            >=> everywhereM (mkM (annotateIfs flags))
                m_bodies' <- transform m_bodies
                return (Match m_x m_ctx m_ps m_bodies')
        x -> return x

----------------------------------------

-- | Helpers
message :: String -> Hsc ()
message str = liftIO $ putStrLn $ "[MUTAGEN] " <> str

instrumentedMessage :: DynFlags -> String -> Int -> SrcSpan -> Hsc ()
instrumentedMessage flags reason n loc = do
    message $
        "inoculating tracer #"
            <> show n
            <> " on "
            <> reason
            <> " at "
            <> showPpr flags loc

-- Wrap an expression with a tracer
wrapTracer :: Int -> LHsExpr GhcPs -> LHsExpr GhcPs
wrapTracer n expr =
    var tracerFunName
        `app` numLit n
        `app` paren expr

-- Check whether an annotation pragma is of the shape:
-- {-# ANN ident TRACE #-}

pattern HsAnn :: RdrName -> RdrName -> AnnDecl GhcPs
pattern HsAnn lhs rhs <-
    HsAnnotation
        _
        (ValueAnnProvenance (L _ lhs))
        (L _ (HsVar _ (L _ rhs)))

isAnn :: DynFlags -> AnnDecl GhcPs -> Bool
isAnn flags (HsAnn _ rhs) = showPpr flags rhs == showPpr flags tracerAnnName
isAnn _ _ = False

extractAnn :: AnnDecl GhcPs -> RdrName
extractAnn (HsAnn target _) = target
extractAnn _ = error "this should not happen"

-- Is this a patter matching an argument of a function binding?
isFunRhs :: HsMatchContext id -> Bool
isFunRhs (FunRhs{}) = True
isFunRhs _ = False

----------------------------------------

-- | Constant names
tracerFunName :: RdrName
tracerFunName = mkRdrName "__trace__"

tracerAnnName :: RdrName
tracerAnnName = mkRdrName "TRACE"

tracerModuleName :: ModuleName
tracerModuleName = mkModuleName "Test.Mutagen.Tracer.Plugin"

----------------------------------------

-- | Builders
mkRdrName :: String -> RdrName
mkRdrName str = mkUnqual Name.varName (mkFastString str)

var :: RdrName -> LHsExpr GhcPs
var v = noLocA (HsVar noExtField (noLocA v))

app :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
app x y = noLocA (HsApp noComments x y)

infixl 5 `app`

paren :: LHsExpr GhcPs -> LHsExpr GhcPs
paren e = noLocA (HsPar noComments (L NoTokenLoc HsTok) e (L NoTokenLoc HsTok))

numLit :: Int -> LHsExpr GhcPs
numLit n = noLocA (HsLit noComments (HsInt noExtField (mkIntegralLit n)))
