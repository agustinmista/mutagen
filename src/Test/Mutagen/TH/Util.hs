{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Template Haskell utilities
module Test.Mutagen.TH.Util
  ( -- * Reification helpers
    reifyName
  , reifyTypeDef

    -- * DType helpers
  , (.==.)
  , isMaybeOf

    -- * DTyVarBndrVis helpers
  , dTyVarBndrName

    -- * DCon helpers
  , dConName
  , dConFields
  , dConFieldsTypes
  , dConFieldsNum

    -- * Pure builders
  , applyTyVars
  , mkConDExp
  , mkApplicativeDExp
  , mkListDExp

    -- * Impure builders
  , createDPat

    -- * Error and logging messages
  , unsupported
  , dump
  , withColor
  , mutagenLog
  , mutagenError
  )
where

import Control.Exception (bracket_)
import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (on)
import Language.Haskell.TH
  ( Name
  , Ppr
  , Q
  , newName
  , pprint
  , runIO
  )
import Language.Haskell.TH.Desugar
  ( DCon (..)
  , DConFields (..)
  , DDec (..)
  , DExp (..)
  , DInfo (..)
  , DPat (..)
  , DTyVarBndr (..)
  , DTyVarBndrVis
  , DType (..)
  , DTypeArg (..)
  , applyDType
  , dsReify
  )
import System.Console.ANSI
  ( Color (..)
  , ColorIntensity (..)
  , ConsoleLayer (..)
  , SGR (..)
  , setSGR
  )

{-------------------------------------------------------------------------------
-- * Reification helpers
-------------------------------------------------------------------------------}

-- | Reify a name or die gracefully
reifyName :: Name -> Q DInfo
reifyName name = do
  dsReify name >>= \case
    Just info -> return info
    Nothing -> mutagenError "could not reify name" [name]

-- | Reify a type definition or die gracefully
reifyTypeDef :: Name -> Q ([DTyVarBndrVis], [DCon])
reifyTypeDef name = do
  reifyName name >>= \case
    DTyConI (DDataD _ _ _ tvbs _ dcons _) _ -> return (tvbs, dcons)
    info -> mutagenError ("could not reify type definition " <> dump name) [info]

{-------------------------------------------------------------------------------
-- * DType helpers
-------------------------------------------------------------------------------}

-- | Compare DTypes for equality
(.==.) :: DType -> DType -> Bool
(.==.) = (==) `on` simplifyDType
  where
    -- \| Simplify a 'DType' removing foralls and signatures
    simplifyDType = \case
      DForallT _ t -> simplifyDType t
      DSigT t _ -> simplifyDType t
      DAppT l r -> DAppT (simplifyDType l) (simplifyDType r)
      t -> t

-- | Is this type a Maybe thing?
isMaybeOf :: DType -> Maybe DType
isMaybeOf = \case
  (DConT f `DAppT` a) | f == ''Maybe -> Just a
  _ -> Nothing

{-------------------------------------------------------------------------------
-- * DTyVarBndrVis helpers
-------------------------------------------------------------------------------}

-- | Get the name of a 'DTyVarBndrVis'.
dTyVarBndrName :: DTyVarBndrVis -> Name
dTyVarBndrName = \case
  (DPlainTV tv _) -> tv
  (DKindedTV tv _ _) -> tv

{-------------------------------------------------------------------------------
-- * DCon helpers
-------------------------------------------------------------------------------}

-- | Get the name of a 'DCon'.
dConName :: DCon -> Name
dConName (DCon _ _ name _ _) = name

-- | Get the fields of a 'DCon'.
dConFields :: DCon -> DConFields
dConFields (DCon _ _ _ conFields _) = conFields

-- | Get the types of the fields of a 'DCon'.
dConFieldsTypes :: DConFields -> [DType]
dConFieldsTypes (DNormalC _ bts) = snd <$> bts
dConFieldsTypes (DRecC bts) = (\(_, _, t) -> t) <$> bts

-- | Get the number of fields of a 'DCon'.
dConFieldsNum :: DConFields -> Int
dConFieldsNum (DNormalC _ bts) = length bts
dConFieldsNum (DRecC bts) = length bts

{-------------------------------------------------------------------------------
-- * Pure builders
-------------------------------------------------------------------------------}

-- | Apply a list of type variables to a head constructor.
applyTyVars :: Name -> [DTyVarBndrVis] -> DType
applyTyVars typeName vars =
  applyDType
    (DConT typeName)
    (dTyVarBndrToDTypeArg <$> vars)
  where
    dTyVarBndrToDTypeArg = \case
      (DPlainTV tv _) -> DTANormal (DVarT tv)
      (DKindedTV tv _ _) -> DTANormal (DVarT tv)

-- | Apply a constructor name to a list of field expressions.
mkConDExp :: Name -> [DExp] -> DExp
mkConDExp conName =
  foldl DAppE (DConE conName)

-- | Build an applicative expression by chaining '<*>' after 'pure'.
mkApplicativeDExp :: Name -> [DExp] -> DExp
mkApplicativeDExp headName =
  foldl appExp pureExp
  where
    pureExp = DVarE 'pure `DAppE` DConE headName
    appExp l r = DVarE '(<*>) `DAppE` l `DAppE` r

-- | Build a list expression by chaining '(:)'.
mkListDExp :: [DExp] -> DExp
mkListDExp =
  foldr consExp nilExp
  where
    nilExp = DConE '[]
    consExp l r = DConE '(:) `DAppE` l `DAppE` r

{-------------------------------------------------------------------------------
-- * Impure builders
-------------------------------------------------------------------------------}

-- | Create a pattern from a constructor using fresh variable names.
--
-- Returns the patterns as well as the freshly bound variables.
createDPat :: DCon -> Q ([Name], DPat)
createDPat (DCon _ _ cname cfields _) = do
  vars <- replicateM (dConFieldsNum cfields) (newName "_v")
  let pat = DConP cname [] [DVarP var | var <- vars]
  return (vars, pat)

{-------------------------------------------------------------------------------
-- * Error and logging messages
-------------------------------------------------------------------------------}

-- | Pretty-print a value for debugging purposes.
--
-- NOTE: this is a simple wrapper around 'pprint' in case we want to change
-- the pretty-printing implementation later.
dump :: (Ppr a) => a -> String
dump = pprint

-- | Run an IO action with a given terminal color, resetting afterwards.
withColor :: Color -> IO () -> IO ()
withColor color io = do
  bracket_
    (setSGR [SetColor Foreground Vivid color])
    (setSGR [Reset])
    io

-- | Log a message with the [MUTAGEN] prefix.
mutagenLog :: (MonadIO m) => String -> m ()
mutagenLog str =
  liftIO $ putStrLn $ "[MUTAGEN] " <> str

-- | Report a derivation error and die gracefully.
mutagenError :: (Show a) => String -> [a] -> Q b
mutagenError msg inputs = runIO $ do
  withColor Red $ do
    mutagenLog "an error happened:"
    mutagenLog msg
    mutagenLog "input was:"
    forM_ inputs $ \i -> do
      mapM_ (liftIO . putStrLn) (lines (show i))
  -- Finally, die
  error "MUTAGEN derivation error"

-- | Blow up on unsupported inputs.
unsupported :: (Show a) => Name -> a -> b
unsupported funName input =
  error
    $ "[MUTAGEN]"
      <> show funName
      <> ": unsupported input:\n"
      <> show input
