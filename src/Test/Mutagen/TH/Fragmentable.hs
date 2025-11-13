{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Derive a 'Fragmentable' instance for a given data type
module Test.Mutagen.TH.Fragmentable
  ( deriveFragmentable
  )
where

import Control.Monad (forM)
import Language.Haskell.TH
  ( Name
  , Q
  , newName
  )
import Language.Haskell.TH.Desugar
  ( DClause (..)
  , DCon (..)
  , DDec (..)
  , DExp (..)
  , DLetDec (..)
  , DMatch (..)
  , DPat (..)
  , DType (..)
  , dCaseE
  )
import Test.Mutagen.Fragment
  ( Fragmentable
  , fragmentize
  , singleton
  )
import Test.Mutagen.TH.Util
  ( applyTyVars
  , createDPat
  , dConName
  , dTyVarBndrName
  , reifyTypeDef
  )

{-------------------------------------------------------------------------------
-- * Deriving Fragmentable instances
-------------------------------------------------------------------------------}

-- | Derive a 'Fragmentable' instance for the given data type.
deriveFragmentable :: Name -> [Name] -> Q [DDec]
deriveFragmentable typeName ignoredCons = do
  -- Reify the type definition
  (dtvbs, dcons) <- reifyTypeDef typeName
  -- Apply the context type variables to the type name to get 'Type'-kinded
  -- target type to derive the instance for
  let targetType = applyTyVars typeName dtvbs
  -- Keep only the constructors that are not ignored
  let wantedCons = filter (\con -> dConName con `notElem` ignoredCons) dcons
  -- Derive the fragmentize clause
  insClause <- deriveFragmentize wantedCons
  -- Build the Mutable instance
  let insCxt = [DConT ''Fragmentable `DAppT` DVarT (dTyVarBndrName tvb) | tvb <- dtvbs]
  let insTy = DConT ''Fragmentable `DAppT` targetType
  let insBody = [DLetDec (DFunD 'fragmentize [insClause])]
  return [DInstanceD Nothing Nothing insCxt insTy insBody]

-- | Derive the 'fragmentize' clause for the given constructors.
--
-- This one is a bit tricky: the TH desugarer removes as (@) patterns, so the
-- only way to have a variable binding the full input is to introduce it as a
-- variable and then perform a case statement to find the actual constructor.
-- Reconstructing the input using the LHS pattern doesn't work because GHC
-- cannot guarantee its type to be the same as the one being matched against.
-- One can solve this also using TypeApplications and ScopedTypeVariables, but
-- it is unnecessary and we want to avoid needing extra extensions as much as
-- possible.
deriveFragmentize :: [DCon] -> Q DClause
deriveFragmentize cons = do
  input_ <- newName "input"
  let inputFragment = DVarE 'singleton `DAppE` DVarE input_
  let mappendExp x y = DVarE '(<>) `DAppE` x `DAppE` y
  caseCons <-
    forM cons $ \con -> do
      (vars, pat) <- createDPat con
      let fragmentizeExprs =
            [ DVarE 'fragmentize `DAppE` DVarE var
            | var <- vars
            ]
      let caseBody = foldl mappendExp inputFragment fragmentizeExprs
      return (DMatch pat caseBody)
  let clauseBody = dCaseE (DVarE input_) caseCons
  return (DClause [DVarP input_] clauseBody)
