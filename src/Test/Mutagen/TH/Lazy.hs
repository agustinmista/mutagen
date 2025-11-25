{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Derive a 'Lazy' instance for a given data type.
module Test.Mutagen.TH.Lazy
  ( deriveLazy
  )
where

import Language.Haskell.TH
  ( Lit (..)
  , Name
  , Q
  , newName
  )
import Language.Haskell.TH.Desugar
  ( DClause (..)
  , DCon (..)
  , DDec (..)
  , DExp (..)
  , DLetDec (..)
  , DPat (..)
  , DType (..)
  )
import Test.Mutagen
  ( Lazy
  , lazyNode
  , __evaluated__
  )
import Test.Mutagen.TH.Util
  ( applyTyVars
  , createDPat
  , dConName
  , dTyVarBndrName
  , mkConDExp
  , reifyTypeDef
  )

{-------------------------------------------------------------------------------
-- * Deriving Lazy instances
-------------------------------------------------------------------------------}

-- | Derive a 'Lazy' instance for the given data type.
deriveLazy :: Name -> [Name] -> Q [DDec]
deriveLazy typeName ignoredCons = do
  -- Reify the type definition
  (dtvbs, dcons) <- reifyTypeDef typeName
  -- Apply the context type variables to the type name to get 'Type'-kinded
  -- target type to derive the instance for
  let targetType = applyTyVars typeName dtvbs
  -- Keep only the constructors that are not explicitly ignored by the user
  let wantedCons = filter (\con -> dConName con `notElem` ignoredCons) dcons
  -- Derive 'lazyNode' for each constructor separately
  insClauses <- mapM deriveLazyNode wantedCons
  -- Build the Lazy instance
  let insCxt = [DConT ''Lazy `DAppT` DVarT (dTyVarBndrName tvb) | tvb <- dtvbs]
  let insTy = DConT ''Lazy `DAppT` targetType
  let insBody = [DLetDec (DFunD 'lazyNode insClauses)]
  return [DInstanceD Nothing Nothing insCxt insTy insBody]

-- | Derive a single clause of the 'lazyNode' method for a given constructor.
deriveLazyNode :: DCon -> Q DClause
deriveLazyNode con = do
  acc_ <- newName "acc"
  (vars, pat) <- createDPat con

  let lazyNodeExprs =
        [ DVarE 'lazyNode
            `DAppE` (DConE '(:) `DAppE` DLitE (IntegerL idx) `DAppE` DVarE acc_)
            `DAppE` DVarE var
        | (idx, var) <- zip [0 ..] vars
        ]
  let clauseBody =
        DVarE '__evaluated__
          `DAppE` DVarE acc_
          `DAppE` mkConDExp (dConName con) lazyNodeExprs
  return (DClause [DVarP acc_, pat] clauseBody)
