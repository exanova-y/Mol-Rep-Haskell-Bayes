{-# LANGUAGE OverloadedStrings #-}
module Chem.Validate
  ( ValidationError(..)
  , validateMolecule
  , usedElectronsAt
  ) where

import           Chem.Molecule
import           Chem.Dietz
import           Constants (getMaxBondsSymbol)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

-- | Specific validation errors.
data ValidationError
  = SelfBond Edge                    -- ^ bond from an atom to itself
  | MissingAtom Edge AtomId          -- ^ bond references a missing atom
  | SystemMissingAtom SystemId Edge  -- ^ system references a missing atom
  | ElectronLimitExceeded AtomId Double Double -- ^ actual vs allowed electrons
  deriving (Eq, Show)

-- | Total electrons used at an atom, combining σ bonds and Dietz pools.
-- Uses the Dietz pool formula: e_S(v) = s * deg_S(v) / (2*|E_S|).
usedElectronsAt :: Molecule -> AtomId -> Double
usedElectronsAt m v = sigma + system
  where
    sigma = fromIntegral (length (neighborsSigma m v))
    system = sum [ ePart v bs | bs <- M.elems (systems m) ]

    ePart :: AtomId -> BondingSystem -> Double
    ePart a bs =
      let degSv = fromIntegral $ length
                    [ ()
                    | Edge x y <- S.toList (memberEdges bs)
                    , x == a || y == a ]
          s = fromIntegral (sharedElectrons bs)
          totalEdges = fromIntegral (S.size (memberEdges bs))
      in if totalEdges == 0 then 0 else s * degSv / (2 * totalEdges)

-- | Validate a molecule according to Dietz bonding rules.
validateMolecule :: Molecule -> Either [ValidationError] Molecule
validateMolecule m =
  case errs of
    [] -> Right m
    _  -> Left errs
  where
    atomSet = M.keysSet (atoms m)

    -- Check each sigma bond.
    sigmaErrs = concat
      [ checkEdge e
      | e@(Edge i j) <- S.toList (localBonds m) ]
    checkEdge e@(Edge i j)
      | i == j = [SelfBond e]
      | not (i `S.member` atomSet) = [MissingAtom e i]
      | not (j `S.member` atomSet) = [MissingAtom e j]
      | otherwise = []

    -- Check bonding systems for nonexistent atoms.
    systemErrs =
      [ SystemMissingAtom sid e
      | (sid, bs) <- M.toList (systems m)
      , e@(Edge i j) <- S.toList (memberEdges bs)
      , not (i `S.member` atomSet) || not (j `S.member` atomSet)
      ]

    -- Electron accounting per atom.
    electronErrs =
      [ ElectronLimitExceeded i count limit
      | (i, atom) <- M.toList (atoms m)
      , let count = usedElectronsAt m i
            limit = fromIntegral (2 * getMaxBondsSymbol (symbol (attributes atom)))
      , count > limit
      ]

    errs = sigmaErrs ++ systemErrs ++ electronErrs
