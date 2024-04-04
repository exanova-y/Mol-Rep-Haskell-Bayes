{-# LANGUAGE RankNTypes #-}

import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Weighted
import Data.List (sort)
import Numeric.Log( Log( Exp ), ln )

newtype Molecule = Root Atom

data Bond = DelocalisedI Integer ([Atom], BondType)
            | Bond (Atom, BondType)

data Atom = Atom {
    atomId                   :: Integer,
    atomicSpec               :: ElementAttributes, 
    coordinate               :: (Double, Double, Double),
    bondList                 :: [Bond]
  }

data BondType = HydrogenBond 
              | CovalentBond {bondOrder :: Integer}  
              | IonicBond deriving (Eq, Read, Show)

data AtomicSymbol = O | H | N | C | P | S | Cl | B | Fe deriving (Eq, Read, Show)

data ElementAttributes = ElementAttributes
  { symbol :: AtomicSymbol,
    atomicNumber :: Integer,
    atomicWeight :: Double
  } deriving (Eq, Read, Show)


getMaxBonds :: AtomicSymbol -> Int
getMaxBonds symbol =
    case symbol of
        O  -> 2
        H  -> 1
        N  -> 3
        C  -> 4
        P  -> 3
        S  -> 2
        Cl -> 1
        B  -> 3
        Fe -> 3

-- appendAtom :: MonadSample m => Atom -> [Integer] -> m Atom
-- appendAtom currentAtom listIDs@(nextID:restIDs) = do
--     let currentSymbol = symbol (atomicSpec currentAtom)
--     let maxBonds = getMaxBonds currentSymbol
--     let numExistingBonds = length (bondList currentAtom)
--     if numExistingBonds >= maxBonds
--         then return currentAtom
--         else do
--             let atomicSymbols = [O, H, N, C, P, S, Cl, B, Fe]
--             nextAtomSymbol <- uniformD atomicSymbols
--             let nextAtomSpec = elementAttributes nextAtomSymbol
--             let nextAtomId = nextID

--             -- nextAtomSymbol <- uniform [O, H, N, C, P, S, Cl, B, Fe]
--             -- let nextAtomSpec = elementAttributes nextAtomSymbol
--             -- let nextAtomId = atomId currentAtom + 1
--             -- let nextCoordinate = sampleCoordinate currentAtom
--             -- let nextAtom = Atom nextAtomId nextAtomSpec nextCoordinate []
--             -- let bondType = sampleBondType currentSymbol nextAtomSymbol
--             -- let bondLength = sampleBondLength bondType
--             -- let bond = Bond (nextAtom, bondType, bondLength)
--             -- let updatedBondList = bond : bondList currentAtom
--             -- return $ currentAtom {bondList = updatedBondList}

-- -- buildMolecule :: MonadSample m => () -> m InductiveMolecule
-- -- buildMolecule = do 
-- --   ea <- priorElementAttributes 
-- --   listOfIds <- [1..]

-- addRing :: Atom -> [Integer]
-- addRing currentAtom listIDs@(nextID:restIDs) = do
--   ringSize <- uniformD [3..8]


testOrbital :: MonadSample m => Double -> m Double
testOrbital threshold = do
  sample <- normal 0 1
  return sample

priorAbundances :: V.Vector Double
priorAbundances = V.fromList [0.49, 0.26, 0.03, 0.01, 0.008, 0.006, 0.004, 0.002, 0.001]
-- O (Oxygen): 0.49
-- H (Hydrogen): 0.26
-- N (Nitrogen): 0.03
-- C (Carbon): 0.01
-- P (Phosphorus): 0.008
-- S (Sulfur): 0.006
-- Cl (Chlorine): 0.004
-- B (Boron): 0.002
-- Fe (Iron): 0.001

elementAttributes :: AtomicSymbol -> ElementAttributes
elementAttributes O = ElementAttributes O 8 15.999 
elementAttributes H = ElementAttributes H 1 1.008 
elementAttributes N = ElementAttributes N 7 14.007
elementAttributes C = ElementAttributes C 6 12.011
elementAttributes P = ElementAttributes P 15 30.974
elementAttributes S = ElementAttributes S 16 32.065
elementAttributes Cl = ElementAttributes Cl 17 35.453
elementAttributes B = ElementAttributes B 5 10.811  
elementAttributes Fe = ElementAttributes Fe 26 55.845

categElementAttributes :: MonadInfer m => V.Vector Double -> m ElementAttributes
categElementAttributes abundances = do
  index <- categorical abundances
  return $ case index of
    0 -> elementAttributes O
    1 -> elementAttributes H
    2 -> elementAttributes N
    3 -> elementAttributes C
    4 -> elementAttributes P
    5 -> elementAttributes S
    6 -> elementAttributes Cl
    7 -> elementAttributes B
    8 -> elementAttributes Fe
    _ -> elementAttributes C