
import Constants 
import Molecule 
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Weighted
import Data.List (sort)
import Numeric.Log( Log( Exp ), ln )

sampleNewPosition :: MonadSample m => EquilibriumBondLength -> m (Double, Double, Double)
sampleNewPosition (Angstrom bondLength) = do
    theta <- uniform 0 (2 * pi)
    u <- uniform (-1) 1
    let phi = acos u
    let x = bondLength * sin phi * cos theta
    let y = bondLength * sin phi * sin theta
    let z = bondLength * cos phi
    return (x, y, z)

-- appendAtom :: MonadSample m => Atom -> [Integer] -> m (Atom, Bool)
-- appendAtom currentAtom listIDs@(nextID:restIDs) = do
--     let currentSymbol = symbol (atomicSpec currentAtom)
--     let maxBonds = getMaxBonds currentSymbol
--     let numExistingBonds = length (bondList currentAtom)
--     if numExistingBonds == maxBonds
--         then return (currentAtom, False)
--         else do
--           nextAtom <- categElementAttributes priorAbundances
--           nextBondPosition <- sampleNewPosition (equilibriumBondLengths currentSymbol (symbol nextAtom)) 
            --  let updatedNextAtom = Atom { coordinate = nextCoordinate }
            
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

categElementAttributes :: MonadInfer m => V.Vector Double -> m ElementAttributes
categElementAttributes abundances = do
  index <- categorical abundances
  return $ case index of
    0 -> elementAttributes O
    1 -> elementAttributes H
    2 -> elementAttributes N
    3 -> elementAttributes C
    4 -> elementAttributes B
    5 -> elementAttributes Fe
    _ -> elementAttributes C