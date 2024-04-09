module GenerativeMolecule where

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

crossMol :: MonadSample m => m Atom
crossMol = do
    (root, ids) <- initRoot [1..]
    -- (updatedRoot, remainingIDs) <- appendAtoms 4 root ids
    return root

appendAtoms :: MonadSample m => Int -> Atom -> [Integer] -> m (Atom, [Integer])
appendAtoms 0 atom ids = return (atom, ids)
appendAtoms n atom ids = do
    (updatedAtom, success, remainingIDs) <- appendAtom atom ids
    if success
        then appendAtoms (n - 1) updatedAtom remainingIDs
    else return (updatedAtom, remainingIDs)

initRoot :: MonadSample m => [Integer] -> m (Atom, [Integer])
initRoot listIDs@(nextID:restIDs) = do 
    nextSymbol <- uniformD [O, H, N, C, B, Fe]
    initPosition <- liftM3 (,,) (normal 0.0 1.0) (normal 0.0 1.0) (normal 0.0 1.0)
    return (Atom {atomID = nextID, atomicAttr = elementAttributes nextSymbol, coordinate = initPosition, bondList = []}, restIDs)
initRoot [] = undefined


-- This takes an atom, a stream of IDs and returns the same atom with a bond to 
-- a new atom, a boolean value indicating whether the max number of bonds has been 
-- reached for that atom and the remainder of the stream of IDs. It currently does not 
-- deal with Delocalised bonds that may have fractional bond orders.
appendAtom :: MonadSample m => Atom -> [Integer] -> m (Atom, Bool, [Integer])
appendAtom currentAtom listIDs@(nextID:restIDs) = do
    let currentSymbol = symbol (atomicAttr currentAtom)
    let currentBondList = bondList currentAtom
    if getMaxBonds currentAtom <= 0
        then return (currentAtom, False, listIDs)
        else do
            nextSymbol <- uniformD [O, H, N, C, B, Fe]
            let nextAtomicAttr = elementAttributes nextSymbol
            bondOrder <- uniformD [1, 2, 3]
            let bondLength = equilibriumBondLengths bondOrder currentSymbol nextSymbol
            nextAtomCoordinate <- sampleNewPosition (coordinate currentAtom) bondLength
            let newAtom = Atom { atomID = nextID
                               , atomicAttr = nextAtomicAttr
                               , coordinate = nextAtomCoordinate
                               , bondList = [Bond {connectedAtom = currentAtom, bondType = CovalentBond {bondOrder = bondOrder}}]
                               }
            let updatedBondList = currentBondList ++ [Bond {connectedAtom = newAtom, bondType = CovalentBond {bondOrder = bondOrder}}]
            return (currentAtom { bondList = updatedBondList }, True, restIDs)
appendAtom currentAtom [] = undefined



-- This takes the current atom position and returns a random position
-- from a sphere with radius bondlength away from the atom.
sampleNewPosition :: MonadSample m => (Double, Double, Double) -> EquilibriumBondLength -> m (Double, Double, Double)
sampleNewPosition (currX, currY, currZ) (Angstrom bondLength) = do
    theta <- uniform 0 (2 * pi)
    u <- uniform (-1) 1
    let phi = acos u
    let x = bondLength * sin phi * cos theta
    let y = bondLength * sin phi * sin theta
    let z = bondLength * cos phi
    return (currX + x, currY + y, currZ + z)
