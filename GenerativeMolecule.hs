module GenerativeMolecule where

import Constants 
import Molecule 
import qualified Data.Vector as V
import Control.Monad
import Data.List (sort)
import LazyPPL
import Distr
import Extra
import Coordinate
import System.Random (RandomGen(next))
import Data.ByteString (elem)

-- crossMol :: Atom -> Meas Atom
-- crossMol m = do
--     let (rootLocation, childLocations) = getCoordinates m 
--     (root, ids) <- initRoot [1..] m
--     (updatedRoot, remainingIDs) <- appendAtoms (getMaxBondsSymbol (symbol $ atomicAttr m)) root m ids
--     return updatedRoot

-- initRoot :: [Integer] -> Atom -> Meas (Atom, [Integer])
-- initRoot listIDs@(nextID:restIDs) m = do 
--     nextSymbol <- sample $ uniformD [O, H, N, C, B, Fe]
--     condition (nextSymbol == (symbol $ atomicAttr m))
--     xPos <- sample $ normal 0.0 1.0
--     score (normalPdf (getX m) 0.01 xPos)
--     yPos <- sample $ normal 0.0 1.0 
--     score (normalPdf (getY m) 0.01 yPos)
--     zPos <- sample $ normal 0.0 1.0
--     score (normalPdf (getZ m) 0.01 zPos)
--     return (Atom {atomID = nextID, atomicAttr = elementAttributes nextSymbol, coordinate = Coordinate {x = xPos, y = yPos, z = zPos}, bondList = [], shells = elementShells nextSymbol}, restIDs)
-- initRoot [] m = undefined



-- appendAtoms :: Integer -> Atom -> Atom -> [Integer] -> Meas (Atom, [Integer])
-- appendAtoms 0 atom cmpAtom ids = return (atom, ids) 
-- appendAtoms n atom cmpAtom ids = do 
--     (updatedAtom, success, remainingIDs) <- appendAtom atom cmpAtom ids
--     if success 
--         then do
--             (finalAtom, finalIDs) <- appendAtoms (n - 1) updatedAtom cmpAtom remainingIDs
--             return (finalAtom, finalIDs)
--     else return (updatedAtom, remainingIDs)

-- -- This takes an atom, a stream of IDs and returns the same atom with a bond to 
-- -- a new atom, a boolean value indicating whether the max number of bonds has been 
-- -- reached for that atom and the remainder of the stream of IDs.
-- appendAtom :: Atom -> Atom -> [Integer] -> Meas (Atom, Bool, [Integer])
-- appendAtom currentAtom compareAtom listIDs@(nextID:restIDs) = do
--     let currentSymbol = symbol (atomicAttr currentAtom)
--     let currentBondList = bondList currentAtom
--     if getMaxBonds currentAtom <= 0
--         then return (currentAtom, False, listIDs)
--         else do
--             nextSymbol <- sample $ uniformD [O, H, N, C, B, Fe]
--             condition (nextSymbol == H)
--             bondOrder <- sample $ uniformD [1, 2, 3]
--             condition (bondOrder == 1)
--             let bondLength = equilibriumBondLengths 1 currentSymbol nextSymbol
--             nextAtomCoordinate <- sampleNewPosition (coordinate currentAtom) bondLength
--             let newAtom = Atom { atomID = nextID
--                                , atomicAttr = elementAttributes nextSymbol
--                                , coordinate = nextAtomCoordinate
--                                , bondList = [Bond {connectedAtom = currentAtom, bondType = CovalentBond {bondOrder = bondOrder}}]
--                                , shells = elementShells nextSymbol
--                                }
--             let updatedBondList = currentBondList ++ [Bond {connectedAtom = newAtom, bondType = CovalentBond {bondOrder = bondOrder}}]
--             return (currentAtom { bondList = updatedBondList }, True, restIDs)
-- appendAtom currentAtom compareAtom [] = return (currentAtom, False, [])



-- This takes the current atom position and returns a random position
-- from a sphere with radius bondlength away from the atom.
sampleNewPosition :: Coordinate -> EquilibriumBondLength -> Meas Coordinate
sampleNewPosition coord (Angstrom bondLength) = do
    theta <- sample $ uniformbounded 0 (2 * pi)
    u <- sample $ uniformbounded (-1) 1
    let phi = acos u
    let xPos = bondLength * sin phi * cos theta
    let yPos = bondLength * sin phi * sin theta
    let zPos = bondLength * cos phi
    return Coordinate {x = (x coord) + xPos, y = (y coord) + yPos, z = (z coord) + zPos}
