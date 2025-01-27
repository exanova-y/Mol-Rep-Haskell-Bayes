{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module RandomGraph where
import Molecule
import LazyPPL
import Distr
import Constants
import Coordinate
import Control.Monad
import Data.Maybe
import ExtraF
import Data.Array
import qualified Data.Map as M

sampleMolecule :: Int -> Meas Molecule
sampleMolecule numAtoms = do
  let atomIDs = [1..numAtoms]
  let bondMatrix = M.empty
  atoms <- foldM (\atoms' atomID -> do
    newAtom <- sampleAtom atomID (Molecule atoms' bondMatrix)
    return (newAtom : atoms')) [] atomIDs
  let molecule = Molecule (reverse atoms) bondMatrix
  foldM updateBondMatrix molecule atoms

updateBondMatrix :: Molecule -> Atom -> Meas Molecule
updateBondMatrix molecule newAtom = do
  let newAtomID = atomID newAtom
  let existingAtoms = atoms molecule
  updatedMolecule <- foldM (\mol existingAtom -> do
    let existingAtomID = atomID existingAtom
    let numMaxBonds = getMaxBonds mol existingAtomID 
    if numMaxBonds <= 0 
        then return mol
        else do
        shouldBond <- sample $ bernoulli 0.5 -- Adjust the probability as needed
        shouldEdge <- sample $ bernoulli $ normalPdf 1.5 0.3 (euclideanDistance (coordinate newAtom) (coordinate existingAtom))
        if shouldBond && shouldEdge
          then do
            delocNum <- sample $ uniformD [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 6]
            let bondType = CovalentBond delocNum Nothing
            let updatedBonds :: M.Map (Integer, Integer) BondType
                updatedBonds = M.insert (fromIntegral newAtomID, fromIntegral existingAtomID)  bondType $
                                M.insert (fromIntegral existingAtomID, fromIntegral newAtomID) bondType (bonds mol)
            return mol {bonds = updatedBonds}
        else return mol) molecule existingAtoms
  return updatedMolecule


sampleAtom :: Int -> Molecule -> Meas Atom
sampleAtom atomID mol = do
  symbol <- sample $ uniformD [O, H, N, C, B, Fe, F, Cl, S, Br, P, I]
  let attr = elementAttributes symbol
  let shellsVar = elementShells symbol
  coord <- if atomID == 1
    then do
      x <- sample $ normal 0 1
      y <- sample $ normal 0 1
      z <- sample $ normal 0 1
      return $ Coordinate x y z
    else do
      let prevAtomID = atomID - 1
      let prevAtom = fromJust $ findAtom mol (fromIntegral prevAtomID)
      bondLength <- sample $ normal 1.5 0.5
      let angstromBondLength = Angstrom bondLength
      sampleNewPosition (coordinate prevAtom) angstromBondLength
  return Atom {atomID = fromIntegral atomID, atomicAttr = attr, coordinate = coord, shells = shellsVar}
  
-- Calculate the Euclidean distance between two coordinates.
euclideanDistance :: Coordinate -> Coordinate -> Double
euclideanDistance (Coordinate x1 y1 z1) (Coordinate x2 y2 z2) =
  sqrt $ (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2




-- Sample a new position for an atom based on the previous position and bond length.
sampleNewPosition :: Coordinate -> EquilibriumBondLength -> Meas Coordinate
sampleNewPosition coord (Angstrom bondLength) = do
  theta <- sample $ uniformbounded 0 (2 * pi)
  u <- sample $ uniformbounded (-1) 1
  let phi = acos u
  let xPos = bondLength * sin phi * cos theta
  let yPos = bondLength * sin phi * sin theta
  let zPos = bondLength * cos phi
  return Coordinate {x = x coord + xPos, y = y coord + yPos, z = z coord + zPos}