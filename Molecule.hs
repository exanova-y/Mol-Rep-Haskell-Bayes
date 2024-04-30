module Molecule where
import Control.Applicative (Alternative((<|>)))
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Monad
import Data.List 
import Data.Maybe
import LazyPPL
import Text.Printf
import Orbital
import Coordinate
import Data.Maybe

data Molecule = Molecule {
    atoms :: [Atom],
    bonds :: M.Map (Integer, Integer) BondType
  } deriving (Show, Read)

data Atom = Atom {
    atomID                   :: Integer,
    atomicAttr               :: ElementAttributes, 
    coordinate               :: Coordinate,
    shells                   :: Shells
  } deriving (Show, Read)

data BondType = HydrogenBond 
              | CovalentBond {delocNum :: Integer, atomIDs :: Maybe [Integer]}
              | IonicBond deriving (Eq, Read, Show)

data AtomicSymbol = O | H | N | C | B | Fe | F | Cl | S | Br | P | I deriving (Eq, Read, Show)

data ElementAttributes = ElementAttributes
  { symbol :: AtomicSymbol,
    atomicNumber :: Integer,
    atomicWeight :: Double
  } deriving (Eq, Read, Show)

type EquilibriumBondLength = Angstrom
newtype Angstrom = Angstrom Double deriving (Read, Show, Eq)

instance Eq Atom where
    (==) a1 a2 = atomID a1 == atomID a2


getX :: Atom -> Double
getX atom = x (coordinate atom)

getY :: Atom -> Double
getY atom = y (coordinate atom)

getZ :: Atom -> Double
getZ atom = z (coordinate atom)

getSymmetricBonds :: [((Integer, Integer), BondType)] -> [((Integer, Integer), BondType)]
getSymmetricBonds bonds = bonds ++ map swapBond bonds
  where
    swapBond ((a, b), bondType) = ((b, a), bondType)

getConnectedAtoms :: Molecule -> Integer -> Maybe [Integer]
getConnectedAtoms molecule atomID =
    case [j | (i, j) <- M.keys (bonds molecule), i == fromIntegral atomID] of
        [] -> Nothing
        connectedAtoms -> Just connectedAtoms

findAtom :: Molecule -> Integer -> Maybe Atom
findAtom mol atomId = find (\atom -> atomID atom == atomId) (atoms mol)

getBondTypeMaybe :: Molecule -> Integer -> Integer -> Maybe BondType
getBondTypeMaybe molecule atomID otherAtomID =
    M.lookup (fromIntegral atomID, fromIntegral otherAtomID) (bonds molecule)

getBondType :: Molecule -> Integer -> Integer -> BondType
getBondType molecule atomID otherAtomID =
    case getBondTypeMaybe molecule atomID otherAtomID of
        Just bondType -> bondType
        Nothing       -> error "Bond not found"

setBondType :: Molecule -> Integer -> Integer -> BondType -> Molecule
setBondType molecule atomID otherAtomID bondType =
    let key1 = (fromIntegral atomID, fromIntegral otherAtomID)
        key2 = (fromIntegral otherAtomID, fromIntegral atomID)
        newBonds = M.insert key1 bondType $ M.insert key2 bondType (bonds molecule)
    in  molecule { bonds = newBonds }

prettyPrintMolecule :: Molecule -> String
prettyPrintMolecule molecule = "Atoms:\n" ++ concatMap prettyPrintAtom (atoms molecule) ++ "\nBonds:\n" ++ prettyPrintBondMatrix molecule
  where
    prettyPrintAtom :: Atom -> String
    prettyPrintAtom atom =
      printf "%s %d at (%.1f, %.1f, %.1f) with %d children\n%s\n"
        (show (symbol (atomicAttr atom)))
        (atomID atom)
        (getX atom)
        (getY atom)
        (getZ atom)
        (length children)
        (concatMap prettyPrintChild children)
      where
        children = case getConnectedAtoms molecule (atomID atom) of
          Just atoms -> atoms
          Nothing -> []

    prettyPrintChild :: Integer -> String
    prettyPrintChild atomID =
      case findAtom molecule atomID of
        Just atom -> printf "\t%s %d\n" (show (symbol (atomicAttr atom))) atomID
        Nothing -> ""

    prettyPrintBondMatrix :: Molecule -> String
    prettyPrintBondMatrix molecule =
        unlines $ map formatRow [1..numAtoms]
      where
        numAtoms = length (atoms molecule)
        formatRow i = concatMap (printf "%d ") [getBondOrder molecule i j | j <- [1..numAtoms]]

getBondOrder :: Molecule -> Int -> Int -> Int
getBondOrder molecule i j =
    case M.lookup (toInteger i, toInteger j) (bonds molecule) of
        Just (CovalentBond delocNum _) -> fromIntegral delocNum
        _                              -> 0