
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Molecule where
import Control.Applicative (Alternative((<|>)))
import qualified Data.Vector as V
import Control.Monad
import Data.List 
import Data.Maybe
import LazyPPL
import Text.Printf
import Orbital
import Coordinate
import Data.Array
import Data.Maybe

data Molecule = Molecule {
    atoms :: [Atom],
    bonds :: Array (Int, Int) (Maybe BondType)
  }

data Atom = Atom {
    atomID                   :: Integer,
    atomicAttr               :: ElementAttributes, 
    coordinate               :: Coordinate,
    shells                   :: Shells
  } deriving (Show, Read)

data Bond = Bond {connectedAtomID :: Integer, 
                    bondType :: BondType}

data BondType = HydrogenBond 
              | CovalentBond {delocNum :: Integer, ring :: Maybe [Integer]}
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


getConnectedAtoms :: Molecule -> Integer -> Maybe [Integer]
getConnectedAtoms molecule atomID =
  let connectedAtoms = [fromIntegral j | (i, j) <- range (bounds (bonds molecule)), i == fromIntegral atomID, isJust (bonds molecule ! (i, j))]
  in if null connectedAtoms then Nothing else Just connectedAtoms

findAtom :: Molecule -> Integer -> Maybe Atom
findAtom mol atomId = find (\atom -> atomID atom == atomId) (atoms mol)

getBondTypeMaybe :: Molecule -> Integer -> Integer -> Maybe BondType
getBondTypeMaybe molecule atomID otherAtomID =
  bonds molecule ! (fromIntegral atomID, fromIntegral otherAtomID)

getBondType :: Molecule -> Integer -> Integer -> BondType
getBondType molecule atomID otherAtomID = case bonds molecule ! (fromIntegral atomID, fromIntegral otherAtomID) of
  Just bondType -> bondType
  Nothing -> error "Bond not found"

setBondType :: Molecule -> Integer -> Integer -> BondType -> Molecule
setBondType molecule atomID otherAtomID bondType =
    molecule { bonds = bonds molecule // [((fromIntegral atomID, fromIntegral otherAtomID), Just bondType),
                                          ((fromIntegral otherAtomID, fromIntegral atomID), Just bondType)] }



prettyPrintMolecule :: Molecule -> String
prettyPrintMolecule molecule = "Atoms:\n" ++ concatMap prettyPrintAtom (atoms molecule) ++ "\nBonds:\n" ++ prettyPrintBondMatrix molecule
  where
    prettyPrintAtom :: Atom -> String
    prettyPrintAtom atom =
      printf "%s %d at (%.2f, %.2f, %.2f) with %d children\n%s\n"
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
    prettyPrintBondMatrix molecule = unlines $ map (\i -> concatMap (\j -> printf "%d " (getBondOrder i j)) [1..numAtoms]) [1..numAtoms]
      where
        numAtoms = length (atoms molecule)
        getBondOrder i j =
          case getBondTypeMaybe molecule (toInteger i) (toInteger j) of
            Just (CovalentBond delocNum _) -> delocNum
            _ -> 0