
{-# LANGUAGE RankNTypes #-}

module Molecule where

import qualified Data.Vector as V
import Control.Monad
import Data.List 
import Data.Maybe
import LazyPPL
import Text.Printf
import Orbital
import Coordinate

data Atom = Atom {
    atomID                   :: Integer,
    atomicAttr               :: ElementAttributes, 
    coordinate               :: Coordinate,
    bondList                 :: [Bond],
    shells                   :: Shells
  }

data Bond = Bond {connectedAtom :: Atom, 
                    bondType :: BondType}

data BondType = HydrogenBond 
              | CovalentBond 
              | IonicBond deriving (Eq, Read, Show)

data AtomicSymbol = O | H | N | C | B | Fe deriving (Eq, Read, Show)

data ElementAttributes = ElementAttributes
  { symbol :: AtomicSymbol,
    atomicNumber :: Integer,
    atomicWeight :: Double
  } deriving (Eq, Read, Show)

type EquilibriumBondLength = Angstrom
newtype Angstrom = Angstrom Double deriving (Read, Show, Eq)


getX :: Atom -> Double
getX atom = x (coordinate atom)

getY :: Atom -> Double
getY atom = y (coordinate atom)

getZ :: Atom -> Double
getZ atom = z (coordinate atom)


instance Show Atom where
  show atom = showAtom atom 0

showAtom :: Atom -> Int -> String
showAtom atom indent =
  let indentStr = replicate indent ' '
      atomStr = show (symbol (atomicAttr atom)) ++ " with position "
             ++ showCoordinate (coordinate atom)
      bondListStr = concatMap (showBond (indent + 2)) (bondList atom)
  in indentStr ++ atomStr ++ " has " ++ show (length (bondList atom)) ++ " children\n" ++ bondListStr

showCoordinate :: Coordinate -> String
showCoordinate (Coordinate x y z) =
  "(" ++ showCoord x ++ "," ++ showCoord y ++ "," ++ showCoord z ++ ")"

showCoord :: Double -> String
showCoord coord = printf "%.3f" coord

showBond :: Int -> Bond -> String
showBond indent (Bond atom bondType) =
  let indentStr = replicate indent ' '
      atomStr = show (symbol (atomicAttr atom)) ++ " with position "
             ++ showCoordinate (coordinate atom)
  in indentStr ++ atomStr ++ " has " ++ show (length (bondList atom)) ++ " children\n"
  
getCoordinates :: Atom -> (Coordinate, [Coordinate])
getCoordinates atom =
    let coord = coordinate atom
        childCoords = map getChildCoordinate (bondList atom)
    in (coord, childCoords)

getChildCoordinate :: Bond -> Coordinate
getChildCoordinate (Bond childAtom _) = coordinate childAtom
