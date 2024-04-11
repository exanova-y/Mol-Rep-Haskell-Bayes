
{-# LANGUAGE RankNTypes #-}

module Molecule where

import qualified Data.Vector as V
import Control.Monad
import Data.List 
import Data.Maybe
import LazyPPL
import Text.Printf
import Orbital

data Atom = Atom {
    atomID                   :: Integer,
    atomicAttr               :: ElementAttributes, 
    coordinate               :: Coordinate,
    bondList                 :: [Bond],
    shells                   :: Shells
  }

data Coordinate = Coordinate
    { x :: Double
    , y :: Double
    , z :: Double
    }

data Bond = Delocalised {delocNum :: Integer, 
                           atoms :: [Atom], 
                           bondType :: BondType}
            | Bond {connectedAtom :: Atom, 
                    bondType :: BondType}

data BondType = HydrogenBond 
              | CovalentBond {bondOrder :: Integer}  
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
        atomStr = show (symbol (atomicAttr atom)) ++ " with position (" ++ showCoord (x $ coordinate atom) ++ "," ++ showCoord (y $ coordinate atom) ++ "," ++ showCoord (z $ coordinate atom) ++ ")"
        bondListStr = concatMap (showBond indent) (bondList atom)
    in indentStr ++ atomStr ++ " has " ++ show (length (bondList atom)) ++ " children\n" ++ bondListStr

showCoord :: Double -> String
showCoord coord = printf "%.3f" coord

showBond :: Int -> Bond -> String
showBond indent (Bond atom bondType) =
    let indentStr = replicate (indent + 4) ' '
        atomStr = show (symbol (atomicAttr atom)) ++ " with position (" ++ showCoord (x $ coordinate atom) ++ "," ++ showCoord (y $ coordinate atom) ++ "," ++ showCoord (z $ coordinate atom) ++ ")"
        bondOrderStr = case bondType of
            CovalentBond order -> " with bondOrder " ++ show order
            _ -> ""
    in indentStr ++ atomStr ++ bondOrderStr ++ "\n"
showBond _ (Delocalised _ _ _) = ""

getCoordinates :: Atom -> (Coordinate, [Coordinate])
getCoordinates atom =
    let coord = coordinate atom
        childCoords = map getChildCoordinate (bondList atom)
    in (coord, childCoords)

getChildCoordinate :: Bond -> Coordinate
getChildCoordinate (Bond childAtom _) = coordinate childAtom
getChildCoordinate (Delocalised _ childAtoms _) = coordinate (head childAtoms)
