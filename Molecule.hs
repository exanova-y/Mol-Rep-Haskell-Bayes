
{-# LANGUAGE RankNTypes #-}

module Molecule where

import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Weighted
import Data.List 
import Data.Maybe
import Numeric.Log( Log( Exp ), ln )
import Data.Time.Format.ISO8601 (yearFormat)

data Atom = Atom {
    atomID                   :: Integer,
    atomicAttr               :: ElementAttributes, 
    coordinate               :: (Double, Double, Double),
    bondList                 :: [Bond]
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

newtype EquilibriumBondLength = Angstrom Double deriving (Read, Show, Eq)



instance Show Atom where
  show atom =
    "atomID: "
      ++ show (atomID atom)
      ++ ", atomicAttr = "
      ++ show (symbol $ atomicAttr atom)
      ++ ", coordinate = "
      ++ show (coordinate atom)
      ++ ", bondList = ["
      ++ showBonds (bondList atom)
      ++ "]}"
    where
      showBonds [] = ""
      showBonds [bond] = showBond bond
      showBonds (bond : bonds) = showBond bond ++ ", " ++ showBonds bonds
      
      showBond (Bond atom bondType) =
        "Bond {connectedAtom = "
          ++ show (atomID atom)
          ++ ", bondType = "
          ++ show bondType
          ++ "}"
      showBond (Delocalised num atoms bondType) =
        "Delocalised"

prettyPrintCross :: Atom -> String
prettyPrintCross atom = prettyPrintAtom 0 atom ++ concatMap (prettyPrintAtom 1) (getChildren atom)

prettyPrintAtom :: Int -> Atom -> String
prettyPrintAtom n atom = replicate (n*5) ' ' ++ show atom ++ "\n"

getChildren :: Atom -> [Atom]
getChildren atom = concatMap extractAtoms (bondList atom)
  where
    extractAtoms (Delocalised _ atoms _) = atoms
    extractAtoms (Bond connectedAtom _)  = [connectedAtom]