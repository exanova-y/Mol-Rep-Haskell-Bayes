
{-# LANGUAGE RankNTypes #-}

module Molecule where

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

data AtomicSymbol = O | H | N | C | B | Fe deriving (Eq, Read, Show)

data ElementAttributes = ElementAttributes
  { symbol :: AtomicSymbol,
    atomicNumber :: Integer,
    atomicWeight :: Double
  } deriving (Eq, Read, Show)

newtype EquilibriumBondLength = Angstrom Double deriving (Read, Show, Eq)

