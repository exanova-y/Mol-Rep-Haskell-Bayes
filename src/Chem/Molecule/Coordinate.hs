{-# LANGUAGE DeriveGeneric #-}

module Chem.Molecule.Coordinate
  ( Angstrom(..)
  , mkAngstrom
  , unAngstrom
  , Coordinate(..)
  ) where

import GHC.Generics (Generic)

newtype Angstrom = Angstrom Double
  deriving (Eq, Ord, Show, Read, Generic)

mkAngstrom :: Double -> Angstrom
mkAngstrom = Angstrom

unAngstrom :: Angstrom -> Double
unAngstrom (Angstrom d) = d

-- | Cartesian coordinates in Angstroms
data Coordinate = Coordinate
  { x :: Angstrom
  , y :: Angstrom
  , z :: Angstrom
  } deriving (Eq, Show, Read, Generic)
