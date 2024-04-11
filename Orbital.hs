module Orbital where 

import Distr 
import LazyPPL
import Data.Maybe
import Coordinate

-- Orbital types for each subshell
data S = S deriving (Show, Eq)
data P = Px | Py | Pz deriving (Show, Eq)
data D = Dxy | Dyz | Dxz | Dx2y2 | Dz2 deriving (Show, Eq)
data F = Fxxx | Fxxy | Fxxz | Fxyy | Fxyz | Fxzz | Fzzz deriving (Show, Eq)

-- The Orbital has a type and a number of electrons.
data Orbital subshellType = Orbital
  { orbitalType :: subshellType
  , electronCount :: Int
  , orientation :: Maybe Coordinate
  } deriving (Show, Eq)

-- Each SubShell consists of a list of Orbitals.
-- We ensure that all orbitals within a subshell are of the same type
-- by using the subshellType parameter.
newtype SubShell subshellType = SubShell
  { orbitals :: [Orbital subshellType]
  } deriving (Show, Eq)

-- Each Shell has a principal quantum number and consists of a list of SubShells.
data Shell = Shell
  { principalQuantumNumber :: Int
  , sSubShell :: Maybe (SubShell S)
  , pSubShell :: Maybe (SubShell P)
  , dSubShell :: Maybe (SubShell D)
  , fSubShell :: Maybe (SubShell F)
  } deriving (Show, Eq)

-- An Atom consists of a list of Shells.
type Shells = [Shell]
-- Hydrogen atom



-- Type class for subshells
class SubShellType subshellType where
  -- Function to get the maximum number of orbitals for a subshell type
  maxOrbitals :: SubShell subshellType -> Int

instance SubShellType S where
  maxOrbitals :: SubShell S -> Int
  maxOrbitals _ = 1

instance SubShellType P where
  maxOrbitals :: SubShell P -> Int
  maxOrbitals _ = 3

instance SubShellType D where
  maxOrbitals :: SubShell D -> Int
  maxOrbitals _ = 5

instance SubShellType F where
  maxOrbitals :: SubShell F -> Int
  maxOrbitals _ = 7

-- Hydrogen atom
hydrogen :: Shells
hydrogen =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 1 Nothing]) -- 1s^1
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

-- Carbon atom
carbon :: Shells
carbon =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate 1 0 0)), Orbital Py 1 (Just (Coordinate 0 1 0))]) -- 2p^2, with one electron in each of two p orbitals
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

-- Nitrogen atom
nitrogen :: Shells
nitrogen =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate 1 0 0)), Orbital Py 1 (Just (Coordinate 0 1 0)), Orbital Pz 1 (Just (Coordinate 0 0 1))]) -- 2p^3
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

-- Oxygen atom
oxygen :: Shells
oxygen =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 1 (Just (Coordinate 0 1 0)), Orbital Pz 1 (Just (Coordinate 0 0 1))]) -- 2p^4
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

-- Boron atom
boron :: Shells
boron =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate 1 0 0))]) -- 2p^1
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

-- Iron atom
iron :: Shells
iron =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 2p^6
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 3
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 3s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 3p^6
      , dSubShell = Just (SubShell [Orbital Dxy 1 (Just (Coordinate (1/sqrt 2) (1/sqrt 2) 0)), Orbital Dyz 1 (Just (Coordinate 0 (1/sqrt 2) (1/sqrt 2))), Orbital Dxz 1 (Just (Coordinate (1/sqrt 2) 0 (1/sqrt 2))), Orbital Dx2y2 1 (Just (Coordinate (1/sqrt 2) (-1/sqrt 2) 0)), Orbital Dz2 1 (Just (Coordinate 0 0 1))]) -- 3d^6
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 4
      , sSubShell = Just (SubShell [Orbital S 2 Nothing]) -- 4s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]