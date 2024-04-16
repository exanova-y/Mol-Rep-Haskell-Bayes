{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Orbital where 

import Distr 
import LazyPPL
import Data.Maybe
import Coordinate

data Nat = Z | Succ Nat

type SoMax = Succ Z -- 1
type PMax = Succ (Succ (Succ Z)) -- 3
type DMax = Succ (Succ (Succ (Succ (Succ Z)))) -- 5
type FMax = Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))) -- 7

-- Orbital types for each subshell
data So = So deriving (Show, Eq, Read)
data P = Px | Py | Pz deriving (Show, Eq, Read)
data D = Dxy | Dyz | Dxz | Dx2y2 | Dz2 deriving (Show, Eq, Read)
data F = Fxxx | Fxxy | Fxxz | Fxyy | Fxyz | Fxzz | Fzzz deriving (Show, Eq, Read)

-- The Orbital has a type and a number of electrons.
data Orbital subshellType = Orbital
  { orbitalType :: subshellType
  , electronCount :: Int
  , orientation :: Maybe Coordinate
  } deriving (Show, Eq, Read)

-- Each SubShell consists of a list of Orbitals.
-- We ensure that all orbitals within a subshell are of the same type
-- by using the subshellType parameter.
newtype SubShell (maxOrbitals :: Nat) subshellType = SubShell
  { orbitals :: [Orbital subshellType]
  } deriving (Show, Eq, Read)

-- Each Shell has a principal quantum number and consists of a list of SubShells.
data Shell = Shell
  { principalQuantumNumber :: Int
  , sSubShell :: Maybe (SubShell SoMax So)
  , pSubShell :: Maybe (SubShell PMax P)
  , dSubShell :: Maybe (SubShell DMax D)
  , fSubShell :: Maybe (SubShell FMax F)
  } deriving (Show, Eq, Read)

-- An Atom consists of a list of Shells.
type Shells = [Shell]
-- Hydrogen atom

class SubShellType subshellType where
  type MaxOrbitals subshellType :: Nat

instance SubShellType So where
  type MaxOrbitals So = SoMax

instance SubShellType P where
  type MaxOrbitals P = PMax

instance SubShellType D where
  type MaxOrbitals D = DMax

instance SubShellType F where
  type MaxOrbitals F = FMax

-- Hydrogen atom
hydrogen :: Shells
hydrogen =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital So 1 Nothing]) -- 1s^1
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
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
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
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
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
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
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
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
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
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 2p^6
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 3
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 3s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 3p^6
      , dSubShell = Just (SubShell [Orbital Dxy 1 (Just (Coordinate (1/sqrt 2) (1/sqrt 2) 0)), Orbital Dyz 1 (Just (Coordinate 0 (1/sqrt 2) (1/sqrt 2))), Orbital Dxz 1 (Just (Coordinate (1/sqrt 2) 0 (1/sqrt 2))), Orbital Dx2y2 1 (Just (Coordinate (1/sqrt 2) (-1/sqrt 2) 0)), Orbital Dz2 1 (Just (Coordinate 0 0 1))]) -- 3d^6
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 4
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 4s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

chlorine :: Shells
chlorine =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 2p^5
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 3
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 3s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 3p^5
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

fluorine :: Shells
fluorine =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 1 (Just (Coordinate 0 0 1))]) -- 2p^5
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

sulfur :: Shells
sulfur =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 2p^6
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 3
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 3s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 1 (Just (Coordinate 0 1 0)), Orbital Pz 1 (Just (Coordinate 0 0 1))]) -- 3p^4
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

bromine :: Shells
bromine =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 2p^6
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 3
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 3s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 3p^6
      , dSubShell = Just (SubShell [Orbital Dxy 2 (Just (Coordinate (1/sqrt 2) (1/sqrt 2) 0)), Orbital Dyz 2 (Just (Coordinate 0 (1/sqrt 2) (1/sqrt 2))), Orbital Dxz 2 (Just (Coordinate (1/sqrt 2) 0 (1/sqrt 2))), Orbital Dx2y2 2 (Just (Coordinate (1/sqrt 2) (-1/sqrt 2) 0)), Orbital Dz2 2 (Just (Coordinate 0 0 1))]) -- 3d^10
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 4
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 4s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 1 (Just (Coordinate 0 1 0)), Orbital Pz 0 Nothing]) -- 4p^5
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

phosphorus :: Shells
phosphorus =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 2p^6
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 3
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 3s^2
      , pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate 1 0 0)), Orbital Py 1 (Just (Coordinate 0 1 0)), Orbital Pz 1 (Just (Coordinate 0 0 1))]) -- 3p^3
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

iodine :: Shells
iodine =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 2p^6
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 3
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 3s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 3p^6
      , dSubShell = Just (SubShell [Orbital Dxy 2 (Just (Coordinate (1/sqrt 2) (1/sqrt 2) 0)), Orbital Dyz 2 (Just (Coordinate 0 (1/sqrt 2) (1/sqrt 2))), Orbital Dxz 2 (Just (Coordinate (1/sqrt 2) 0 (1/sqrt 2))), Orbital Dx2y2 2 (Just (Coordinate (1/sqrt 2) (-1/sqrt 2) 0)), Orbital Dz2 2 (Just (Coordinate 0 0 1))]) -- 3d^10
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 4
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 4s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 2 (Just (Coordinate 0 1 0)), Orbital Pz 2 (Just (Coordinate 0 0 1))]) -- 4p^6
      , dSubShell = Just (SubShell [Orbital Dxy 2 (Just (Coordinate (1/sqrt 2) (1/sqrt 2) 0)), Orbital Dyz 2 (Just (Coordinate 0 (1/sqrt 2) (1/sqrt 2))), Orbital Dxz 2 (Just (Coordinate (1/sqrt 2) 0 (1/sqrt 2))), Orbital Dx2y2 2 (Just (Coordinate (1/sqrt 2) (-1/sqrt 2) 0)), Orbital Dz2 2 (Just (Coordinate 0 0 1))]) -- 4d^10
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 5
      , sSubShell = Just (SubShell [Orbital So 2 Nothing]) -- 5s^2
      , pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 1 (Just (Coordinate 0 1 0)), Orbital Pz 0 Nothing]) -- 5p^5
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]








