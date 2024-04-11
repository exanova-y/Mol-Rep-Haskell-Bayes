module Orbital where 

import Distr 
import LazyPPL

-- Orbital types for each subshell
data S = S deriving (Show, Eq)
data P = Px | Py | Pz deriving (Show, Eq)
data D = Dxy | Dyz | Dxz | Dx2y2 | Dz2 deriving (Show, Eq)
data F = Fxxx | Fxxy | Fxxz | Fxyy | Fxyz | Fxzz | Fzzz deriving (Show, Eq)

-- The Orbital has a type and a number of electrons.
data Orbital subshellType = Orbital
  { orbitalType :: subshellType
  , electronCount :: Int
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


hydrogen :: Shells
hydrogen =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 1]) -- 1s^1
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
      , sSubShell = Just (SubShell [Orbital S 2]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital S 2]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 1, Orbital Py 1]) -- 2p^2, with one electron in each of two p orbitals
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

-- Nitrogen atom
nitrogen :: Shells
nitrogen =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 2]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital S 2]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 1, Orbital Py 1, Orbital Pz 1]) -- 2p^3
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

-- Oxygen atom
oxygen :: Shells
oxygen =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 2]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital S 2]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2, Orbital Py 1, Orbital Pz 1]) -- 2p^4
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

-- Boron atom
boron :: Shells
boron =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 2]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital S 2]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 1]) -- 2p^1
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]

-- Iron atom
iron :: Shells
iron =
  [ Shell
      { principalQuantumNumber = 1
      , sSubShell = Just (SubShell [Orbital S 2]) -- 1s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 2
      , sSubShell = Just (SubShell [Orbital S 2]) -- 2s^2
      , pSubShell = Just (SubShell [Orbital Px 2, Orbital Py 2, Orbital Pz 2]) -- 2p^6
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 3
      , sSubShell = Just (SubShell [Orbital S 2]) -- 3s^2
      , pSubShell = Just (SubShell [Orbital Px 2, Orbital Py 2, Orbital Pz 2]) -- 3p^6
      , dSubShell = Just (SubShell [Orbital Dxy 1, Orbital Dyz 1, Orbital Dxz 1, Orbital Dx2y2 1, Orbital Dz2 1]) -- 3d^6
      , fSubShell = Nothing
      }
  , Shell
      { principalQuantumNumber = 4
      , sSubShell = Just (SubShell [Orbital S 2]) -- 4s^2
      , pSubShell = Nothing
      , dSubShell = Nothing
      , fSubShell = Nothing
      }
  ]



bohrRadius = 



-- -- Constants
-- a0 :: Double -- Bohr radius
-- a0 = 1.0

-- psiNs :: Int -> Double -> Double
-- psiNs n r
--   | n == 1 = (1 / sqrt(pi * a0^3)) * exp (-r / a0)
--   | n == 2 = (1 / (4 * sqrt(2*pi * a0^3))) * (2 - r / a0) * exp (-r / (2* a0))
--   | otherwise = 0


-- psi2px :: Double -> Double -> Double
-- psi2px r cosTheta = (1 / (4 * sqrt(2*pi * a0^3))) * (r / a0) * exp (-r / (2* a0)) * cosTheta

-- psi2py :: Double -> Double -> Double
-- psi2py r cosTheta = (1 / (4 * sqrt(2*pi * a0^3))) * (r / a0) * exp (-r / (2* a0)) * cosTheta

-- psi2pz :: Double -> Double -> Double
-- psi2pz r cosTheta = (1 / (4 * sqrt(2*pi * a0^3))) * (r / a0) * exp (-r / (2* a0)) * cosTheta

-- -- Calculate the total number of electrons in a Shell
-- electronCountInShell :: Shell -> Int
-- electronCountInShell shell =
--   sum
--     [ maybe 0 (sum . map electronCount . orbitals) (sSubShell shell),
--       maybe 0 (sum . map electronCount . orbitals) (pSubShell shell),
--       maybe 0 (sum . map electronCount . orbitals) (dSubShell shell),
--       maybe 0 (sum . map electronCount . orbitals) (fSubShell shell)
--     ]

-- -- Calculate the distribution for an orbital based on its wave function
-- orbitalDistribution :: (Double -> Double) -> Int -> Prob Double
-- orbitalDistribution waveFunc electronCount =
--   do
--     r <- exponential 1
--     let prob = waveFunc r ^ 2 * r ^ 2
--     return (fromIntegral electronCount * prob)

-- -- Calculate the distribution for a p orbital based on its wave function
-- pOrbitalDistribution :: (Double -> Double -> Double) -> Int -> Prob Double
-- pOrbitalDistribution waveFunc electronCount =
--   do
--     r <- exponential 1
--     cosTheta <- uniformbounded (-1) 1
--     let prob = waveFunc r cosTheta ^ 2 * r ^ 2 * sin (acos cosTheta)
--     return (fromIntegral electronCount * prob)

-- -- Combine the distributions for each orbital type in a shell
-- shellDistribution :: Shell -> Prob Double
-- shellDistribution shell =
--   do
--     sDist <- maybe (return 0) (fmap sum . mapM (orbitalDistribution psi1s . electronCount) . orbitals) (sSubShell shell)
--     pDist <- maybe (return 0) (fmap sum . mapM (pOrbitalDistribution psi2px . electronCount) . orbitals) (pSubShell shell)
--     dDist <- maybe (return 0) (fmap sum . mapM (orbitalDistribution (const 0) . electronCount) . orbitals) (dSubShell shell)
--     fDist <- maybe (return 0) (fmap sum . mapM (orbitalDistribution (const 0) . electronCount) . orbitals) (fSubShell shell)
--     return (sDist + pDist + dDist + fDist)

-- -- Sum the distributions for all shells to create a distribution for the entire atom
-- atomDistribution :: Shells -> Prob Double
-- atomDistribution shells = fmap sum (mapM shellDistribution shells)