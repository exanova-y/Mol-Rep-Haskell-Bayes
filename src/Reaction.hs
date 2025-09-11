module Reaction where

import Chem.Molecule
import Chem.Dietz ()
import SampleMolecules (hydrogen, oxygen, water)


-- Reactions or transformations between chemical species
data Reaction = Reaction
    { reactants :: [(Double, Molecule)]
    , products :: [(Double, Molecule)]
    , conditions :: [Condition]
    , rate :: Double
    }

-- Conditions under which a reaction occurs
data Condition = TempCondition {temperature :: Double}  
                | PressureCondition {presssure :: Double}

data Times = Times { startTime :: Double, endTime :: Double }

-- Example reaction: 2H2 + O2 -> 2H2O
exampleReaction :: Reaction
exampleReaction = Reaction
  { reactants = [(2.0, hydrogen), (1.0, oxygen)]
  , products = [(2.0, water)]
  , conditions =
      [ TempCondition 500.0
      , PressureCondition 1.0
      ]
  , rate = 0.1
  }
