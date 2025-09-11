module Reaction where
import Chem.Molecule


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
  { reactants = [(2.0, undefined), (1.0, undefined)]
  , products = [(2.0, undefined)]
  , conditions =
      [ TempCondition 500.0
      , PressureCondition 1.0
      ]
  , rate = 0.1
  }
