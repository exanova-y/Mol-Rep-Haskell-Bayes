module Reaction where
import Molecule

-- Reactions or transformations between chemical species
data Reaction = Reaction
    { reactants :: [(Double, Atom)]
    , products :: [(Double, Atom)]
    , conditions :: [Condition]
    , rate :: ReactionRate
    }

-- Conditions under which a reaction occurs
data Condition = TempCondition {temperature :: Double Times}  
                | PressureCondition {presssure :: Double Times}

newtype Times = (StartTime Double, EndTime Double)

newtype ChemicalReactionNetwork = ChemicalReactionNetwork { reactions :: [Reaction] }

-- Example reaction: 2H2 + O2 -> 2H2O
exampleReaction :: Reaction
exampleReaction = Reaction
  { reactants = [(2.0, h2), (1.0, o2)]
  , products = [(2.0, h2o)]
  , conditions =
      [ TempCondition 500.0 (Times (0.0, 10.0))
      , PressureCondition 1.0 (Times (0.0, 10.0))
      ]
  , rate = ReactionRate 0.1
  }
