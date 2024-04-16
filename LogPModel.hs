module LogPModel where

import Molecule
import Distr
import LazyPPL
import Control.Monad
import Numeric.Log

-- | A simple feature extraction function that counts the number of atoms in a molecule
moleculeSize :: Molecule -> Double
moleculeSize = fromIntegral . length . atoms

-- | The generative model for logP values
logPModel :: Molecule -> Meas Double
logPModel mol = do
    -- Prior distributions for model parameters
    intercept <- sample $ normal 0 1
    sizeCoeff <- sample $ normal 0 1
    
    -- Compute the molecule size feature
    let size = moleculeSize mol
    
    -- Predict logP value using a linear combination of features
    let logP = intercept + sizeCoeff * size
    
    -- Return the predicted logP value
    return logP

-- | Perform inference on the model given observed data
inferLogP :: [(Molecule, Double)] -> Meas ()
inferLogP observedData = do
    -- Loop over each observed molecule and logP value
    forM_ observedData $ \(mol, observedLogP) -> do
        -- Predict logP value for the current molecule
        predictedLogP <- logPModel mol
        
        -- Score the predicted value against the observed value
        scoreLog $ Exp $ log $ normalPdf predictedLogP 1 observedLogP

-- | Run the inference and return the posterior distribution
runLogPInference :: [(Molecule, Double)] -> Meas Double
runLogPInference observedData = do
    inferLogP observedData
    logPModel (fst $ head observedData)