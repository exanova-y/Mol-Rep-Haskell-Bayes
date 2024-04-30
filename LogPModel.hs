module LogPModel where

import Molecule
import Distr
import LazyPPL
import Control.Monad
import Molecules.Methane
import Numeric.Log
import Parser
import qualified Data.Map as M
import Extra


-- | A simple feature extraction function that counts the number of atoms in a molecule
moleculeSize :: Molecule -> Double
moleculeSize = fromIntegral . length . atoms

-- | Compute the total molecular weight of a molecule
moleculeWeight :: Molecule -> Double
moleculeWeight = Prelude.sum . map (atomicWeight . atomicAttr) . atoms

-- | Compute the approximate surface area of a molecule (using a simple heuristic)
moleculeSurfaceArea :: Molecule -> Double
moleculeSurfaceArea mol = let size = moleculeSize mol in 4.0 * pi * (size ** (2.0/3.0))

-- | Compute the total bond order of a molecule
moleculeBondOrder :: Molecule -> Double
moleculeBondOrder = fromIntegral . Prelude.sum . map (uncurry bondOrder) . M.toList . bonds
  where
    bondOrder _ HydrogenBond = 1
    bondOrder _ (CovalentBond n _) = n
    bondOrder _ IonicBond = 1

-- | The generative model for logP values
logPModel :: (Molecule, Double) -> Meas (Double, Double, Double, Double, Double)
logPModel (mol, observedLogP) = do
  -- Prior distributions for model parameters
  intercept <- sample $ normal 0.0 0.1
  sizeCoeff <- sample $ normal 0.0 0.1
  weightCoeff <- sample $ normal 0.0 0.1
  surfaceAreaCoeff <- sample $ normal 0.0 0.1
  bondOrderCoeff <- sample $ normal 0.0 0.1

  -- Compute the molecule features
  let size = moleculeSize mol
  let weight = moleculeWeight mol
  let surfaceArea = moleculeSurfaceArea mol
  let bondOrder = moleculeBondOrder mol

  -- Predict logP value using a linear combination of features
  let predictedLogP = intercept +
                      sizeCoeff * size +
                      weightCoeff * weight +
                      surfaceAreaCoeff * surfaceArea +
                      bondOrderCoeff * bondOrder

  -- Score the predicted value against the observed value
  score $ normalPdf observedLogP 0.2 predictedLogP

  -- Return the coefficients
  return (intercept, sizeCoeff, weightCoeff, surfaceAreaCoeff, bondOrderCoeff)

-- | Perform inference on the model given observed data
inferLogP :: [(Molecule, Double)] -> Meas [(Double, Double, Double, Double, Double)]
inferLogP observedData = mapM inferSingle observedData
  where
    inferSingle :: (Molecule, Double) -> Meas (Double, Double, Double, Double, Double)
    inferSingle (mol, observedLogP) = do
      (intercept, sizeCoeff, weightCoeff, surfaceAreaCoeff, bondOrderCoeff) <- logPModel (mol, observedLogP)
      return (intercept, sizeCoeff, weightCoeff, surfaceAreaCoeff, bondOrderCoeff)

-- Function to calculate the pointwise mean of a list of tuples
pointwiseMean :: [(Double, Double, Double, Double, Double)] -> (Double, Double, Double, Double, Double)
pointwiseMean xs = (mean (map fst5 xs),
                    mean (map snd5 xs),
                    mean (map thd5 xs),
                    mean (map fth5 xs),
                    mean (map fifth xs))
  where
    mean :: [Double] -> Double
    mean xs = Prelude.sum xs / fromIntegral (length xs)

-- Helper functions to access tuple elements
fst5 :: (a, b, c, d, e) -> a
fst5 (x, _, _, _, _) = x

snd5 :: (a, b, c, d, e) -> b
snd5 (_, x, _, _, _) = x

thd5 :: (a, b, c, d, e) -> c
thd5 (_, _, x, _, _) = x

fth5 :: (a, b, c, d, e) -> d
fth5 (_, _, _, x, _) = x

fifth :: (a, b, c, d, e) -> e
fifth (_, _, _, _, x) = x

main :: Int -> Int -> Int -> Double -> IO ()
main numMol burnIn samplesize jitter = do
    let db1FilePath = "./logp/QuickDB1.sdf"
    db1Molecules <- parseDB1File db1FilePath
    putStrLn $ "Parsed " ++ show (length db1Molecules) ++ " molecules from file: " ++ db1FilePath
    
    -- Take the first 100 molecules
    let observedData = take numMol db1Molecules
    
    -- Run inference
    inferredSamples <- mh jitter (inferLogP observedData)
    
    -- Take the last n elements of the list
    let lastNSamples = map pointwiseMean $ take samplesize $ drop burnIn $ map (\(xs, _) -> xs) inferredSamples
    
    -- Average the mean coefficients pointwise again
    let means = pointwiseMean lastNSamples
    
    let intercept = fst5 means
    let sizeCoeff = snd5 means
    let weightCoeff = thd5 means
    let surfaceAreaCoeff = fth5 means
    let bondOrderCoeff = fifth means
    -- -- Print the mean coefficients and the pointwise mean of mean coefficients
    putStrLn $ "Mean Intercept: " ++ show intercept
    putStrLn $ "Mean Size Coefficient: " ++ show sizeCoeff
    putStrLn $ "Mean Weight Coefficient: " ++ show weightCoeff
    putStrLn $ "Mean Surface Area Coefficient: " ++ show surfaceAreaCoeff
    putStrLn $ "Mean Bond Order Coefficient: " ++ show bondOrderCoeff

    let size = moleculeSize testMol
    let weight = moleculeWeight testMol
    let surfaceArea = moleculeSurfaceArea testMol
    let bondOrder = moleculeBondOrder testMol

    let predictedLogP = intercept +
                        sizeCoeff * size +
                        weightCoeff * weight +
                        surfaceAreaCoeff * surfaceArea +
                        bondOrderCoeff * bondOrder

    putStrLn $ "Predicted LogP for testMol: " ++ show predictedLogP

    -- Open the DB2.sdf file and process each molecule
    let db2FilePath = "./logp/DB2.sdf"
    db2Molecules <- parseDB1File db2FilePath
    putStrLn $ "Parsed " ++ show (length db2Molecules) ++ " molecules from file: " ++ db2FilePath
    
    putStrLn "Predicted and Actual LogP values for DB2 molecules:"
    forM_ db2Molecules $ \(mol, actualLogP) -> do
        let size = moleculeSize mol
        let weight = moleculeWeight mol
        let surfaceArea = moleculeSurfaceArea mol
        let bondOrder = moleculeBondOrder mol
        
        let predictedLogP = intercept +
                            sizeCoeff * size +
                            weightCoeff * weight +
                            surfaceAreaCoeff * surfaceArea +
                            bondOrderCoeff * bondOrder
        
        putStrLn $ "Molecule: " ++ show mol
        putStrLn $ "Predicted LogP: " ++ show predictedLogP
        putStrLn $ "Actual LogP: " ++ show actualLogP
        putStrLn ""