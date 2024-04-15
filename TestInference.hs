{-# LANGUAGE DeriveGeneric , DeriveAnyClass #-}
module TestInference where
import Molecule
import Molecules.Methane
import LazyPPL
import Distr
import RandomGraph
import System.Random


main :: IO ()
main = do
    g <- newStdGen  -- Get a new standard generator
    let randomNumbers = randoms g :: [Double]  -- Generate an infinite list of random numbers
        tree = randomTree g  -- Create a tree using the random generator
        randomValue = runProb LazyPPL.uniform tree  -- Run the uniform Prob to get a single Double
    print randomValue  -- Print the random value

-- main :: IO ()
-- main = do
--   let numAtoms = (sample $ uniform uniformdiscrete) + 1
--   mol <- mh 0.1 (sampleMolecule numAtoms)
--   let (molecule, _) = head mol
--   putStrLn $ prettyPrintMolecule molecule
  
-- main = do
--   x <- map fst <$> take 200000 <$> drop 1 <$> mh 0.1 ( methane) 
--   putStrLn $ unlines (map show x)