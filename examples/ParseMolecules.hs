module Main where

import Chem.IO.SDF (readSDF)

import Chem.Molecule (prettyPrintMolecule)
import Chem.Dietz ()
import Text.Megaparsec (errorBundlePretty)
import Chem.Validate (validateMolecule)

-- | Simple example parsing the provided benzene and water SDF files.
main :: IO ()
main = do
    putStrLn "Parsing benzene.sdf"
    benzene <- readSDF "molecules/benzene.sdf"
    case benzene of
        Left err -> putStrLn $ errorBundlePretty err
        Right mol ->
          case validateMolecule mol of
            Left errs -> do
              putStrLn "Benzene invalid:"
              mapM_ (putStrLn . show) errs
            Right _ -> putStrLn $ prettyPrintMolecule mol

    putStrLn "\nParsing water.sdf"
    water <- readSDF "molecules/water.sdf"
    case water of
        Left err -> putStrLn $ errorBundlePretty err
        Right mol ->
          case validateMolecule mol of
            Left errs -> do
              putStrLn "Water invalid:"
              mapM_ (putStrLn . show) errs
            Right _ -> putStrLn $ prettyPrintMolecule mol
