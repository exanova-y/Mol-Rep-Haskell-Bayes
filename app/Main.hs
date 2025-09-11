module Main where

import Chem.IO.SDF (readSDF)
import Chem.Molecule (prettyPrintMolecule)
import Chem.Validate (validateMolecule, ValidationWarning(..))
import LogPModel (runLogPRegression)
import Text.Megaparsec (errorBundlePretty)

-- | Parse and validate benzene for demonstration,
-- then predict the logP of water using the learned model.
main :: IO ()
main = do
  putStrLn "Parsing molecules/benzene.sdf"
  benzeneParsed <- readSDF "molecules/benzene.sdf"
  case benzeneParsed of
    Left err -> putStrLn (errorBundlePretty err)
    Right benzene ->
      case validateMolecule benzene of
        Left errs -> do
          putStrLn "Benzene invalid:"
          mapM_ (putStrLn . show) errs
        Right (_, warns) -> do
          mapM_ (putStrLn . ("Warning: " ++) . show) warns
          putStrLn (prettyPrintMolecule benzene)
          putStrLn "Parsing molecules/water.sdf"
          waterParsed <- readSDF "molecules/water.sdf"
          case waterParsed of
            Left err -> putStrLn (errorBundlePretty err)
            Right water ->
              case validateMolecule water of
                Left errs2 -> do
                  putStrLn "Water invalid:"
                  mapM_ (putStrLn . show) errs2
                Right _ -> do
                  putStrLn "Running LogP regression over DB1 and predicting for water and DB2:"
                  runLogPRegression water 10 20 0.1
