module Main where

import Chem.IO.SDF (readSDF)
import Chem.Molecule (Molecule, prettyPrintMolecule)
import Chem.Validate (validateMolecule, ValidationWarning(..))
import LogPModel (runLogPRegression)
import Text.Megaparsec (errorBundlePretty)

-- | Parse and validate benzene, then run LogP regression.
main :: IO ()
main = do
  putStrLn "Parsing molecules/benzene.sdf"
  parsed <- readSDF "molecules/benzene.sdf"
  case parsed of
    Left err -> putStrLn (errorBundlePretty err)
    Right mol ->
      case validateMolecule mol of
        Left errs -> do
          putStrLn "Benzene invalid:"
          mapM_ (putStrLn . show) errs
        Right (_, warns) -> do
          mapM_ (putStrLn . ("Warning: " ++) . show) warns
          putStrLn (prettyPrintMolecule mol)
          putStrLn "Running LogP regression over DB1 and predicting for DB2:"
          runLogPRegression mol 10 20 0.1
