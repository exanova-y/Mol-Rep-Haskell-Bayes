module Main where

import ParserSingle (parseSDFFileNoLog)

import Chem.Molecule (prettyPrintMolecule)
import Text.Megaparsec (errorBundlePretty)

-- | Simple example parsing the provided benzene and water SDF files.
main :: IO ()
main = do
    putStrLn "Parsing benzene.sdf"
    benzene <- parseSDFFileNoLog "molecules/benzene.sdf"
    case benzene of
        Left err -> putStrLn $ errorBundlePretty err
        Right mol -> putStrLn $ prettyPrintMolecule mol

    putStrLn "\nParsing water.sdf"
    water <- parseSDFFileNoLog "molecules/water.sdf"
    case water of
        Left err -> putStrLn $ errorBundlePretty err
        Right mol -> putStrLn $ prettyPrintMolecule mol
