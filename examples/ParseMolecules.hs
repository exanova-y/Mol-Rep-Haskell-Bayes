module Main where

import Parser (parseSDFFile)
import Molecule (prettyPrintMolecule)
import Text.Megaparsec (errorBundlePretty)

-- | Simple example parsing the provided benzene and water SDF files.
main :: IO ()
main = do
    putStrLn "Parsing benzene.sdf"
    benzene <- parseSDFFile "molecules/benzene.sdf"
    case benzene of
        Left err -> putStrLn $ errorBundlePretty err
        Right (mol, logS) -> do
            putStrLn $ "logS: " ++ show logS
            putStrLn $ prettyPrintMolecule mol

    putStrLn "\nParsing water.sdf"
    water <- parseSDFFile "molecules/water.sdf"
    case water of
        Left err -> putStrLn $ errorBundlePretty err
        Right (mol, logS) -> do
            putStrLn $ "logS: " ++ show logS
            putStrLn $ prettyPrintMolecule mol
