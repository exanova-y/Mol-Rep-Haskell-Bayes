{-# LANGUAGE DeriveGeneric , DeriveAnyClass #-}
module TestInference where
import Molecule
import Molecules.Methane
import LazyPPL
import GenerativeMolecule

main = do
  x <- map fst <$> take 200000 <$> drop 1 <$> mh 0.1 (crossMol methane) 
  putStrLn $ unlines (map show x)