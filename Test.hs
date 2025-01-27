module Test where
import Parser 
import Serialisable
import Molecule
import Orbital

test2 :: IO ()
test2 = do
    let db1FilePath = "./molecules/benzene.sdf"
    db1Molecules <- parseDB1File db1FilePath
    writeMoleculeToFile "./molecules/" (fst . head $ dblMolecules)