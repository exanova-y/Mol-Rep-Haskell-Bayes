This is a datatype for molecules, complete with orbitals, reaction dynamics and group theoretic properties. It corresponds to the article here: https://arxiv.org/abs/2501.13633

## Quick Start

- `stack build`
- `stack exec chemalgprog`

## Datasets

Sample SDF files are expected in the `molecules/` and `logp/` directories. Additional molecules can be downloaded from public chemistry databases such as [PubChem](https://pubchem.ncbi.nlm.nih.gov/) or [ChEMBL](https://www.ebi.ac.uk/chembl/) and placed in these folders.

Currently the molecular simulation does not have any predefined molecules, but they will appear soon, so replace *undefined* with a molecule of your choosing.

Alternatively use nix-shell shell.nix as a nix file to load in only the essential pre-requisites.

The following libraries are pre-requisites to use the probabilistic programming aspects of the library:

- random
- monad-extras
- log-domain
- statistics
- megaparsec

This library was last tested on:
base-4.17.2.1  
ghc   9.4.8   
cabal 3.14.1.1  

This library is also listed here: https://github.com/OpenSourceMolecularModeling/OpenSourceMolecularModeling.github.io

Author: Oliver Goldstein (oliverjgoldstein@gmail.com / oliver.goldstein@reuben.ox.ac.uk)
