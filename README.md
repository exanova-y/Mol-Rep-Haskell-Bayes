This is a datatype for molecules, complete with orbitals, reaction dynamics and group theoretic properties. It corresponds to the article here: https://arxiv.org/abs/2501.13633

## Quick Start

If you simply want to try the executable, build and run the default example:

```
stack build
stack exec chemalgprog
```

## Building from Source

The project uses `hpack` and `stack` for builds. To build from source:

1. Run `hpack` to generate `chemalgprog.cabal` from `package.yaml`.
2. Compile the library and executable with `stack build`.
3. Execute the sample program with `stack exec chemalgprog`.

## Reproducing Paper Experiments

The accompanying paper describes two illustrative experiments:

- **Molecular inference** – Metropolis–Hastings sampling of a three‑atom molecule scored against an observed structure. Running `stack exec chemalgprog` reproduces this experiment using `molecules/water.sdf` as the observed molecule.
- **LogP regression** – Estimating partition coefficients from simple molecular features. After building, open a REPL with `stack exec -- ghci` and evaluate `LogPModel.main` with your chosen parameters to rerun the regression on the SDF files in `logp/`.

## Datasets

Sample SDF files are expected in the `molecules/` and `logp/` directories. These correspond to the data used in the paper’s experiments: `molecules/water.sdf` provides the observed molecule for the inference demo, while `logp/DB1.sdf` (training) and `logp/DB2.sdf` (evaluation) supply logP values for the regression experiment. Additional molecules can be downloaded from public chemistry databases such as [PubChem](https://pubchem.ncbi.nlm.nih.gov/) or [ChEMBL](https://www.ebi.ac.uk/chembl/) and placed in these folders.

Currently the molecular simulation does not have any predefined molecules beyond the provided examples, so replace *undefined* with a molecule of your choosing.

Alternatively use `nix-shell shell.nix` as a nix file to load in only the essential pre-requisites.

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
