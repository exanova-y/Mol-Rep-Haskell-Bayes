This is a datatype for molecules, complete with orbitals, reaction dynamics and group theoretic properties. It corresponds to the article here: https://arxiv.org/abs/2501.13633

## Quick Start

```bash
stack build
stack exec chemalgprog
```

`package.yaml` is processed by `hpack` to generate `chemalgprog.cabal`.

## Reproducing Paper Experiments

The accompanying paper describes two illustrative experiments:

- **LogP regression** – `stack exec chemalgprog` parses and validates `molecules/benzene.sdf`, then performs a Metropolis–Hastings regression over the molecules in `logp/DB1.sdf` to fit coefficients predicting the partition coefficient (logP). The learned model is applied to the molecules in `logp/DB2.sdf` and prints both predicted and observed values.

Sample SDF files for this experiment are provided in `molecules/` and `logp/`.

An example Haskell representation of a molecule is available in `src/Benzene.hs`, which defines the `benzene` structure programmatically.

The `examples/ParseMolecules.hs` program shows how to parse the provided `molecules/benzene.sdf` and `molecules/water.sdf` files:

```bash
stack exec parse-molecules
```

The example pretty-prints each molecule's structure using the library parser.


Author: Oliver Goldstein (oliverjgoldstein@gmail.com / oliver.goldstein@reuben.ox.ac.uk)

## License

Distributed under the terms of the GNU Affero General Public License v3.0 only. See [LICENSE](LICENSE) for details.

## LazyPPL Disclaimer

The probabilistic programming components (`LazyPPL.hs` and `Distr.hs`) are taken from the [LazyPPL project](https://github.com/lazyppl-team/lazyppl) by Swaraj Dash, Younesse Kaddar, Hugo Paquet, and Sam Staton. These files were not written by us and are included here with permission.

## What the Program Does

The `chemalgprog` executable parses the sample benzene molecule (`molecules/benzene.sdf`), validates it with `Chem.Validate`, pretty‑prints the structure, and then runs the `LogPModel` regression. Coefficients are inferred from the training set `logp/DB1.sdf` and used to predict logP values for the molecules in `logp/DB2.sdf`. The `parse-molecules` example demonstrates parsing and validating multiple SDF files.


