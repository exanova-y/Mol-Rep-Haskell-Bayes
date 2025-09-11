This is a datatype for molecules, complete with orbitals, reaction dynamics and group theoretic properties. It corresponds to the article here: https://arxiv.org/abs/2501.13633

## Quick Start

1. **Build the project**

   ```bash
   stack build
   ```

   `package.yaml` is processed by `hpack` to generate `chemalgprog.cabal`.

2. **Run the demonstration program**

   ```bash
   stack exec chemalgprog
   ```

   This executable:

   - Parses and validates `molecules/benzene.sdf`, printing the structure as a sanity check.
   - Parses `molecules/water.sdf` and uses it as a test molecule.
   - Performs a Metropolis–Hastings regression on the molecules in `logp/DB1.sdf` to learn coefficients that predict the partition coefficient (logP).
   - Applies the learned model to predict the logP of water and then prints predicted and observed values for each molecule in `logp/DB2.sdf`.

3. **Parse molecules independently (optional)**

   ```bash
   stack exec parse-molecules
   ```

   The example pretty-prints the contents of `molecules/benzene.sdf` and `molecules/water.sdf` using the library parser.

Sample SDF files for these experiments are provided in `molecules/` and `logp/`.

An example Haskell representation of a molecule is available in `src/Benzene.hs`, which defines the `benzene` structure programmatically.


Author: Oliver Goldstein (oliverjgoldstein@gmail.com / oliver.goldstein@reuben.ox.ac.uk)

## License

Distributed under the terms of the GNU Affero General Public License v3.0 only. See [LICENSE](LICENSE) for details.

## LazyPPL Disclaimer

The probabilistic programming components (`LazyPPL.hs` and `Distr.hs`) are taken from the [LazyPPL project](https://github.com/lazyppl-team/lazyppl) by Swaraj Dash, Younesse Kaddar, Hugo Paquet, and Sam Staton. These files were not written by us and are included here with permission.

## What the Program Does

The `chemalgprog` executable parses the sample benzene molecule (`molecules/benzene.sdf`) to showcase structural validation, then loads `molecules/water.sdf` as the test molecule for the regression. Coefficients are inferred from the training set `logp/DB1.sdf`, the logP of water is predicted from these coefficients, and predicted versus observed values for all molecules in `logp/DB2.sdf` are printed. The `parse-molecules` example demonstrates parsing and validating multiple SDF files.


