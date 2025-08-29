This is a datatype for molecules, complete with orbitals, reaction dynamics and group theoretic properties. It corresponds to the article here: https://arxiv.org/abs/2501.13633

## Quick Start

```bash
stack build
stack exec chemalgprog
```

`package.yaml` is processed by `hpack` to generate `chemalgprog.cabal`.

## Reproducing Paper Experiments

The accompanying paper describes two illustrative experiments:

- **Molecular inference** – Metropolis–Hastings sampling of a three‑atom molecule scored against an observed structure. Running `stack exec chemalgprog` reproduces this experiment using `molecules/water.sdf`.
- **LogP regression** – Estimating partition coefficients from simple molecular features. After building, open a REPL with `stack exec -- ghci` and evaluate `LogPModel.main` to rerun the regression on the SDF files in `logp/`.

Sample SDF files for these experiments are provided in `molecules/` and `logp/`.

An example Haskell representation of a molecule is available in `src/Benzene.hs`, which defines the `benzene` structure programmatically.

The `examples/ParseMolecules.hs` program shows how to parse the provided `molecules/benzene.sdf` and `molecules/water.sdf` files:

```bash
stack exec parse-molecules
```

The example prints each molecule's `logS` value and structure using the library parsers.

Author: Oliver Goldstein (oliverjgoldstein@gmail.com / oliver.goldstein@reuben.ox.ac.uk)

## License

Distributed under the terms of the GNU Affero General Public License v3.0 only. See [LICENSE](LICENSE) for details.

## LazyPPL Disclaimer

The probabilistic programming components (`LazyPPL.hs` and `Distr.hs`) are taken from the [LazyPPL project](https://github.com/lazyppl-team/lazyppl) by Swaraj Dash, Younesse Kaddar, Hugo Paquet, and Sam Staton. These files were not written by us and are included here with permission.

## What the Program Does

The entrypoint `app/Main.hs` simply delegates to `TestInference.main`, so the core behavior is defined in `src/TestInference.hs`.

At startup, `TestInference.main`:

1. **Loads and validates data**
   - Reads the observed molecule from `molecules/water.sdf`.
   - Runs a validation check on the input structure.

2. **Defines a probabilistic model**
   - The model (`moleculeModel`) randomly samples atoms, bonds, and coordinates for a **3-atom molecule**.
   - It then scores the generated structure by comparing its geometry to the observed molecule using:
     - **Hausdorff distance** (shape similarity).
     - A **normal likelihood** on that distance.

3. **Runs Metropolis–Hastings inference**
   - Uses `mh 0.1` from the `LazyPPL` module (random-walk MCMC).
   - At each step:
     - Mutates random sites in the generative program.
     - Re-evaluates the model.
     - Accepts or rejects proposals via the MH acceptance ratio.

4. **Collects samples**
   - Drops the first **60000 draws** (burn-in).
   - Takes the next **2000 (molecule, weight) pairs**.
   - Prints the results.

---

**In short:**  
The executable performs **Metropolis–Hastings MCMC** to sample molecular structures that are consistent with the observed “water” molecule, according to the probabilistic model in `moleculeModel`.


