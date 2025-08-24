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

Author: Oliver Goldstein (oliverjgoldstein@gmail.com / oliver.goldstein@reuben.ox.ac.uk)
