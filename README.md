This is a datatype for molecules, complete with orbitals, reaction dynamics and group theoretic properties.

To use this, install LazyPPL with stack and copy all of these files into the Src directory of LazyPPL.

Once you have done this run stack ghci TestInference and then type main to run inference.

Alternatively use nix-shell shell.nix as a nix file to load in only the essential pre-requisties.

The following libraries are pre-requisites to use the probabilistic programming aspects of the library.
random
monad-extras
log-domain
statistics
megaparsec
