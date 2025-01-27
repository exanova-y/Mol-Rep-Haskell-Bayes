This is a datatype for molecules, complete with orbitals, reaction dynamics and group theoretic properties.

To use this, install LazyPPL with stack and copy all of these files into the Src directory of LazyPPL.

Once you have done this run stack ghci TestInference and then type main to run inference.

Alternatively use nix-shell shell.nix as a nix file to load in only the essential pre-requisties.

The following libraries are pre-requisites to use the probabilistic programming aspects of the library.
<ul>
<li>random</li>
<li>monad-extras</li>
<li>log-domain</li>
<li>statistics</li>
<li>megaparsec</li>
</ul>

Last tested on: <br> 
base-4.17.2.1 <br>
ghc   9.4.8   <br>
cabal 3.14.1.1 <br>


<ul>
  <li>:set -package transformers</li>
  <li>:set -package random</li>
  <li>:set -package mtl</li>
  <li>:set -package deepseq</li>
  <li>:set -package ghc-heap</li>
  <li>:set -package containers</li>
  <li>:set -package array</li>
  <li>:set -package vector</li>
</ul>
