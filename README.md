This is a datatype for molecules, complete with orbitals, reaction dynamics and group theoretic properties.

To use this, install LazyPPL (https://github.com/lazyppl-team/lazyppl) with stack and copy all of these files into the Src directory of LazyPPL.

<b>Alternatively</b> use the files LazyPPL.hs and Distr.hs, which contain the core algorithms. 

These files have not been written by us, but by https://msp.cis.strath.ac.uk/act2022/papers/ACT2022_paper_8887.pdf
Swaraj Dash, Younesse Kaddar, Hugo Paquet, Sam Staton

These files are included with permission.

Once you have done this run stack ghci TestInference and then type main to run inference. Currently h20 is not in the libary, but it will be soon, so replace undefined with a molecule of your choosing.

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

To run TestInference.hs interactively, type the following into your command line:

ghci TestInference.hs -package transformers -package random -package mtl -package deepseq -package ghc-heap -package containers -package array -package vector -package directory -package filepath -package bytestring -package text

This is as the following are not exposed.

<ul>
  <li>:set -package transformers</li>
  <li>:set -package random</li>
  <li>:set -package mtl</li>
  <li>:set -package deepseq</li>
  <li>:set -package ghc-heap</li>
  <li>:set -package containers</li>
  <li>:set -package array</li>
  <li>:set -package vector</li>
  <li>:set -package directory</li>
  <li>:set -package filepath</li>
  <li>:set -package bytestring</li>
  <li>:set -package text</li>
  <li>:set -package filepath</li>
</ul>
