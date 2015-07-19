SSA vs ANF
==========

This repo provides the following:

- AST for representing Static Single Assignment (SSA) programs
- AST for representing Administrative Normal Form (ANF) programs
- Pretty printing of both
- Implementation of the SSA to ANF conversion algorithm presented in [1]
- Implementation of Sparse Conditional Constant Propagation (SCCP) [2] using the algorithm presented in [1]

1. M. Chakravarty, G. Keller, and P. Zadarnowski.
   [A Functional Perspective on SSA Optimisation Algorithms](https://www.jantar.org/papers/chakravarty03perspective.pdf)
   COCV, 2003.

2. M. Wegman, and F. Zadeck.
   [Constant Propagation with Conditional Branches](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.101.2590&rep=rep1&type=pdf)
   ACM TOPLAS, 1991.

## Example Conversion

<img src="https://github.com/jystic/ssa-anf/raw/master/doc/convert.png" width="252" height="600" />

## Example SCCP Transform

<img src="https://github.com/jystic/ssa-anf/raw/master/doc/sccp.png" width="260" height="574" />
