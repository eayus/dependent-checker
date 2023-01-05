# lambda-normaliser

## Overview
A beta-normaliser for untyped lambda calculus terms.

Techniques based on "normalisation by evaluation" (NbE) are used to efficiently compute the normal form for a lambda term with potentially free variables. This normaliser provides a suitable basis for the implementation of a dependently typed language.

The implementation makes use of Haskell's type level programming features to ensure that terms are well-scoped. `Main.hs` provides examples of using the evaluator to perform arithmetic on church-encoded natural numbers.

## Efficiency

### Weakening

The normaliser is constructed specifically to avoid the need for term weakenings, to avoid any unnecessary term traversals. This is the main motivation for using de Bruijn levels over indices. By representing lambdas as closures, no modifications to terms are required when moving to a larger context.

However, since we are indexing terms by the amount of free variables, we must still convince Haskell's type system that this is the case. In a production environment where efficiency is important, this should be done with a type coercion. However for documentation and proof reasons, this implementation manually does weakenings (see `Weaken.hs`).

### Unary Numbers

To simplify the type level programming, a unary representation (Peano-style) is used for numbers and indices. Using standard integer types would improve performance.
