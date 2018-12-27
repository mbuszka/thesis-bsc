# thesis

# Implementation

Current implementation is in the `infer` directory

The structure of a project:
  - `lang.rkt` contains language definition and helper metafunctions,
    the most important one being `ftv`

  - `lib.rkt` contains helper metafunctions for working with environments, lists etc
  - `type.rkt` contains typing and unification relations
  - `eval.rkt` contains reduction relation
  - `examples.rkt` contains few example programs, in AST form
  - `test.rkt` checks example programs, also it has `run-check` function
    which start automatic counterexample generation (currently up to 100000 tries)

# Thesis

Is in the `thesis` directory