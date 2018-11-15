# The language
  - small calculus with numbers, lambda abstractions, effects and handlers
  - type system with implicit, predicative, higher rank polymorphism
  - allow type annotations on binders

# The implementation
  - call by value reduction
  - bidirectional type checking, with application and inference mode
  - solving monotypes with unification
  - solving effect environments with unification

# Problems
  - Merging of effects when using polymorphic function
    ```
    λ f : ∀ e num -> num ! e.
        f (do SomeEffect)
    ```
    Without unions and intersections we cannot assign a resonable type to body ot the function above

  - But on the other hand given type `(eff1 eff2 eff3) ⊔ α` we cannot
    really use it with any handler

# Design
  - Should operations be grouped into effects, a la sum types (as in Helium, Koka)
    or rather each operation should be mentioned in the row
    * first approach requires effects to be declared in an environment, which means that type-checking
      is sufficient, it allows for some abstraction, but no *ad-hoc* effects
    * second approach has to infer types of operations, does not allow for abstracting effects
      but allows for *ad-hoc* effects

# Links
  - https://dvanhorn.github.io/redex-aam-tutorial/

# Papers
  - Let Arguments Go First
  - Koka
  - TAPL
  - Complete And Easy Bidirectional ...
  - Algebraic Subtyping
  - Liberating Effects with rows and handlers
  - Semantics Engineering with PLT Redex