# The language
  - small calculus with numbers, lambda abstractions, effects and handlers
  - type system with simple types for values and polymorphism for types
  - core calculus with explicit types:
    * binders
    * function eval effects
    * type arg application
    
    only type checking
  
  - maybe add surface language with type reconstruction/inference (bidirectional?)
    which translates into core

# The implementation
  - call by value reduction
  - effect/type subsumption relation