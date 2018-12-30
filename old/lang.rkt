#lang racket

(require redex
         "lib.rkt")

(provide AlgEff value?)

(define-extended-language AlgEff Base
  (v   ::= number (λ [x t] m) (Λ a m))
  (m n ::=
     v (m m) (m t) x
     (do op t m)
     (handle m row ret hs)
     (lift crow m))
  (ret ::= (return x m))
  (hs  ::= (h ...))
  (h   ::= (op [x t] [r t] m))

  (t   ::= Int (t -> t ! row) (∀ a t))
  (x y r ::= (variable-prefix var:))
  (a b ::= (variable-prefix tvar:))
  (op  ::= (variable-prefix op:))
  (row ::= ([op t t] ...) ([op t t] ... a))
  (crow ::= ([op t t] ...))

  ; Execution context
  (E ::=
     hole (E m) (E t) (v E)
     (do op t E)
     (handle E row ret hs)
     (lift row E))
  
  #:binding-forms
  (λ [x t] m #:refers-to x)
  (Λ a m #:refers-to a)
  (∀ a t #:refers-to a)
  (op x y m #:refers-to x y)
  (return x m #:refers-to x)
  )

(define value? (redex-match? AlgEff v))