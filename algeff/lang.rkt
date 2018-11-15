#lang racket

(require redex
         "lib.rkt")

(provide AlgEff)

(define-extended-language AlgEff Base
  (v   ::= number (λ [x t] m) (Λ a m))
  (m n ::= v (m m) (m t) x (do op l m)
     (handle l m row with (return x m) hs)
     (lift l m))
  (hs  ::= (h ...))
  (h   ::= (op x y m))
  (t   ::= Int (t -> t ! row) (∀ a t))
  (x y ::= (variable-prefix var:))
  (a b ::= (variable-prefix tvar:))
  (l   ::= (variable-prefix lbl:))
  (op  ::= (variable-prefix op:))
;  (uv  ::= (variable-prefix uvar:))
  (row ::= (l ...) (l ... a) (l ... uv))

  ; Execution context
  (E ::= hole (E m) (E t) (v E) (do op l E)
     (handle l E row with (return x m) hs)
     (lift l E))
  
  #:binding-forms
  (λ [x t] m #:refers-to x)
  (Λ a m #:refers-to a)
  (∀ a t #:refers-to a)
  (op x y m #:refers-to x y)
  (return x m #:refers-to x)
  )
