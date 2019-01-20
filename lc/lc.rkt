#lang racket/base

(require redex)

(define-language LC
  (v ::= (λ x e) number)
  (e ::= v (e e) x)
  (x ::= variable-not-otherwise-mentioned)
  
  (E ::= hole (E e) (v E))

  #:binding-forms
  (λ x e #:refers-to x))

(define red
  (reduction-relation
   LC
   (--> (in-hole E ((λ x e) v))
        (in-hole E (substitute e x v))
        β)))

(module+ test
  (traces/ps red (term ((λ x x) ((λ y y) 42))) "../thesis/lc-red.eps"
             #:edge-labels? #t
             #:x-spacing 45))