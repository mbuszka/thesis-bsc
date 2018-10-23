#lang racket
(require redex)
(require "lang.rkt")

(define red-rel
  (reduction-relation
   F
   (--> (in-hole E ((Λ α e) @ t))
        (in-hole E e))

   (--> (in-hole E ((λ (x : t) e) v))
        (in-hole E (substitute e x v)))
   ))

(module+ test
  (require (submod "lang.rkt" test))

  (test-->> red-rel
            id
            id)
  (test-->> red-rel
            (term ((,id @ num) 5))
            5)
  )
  
   