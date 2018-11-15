#lang racket

(require redex
         "lang.rkt"
         "lib.rkt"
         "type.rkt")

(define-judgment-form AlgEff
  #:mode (free I I O)
  #:contract (free l E natural)

  [(free l hole 0)]

  [(free l (lift l E) ,(+ (term n) 1))
   (free l E n)]

  [(free (name lbl l_!_1) (lift l_!_1 E) n)
   (free lbl E n)]

  [(free l (handle l E with any_rest ...) ,(- (term n) 1))
   (free l E n)]

  [(free (name lbl l_!_1) (handle l_!_1 E with any_rest ...) n)
   (free lbl E n)]

  [(free l (E e) n)
   (free l E n)]

  [(free l (v E) n)
   (free l E n)]

  [(free l (E t) n)
   (free l E n)]

  [(free l_1 (do op l_2 E) n)
   (free l_1 E n)])


(define red
  (reduction-relation
   AlgEff
   (--> (in-hole E ((λ [x t] m) v))
        (in-hole E (substitute m x v)))

   (--> (in-hole E ((Λ a m) t))
        (in-hole E m))

   (--> (in-hole E (lift l v))
        (in-hole E v))

   (--> (in-hole E (handle l v row with (return x m) _ ...))
        (in-hole E (substitute m x v)))
   
   (--> (in-hole E_out
                 (handle l
                         (in-hole E_in (do op l v)) row
                         with any_ret (any_1 ... (op x y m) any_2 ...)))
   (in-hole E_out (substitute (substitute m x v) y
                              (λ [var:z Int] (handle l (in-hole E_in var:z) row
                                                     with any_ret (any_1 ... (op x y m) any_2 ...)))))
   (judgment-holds (free l E_in 0))
   (fresh var:z))
  ))

(traces red (term example-1))