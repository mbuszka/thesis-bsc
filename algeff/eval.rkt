#lang racket

(require redex
         "lang.rkt"
         "lib.rkt"
         "type.rkt")

(provide red reduces?)


(define-judgment-form AlgEff
  #:mode (free I I O)
  #:contract (free op E natural)

  [(free op hole 0)]

  [(free op (lift row E) ,(+ (term n) 1))
   (find row op _)
   (free op E n)]

  [(free op (lift row E) n)
   (not-in row op)
   (free lbl E n)]

  [(free op (handle E row ret hs) ,(- (term n) 1))
   (handlers->row hs row)
   (find row op _)
   (free op E n)]

  [(free op (handle E row ret hs) n)
   (handlers->row hs row)
   (not-in row op)
   (free op E n)]

  [(free op (E e) n)
   (free op E n)]

  [(free op (v E) n)
   (free op E n)]

  [(free op (E t) n)
   (free op E n)]

  [(free op_1 (do op_2 t E) n)
   (free op_1 E n)])


(define red
  (reduction-relation
   AlgEff
   (--> (in-hole E ((λ [x t] m) v))
        (in-hole E (substitute m x v)))

   (--> (in-hole E ((Λ a m) t))
        (in-hole E m))

   (--> (in-hole E (lift row v))
        (in-hole E v))

   (--> (in-hole E (handle v row (return x m) hs))
        (in-hole E (substitute m x v)))
   
   (--> (in-hole E_out
                 (handle (in-hole E_in (do op t v)) row
                         ret hs))
   (in-hole E_out (substitute (substitute m x v)
                              r (λ [var:z t] (handle (in-hole E_in var:z) row ret hs))))
   (judgment-holds (find hs op ([x _] [r _] m)))
   (judgment-holds (free op E_in 0))
   (fresh var:z))
  ))

(define (reduces? e)
 (let ([xs (apply-reduction-relation* red e)])
   (and (= (length xs) 1)
     (value? (car xs)))))