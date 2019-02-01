#lang racket/base

(require redex/reduction-semantics
         "lang.rkt"
         "lib.rkt"
         "type.rkt")

(provide red reduce free)


(define-judgment-form Infer
  #:mode (free I I O)
  #:contract (free op E natural)

  [(free op hole 0)]

  [(free op (lift op E) ,(+ (term n) 1))
   (free op E n)]

  [(free (name op op_!_1) (lift op_!_1 E) n)
   (free op E n)]

  [(free op (handle E hs ret) ,(- (term n) 1))
   (side-condition (in op (ops hs)))
   (free op E n)]

  [(free op (handle E hs ret) n)
   (side-condition (not-in op (ops hs)))
   (free op E n)]

  [(free op (app E e) n)
   (free op E n)]

  [(free op (app v E) n)
   (free op E n)]

  [(free op (prim v ... E e ...) n)
   (free op E n)]

  [(free op_1 (op_2 E) n)
   (free op_1 E n)]

  [(free op (if E e_1 e_2) n)
   (free op E n)])
  

(define red
  (reduction-relation
   Infer
   (--> (in-hole E (app (λ x e) v))
        (in-hole E (substitute e x v))
        β-λ)

   (--> (in-hole E (app (rec x_f x_a e) v))
        (in-hole E (substitute (substitute e x_f (rec x_f x_a e)) x_a v))
        β-rec)

   (--> (in-hole E (prim v ...))
        (in-hole E (prim-apply prim v ...))
        prim-op)

   (--> (in-hole E (if true e_1 e_2))
        (in-hole E e_1)
        if-true)

   (--> (in-hole E (if false e_1 e_2))
        (in-hole E e_2)
        if-false)


   (--> (in-hole E (lift op v))
        (in-hole E v)
        lift-compat)

   (--> (in-hole E (handle v hs (return x e)))
        (in-hole E (substitute e x v))
        handle-return)
   
   (--> (in-hole E_1 (handle (in-hole E_2 (op v)) hs ret))
        (in-hole E_1 (substitute (substitute e x_1 v)
                                 x_2 (λ v:z (handle (in-hole E_2 v:z) hs ret))))
        (judgment-holds (free op E_2 0))
        (judgment-holds (get-handler op hs (x_1 x_2 e)))
        (fresh v:z)
        handle-op)
   ))

(define (reduce e)
  (let ([xs (apply-reduction-relation* red e)])
    (if (and (= (length xs) 1)
             (value? (car xs)))
        (car xs)
        #f)))