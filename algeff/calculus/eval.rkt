#lang racket

(require redex
         "lang.rkt"
         "lib.rkt"
         "type.rkt")

(provide red reduce)


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

  [(free op (E e) n)
   (free op E n)]

  [(free op (v E) n)
   (free op E n)]

  [(free op (prim E e) n)
   (free op E n)]

  [(free op (prim v E) n)
   (free op E n)]

  [(free op_1 (op_2 E) n)
   (free op_1 E n)]

  [(free op (if E e_1 e_2) n)
   (free op E n)]

  [(free op (fix E) n)
   (free op E n)])
  

(define red
  (reduction-relation
   Infer
   (--> (in-hole E ((λ x e) v))
        (in-hole E (substitute e x v))
        β)

   (--> (in-hole E (prim v_1 v_2))
        (in-hole E (prim-apply prim v_1 v_2))
        prim-op)

   (--> (in-hole E (if true e_1 e_2))
        (in-hole E e_1)
        if-true)

   (--> (in-hole E (if false e_1 e_2))
        (in-hole E e_2)
        if-false)

   (--> (in-hole E (fix (λ x e)))
        (in-hole E (substitute e x (fix (λ x e))))
        fix)

   (--> (in-hole E (lift op v))
        (in-hole E v)
        lift-compat)

   (--> (in-hole E (handle v hs (return x e)))
        (in-hole E (substitute e x v))
        handle-return)
   
   (--> (in-hole E_out
                 (handle (in-hole E_in (op v)) hs ret))
        (in-hole E_out (substitute (substitute e x_1 v)
                                   x_2 (λ v:z (handle (in-hole E_in v:z) hs ret))))
        (judgment-holds (free op E_in 0))
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