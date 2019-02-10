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

  [(free op E n)
   --------------------------------------
   (free op (lift op E) (incr n))]

  [(free op_1 E n) (neq op_1 op_2)
   ------------------------------------------
   (free op_1 (lift op_2 E) n)]

  [(free op E n) (side-condition (in op (ops hs))) (gt n 0)
   -----------------------------------------------
   (free op (handle E hs ret) (decr n))]

  [(side-condition (not-in op (ops hs))) (free op E n)
   ---------------------------------------------------
   (free op (handle E hs ret) n)]

  [(free op E n)
   ----------------------
   (free op (app E e) n)]

  [(free op E n)
   ----------------------
   (free op (app v E) n)]

  [(free op E n)
   ---------------------------------
   (free op (prim v ... E e ...) n)]

  [(free op_1 E n)
   -----------------------
   (free op_1 (op_2 E) n)]

  [(free op E n)
   ---------------------------
   (free op (if E e_1 e_2) n)])

(define red
  (reduction-relation
   Infer
   (--> (in-hole E (app (λ x e) v))
        (in-hole E (subst e v x))
        β-λ)

   (--> (in-hole E (app (rec x_f x_a e) v))
        (in-hole E (subst e (rec x_f x_a e) x_f v x_a))
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
        (in-hole E (subst e v x))
        handle-return)
   
   (--> (in-hole E_1 (handle (in-hole E_2 (op v)) hs ret))
        (in-hole E_1 (subst e v x_1 v_r x_2))
        (judgment-holds (free op E_2 0))
        (fresh v:z)
        (where v_r (λ v:z (handle (in-hole E_2 v:z) hs ret)))
        (judgment-holds (get-handler op hs (x_1 x_2 e)))
        handle-op)
   ))

(define (reduce e)
  (let ([xs (apply-reduction-relation* red e)])
    (if (and (= (length xs) 1)
             (value? (car xs)))
        (car xs)
        #f)))
