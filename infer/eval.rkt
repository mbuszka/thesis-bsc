#lang racket

(require redex
         "lang.rkt"
         "lib.rkt"
         "type.rkt")

(provide red reduces?)


(define-judgment-form Infer
  #:mode (free I I O)
  #:contract (free op E natural)

  [(free op hole 0)]

  [(free op (lift op E) ,(+ (term n) 1))
   (free op E n)]

  [(free (name op op_!_1) (lift op_!_1 E) n)
   (free op E n)]

  [(free op (handle E h) ,(- (term n) 1))
   (side-condition (in op (ops h)))
   (free op E n)]

  [(free op (handle E h) n)
   (side-condition (not-in op (ops h)))
   (free op E n)]

  [(free op (E e) n)
   (free op E n)]

  [(free op (v E) n)
   (free op E n)]

  [(free op_1 (op_2 E) n)
   (free op_1 E n)])

(define-metafunction Infer
  return-clause : h -> h

  [(return-clause (return x e)) (return x e)]
  [(return-clause (op _ _ _ h)) (return-clause h)])

(define-judgment-form Infer
  #:mode (get-handler I I O)

  [(get-handler op (op x_1 x_2 e _) (x_1 x_2 e))]
  [(get-handler (name op op_!_1) (op_!_1 _ _ _ h) any_r)
   (get-handler op h any_r)])

(define red
  (reduction-relation
   Infer
   (--> (in-hole E ((λ x e) v))
        (in-hole E (substitute e x v))
        β)

   (--> (in-hole E (lift op v))
        (in-hole E v)
        lift-compat)

   (--> (in-hole E (handle v h))
        (in-hole E (substitute e x v))
        (where (return x e) (return-clause h))
        handle-return)
   
   (--> (in-hole E_out
                 (handle (in-hole E_in (op v)) h))
        (in-hole E_out (substitute (substitute e x_1 v)
                                   x_2 (λ v:z (handle (in-hole E_in v:z) h))))
        (judgment-holds (free op E_in 0))
        (judgment-holds (get-handler op h (x_1 x_2 e)))
        (fresh v:z)
        handle-op)
   ))

(define (reduces? e)
  (let ([xs (apply-reduction-relation* red e)])
    (and (= (length xs) 1)
         (value? (car xs)))))