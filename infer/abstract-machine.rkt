#lang racket

(require "lang.rkt"
         "lib.rkt"
         redex
         )

(define-extended-language AM Infer
  ; Machine configuration
  (C ::= (e ρ K) (V ρ K) (op V natural K K) V)

  ; Machine values
  (V ::= (λ ρ x e) (num number) K)

  ; Value environment
  (ρ ::= ([x V] ...))

  ; Pure continuation frames
  (σ ::= (arg e ρ) (app V) (do op) (prim-l prim e ρ) (prim-r prim V))

  ; Effect continuation frames
  (ϕ ::= (handle hs ret ρ) (lift op))

  ; Continuation frame
  (κ ::= ([σ ...] ϕ))

  ; Continuation
  (K ::= (κ ...)))

(define Value? (redex-match? AM V))

(define-metafunction AM
  initial-conf : e -> C

  [(initial-conf e) (e () ([() (handle () (return v:x v:x) ())]))])

(define abstract-machine
  (reduction-relation
   AM
   ; Variable lookup
   (--> (x ρ K)
        ((lookup-ρ ρ x) ρ K))

   ; Value conversion
   (--> ((λ x e) ρ K)
        ((λ ρ x e) ρ K))

   (--> (number ρ K)
        ((num number) ρ K))

   ; Continuation building normal
   (--> ((e_1 e_2) ρ K)
        (e_1 ρ (push-normal (arg e_2 ρ) K)))

   (--> ((op e) ρ K)
        (e ρ (push-normal (do op) K)))

   (--> ((prim e_1 e_2) ρ K)
        (e_1 ρ (push-normal (prim-l prim e_2 ρ) K)))

   ; Continuation building effect
   (--> ((handle e hs ret) ρ K)
        (e ρ (push-effect (handle hs ret ρ) K)))

   (--> ((lift op e) ρ K)
        (e ρ (push-effect (lift op) K)))

   ; Binary operation sequencing
   (--> (V ρ_1 ([([arg e ρ_2] σ ...) ϕ] κ ...))
        (e ρ_2 ([([app V] σ ...) ϕ] κ ...)))

   (--> (V ρ_1 ([([prim-l prim e ρ_2] σ ...) ϕ] κ ...))
        (e ρ_2 ([([prim-r prim V] σ ...) ϕ] κ ...)))

   ; Contraction
   (--> (V ρ_2 ([([app (λ ρ_1 x e)] σ ...) ϕ] κ ...))
        (e (extend ρ_1 x V) ([(σ ...) ϕ] κ ...)))

   (--> (V ρ ([([app (κ_1 ...)] σ ...) ϕ] κ_2 ...))
        (V ρ (κ_1 ... [(σ ...) ϕ] κ_2 ...)))
   
   ; Primitive operation
   (--> ((num number_2) ρ ([([prim-r prim (num number_1)] σ ...) ϕ] κ ...))
        ((num (prim-apply prim number_1 number_2)) ρ ([(σ ...) ϕ] κ ...)))

   ; Operation invocation
   (--> (V ρ ([([do op] σ ...) ϕ] κ ...))
        (op V 0 ([(σ ...) ϕ] κ ...) ()))

   ; Lift
   (--> (op V n ([(σ ...) (lift op)] κ_1 ...) (κ_2 ...))
        (op V ,(+ (term n) 1) (κ_1 ...) (κ_2 ... [(σ ...) (lift op)])))

   (--> ((name op_1 op_!_1) V n ([(σ ...) (lift (name op_2 op_!_1))] κ_1 ...) (κ_2 ...))
        (op V ,(+ (term n) 1) (κ_1 ...) (κ_2 ... [(σ ...) (lift op)])))

   ; Handle
   (--> (op V n ([(σ ...) (handle hs ret ρ)] κ_1 ...) (κ_2 ...))
        (op V ,(- (term n) 1) (κ_1 ...) (κ_2 ... [(σ ...) (handle hs ret ρ)]))
        (judgment-holds (in op (ops hs)))
        (side-condition (> (term n) 0)))

   (--> (op V n ([(σ ...) (handle hs ret ρ)] κ_1 ...) (κ_2 ...))
        (op V n (κ_1 ...) (κ_2 ... [(σ ...) (handle hs ret ρ)]))
        (judgment-holds (not-in op (ops hs))))

   (--> (op V 0 ([(σ ...) (handle hs ret ρ)] κ_1 ...) (κ_2 ...))
        (e (extend (extend ρ x_1 V) x_2 (κ_2 ... [(σ ...) (handle hs ret ρ)])) (κ_1 ...))
        (judgment-holds (get-handler op hs (x_1 x_2 e))))

   ; Return
   (--> (V ρ_1 ([() (handle hs (return x e) ρ)] κ_1 ...))
        (e (extend ρ x V) (κ_1 ...)))

   (--> (V ρ ([() (lift op)] κ ...))
        (V ρ (κ ...)))

   (--> (V ρ ())
        V)
   ))

(define-metafunction AM
  push-normal : σ K -> K

  [(push-normal σ [([σ_1 ...] ϕ) κ ...]) ([(σ σ_1 ...) ϕ] κ ...)])

(define-metafunction AM
  push-effect : ϕ K -> K

  [(push-effect ϕ (κ ...)) ([() ϕ] κ ...)])

(define-metafunction AM
  extend : ρ x V -> ρ

  [(extend ([x_1 V_1] ... [x V_2] [x_3 V_3] ...) x V) ([x_1 V_1] ... [x V] [x_3 V_3] ...)]
  [(extend ([x_1 V_1] ...) x V) ([x V] [x_1 V_1] ...)])

(define-metafunction AM
  lookup-ρ : ρ x -> V

  [(lookup-ρ ([x_1 V_1] ... [x V] [x_2 V_2] ...) x) V])

(define (reduce e)
  (let ([xs (apply-reduction-relation* abstract-machine (term (initial-conf ,e)))])
    (if (and (= (length xs) 1)
             (Value? (car xs)))
        (car xs)
        #f)))