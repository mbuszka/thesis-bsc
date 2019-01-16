#lang racket

(require "lang.rkt"
         "lib.rkt"
         redex
         )

(provide abstract-machine reduce)

(define-extended-language AM Infer
  ; Machine configuration
  (C ::= (e ρ K) (V ρ K) (op V natural K K) V)

  ; Machine values
  (V ::= (λ ρ x e) (rec ρ x x e) (val number) (val true) (val false) K)

  ; Value environment
  (ρ ::= ([x V] ...))

  ; Pure continuation frames
  (σ ::= (arg e ρ) (app V) (do op) (prim-l prim e ρ) (prim-r prim V) (if e e ρ))

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

   (--> ((rec x_f x_a e) ρ K)
        ((rec ρ x_f x_a e) ρ K))

   (--> (number ρ K)
        ((val number) ρ K))

   ; Continuation building normal
   (--> ((e_1 e_2) ρ K)
        (e_1 ρ (push-normal (arg e_2 ρ) K)))

   (--> ((op e) ρ K)
        (e ρ (push-normal (do op) K)))

   (--> ((prim e_1 e_2) ρ K)
        (e_1 ρ (push-normal (prim-l prim e_2 ρ) K)))

   (--> ((if e_cond e_then e_else) ρ K)
        (e_cond ρ (push-normal (if e_then e_else ρ) K)))

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

   (--> (V ρ_2 ([([app (rec ρ_1 x_f x_a e)] σ ...) ϕ] κ ...))
        (e (extend (extend ρ_1 x_f (rec ρ_1 x_f x_a e)) x_a V) ([(σ ...) ϕ] κ ...)))

   ; Primitive operation
   (--> ((val number_2) ρ ([([prim-r prim (val number_1)] σ ...) ϕ] κ ...))
        ((val (prim-apply prim number_1 number_2)) ρ ([(σ ...) ϕ] κ ...)))

   ; If expression
   (--> ((val true) ρ_1 ([([if e any ρ] σ ...) ϕ] κ ...))
        (e ρ ([(σ ...) ϕ] κ ...)))

   (--> ((val false) ρ_1 ([([if any e ρ] σ ...) ϕ] κ ...))
        (e ρ ([(σ ...) ϕ] κ ...)))
   
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