#lang racket/base

(require redex/reduction-semantics
         "lang.rkt"
         "lib.rkt")

(provide abstract-machine am-a am-b am-c am-e am-reduce initial-conf AM)

(define-extended-language AM Infer
  ; Machine configuration
  (C ::= (e ρ Σ ϕ K) (val V Σ ϕ K) (op V n K K) V)
  ; Machine values
  (V ::= (λ ρ x e) (rec ρ x x e) m b (V ...) K)
  ; Value environment
  (ρ ::= ([x V] ...))
  ; Pure continuation frames
  (σ ::= (arg e ρ) (app V) (do op) (prim ρ V ... / e ...) (if e e ρ))
  (Σ ::= (σ ...))
  ; Effect continuation frames
  (ϕ ::= (handle hs ret ρ) (lift op) done)
  ; Continuation frame
  (κ ::= (Σ ϕ))
  ; Continuation
  (K ::= (κ ...)))

(define Value? (redex-match? AM V))

; Administrative transitions
(define am-a
  (reduction-relation
   AM
   ; Value conversion and nullary primop
   (--> ((λ x e) ρ Σ ϕ K)
        (val (λ ρ x e) Σ ϕ K))

   (--> ((rec x_f x_a e) ρ Σ ϕ K)
        (val (rec ρ x_f x_a e) Σ ϕ K))

   (--> (m ρ Σ ϕ K)
        (val m Σ ϕ K))

   (--> (b ρ Σ ϕ K)
        (val b Σ ϕ K))

   (--> ((prim) ρ Σ ϕ K)
        (val (prim-apply prim) Σ ϕ K))

   ; Application and primitive operation sequencing
   (--> (val V ([arg e ρ] σ ...) ϕ K)
        (e ρ ([app V] σ ...) ϕ K))

   (--> (val V ([prim ρ V_1 ... / e e_1 ...] σ ...) ϕ K)
        (e ρ ([prim ρ V_1 ... V / e_1 ...] σ ...) ϕ K))

   ; Operation invocation
   (--> (val V ([do op] σ ...) ϕ (κ ...))
        (op V 0 ([(σ ...) ϕ] κ ...) ()))

   (--> (val V () done ())
        V)
   ))

; Continuation building transitions
(define am-b
  (reduction-relation
   AM
   ; Continuation building normal
   (--> ((app e_1 e_2) ρ (σ ...) ϕ K)
        (e_1 ρ ([arg e_2 ρ] σ ...) ϕ K))

   (--> ((op e) ρ (σ ...) ϕ K)
        (e ρ ([do op] σ ...) ϕ K))

   (--> ((prim e e_1 ...) ρ (σ ...) ϕ K)
        (e ρ ([prim ρ / e_1 ...] σ ...) ϕ K))

   (--> ((if e_cond e_then e_else) ρ (σ ...) ϕ K)
        (e_cond ρ ([if e_then e_else ρ] σ ...) ϕ K))

   ; Continuation building effect
   (--> ((handle e hs ret) ρ Σ ϕ (κ ...))
        (e ρ () (handle hs ret ρ) ([Σ ϕ] κ ...)))

   (--> ((lift op e) ρ  Σ ϕ (κ ...))
        (e ρ () (lift op) ([Σ ϕ] κ ...)))
   ))

; Reduction transitions
(define am-c
  (reduction-relation
   AM
   ; Variable lookup
   (--> (x ρ Σ ϕ K)
        (val (lookup-ρ ρ x) Σ ϕ K))

   ; Contraction
   (--> (val V ([app (λ ρ x e)] σ ...) ϕ K)
        (e (ext ρ x V) (σ ...) ϕ K))

   (--> (val V ([app (rec ρ x_f x_a e)] σ ...) ϕ K)
        (e (ext ρ x_f (rec ρ x_f x_a e) x_a V) (σ ...) ϕ K))

   (--> (val V ([app ([Σ ϕ_1] κ_1 ...)] σ ...) ϕ_2 (κ_2 ...))
        (val V Σ ϕ_1 (κ_1 ... [(σ ...) ϕ_2] κ_2 ...)))

   ; Primitive operation
   (--> (val V ([prim ρ V_1 ... /] σ ...) ϕ K)
        (val (prim-apply prim V_1 ... V) (σ ...) ϕ K))

   ; If expression
   (--> (val true ([if e any ρ] σ ...) ϕ K)
        (e ρ (σ ...) ϕ K))

   (--> (val false ([if any e ρ] σ ...) ϕ K)
        (e ρ (σ ...) ϕ K))
  ))

; Effect transitions
(define am-e
  (reduction-relation
   AM
   ; Lift
   (--> (op V n ([Σ (lift op)] κ_1 ...) (κ_2 ...))
        (op V (incr n) (κ_1 ...) (κ_2 ... [Σ (lift op)])))

   (--> (op_1 V n ([Σ (lift op_2)] κ_1 ...) (κ_2 ...))
        (op_1 V n (κ_1 ...) (κ_2 ... [Σ (lift op_2)]))
        (judgment-holds (neq op_1 op_2)))

   ; Handle
   (--> (op V n ([Σ (handle hs ret ρ)] κ_1 ...) (κ_2 ...))
        (op V (decr n) (κ_1 ...) (κ_2 ... [Σ (handle hs ret ρ)]))
        (judgment-holds (in op (ops hs)))
        (judgment-holds (gt n 0)))

   (--> (op V n ([Σ (handle hs ret ρ)] κ_1 ...) (κ_2 ...))
        (op V n (κ_1 ...) (κ_2 ... [Σ (handle hs ret ρ)]))
        (judgment-holds (not-in op (ops hs))))

   (--> (op V 0 ([Σ (handle hs ret ρ)] [Σ_2 ϕ] κ_1 ...) (κ_2 ...))
        (e (ext ρ x_1 V x_2 (κ_2 ... [Σ (handle hs ret ρ)])) Σ_2 ϕ (κ_1 ...))
        (judgment-holds (get-handler op hs (x_1 x_2 e))))

   ; Return
   (--> (val V () (handle hs (return x e) ρ) ([Σ ϕ] κ ...))
        (e (ext ρ x V) Σ ϕ (κ ...)))

   (--> (val V () (lift op) ([Σ ϕ] κ ...))
        (val V Σ ϕ (κ ...)))
   ))

; Abstract machine is the union of all transitions
(define abstract-machine
  (union-reduction-relations am-a am-b am-c am-e))

; Initial configuration for an expression e
(define-metafunction AM
  initial-conf : e -> C

  [(initial-conf e) (e () () done ())])

; Extend the environment ρ at x with V
(define-metafunction AM
  extend : ρ x V -> ρ

  [(extend ([x_1 V_1] ... [x V_2] [x_3 V_3] ...) x V) ([x_1 V_1] ... [x V] [x_3 V_3] ...)]
  [(extend ([x_1 V_1] ...) x V) ([x V] [x_1 V_1] ...)])

(define-metafunction AM
  ext : ρ any ... -> ρ

  [(ext ρ) ρ]
  [(ext ρ x V any ...) (extend (ext ρ any ...) x V)])

; Lookup a value for variable in environment
(define-metafunction AM
  lookup-ρ : ρ x -> V

  [(lookup-ρ ([x_1 V_1] ... [x V] [x_2 V_2] ...) x) V])

; Fully reduce an expression using abstract machine
(define (am-reduce e)
  (let ([xs (apply-reduction-relation* abstract-machine (term (initial-conf ,e)))])
    (if (and (= (length xs) 1)
             (Value? (car xs)))
        (car xs)
        #f)))
