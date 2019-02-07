#lang racket/base

(require redex)

(provide (all-defined-out))

(define-language LC
  (x ::= variable-not-otherwise-mentioned)
  (n ::= number)
  (v ::= (λ x e) n)
  (e ::= v (e e) x)
  
  (K ::= hole (K e) (v K))

  #:binding-forms
  (λ x e #:refers-to x))

(define red
  (reduction-relation
   LC
   (--> (in-hole K ((λ x e) v))
        (in-hole K (substitute e x v))
        β))
  )

(define-extended-language LC-typed LC
  (E ::= (λ [x t] E) n (E E) x)
  (t ::= num (t → t))
  (Γ ::= · (x t Γ))

  #:binding-forms
  (λ [x t] E #:refers-to x))

(define-judgment-form LC-typed
  #:mode (types I I O)

  [(in x Γ t)
   -------------
   (types Γ x t)]

  [---------------
   (types Γ n num)]

  [(types (x t_1 Γ) e t_2)
   -----------------------------------
   (types Γ (λ [x t_1] e) (t_1 → t_2))]

  [(types Γ e_1 (t_1 → t_2))
   (types Γ e_2 t_1)
   -----------------------
   (types Γ (e_1 e_2) t_2)])

(define-relation LC
  neq ⊆ t × t

  [(neq x_! x_!)])

(define-metafunction LC-typed
  erase : E -> e
  [(erase x) x]
  [(erase n) n]
  [(erase E_1 E_2) ((erase E_1) (erase E_2))]
  [(erase (λ [x t] E)) (λ x (erase E))])

(define-judgment-form LC-typed
  #:mode (in I I O)

  [----------------
   (in x (x any_v any_r) any_v)]

  [(neq x y) (in x any_r any_v)
   --------------------
   (in x (y any any_r) any_v)])

(define-extended-language LC-am LC
  (V ::= (λ ρ x e) n)
  (σ ::= [arg e ρ] [app V])
  (κ ::= (σ  ...))
  (ρ ::= · (x V ρ))
  (C ::= (e ρ κ) (val V κ) V))

(define abstract-machine
  (reduction-relation LC-am
    (--> (x ρ κ)
         (val V κ)
         (judgment-holds (in x ρ V))
         val-x)
    (--> (n ρ κ)
         (val n κ)
         val-n)
    (--> ((λ x e) ρ κ)
         (val (λ ρ x e) κ)
         val-λ)
    (--> ((e_1 e_2) ρ (σ ...))
         (e_1 ρ ([arg e_2 ρ] σ ...))
         push-e)
    (--> (val V ([arg e ρ] σ ...))
         (e ρ ([app V] σ ...))
         push-λ)
    (--> (val V ([app (λ ρ x e)] σ ...))
         (e (x V ρ) (σ ...))
         β)
    (--> (val V ())
         V
         done)))

(define-metafunction LC-am
  initial-conf : e -> C
  [(initial-conf e) (e · ())])

(module+ test
  (define-term example ((λ x x) ((λ y y) 42)))
  (traces red (term example)
             #:edge-labels? #t
             #:x-spacing 45)
  (traces abstract-machine (term (initial-conf example))))