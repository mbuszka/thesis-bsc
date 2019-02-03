#lang racket/base

(require redex/reduction-semantics)

(provide LC red)

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
   (in x (x t Γ) t)]

  [(neq x y) (in x Γ t)
   --------------------
   (in x (y t_1 Γ) t)])

;(module+ test
;  (traces/ps red (term ((λ x x) ((λ y y) 42))) "../thesis/lc-red.eps"
;             #:edge-labels? #t
;             #:x-spacing 45))