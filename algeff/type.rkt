#lang racket
(require redex)
(require "lang.rkt")

(define-judgment-form F
  #:mode (types I I O)
  #:contract (types Γ e t)

  [(well-formed Γ t_x) (types (x : t_x Γ) e t_r)
   --------------------------------------
   (types Γ (λ (x : t_x) e) (t_x -> t_r))]

  [(types (α Γ) e t)
   -------------------------
   (types Γ (Λ α e) (∀ α t))]

  [(types Γ e_1 (t_1 -> t_2))
   (types Γ e_2 t_1)
   --------------------------
   (types Γ (e_1 e_2) t_2)]

  [(types Γ e_1 (∀ α_1 t_2))
   (where t_res (substitute t_2 α_1 t_1))
   ---------------------
   (types Γ (e_1 @ t_1) t_res)]

  [--------------------
   (types Γ number num)]

  [(where t (in x Γ))
   -----------------
   (types Γ x t)]
  )

(define-relation F
  well-formed ⊂ Γ × t
  [(well-formed _ num)]

  [(well-formed Γ α)
   (in α Γ)]

  [(well-formed Γ (∀ α t))
   (well-formed (α Γ) t)])

(module+ test
  (require (submod "lang.rkt" test))

  (test-equal
   (judgment-holds (types · (λ (x : num) x) t)) #t)

  (test-equal
   (judgment-holds (types · ,id t)) #t)

  (test-equal
   (judgment-holds (types · ((,id @ num) 5) t)) #t)
  )