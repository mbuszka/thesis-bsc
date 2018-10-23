#lang racket
(require redex)
(provide F in)

(define-language F
  ; Values
  (v ::= number (λ (x : t) e) (Λ α e))

  ; Expressions
  (e ::= v (e e) (e @ t) x)

  ; Types
  (t ::= num (t -> t) (∀ α t) α)

  ; Variables
  (x α ::= variable-not-otherwise-mentioned)

  ; Typing contexts
  (Γ ::= · (x : t Γ) (α Γ))

  ; Evaluation contexts
  (E ::= hole (E e) (E @ t) (v E))

  #:binding-forms
  (λ (x : t) e #:refers-to x)
  (Λ α e #:refers-to α)
  (∀ α t #:refers-to α)
  )

(define-metafunction F
  [(in x_1 (x_1 : t_1 Γ)) t_1]
  [(in x_1 (x_1 Γ)) #t]
  [(in x_1 (x_2 : _ Γ)) (in x_1 Γ)]
  [(in x_1 (x_2 Γ)) (in x_1 Γ)]
  [(in x_1 ·) #f])

(define-metafunction F
  [(different x_1 x_1) #t]
  [(different x_1 x_2) #f])

(define value? (redex-match? F v))

(define expression? (redex-match? F e))

(define type? (redex-match? F t))

(module+ test
  (provide id)
  (define id (term (Λ α (λ (x : α) x))))
  (test-equal (expression? id) #t)
  (test-equal (value? id) #t)
  (test-equal (expression? (term (,id @ num))) #t)
  )
  