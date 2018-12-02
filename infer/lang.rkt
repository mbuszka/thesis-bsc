#lang racket

(require redex
         racket/set)

(provide Infer ftv ops value?)

(define-language Infer
  (v ::= natural (λ x e))
  (e ::= v x (e e)
     (op e) (handle e h)
     (lift op e))
  (easy ::= natural (λ x easy) x (easy easy) (op easy))
  (ret ::= (return x e))
  (h   ::= (op x x e h) ret)
  (t ::= Int (t -> row t) (t => t) row (∀ a ... t) a)
  (nvt ::= Int (t -> row t) (t => t) (op t row) (∀ a ... t))
  (row ::= (op t row) a)

  (x ::= (variable-prefix v:))
  (a ::= (variable-prefix t:))
  (op ::= (variable-prefix op:))

  (Γ ::= (x t Γ) ·)
  (E ::= hole (E e) (v E)
     (op E) (handle E h)
     (lift op E))
  
  (S ::= (a t S) ·)
  (N ::= natural)
  (SN ::= (S N))
  
  #:binding-forms
  (λ x e #:refers-to x)
  (∀ a ... t #:refers-to (shadow a ...))
  (op x_1 x_2 e #:refers-to (shadow x_1 x_2) h)
  (return x e #:refers-to x))

(define value? (redex-match? Infer v))

(define-metafunction Infer
  ops : h -> (op ...)

  [(ops ret) ()]
  [(ops (op _ _ _ h)) (op op_1 ...)
                      (where (op_1 ...) (ops h))])

(define-metafunction Infer
  ftv : t -> (a ...)

  [(ftv t) ,(set->list (term (ftv/s t)))])

(define-metafunction Infer
  ftv/s : t -> any

  [(ftv/s a) ,(set (term a))]
  [(ftv/s Int) ,(set)]
  [(ftv/s (t_1 -> row t_2)) ,(set-union (term any_1) (term any_2) (term any_3))
                            (where any_1 (ftv/s t_1))
                            (where any_2 (ftv/s row))
                            (where any_3 (ftv/s t_2))]
  [(ftv/s (op t row)) ,(set-union (term any_1) (term any_2))
                      (where any_1 (ftv/s t))
                      (where any_2 (ftv/s row))]
  [(ftv/s (t_1 => t_2)) ,(set-union (term any_1) (term any_2))
                        (where any_1 (ftv/s t_1))
                        (where any_2 (ftv/s t_2))]
  [(ftv/s (∀ a ... t)) ,(set-subtract (term any_1) (term any_2))
                       (where any_1 (ftv/s t))
                       (where any_2 ,(list->set (term (a ...))))])
