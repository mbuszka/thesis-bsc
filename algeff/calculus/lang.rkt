#lang racket

(require redex
         racket/set)

(provide Infer ftv ops subst is-var not-var value?)

(define-language Infer
  ; Values
  (v     ::= number (λ x e) true false)
  ; TODO add booleans, letrec.
  ; Primitive operations on numbers
  (prim  ::= + - *)

  ; Expressions
  (e     ::= v x (e e) (if e e e) (fix e)
         (op e) (handle e hs ret) (lift op e) (prim e ...))

  ; Handlers
  (hs    ::= ((op_!_1 hexpr) ...))

  ; Operation handler expr
  (hexpr ::= (x_!_1 x_!_1 e))

  ; Return clause
  (ret   ::= (return x e))
  (h     ::= (op hexpr))

  ; Types
  (t     ::= Int Bool (t -> row t) (t => t) row a)
  (row   ::= (op t row) a ·)

  ; Term variables
  (x     ::= (variable-prefix v:))

  ; Type variables
  (a     ::= (variable-prefix t:))

  ; Operations
  (op    ::= (variable-prefix op:))

  ; Typing environments
  (Γ     ::= (x t Γ) ·)

  ; Evaluation contexts
  (E     ::= hole (E e) (v E) (prim E e) (prim v E) (if E e e) (fix E)
         (op E) (handle E hs ret) (lift op E))
  
  ; Substitution
  (S     ::= (a t S) ·)

  ; Name supply token (a number)
  (N n   ::= natural)

  ; Pair of substiution and token, passed around by relations
  (SN    ::= (S N))
  
  #:binding-forms
  (λ x e #:refers-to x)
  (∀ a ... t #:refers-to (shadow a ...))
  (op x_1 x_2 e #:refers-to (shadow x_2 x_1) h)
  (return x e #:refers-to x))

(define var?
  (redex-match? Infer a))

(define-relation Infer
  is-var ⊂ t
  [(is-var a)])

(define-relation Infer
  not-var ⊂ t
  [(not-var t)
   (side-condition (not (var? (term t))))])

(define value? (redex-match? Infer v))

; Project substitution from pair
(define-metafunction Infer
  subst : [S N] -> S

  [(subst [S N]) S])

(define-metafunction Infer
  ops : hs -> (op ...)

  [(ops ()) ()]
  [(ops ((op _) h ...)) (op op_1 ...)
                        (where (op_1 ...) (ops (h ...)))])

(define-metafunction Infer
  ftv : t -> (a ...)

  [(ftv t) ,(set->list (term (ftv/s t)))])

(define-metafunction Infer
  ftv/s : t -> any

  [(ftv/s a) ,(set (term a))]
  [(ftv/s ·) ,(set)]
  [(ftv/s Int) ,(set)]
  [(ftv/s Bool) ,(set)]
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
