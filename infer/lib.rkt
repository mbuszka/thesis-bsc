#lang racket
(require redex
         "lang.rkt")

(provide lookup in not-in dom-S fresh-row fresh-arr fresh-var neq eq)

(define-judgment-form Infer
  #:mode (eq I I)

  [(eq any_1 any_1)])

(define-judgment-form Infer
  #:mode (neq I I)

  [(neq any_!_1 any_!_1)])

(define-judgment-form Infer
  #:mode (not-in I I)

  [(not-in a ())]

  [(not-in (name a a_!_1) (a_!_1 a_rest ...))
   (not-in a (a_rest ...))])

(define-judgment-form Infer
  #:mode (in I I)
  
  [(in a (a_1 ... a a_2 ...))])

(define-judgment-form Infer
  #:mode (lookup I I O)
  [(lookup (x t Γ) x t)]
  
  [(lookup (x_!_1 _ Γ) (name x x_!_1) t)
   (lookup Γ x t)])

(define-metafunction Infer
  dom-S : S -> (a ...)

  [(dom-S ·) ()]

  [(dom-S (a t S)) (a a_s ...)
   (where (a_s ...) (dom-S S))])

(define-judgment-form Infer
  #:mode (fresh-row I O O)
  [(fresh-row N ,(string->symbol (string-append "t:r" (number->string (term N)))) ,(+ (term N) 1))])

(define-judgment-form Infer
  #:mode (fresh-var I O O)
  [(fresh-var N ,(string->symbol (string-append "t:g" (number->string (term N)))) ,(+ (term N) 1))])

(define-judgment-form Infer
  #:mode (fresh-arr I O I O O O)
  [(fresh-arr N_1 a_1 -> a_r a_2 N_4)
   (fresh-var N_1 a_1 N_2)
   (fresh-row N_2 a_r N_3)
   (fresh-var N_3 a_2 N_4)])
