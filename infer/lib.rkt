#lang racket
(require redex
         "lang.rkt")

(provide lookup in not-in dom-S fresh-row fresh-arr fresh-var neq eq trace get-handler prim-apply)

; Switch enabling tracing
(define trace-enabled? #f)

; Helper judgment which allows for printing arbitrary messages during
; evalution of judgment forms
(define-judgment-form Infer
  #:mode (trace I)

  [(trace (any_s any_vals ...))
   (where #t ,(begin
                (when trace-enabled?
                  (apply printf (term (any_s any_vals ...))))
                #t))])

(define-judgment-form Infer
  #:mode (eq I I)

  [(eq any_1 any_1)])

(define-judgment-form Infer
  #:mode (neq I I)

  [(neq any_!_1 any_!_1)])

; checks whether variable is not in a list
(define-judgment-form Infer
  #:mode (not-in I I)

  [-------------
   (not-in a ())]

  [(not-in a (a_rest ...))
   -----------------------
   (not-in (name a a_!_1) (a_!_1 a_rest ...))])

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

(define-metafunction Infer
  prim-apply : prim number number -> number

  [(prim-apply + number_1 number_2) ,(+ (term number_1) (term number_2))]
  [(prim-apply - number_1 number_2) ,(- (term number_1) (term number_2))]
  [(prim-apply * number_1 number_2) ,(* (term number_1) (term number_2))]
  [(prim-apply / number_1 number_2) ,(/ (term number_1) (term number_2))]
  )

(define-judgment-form Infer
  #:mode (get-handler I I O)

  [(get-handler op [(op (x_1 x_2 e)) h ...] (x_1 x_2 e))]
  [(get-handler (name op op_!_1) [(op_!_1 _) h ...] any_r)
   (get-handler op [h ...] any_r)])

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
