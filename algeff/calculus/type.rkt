#lang racket/base

(require redex
         "lang.rkt"
         "lib.rkt"
         "examples.rkt")

(provide types? types-int? infer-type infer)

; Infers a type and effect row of an expression.
; Takes:
;   - typing environment
;   - a pair of substitution and name supply
;   - expression
; Produces:
;   - type
;   - effect row
;   - a pair of substitution and name supply
(define-judgment-form Infer
  #:mode (infer I I I O O O)
  #:contract (infer Γ SN e t row SN)

  [(fresh-row N_1 row N_2)
   ---------------------------------------
   (infer Γ [S N_1] true Bool row [S N_2])]

  [(fresh-row N_1 row N_2)
   ---------------------------------------
   (infer Γ [S N_1] false Bool row [S N_2])]
  
  [(trace ("=== inferring number ===\n~s\n" number))
   (fresh-row N_1 row N_2)
   (trace ("<<< inferred number <<<\n~s\n~s\n" (apply S Int) (apply S row)))
   --------------
   (infer Γ [S N_1] number Int row [S N_2])]
  
  [(trace ("=== inferring variable ===\n~s\n" x))
   (lookup Γ x t) (fresh-row N_1 row N_2)
   (trace ("<<< inferred variable <<<\n~s\n~s\n" (apply S t) (apply S row)))
   ------------------------------------
   (infer Γ [S N_1] x t row [S N_2])]

  [(trace ("=== inferring lambda ===\n~s\n" (λ x e)))
   (fresh-var N_1 t_1 N_2)
   (fresh-row N_2 row_2 N_3)
   (infer (x t_1 Γ) [S_1 N_3] e t_2 row_1 SN)
   (trace ("<<< inferred lambda <<<\n~s\n~s\n"
           (apply (subst SN) (t_1 -> row_1 t_2)) (apply (subst SN) row_2)))
   ------------------------------------------------
   (infer Γ [S_1 N_1] (λ x e) (t_1 -> row_1 t_2) row_2 SN)]

  [(trace ("=== inferring rec ===\n~s\n" (rec x_f x_a e)))
   (fresh-arr N_1 t_1 -> row_1 t_2 N_2)
   (fresh-row N_2 row_2 N_3)
   (infer (x_f (t_1 -> row_1 t_2) (x_a t_1 Γ)) [S_1 N_3] e t row SN_1)
   (unify SN_1 row_1 row SN_2)
   (unify SN_2 t_2 t SN_3)
   (trace ("<<< inferred rec <<<\n~s\n~s\n"
           (apply (subst SN_3) (t_1 -> row_1 t_2)) (apply (subst SN_3) row_2)))
   ------------------------------------------------
   (infer Γ [S_1 N_1] (rec x_f x_a e) (t_1 -> row_1 t_2) row_2 SN_3)]

  [(trace ("=== inferring application ===\n~s\n" (e_1 e_2)))
   (infer Γ SN_1 e_1 t_a row_a SN_2)
   (unify-arr SN_2 t_a t_1 -> row_1 t_2 SN_3)
   (infer Γ SN_3 e_2 t_3 row_2 SN_4)
;   (trace ("=== unify arg with expected ===\nexpected ~s\narg: ~s\n"
;           (apply (subst SN_4) t_1) (apply (subst SN_4) t_3))) 
   (unify SN_4 t_1 t_3 SN_5)
;   (trace ("=== unify arrow row with arg row ===\narrow:~s\narg: ~s\n"
;           (apply (subst SN_5) row_1) (apply (subst SN_5) row_2)))
   (unify SN_5 row_1 row_2 SN_6)
;   (trace ("=== unify arrow row with arr env row ===\narrow:~s\narg: ~s\n"
;           (apply (subst SN_6) row_1) (apply (subst SN_6) row_a)))
   (unify SN_6 row_1 row_a SN_7)
   (trace ("<<< inferred application <<<\n~s\n~s\n"
           (apply (subst SN_7) t_2) (apply (subst SN_7) row_2)))
   ---------------------------------
   (infer Γ SN_1 (e_1 e_2) t_2 row_2 SN_7)]

  [(trace ("=== inferring primitive op ===\n~s\n" (prim e ...)))
   (check-prim Γ SN_1 prim (e ...) t row SN_2)
   (trace ("<<< inferred primitive op <<<\n~s\n" (apply (subst SN_2) t)))
   --------------------------------------------
   (infer Γ SN_1 (prim e ...) t row SN_2)]

  [(infer Γ SN_1 e_cond t_cond row_cond SN_2)
   (unify SN_2 t_cond Bool SN_3)
   (infer Γ SN_3 e_then t_then row_then SN_4)
   (infer Γ SN_4 e_else t_else row_else SN_5)
   (unify SN_5 t_then t_else SN_6)
   (unify SN_6 row_cond row_then SN_7)
   (unify SN_7 row_then row_else SN_8)
   -------------------------------------------------------------
   (infer Γ SN_1 (if e_cond e_then e_else) t_then row_then SN_8)]

  [(trace ("=== inferring operation ===\n~s\n" (op e)))
   (infer Γ SN_1 e t_1 row_1 [S_1 N_1])
   (fresh-row N_1 row_2 N_2)
   (fresh-var N_2 t_2 N_3)
   (unify [S_1 N_3] (op (t_1 => t_2) row_2) row_1 SN_2)
   (trace ("<<< inferred operation <<<\n~s\n~s\n"
           (apply (subst SN_2) t_2) (apply (subst SN_2) row_1)))
   ------------------------------------------------------------------
   (infer Γ SN_1 (op e) t_2 row_1 SN_2)]

  [(trace ("=== inferring lift ===\n~s\n" (lift op e)))
   (infer Γ SN_1 e t row [S_1 N_1])
   (fresh-var N_1 a N_2)
   (trace ("<<< inferred lift <<<\n~s\n~s\n" (apply S_1 t) (apply S_1 (op a row))))
   ---------------------------------------
   (infer Γ SN_1 (lift op e) t (op a row) [S_1 N_2])]

  [(trace ("=== inferring handle ===\n~s\n" (handle e hs (return x e_ret))))
   (infer Γ SN_1 e t_1 row_1 SN_2)
;   (trace ("=== inferring return ===\n~s\n" (return x e_ret)))
   (infer (x t_1 Γ) SN_2 e_ret t_ret row_ret SN_3)
;   (trace ("<<< inferred return <<<\n~s\n~s\n"
;           (apply (subst SN_3) t_ret) (apply (subst SN_3) row_ret)))
;   (trace ("about to call\n~s" (Γ SN_3 t_ret hs)))
   (infer-handlers Γ SN_3 t_ret hs row_out row_handled SN_4)
   (unify SN_4 row_out row_ret SN_5)
   (unify SN_5 row_1 row_handled SN_6)
   (trace ("<<< inferred handle <<<\n~s\n~s\n"
           (apply (subst SN_6) t_ret) (apply (subst SN_6) row_out)))
   -----------------------------------
   (infer Γ SN_1 (handle e hs (return x e_ret)) t_ret row_out SN_6)])

; Infers result effects and handled effects for a list of handlers.
; Takes:
;   - a typing evironment
;   - a pair of substitution and name supply
;   - result type (after return)
;   - list of handlers
; Produces:
;   - result effect row
;   - handled effect row
;   - a pair of substitution and name supply
(define-judgment-form Infer
  #:mode (infer-handlers I I I I O O O)
  #:contract (infer-handlers Γ SN t hs row row SN)

  [(fresh-row N_1 a_handled N_3)
   --------------------------------------------------
   (infer-handlers Γ [S N_1] t () a_handled a_handled [S N_3])]

  [(trace ("=== inferring handlers ===\n~s\n" ((op (x_v x_r e)) h ...)))
   (infer-handlers Γ SN_1 t_ret (h ...) row_out row_handled [S N_1])
   (fresh-var N_1 t_v N_2)
   (fresh-var N_2 t_r N_3)
   (infer (x_v t_v (x_r (t_r -> row_out t_ret) Γ)) [S N_3] e t_h row_h SN_2)
   (unify SN_2 row_out row_h SN_3)
   (unify SN_3 t_ret t_h SN_4)
   (trace ("<<< inferred handlers <<<\n~s\n~s\n"
           (apply (subst SN_4) t_ret) (apply (subst SN_4) row_out)))
   -------------------------------------------------------------------------
   (infer-handlers Γ SN_1 t_ret ((op (x_v x_r e)) h ...) row_out (op (t_v => t_r) row_handled) SN_4)])

(define-judgment-form Infer
  #:mode (check-prim I I I I O O O)
  #:contract (check-prim Γ SN prim (e ...) t row SN)

  [(in prim (+ - *))
   (infer Γ SN_1 e_1 t_1 row_1 SN_2)
   (unify SN_2 t_1 Int SN_3)
   (infer Γ SN_3 e_2 t_2 row_2 SN_4)
   (unify SN_4 t_2 Int SN_5)
   (unify SN_5 row_1 row_2 SN_6)
   ------------------------------------------------
   (check-prim Γ SN_1 prim (e_1 e_2) Int row_2 SN_6)]

  [(in prim (== <= >=))
   (infer Γ SN_1 e_1 t_1 row_1 SN_2)
   (unify SN_2 t_1 Int SN_3)
   (infer Γ SN_3 e_2 t_2 row_2 SN_4)
   (unify SN_4 t_2 Int SN_5)
   (unify SN_5 row_1 row_2 SN_6)
   ------------------------------------------------
   (check-prim Γ SN_1 prim (e_1 e_2) Bool row_2 SN_6)]
  )

; Unify a variable, or arrow constructor, returning arrow type.
(define-judgment-form Infer
  #:mode (unify-arr I I O I O O O)
  #:contract (unify-arr SN t t -> row t SN)

  [(fresh-arr N_1 t_1 -> row_1 t_2 N_2)
   (unify [S N_2] a (t_1 -> row_1 t_2) SN)
   ------------------------------------
   (unify-arr [S N_1] a t_1 -> row_1 t_2 SN)]

  [---------------------------------------------------
   (unify-arr SN (t_1 -> row_1 t_2) t_1 -> row_1 t_2 SN)])

; Unify two types. Variables are substituted lazily.
; Takes:
;    - a pair of substitution and name supply
;    - two types
; Returns:
;    - a pair of substitution and name supply
(define-judgment-form Infer
  #:mode (unify I I I O)
  #:contract (unify SN t t SN)

  [-----------------
   (unify SN a a SN)]

  [------------------
   (unify SN Int Int SN)]

  [------------------
   (unify SN Bool Bool SN)]

  [-----------------
   (unify SN · · SN)]

  [(not-in a (dom-S S))
   (where t (apply S t_1))
   (not-in a (ftv t))
   ---------------------
   (unify [S N] a t_1 [(ext a t S) N])]

  [(not-in a (dom-S S))
   (where t (apply S t_1))
   (eq a t)
   -------------------------
   (unify [S N] a t_1 [S N])]

  [(in a (dom-S S))
   (unify [S N] (apply S a) t SN)
   --------------------
   (unify [S N] a t SN)]

  [(side-condition (not-var t))
   (unify SN_1 a t SN_2)
   ---------------------
   (unify SN_1 t a SN_2)]

  [(unify SN_1 t_1-l t_1-r SN_2)
   (unify SN_2 row_l row_r SN_3)
   (unify SN_3 t_2-l t_2-r SN_4)
   -------------------------------------------------------------
   (unify SN_1 (t_1-l -> row_l t_2-l) (t_1-r -> row_r t_2-r) SN_4)]

  [(unify SN_1 t_1-l t_1-r SN_2)
   (unify SN_2 t_2-l t_2-r SN_3)
   -------------------------------------------------
   (unify SN_1 (t_1-l => t_2-l) (t_1-r => t_2-r) SN_3)]

  [(side-condition (not-var row_2))
   (unify-row [S_1 N_1] row_2 op t_2 row_r [S_2 N_2])
   (where a (tail (apply S_1 row_1)))
   (side-condition (not-in a (dom-S S_2)))
   (unify [S_2 N_2] t_1 t_2 SN_3)
   (unify SN_3 row_1 row_r SN_4)
   ---------------------------------
   (unify [S_1 N_1] (op t_1 row_1) row_2 SN_4)]
  )

; Transform row, such that it contains op at the front.
(define-judgment-form Infer
  #:mode (unify-row I I I O O O)
  #:contract (unify-row SN row op t row SN)

  [-----------------------------------
   (unify-row SN (op t row) op t row SN)]

  [(not-in a (dom-S S))
   (fresh-row N_1 row N_2)
   (fresh-var N_2 t N_3)
   -----------------------------------
   (unify-row [S N_1] a op t row [(ext a (op t row) S) N_3])]

  [(in a (dom-S S))
   (unify-row [S N] (apply S a) op t row SN)
   ----------------
   (unify-row [S N] a op t row SN)]

  [(unify-row SN_1 row_1 o t_2 row_2 SN_2)
   ---------------------------------------------------------------------------------
   (unify-row SN_1 ((name o1 op_!_1) t_1 row_1) (name o op_!_1) t_2 (o1 t_1 row_2) SN_2)]
  )

; Checks if expression types with empty effect row
(define-judgment-form Infer
  #:mode (types-top I O)

  [(infer · [· 0] e t row SN_1)
   (unify SN_1 row · [S _])
   -------------------------
   (types-top e (apply S t))])

; Checks if expression has type int with empty effect row
(define-judgment-form Infer
  #:mode (types-top-int I)

  [(infer · [· 0] e t row SN_1)
   (unify SN_1 row · SN_2)
   ; (unify SN_2 t Int [S _])
   -------------------------
   (types-top-int e)])

; Helper function, inferring type in empty environment
(define (infer-type e)
  (judgment-holds
   (infer · [· 0] ,e t row [S N])
   [(apply S t) (apply S row)]))

(define-metafunction Infer
  ext : a t S -> S

  [(ext a t S) (a t (substitute S a t))])

(define-metafunction Infer
  apply : S t -> t

  [(apply · t) t]
  [(apply (a t_1 S) t_2) (apply S (substitute t_2 a t_1))])

(define-metafunction Infer
  tail : row -> a
  [(tail a) a]
  [(tail ·) ·]
  [(tail (op t row)) (tail row)])

(define (types? e)
  (judgment-holds
   (types-top ,e _)))

(define (types-int? e)
  (judgment-holds
   (types-top-int ,e)))

(module+ test
  ; Should this term typecheck?
  ; Problem is that we unify functions row with ambient row and arg row,
  ; but we disallow to unify variable with a type containing it.
  (define does-not-typecheck (term ((λ v:X ((op:y 0) 1)) (lift op:b 1))))
  )