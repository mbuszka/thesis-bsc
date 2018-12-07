#lang racket
(require redex
         "lang.rkt"
         "lib.rkt"
         "examples.rkt")

(provide types? infer-type)

(define-judgment-form Infer
  #:mode (infer I I I O O O)
  #:contract (infer Γ SN e t row SN)

  [(fresh-row N_1 row N_2)
   --------------
   (infer Γ [S N_1] number Int row [S N_2])]
  
  [(lookup Γ x t) (fresh-row N_1 row N_2)
   ------------------------------------
   (infer Γ [S N_1] x t row [S N_2])]

  [(fresh-var N_1 t_1 N_2)
   (fresh-row N_2 row_2 N_3)
   (infer (x t_1 Γ) [S_1 N_3] e t_2 row_1 SN)
   ------------------------------------------------
   (infer Γ [S_1 N_1] (λ x e) (t_1 -> row_1 t_2) row_2 SN)]

  [; (where #t ,(begin (printf "=== inferring application ===\n") #t))
   (infer Γ SN_1 e_1 t_a row_a SN_2)
   ;   (where #t ,(begin (println (term SN_2)) #t))
   (unify-arr SN_2 t_a t_1 -> row_1 t_2 SN_3)
   ;   (where #t ,(begin (println (term SN_3)) #t))
   (infer Γ SN_3 e_2 t_3 row_2 SN_4)
   ;   (where #t ,(begin (println (term SN_4)) #t))
   (unify SN_4 t_1 t_3 SN_5)
   ;   (where #t ,(begin (println (term SN_5)) #t))
   (unify SN_5 row_1 row_2 SN_6)
   (unify SN_6 row_1 row_a SN_7)
   ;   (where #t ,(begin (println (term SN_6)) #t))
   ---------------------------------
   (infer Γ SN_1 (e_1 e_2) t_2 row_2 SN_7)]

  [; (where #t ,(begin (printf "=== inferring op e ===\n") #t))
   (infer Γ SN_1 e t_1 row_1 [S_1 N_1])
;   (where #t ,(begin (printf "=== inferred e ===\n") #t))
   (fresh-row N_1 row_2 N_2)
   (fresh-var N_2 t_2 N_3)
;   (where #t ,(begin (println (term row_1)) #t))
;   (where #t ,(begin (printf "=== unifying row in op e ===\n") #t))
   (unify [S_1 N_3] (op (t_1 => t_2) row_2) row_1 SN_2)
;   (where #t ,(begin (printf "=== inferred op e ===\n") #t))
;   (where #t ,(begin (println (term (hello SN_2))) #t))
   ------------------------------------------------------------------
   (infer Γ SN_1 (op e) t_2 row_1 SN_2)]

  [(infer Γ SN_1 e t row [S_1 N_1])
   (fresh-var N_1 a N_2)
   ---------------------------------------
   (infer Γ SN_1 (lift op e) t (op a row) [S_1 N_2])]

  [; (where #t ,(begin (println (term [SN_1])) #t))
   (infer Γ SN_1 e t_1 row_1 SN_2)
;   (where #t ,(begin (printf "=== inferred handled expression ===\n\n") #t))
   (infer-handlers Γ SN_2 t_1 h t_out row_out row_handled SN_3)
;   (where #t ,(begin (printf "=== inferred handlers ===\n") #t))
;   (where #t ,(begin (printf "=== substitution ===\n") #t))
;   (where #t ,(begin (printf "~s\n" (term (subst SN_3))) #t))
;   (where #t ,(begin (printf "=== expression row ===\n") #t))
;   (where #t ,(begin (printf "~s\n" (term (apply (subst SN_3) row_1))) #t))
;   (where #t ,(begin (printf "=== handled row ===\n") #t))
;   (where #t ,(begin (printf "~s\n" (term (apply (subst SN_3) row_handled))) #t))
   (unify SN_3 row_1 row_handled SN_4)
;   (where #t ,(begin (printf "=== unified expression row with handled row ===\n") #t))
   (where a_tail (tail row_handled))
   (unify SN_4 a_tail row_out SN_5)
;   (where #t ,(begin (printf "=== unified handled tail with result row ===\n") #t))
   -----------------------------------
   (infer Γ SN_1 (handle e h) t_out row_out SN_5)])

(define-judgment-form Infer
  #:mode (infer-handlers I I I I O O O O)

  [(fresh-row N_1 a N_3)
   (infer (x t_ret Γ) [S N_3] e t_2 row_out SN_2)
   ---------------------------------------------
   (infer-handlers Γ [S N_1] t_ret (return x e) t_2 row_out a SN_2)]

  [; (where #t ,(begin (printf "=== checking equality of ~s ~s ===\n" (term x_v) (term x_r)) #t))
   (neq x_v x_r)
   (infer-handlers Γ SN_1 t_ret h t_out row_out row_handled [S N_1])
   (fresh-var N_1 t_v N_2)
   (fresh-var N_2 t_r N_3)
   (infer (x_v t_v (x_r (t_r -> row_out t_out) Γ)) [S N_3] e t_h row_h SN_2)
   (unify SN_2 row_out row_h SN_3)
   (unify SN_3 t_out t_h SN_4)
   -------------------------------------------------------------------------
   (infer-handlers Γ SN_1 t_ret (op x_v x_r e h) t_out row_out (op (t_v => t_r) row_handled) SN_4)])

(define-judgment-form Infer
  #:mode (unify-arr I I O I O O O)
  #:contract (unify-arr SN t t -> row t SN)

  [(fresh-arr N_1 t_1 -> row_1 t_2 N_2)
   (unify [S N_2] a (t_1 -> row_1 t_2) SN)
   ------------------------------------
   (unify-arr [S N_1] a t_1 -> row_1 t_2 SN)]

  [---------------------------------------------------
   (unify-arr SN (t_1 -> row_1 t_2) t_1 -> row_1 t_2 SN)])

(define-judgment-form Infer
  #:mode (unify I I I O)
  #:contract (unify SN t t SN)

  [-----------------
   (unify SN a a SN)]

  [------------------
   (unify SN Int Int SN)]

  [-----------------
   (unify SN · · SN)]

;  [(neq a_1 a_2)
;   (not-in a_1 (dom-S S))
;   (not-in a_2 (dom-S S))
;   -----------------------------------------
;   (unify [S N] a_1 a_2 [(ext a_1 a_2 S) N])]

;  [(not-in a_1 (dom-S S))
;   (in a_2 (dom-S S))
;   (unify [S N] a_1 (apply S a_2) SN)
;   -----------------------------------------
;   (unify [S N] a_1 a_2 SN)]

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

  [(unify SN_1 a nvt SN_2)
   ---------------------
   (unify SN_1 nvt a SN_2)]

  [(unify SN_1 t_1-l t_1-r SN_2)
   (unify SN_2 row_l row_r SN_3)
   (unify SN_3 t_2-l t_2-r SN_4)
   -------------------------------------------------------------
   (unify SN_1 (t_1-l -> row_l t_2-l) (t_1-r -> row_r t_2-r) SN_4)]

  [(unify SN_1 t_1-l t_1-r SN_2)
   (unify SN_2 t_2-l t_2-r SN_3)
   -------------------------------------------------
   (unify SN_1 (t_1-l => t_2-l) (t_1-r => t_2-r) SN_3)]

  [; (where #t ,(begin (printf "=== rewriting rhs row ===\n") #t))
   (unify-row [S_1 N_1] row_2 op t_2 row_r [S_2 N_2])
;   (where #t ,(begin (printf "=== rewritten rhs row ===\n") #t))
;   (where #t ,(begin (printf "~s\n" (term (apply S_2 row_r))) #t))
;   (where #t ,(begin (printf "~s\n" (term S_2)) #t))
   (where a (tail (apply S_1 row_1)))
;   (where #t ,(begin (printf "~s\n" (term a)) #t))
   (side-condition (not-in a (dom-S S_2)))
;   (where #t ,(begin (printf "=== tail check passed ===\n") #t))
   (unify [S_2 N_2] t_1 t_2 SN_3)
;   (where #t ,(begin (printf "=== unified types ===\n") #t))
   (unify SN_3 row_1 row_r SN_4)
;   (where #t ,(begin (printf "=== unified tails ===\n") #t))
   ---------------------------------
   (unify [S_1 N_1] (op t_1 row_1) row_2 SN_4)]
  )

(define-judgment-form Infer
  #:mode (unify-row I I I O O O)
  #:contract (unify-row SN row op t row SN)

  [-----------------------------------
   (unify-row SN (op t row) op t row SN)]

  [(not-in a (dom-S S))
   (fresh-row N_1 row N_2)
   (fresh-var N_2 t N_3)
   -----------------------------------
   (unify-row [S N_1] a op t (op t row) [(ext a (op t row) S) N_3])]

  [(in a (dom-S S))
   (unify-row [S N] (apply S a) op t row SN)
   ----------------
   (unify-row [S N] a op t row SN)]

  [(unify-row SN_1 row_1 o t_2 row_2 SN_2)
   ---------------------------------------------------------------------------------
   (unify-row SN_1 ((name o1 op_!_1) t_1 row_1) (name o op_!_1) t_2 (o1 t_2 row_2) SN_2)]
  )

(define-judgment-form Infer
  #:mode (types-top I O)

  [(infer · [· 0] e t row SN_1)
   (unify SN_1 row · [S _])
   -------------------------
   (types-top e (apply S t))])

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

(module+ test

  (define loops (term ((λ v:X ((op:y 0) 1)) (lift op:b 1))))
  )