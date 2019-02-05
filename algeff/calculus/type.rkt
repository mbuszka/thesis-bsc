#lang racket/base

(require redex/reduction-semantics
         racket/match
         "lang.rkt"
         "lib.rkt"
         "unify.rkt")

(provide infer-type infer-handlers infer)

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
   ------------------------------------- "Bool"
   (infer Γ [S N_1] b Bool row [S N_2])]

  [(fresh-row N_1 row N_2)
   ------------------------------------ "Num"
   (infer Γ [S N_1] m Int row [S N_2])]
  
  [(lookup Γ x t) (fresh-row N_1 row N_2)
   -------------------------------------- "var"
   (infer Γ [S N_1] x t row [S N_2])]

  [(fresh-var N_1 t_1 N_2) (fresh-row N_2 row_2 N_3)
   (infer (x t_1 Γ) [S_1 N_3] e t_2 row_1 SN)
   -------------------------------------------------------- "λ"
   (infer Γ [S_1 N_1] (λ x e) (t_1 -> row_1 t_2) row_2 SN)]

  [(fresh-arr N_1 t_1 -> row_1 t_2 N_2) (fresh-row N_2 row_2 N_3)
   (infer (x_f (t_1 -> row_1 t_2) (x_a t_1 Γ)) [S_1 N_3] e t row SN_1)
   (unify SN_1 row_1 row SN_2) (unify SN_2 t_2 t SN_3)
   ------------------------------------------------------------------- "rec"
   (infer Γ [S_1 N_1] (rec x_f x_a e) (t_1 -> row_1 t_2) row_2 SN_3)]

  [(infer Γ SN_1 e_1 t_a row_a SN_2) (unify-arr SN_2 t_a t_1 -> row_1 t_2 SN_3)
   (infer Γ SN_3 e_2 t_3 row_2 SN_4)
   (unify SN_4 t_1 t_3 SN_5) (unify SN_5 row_1 row_2 SN_6) (unify SN_6 row_1 row_a SN_7)
   ------------------------------------------------------------------------------------- "app"
   (infer Γ SN_1 (app e_1 e_2) t_2 row_2 SN_7)]

  [(check-prim Γ SN_1 prim (e ...) t row SN_2)
   ------------------------------------------- "prim"
   (infer Γ SN_1 (prim e ...) t row SN_2)]

  [(infer Γ SN_1 e_c t_c row_c SN_2) (unify SN_2 t_c Bool SN_3)
   (infer Γ SN_3 e_t t_t row_t SN_4) (infer Γ SN_4 e_e t_e row_e SN_5)
   (unify SN_5 t_t t_e SN_6) (unify SN_6 row_c row_t SN_7) (unify SN_7 row_t row_e SN_8)
   -------------------------------------------------------------- "if"
   (infer Γ SN_1 (if e_c e_t e_e) t_t row_t SN_8)]

  [(infer Γ SN_1 e t_1 row_1 [S_1 N_1]) (fresh-row N_1 row_2 N_2) (fresh-var N_2 t_2 N_3)
   (unify [S_1 N_3] (op (t_1 => t_2) row_2) row_1 SN_2)
   -------------------------------------------------------------------------------------- "op"
   (infer Γ SN_1 (op e) t_2 row_1 SN_2)]

  [(infer Γ SN_1 e t row [S_1 N_1]) (fresh-var N_1 a N_2)
   ------------------------------------------------------ "lift"
   (infer Γ SN_1 (lift op e) t (op a row) [S_1 N_2])]

  [(infer Γ SN_1 e t_1 row_1 SN_2) (infer (x t_1 Γ) SN_2 e_ret t_ret row_ret SN_3)
   (infer-handlers Γ SN_3 t_ret hs row_out row_handled SN_4)
   (unify SN_4 row_out row_ret SN_5) (unify SN_5 row_1 row_handled SN_6)
   ------------------------------------------------------------------------------- "handle"
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
   ------------------------------------------------------------ "tail"
   (infer-handlers Γ [S N_1] t () a_handled a_handled [S N_3])]

  [(infer-handlers Γ SN_1 t_ret (h ...) row_out row_handled [S N_1])
   (fresh-var N_1 t_v N_2) (fresh-var N_2 t_r N_3)
   (infer (x_v t_v (x_r (t_r -> row_out t_ret) Γ)) [S N_3] e t_h row_h SN_2)
   (unify SN_2 row_out row_h SN_3) (unify SN_3 t_ret t_h SN_4)
    -------------------------------------------------------------------------------------------------- "head"
   (infer-handlers Γ SN_1 t_ret ((op (x_v x_r e)) h ...) row_out (op (t_v => t_r) row_handled) SN_4)])

; Check arguments to primitive operation and return result type and effect
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

  [(infer Γ SN_1 e t row [S N_1])
   (fresh-var N_1 a N_2)
   (unify [S N_2] t (List a) SN_2)
   -----------------------------------
   (check-prim Γ SN_1 hd (e) a row SN_2)]

  [(infer Γ SN_1 e t row [S N_1])
   (fresh-var N_1 a N_2)
   (unify [S N_2] t (List a) SN_2)
   -----------------------------------
   (check-prim Γ SN_1 tl (e) t row SN_2)]

  [(in prim (cons? nil?))
   (infer Γ SN_1 e t row [S N_1])
   (fresh-var N_1 a N_2)
   (unify [S N_2] t (List a) SN_2)
   -----------------------------------
   (check-prim Γ SN_1 prim (e) Bool row SN_2)]

  [(fresh-var N_1 a N_2)
   (fresh-row N_2 row N_3)
   -----------------------------------
   (check-prim Γ [S N_1] nil () (List a) row [S N_3])]

  [(infer Γ SN_1 e_hd t_hd row_hd SN_2)
   (infer Γ SN_2 e_tl t_tl row_tl SN_3)
   (unify SN_3 t_tl (List t_hd) SN_4)
   (unify SN_4 row_hd row_tl SN_5)
   -----------------------------------
   (check-prim Γ SN_1 cons (e_hd e_tl) t_tl row_tl SN_5)])

; Infer type with empty effect in the empty environment
(define-judgment-form Infer
  #:mode (types-top I O)

  [(infer · [· 0] e t row SN_1) (unify SN_1 row · [S _])
   -----------------------------------------------------
   (types-top e (apply S t))])

; Infer type and effect in the empty environment
(define (infer-type-eff e)
  (judgment-holds
   (infer · [· 0] ,e t row [S N])
   [(apply S t) (apply S row)]))

; Helper function, infers type with empty effect, in the empty environment
(define (infer-type e)
  (match (judgment-holds
          (types-top ,e t) t)
    [(list t) t]
    [else #f]))
