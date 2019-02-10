#lang racket/base

(require redex/reduction-semantics
         "lang.rkt"
         "lib.rkt")

(provide unify unify-row unify-arr apply)

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

  [------------------ "refl-var"
   (unify SN a a SN)]

  [(in t (Num Bool ·))
   ---------------------- "refl-const"
   (unify SN t t SN)]

  [(not-in a_1 (dom-S S)) (where a_3 (apply S a_2)) (eq a_1 a_3)
   ------------------------------------------------------------- "refl-var-lookup"
   (unify [S N] a_1 a_2 [S N])]

  [(not-in a (dom-S S)) (where t (apply S t_1)) (not-in a (ftv t))
   --------------------------------------------------------------- "ext"
   (unify [S N] a t_1 [(ext a t S) N])]

  [(in a (dom-S S)) (unify [S N] (apply S a) t SN)
   ----------------------------------------------- "lookup"
   (unify [S N] a t SN)]

  [(side-condition (not-var t)) (unify SN_1 a t SN_2)
   -------------------------------------------------- "flip"
   (unify SN_1 t a SN_2)]

  [(unify SN_1 t_1-l t_1-r SN_2) (unify SN_2 row_l row_r SN_3) (unify SN_3 t_2-l t_2-r SN_4)
   ----------------------------------------------------------------------------------------- "->"
   (unify SN_1 (t_1-l -> row_l t_2-l) (t_1-r -> row_r t_2-r) SN_4)]

  [(unify SN_1 t_1-l t_1-r SN_2) (unify SN_2 t_2-l t_2-r SN_3)
   ----------------------------------------------------------- "=>"
   (unify SN_1 (t_1-l => t_2-l) (t_1-r => t_2-r) SN_3)]

  [(unify SN_1 t_1 t_2 SN_2)
   ---------------------------------------- "List"
   (unify SN_1 (List t_1) (List t_2) SN_2)]

  [(side-condition (not-var row_2)) (unify-row [S_1 N_1] row_2 op t_2 row_r [S_2 N_2])
   (where a (last (apply S_1 row_1))) (side-condition (not-in a (dom-S S_2)))
   (unify [S_2 N_2] t_1 t_2 SN_3) (unify SN_3 row_1 row_r SN_4)
   ----------------------------------------------------------------------------------- "row"
   (unify [S_1 N_1] (op t_1 row_1) row_2 SN_4)]
  )

; Transform row, such that it contains op at the front.
(define-judgment-form Infer
  #:mode (unify-row I I I O O O)
  #:contract (unify-row SN row op t row SN)

  [-------------------------------------- "row-head"
   (unify-row SN (op t row) op t row SN)]

  [(not-in a (dom-S S)) (fresh-row N_1 row N_2) (fresh-var N_2 t N_3)
   ------------------------------------------------------------------ "row-var"
   (unify-row [S N_1] a op t row [(ext a (op t row) S) N_3])]

  [(in a (dom-S S)) (unify-row [S N] (apply S a) op t row SN)
   ---------------------------------------------------------- "row-lookup"
   (unify-row [S N] a op t row SN)]

  [(unify-row SN_1 row_1 op_2 t_2 row_2 SN_2) (neq op_1 op_2)
   -------------------------------------------------------------------------------------- "row-swap"
   (unify-row SN_1 (op_1 t_1 row_1) op_2 t_2 (op_1 t_1 row_2) SN_2)]
  )

; Extend the substitution, with fully substituted type
(define-metafunction Infer
  ext : a t S -> S
  [(ext a t S) (a t (substitute S a t))])

; Apply the type substitution to a type
(define-metafunction Infer
  apply : S t -> t

  [(apply · t) t]
  [(apply (a t_1 S) t_2) (apply S (substitute t_2 a t_1))])

; Return the last element of a row - either variable or empty row
(define-metafunction Infer
  last : row -> a
  [(last a) a]
  [(last ·) ·]
  [(last (op t row)) (last row)])
