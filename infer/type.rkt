#lang racket
(require redex
         "lang.rkt"
         "lib.rkt"
         "examples.rkt")

(provide types?)

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

  [(infer Γ SN_1 e_1 t_a row_a SN_2)
;   (where #t ,(begin (println (term SN_2)) #t))
   (unify-arr SN_2 t_a t_1 -> row_1 t_2 SN_3)
;   (where #t ,(begin (println (term SN_3)) #t))
   (infer Γ SN_3 e_2 t_3 row_2 SN_4)
;   (where #t ,(begin (println (term SN_4)) #t))
   (unify SN_4 t_1 t_3 SN_5)
;   (where #t ,(begin (println (term SN_5)) #t))
   (unify SN_5 row_1 row_2 SN_6)
;   (where #t ,(begin (println (term SN_6)) #t))
   ---------------------------------
   (infer Γ SN_1 (e_1 e_2) t_2 row_2 SN_6)]

  [(infer Γ SN_1 e t_1 row_1 [S_1 N_1])
   (fresh-row N_1 row_2 N_2)
   (fresh-var N_2 t_2 N_3)
   (unify [S_1 N_3] (op (t_1 => t_2) row_2) row_1 SN_2)
   ------------------------------------------------------------------
   (infer Γ SN_1 (op e) t_2 row_1 SN_2)]

  [(infer Γ SN_1 e t row [S_1 N_1])
   (fresh-var N_1 a N_2)
   ---------------------------------------
   (infer Γ SN_1 (lift op e) t (op a row) [S_1 N_2])]

  [(infer Γ SN_1 e t_1 row_1 SN_2)
   (infer-handlers Γ SN_2 t_1 h t_out row_out row_handled SN_3)
   (unify SN_3 row_1 row_handled SN_4)
   (where a_tail (tail row_handled))
   (unify SN_4 a_tail row_out SN_5)
   -----------------------------------
   (infer Γ SN_1 (handle e h) t_out row_out SN_5)])

(define-judgment-form Infer
  #:mode (infer-handlers I I I I O O O O)

  [(fresh-row N_1 a N_3)
   (infer (x t_ret Γ) [S N_3] e t_2 row_out SN_2)
   ---------------------------------------------
   (infer-handlers Γ [S N_1] t_ret (return x e) t_2 row_out a SN_2)]

  [(infer-handlers Γ SN_1 t_ret h t_out row_out row_handled [S N_1])
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

  [---------------
   (unify SN a a SN)]

  [------------------
   (unify SN Int Int SN)]

  [(not-in a (dom-S S))
   (not-in a (ftv t))
   ---------------------
   (unify [S N] a t [(ext a t S) N])]

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

  [(unify-row SN_1 row_2 op t_2 row_r SN_2)
   (unify SN_2 t_1 t_2 SN_3)
   (unify SN_3 row_1 row_r SN_4)
   ---------------------------------
   (unify SN_1 (op t_1 row_1) row_2 SN_4)]
  )

(define-judgment-form Infer
  #:mode (unify-row I I I O O O)
  #:contract (unify-row SN row op t row SN)

  [-----------------------------------
   (unify-row SN (op t row) op t row SN)]

  [(fresh-row N_1 row N_2)
   (fresh-var N_2 t N_3)
   (unify [S N_3] a (op t row) SN)
   -----------------------------------
   (unify-row [S N_1] a op t (op t row) SN)]

  [(unify-row SN_1 row_1 o t_2 row_2 SN_2)
   ---------------------------------------------------------------------------------
   (unify-row SN_1 ((name o1 op_!_1) t_1 row_1) (name o op_!_1) t_2 (o1 t_2 row_2) SN_2)]
  )

(define-judgment-form Infer
  #:mode (types-top I O)

  [(infer · [· 0] e t row [S _])
   (where a (apply S row))
   -------------------------
   (types-top e (apply S t))])

(define-metafunction Infer
  ext : a t S -> S

  [(ext a t S) (a t_1 (substitute S a t_1))
               (where t_1 (apply S t))])

(define-metafunction Infer
  apply : S t -> t

  [(apply · t) t]
  [(apply (a t_1 S) t_2) (apply S (substitute t_2 a t_1))])

(define-metafunction Infer
  tail : row -> a
  [(tail a) a]
  [(tail (op t row)) (tail row)])

(define (types? e)
  (judgment-holds
   (types-top ,e _)))

(module+ test
  (redex-check Infer
               e
               (begin
                 (printf "checking ~s\n" (term e))
                 (types? (term e)))
               #:attempts 100000)
  )

;(define (types? t) (judgment-holds (synth () ,t _ ())))
;
;(define-judgment-form AlgEffT
;  #:mode (synth I I O O)
;  #:contract (synth Γ m t row)
;
;  ;; Variable
;  [(synth Γ x t ()) (find Γ x (t))]
;
;  ;; Literal
;  [(synth Γ number Int ())]
;
;  ;; λ abstraction
;  [(synth Γ (λ [x t_1] m) (t_1 -> t_2 ! row) ())
;   (synth (ext [x t_1] Γ) m t_2 row)]
;
;  ;; Λ abstraction
;  [(synth Γ (Λ a m) (∀ a t) ())
;   (synth (ext a Γ) m t ())]
;
;  ;; Application
;  [(synth Γ (m n) t_2 row_res)
;   (synth Γ m (t_1 -> t_2 ! row_1) row_2)
;   (synth Γ n t_1 row_3)
;   (row-merge-all (row_1 row_2 row_3) row_res)]
;
;  ;; Type application
;  [(synth Γ (m @ t) t_2 row)
;   (synth Γ m (∀ a t_1) row)
;   (where t_2 (substitute t_1 a t))]
;
;  ;; Operator invocation
;  [(synth Γ (do op t_out m) t_out row_out)
;   (synth Γ m t_1 row_1)
;   (row-merge ([op t_1 t_out]) row_1 row_out)]
;
;  ;; Effect lifting
;  [(synth Γ (lift crow_1 m) t (ext* crow_1 row_2))
;   (synth Γ m t row_2)]
;
;  ;; Handler expression
;  [(synth Γ (handle m row_out (return x n) hs) t_out row_out)
;   (synth Γ m t_m row_m)
;   (handlers->row hs row_req)
;;   (side-condition ,(begin (println (term row_req)) #t))
;   (synth (ext [x t_m] Γ) n t_out row_ret)
;;   (side-condition ,(begin (println (term t_out)) #t))
;   (row-merge-all (row_m row_ret row_req) row_all)
;;   (side-condition ,(begin (println (term row_all)) #t))
;   (row-diff row_all row_req row_res)
;;   (side-condition ,(begin (println (term row_res)) #t))
;   (row-sub row_res row_out)
;;   (side-condition ,(begin (println 'ok) #t))
;   (check-handlers Γ hs t_out row_out)]
;  )
;
;(define-judgment-form AlgEffT
;  #:mode (handlers->row I O)
;  #:contract (handlers->row hs row)
;
;  [(handlers->row hs (ext [op t_in t_out] row))
;   (uncons hs [op [_ t_in] [_ t_out] _] hs_rest)
;   (handlers->row hs_rest row)]
;
;  [(handlers->row () ())])
;  
;
;(define-judgment-form AlgEffT
;  #:mode (check-handlers I I I I)
;  #:contract (check-handlers Γ hs t row)
;
;  [(check-handlers Γ () _ _)]
;
;  [(check-handlers Γ hs t_res row_out)
;   (uncons hs [op [x t_in] [r t_out] m] hs_rest)
;   (synth (ext* ([x t_in] [r (t_out -> t_res ! row_out)]) Γ)
;          m t_1 row_handler)
;;   (side-condition ,(begin (println 'check-handler (term op)) (println (term t_1)) #t))
;   (row-sub row_handler row_out)
;   (check-handlers Γ hs_rest t_res row_out)])
;
;(define-judgment-form AlgEffT
;  #:mode (row-eq I I)
;
;  [(row-eq row_1 row_2)
;   (row-sub row_1 row_2)
;   (row-sub row_2 row_1)])
;
;(define-judgment-form AlgEffT
;  #:mode (row-diff I I O)
;  #:contract (row-diff row row row)
;
;  [(row-diff row () row)]
;
;  [(row-diff row (any_h any_tl ...) row_out)
;   (uncons any_h op any_ts)
;   (split row op any_ts row_tl)
;   (row-diff row_tl (any_tl ...) row_out)])
;
;(define-judgment-form AlgEffT
;  #:mode (row-merge I I O)
;  #:contract (row-merge row row row)
;
;  [(row-merge (a) (a) (a))]
;  [(row-merge () row row)]
;  [(row-merge row () row)]
;
;  [(row-merge row_1 row_2 (ext any_h row_res))
;   (uncons row_1 any_h row_1-tl)
;   (uncons any_h op any_type)
;   (split row_2 op any_type row_2-tl)
;   (row-merge row_1-tl row_2-tl row_res)]
;
;  [(row-merge row_1 row_2 (ext any_h row_res))
;   (uncons row_1 any_h row_1-tl)
;   (uncons any_h op any_type)
;   (not-in row_2 op)
;   (row-merge row_1-tl row_2 row_res)])
;   
;
;(define-judgment-form AlgEffT
;  #:mode (row-merge-all I O)
;  #:contract (row-merge-all (row ...) row)
;
;  [(row-merge-all (row) row)]
;  [(row-merge-all (row_1 row_2 row ...) row_res)
;   (row-merge row_1 row_2 row_t)
;   (row-merge-all (row_t row ...) row_res)])
;
;(define-judgment-form AlgEffT
;  #:mode (row-sub I I)
;  #:contract (row-sub row row)
;
;  [(row-sub () row)]
;
;  [(row-sub (a) (a))]
;
;  [(row-sub row_1 row_2)
;   (uncons row_1 any_h row_1-tl)
;   (uncons any_h op any_ts)
;   (split row_2 op any_ts row_2-tl)
;   (row-sub row_1-tl row_2-tl)])
