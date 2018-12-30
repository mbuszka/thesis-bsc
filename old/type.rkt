#lang racket
(require redex
         "lang.rkt"
         "lib.rkt"
         "examples.rkt")

(provide handlers->row synth types? AlgEffT)

(define-extended-language AlgEffT AlgEff 
  (Γ ::= (γ ...))
  (γ ::= [x t] a))

(define-judgment-form AlgEffT
  #:mode (synth I I O O)
  #:contract (synth Γ m t row)

  ;; Variable
  [(synth Γ x t ()) (find Γ x (t))]

  ;; Literal
  [(synth Γ number Int ())]

  ;; λ abstraction
  [(synth Γ (λ [x t_1] m) (t_1 -> t_2 ! row) ())
   (synth (ext [x t_1] Γ) m t_2 row)]

  ;; Λ abstraction
  [(synth Γ (Λ a m) (∀ a t) ())
   (synth (ext a Γ) m t ())]

  ;; Application
  [(synth Γ (m n) t_2 row_res)
   (synth Γ m (t_1 -> t_2 ! row_1) row_2)
   (synth Γ n t_1 row_3)
   (row-merge-all (row_1 row_2 row_3) row_res)]

  ;; Type application
  [(synth Γ (m t) t_2 row)
   (synth Γ m (∀ a t_1) row)
   (where t_2 (substitute t_1 a t))]

  ;; Operator invocation
  [(synth Γ (do op t_out m) t_out row_out)
   (synth Γ m t_1 row_1)
   (row-merge ([op t_1 t_out]) row_1 row_out)]

  ;; Effect lifting
  [(synth Γ (lift row_1 m) t (ext* row_1 row_2))
   (synth Γ m t row_2)]

  ;; Handler expression
  [(synth Γ (handle m row_out (return x n) hs) t_out row_out)
   (synth Γ m t_m row_m)
   (handlers->row hs row_req)
;   (side-condition ,(begin (println (term row_req)) #t))
   (synth (ext [x t_m] Γ) n t_out row_ret)
;   (side-condition ,(begin (println (term t_out)) #t))
   (row-merge-all (row_m row_ret row_req) row_all)
;   (side-condition ,(begin (println (term row_all)) #t))
   (row-diff row_all row_req row_res)
;   (side-condition ,(begin (println (term row_res)) #t))
   (row-sub row_res row_out)
;   (side-condition ,(begin (println 'ok) #t))
   (check-handlers Γ hs t_out row_out)]
  )

(define-judgment-form AlgEffT
  #:mode (handlers->row I O)
  #:contract (handlers->row hs row)

  [(handlers->row hs (ext [op t_in t_out] row))
   (uncons hs [op [_ t_in] [_ t_out] _] hs_rest)
   (handlers->row hs_rest row)]

  [(handlers->row () ())])
  

(define-judgment-form AlgEffT
  #:mode (check-handlers I I I I)
  #:contract (check-handlers Γ hs t row)

  [(check-handlers Γ () _ _)]

  [(check-handlers Γ hs t_res row_out)
   (uncons hs [op [x t_in] [r t_out] m] hs_rest)
   (synth (ext* ([x t_in] [r (t_out -> t_res ! row_out)]) Γ)
          m t_1 row_handler)
;   (side-condition ,(begin (println 'check-handler (term op)) (println (term t_1)) #t))
   (row-sub row_handler row_out)
   (check-handlers Γ hs_rest t_res row_out)])

(define-judgment-form AlgEffT
  #:mode (row-eq I I)

  [(row-eq row_1 row_2)
   (row-sub row_1 row_2)
   (row-sub row_2 row_1)])

(define-judgment-form AlgEffT
  #:mode (row-diff I I O)
  #:contract (row-diff row row row)

  [(row-diff row () row)]

  [(row-diff row (any_h any_tl ...) row_out)
   (uncons any_h op any_ts)
   (split row op any_ts row_tl)
   (row-diff row_tl (any_tl ...) row_out)])

(define-judgment-form AlgEffT
  #:mode (row-merge I I O)
  #:contract (row-merge row row row)

  [(row-merge (a) (a) (a))]
  [(row-merge () row row)]
  [(row-merge row () row)]

  [(row-merge row_1 row_2 (ext any_h row_res))
   (uncons row_1 any_h row_1-tl)
   (uncons any_h op any_type)
   (split row_2 op any_type row_2-tl)
   (row-merge row_1-tl row_2-tl row_res)]

  [(row-merge row_1 row_2 (ext any_h row_res))
   (uncons row_1 any_h row_1-tl)
   (uncons any_h op any_type)
   (not-in row_2 op)
   (row-merge row_1-tl row_2 row_res)])
   

(define-judgment-form AlgEffT
  #:mode (row-merge-all I O)
  #:contract (row-merge-all (row ...) row)

  [(row-merge-all (row) row)]
  [(row-merge-all (row_1 row_2 row ...) row_res)
   (row-merge row_1 row_2 row_t)
   (row-merge-all (row_t row ...) row_res)])

(define-judgment-form AlgEffT
  #:mode (row-sub I I)
  #:contract (row-sub row row)

  [(row-sub () row)]

  [(row-sub (a) (a))]

  [(row-sub row_1 row_2)
   (uncons row_1 any_h row_1-tl)
   (uncons any_h op any_ts)
   (split row_2 op any_ts row_2-tl)
   (row-sub row_1-tl row_2-tl)])

(define (types? t)
  (judgment-holds (synth () ,t _ ())))