#lang racket
(require redex
         "lang.rkt"
         "lib.rkt")

(define-extended-language AlgEffT AlgEff 
  (Γ ::= (γ ...))
  (γ ::= [x t] a [l Δ])
  (Δ ::= ([op t t] ...))
  (Ψ ::= (t ...))
  (S ::= ([uv t] ...))
  (N ::= number))

(define-metafunction AlgEffT
  gen-name : N any -> (N any)
  [(gen-name N any_n)
   (,(+ 1 (term N))
    ,(string->symbol (string-append
                      (symbol->string (term any_n))
                      (number->string (term N)))))
   (side-condition (symbol? (term any_n)))])

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
   (synth Γ n t_2 row_3)
   (row-merge-all (row_1 row_2 row_3) row_res)]

  ;; Type application
  [(synth Γ (m t) t_2 row)
   (synth Γ m (∀ a t_1) row)
   (where t_2 (substitute t_1 a t))]

  ;; Operator invocation
  [(synth Γ (do op l m) t_2 row_2)
   (find Γ l (Δ))
   (find Δ op (t_1 t_2))
   (synth Γ m t_1 row_1)
   (row-merge (l) row_1 row_2)]

  ;; Handler expression
  [(synth Γ (handle l m row_out with (return x n) hs) t_out row_out)
   (synth Γ m t_m row_m)
   (split row_m l _ row_1)
   (synth (ext [x t_m] Γ) n t_out row_2)
   (row-merge row_1 row_2 row_ret)
   (row-sub row_ret row_out)
   (find Γ l (Δ))
   (check-handlers Γ Δ hs t_out row_out)]
  )

(define-judgment-form AlgEffT
 #:mode (check-handlers I I I I I)
 #:contract (check-handlers Γ Δ (h ...) t row)

  [(check-handlers Γ () () _ _)]

  [(check-handlers Γ Δ_1 hs_1 t_res row_out)
   (uncons Δ_1 [op t_in t_out] Δ_2)
   (split hs_1 op (x y m) hs_2)
   (synth (ext* ([x t_in] [y (t_out -> t_res ! row_out)]) Γ)
          m t_res row_2)
   (row-sub row_2 row_out)
   (check-handlers Γ Δ_2 hs_2 t_res row_out)])
          

;; [(synth-handlers Γ Δ hs_1 t_out row_out)
;;  (uncons Δ op (t_in t_out))
;;  (split hs_1 op (x y m) hs_2)
;;  (synth (ext* ([x t_in]

(define-judgment-form AlgEffT
  #:mode (row-merge I I O)
  #:contract (row-merge row row row)

  [(row-merge (a) (a) (a))]
  [(row-merge () row row)]
  [(row-merge row () row)]

  [(row-merge (l any_tl ...) row_2 (l any_rest ...))
   (split row_2 l _ row_t)
   (row-merge (any_tl ...) row_t (any_rest ...))])

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

  [(row-sub (l any_tl ...) row_1)
   (split row_1 l _ row_2)
   (row-sub (any_tl ...) row_2)])

(define-term eff-IO
  (lbl:IO ([op:print Int Int])))

(define-term eff-RD
  (lbl:RD ([op:read Int Int])))

(define-term init-Γ
  (eff-IO eff-RD))

(define (rd-handler expr)
  (term
   ((λ [var:y Int]
     (handle lbl:RD ,expr () with
             (return var:x var:x)
             ((op:read var:ignore var:r
                       (var:r var:y)))))
    5)))

(define-term example-1
  ,(rd-handler (term (do op:read lbl:RD 5))))


(module+ test
  (define-term id-int (λ [var:x Int] var:x))
  (println (term (ext [var:x Int] ())))
  (println (judgment-holds (synth () id-int t row)
                           (t row)))
  (println (judgment-holds
            (synth init-Γ (do op:read lbl:RD 5) t row)
            (t row)))
  (println
   (judgment-holds
    (synth init-Γ example-1 t row)
           (t row))))

;;(define-judgment-form ArgsFirstT
;;  #:mode (sub-eff I I)
;;  #:contract (sub-eff eff eff)
;;
;;  [(sub-eff () eff)]
;;  [(sub-eff (l_h l_t ...) eff)
;;   (remove eff l_h eff_t)
;;   (sub-eff (l_t ...) eff_t)]
;;  [(sub-eff (l_l ... a) (l_r ... a))
;;   (eqv-eff (l_l ...) (l_r ...))])
;;  
;;(define-judgment-form ArgsFirstT
;;  #:mode (eqv-eff I I)
;;
;;  [(eqv-eff () ())]
;;  [(eqv-eff (a) (a))]
;;  [(eqv-eff (l any_rest ...) eff)
;;   (remove eff l eff_res)
;;   (eqv-eff (any_rest ...) eff_res)])
;;  
;;(define-judgment-form ArgsFirstT
;;  #:mode (synth-eff I I O)
;;  #:contract (synth-eff eff eff eff)
;;
;;  [(synth-eff (l_1 ... a) (l_2 ... a) (l_1 ... a))
;;   (eqv-eff (l_1 ...) (l_2 ...))]
;;  [(synth-eff (l_1 ... a) (l_2 ...) (l_1 ... a))
;;   (sub-eff (l_2 ...) (l_1 ... a))]
;;  [(synth-eff (l_1 ...) (l_2 ... a) (l_2 ... a))
;;   (sub-eff (l_1 ...) (l_2 ... a))]
;;  [(synth-eff (l_1 ...) (l_2 ...) (synth-eff-closed (l_1 ...) (l_2 ...)))])
;;
;;(define-metafunction ArgsFirstT
;;  synth-eff-closed : (l ...) (l ...) -> (l ...)
;;
;;  [(synth-eff-closed () (l ...)) (l ...)]
;;  [(synth-eff-closed (l l_1 ...) (l_2 ...))
;;   (cons l (synth-eff-closed (l_1 ...) (l_3 ...)))
;;   (judgment-holds (remove (l_2 ...) l (l_3 ...)))]
;;  [(synth-eff-closed (l l_1 ...) (l_2 ...))
;;   (cons l (synth-eff-closed (l_1 ...) (l_2 ...)))])
;;  
;;  
;(define-judgment-form ArgsFirstT
;  #:mode (unif I I I O)
;
;  [(unif Sub UVar UVar Sub)] [(unif Sub Var Var Sub)] [(unif Sub num num Sub)]
;
;  [(lookup Sub_0 UVar MTyp_0) (unif Sub_0 MTyp_0 MTyp Sub_1)
;   -------------------------------------
;   (unif Sub_0 UVar MTyp Sub_1)]
;
;  [(unif Sub_0 UVar MTyp Sub_1)
;   ----------------------------
;   (unif Sub_0 MTyp UVar Sub_1)]
;
;  [(side-condition (not-in (dom Sub_0) UVar))
;   (where MTyp_1 (apply-subst Sub_0 MTyp_0))
;   (side-condition (not-in (fuv MTyp_1) UVar))
;   ------------------------------------------------
;   (unif Sub_0 UVar MTyp_0 (ext Sub_0 UVar MTyp_1))]
;
;  [(unif Sub_0 MTyp_1 MTyp_3 Sub_1) (unif Sub_1 MTyp_2 MTyp_4 Sub_2)
;   --------------------------------------------------------
;   (unif Sub_0 (MTyp_1 -> MTyp_2) (MTyp_3 -> MTyp_4) Sub_2)]
;  )
;
;(define-judgment-form ArgsFirstT
;  #:mode (unif-arr I I O O)
;
;  [(unif-arr Sub (Type_0 -> Type_1) (Type_0 -> Type_1) Sub)]
;
;  [(where UVar_1 ,(gensym "$"))
;   (where UVar_2 ,(gensym "$"))
;   (unif Sub_0 UVar_0 (UVar_1 -> UVar_2) Sub_2)
;   -----------------
;   (unif-arr Sub_0 UVar_0 (UVar_1 -> UVar_2) Sub_2)]
;  )
;
;(define-metafunction ArgsFirstT
;  apply-subst : ([UVar MTyp] ...) MTyp -> MTyp
;  [(apply-subst () MTyp) MTyp]
;  [(apply-subst ([UVar MTyp_0] any_rest ...) MTyp)
;   (apply-subst (any_rest ...) (substitute MTyp UVar MTyp_0))])
;
;(define-metafunction ArgsFirstT
;  fuv : MTyp -> (UVar ...)
;  [(fuv num) ()]
;  [(fuv (MTyp_1 -> MTyp_2)) (sum (fuv MTyp_1) (fuv MTyp_2))]
;  [(fuv a) ()]
;  [(fuv UVar) (UVar)])
