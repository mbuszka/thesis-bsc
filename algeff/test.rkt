#lang racket

(require redex
         "type.rkt"
         "eval.rkt"
         "examples.rkt")



(define-term id-int (λ [var:x Int] var:x))
(println (judgment-holds
          (synth () id-int t row)
          (t row)))
(println (judgment-holds
          (synth () (do op:read Int 0) t row)
          (t row)))
(println (judgment-holds
          (synth () example-1 t row)
          (t row)))
(println (judgment-holds
          (synth () ,(handle-expr-state (term (do op:put Int 0)) '()) t row)
          (t row)))
(println (judgment-holds
          (synth () example-2 t row)
          (t row)))
(println (judgment-holds
          (synth () (example-3 5) t row)
          (t row)))

(traces red (term example-2))
