#lang racket

(require redex
         "type.rkt"
         "eval.rkt"
         "lang.rkt")



;(define-term id-int (λ [var:x Int] var:x))
;(println (judgment-holds
;          (synth () id-int t row)
;          (t row)))
;(println (judgment-holds
;          (synth () (do op:read Int 0) t row)
;          (t row)))
;(println (judgment-holds
;          (synth () example-1 t row)
;          (t row)))
;(println (judgment-holds
;          (synth () ,(handle-expr-state (term (do op:put Int 0)) '()) t row)
;          (t row)))
;(println (judgment-holds
;          (synth () example-2 t row)
;          (t row)))
;(println (judgment-holds
;          (synth () (example-3 5) t row)
;          (t row)))

;(traces red (term example-2))

;(define (reduces? t)
;  (not (null? (apply-reduction-relation
;               red
;               (term (,t))))))

(define (progress-holds? t)
  (if (types? t)
      (or (reduces? t)
          (value? t))
      #t))

(let ([c (make-coverage red)])
    (parameterize ([relation-coverage (list c)])
      (redex-check Infer
                   e
                   (progress-holds? (term e))
                   #:attempts 1000)
      (covered-cases c)))