#lang racket

(require redex
         "type.rkt"
         "eval.rkt"
         "lang.rkt"
         "examples.rkt")



(define-term id-int (λ v:x v:x))
(println (infer-type (term id-int)))
(println (infer-type (term (op:read 0))))
(println (infer-type (term example-1)))
(println (infer-type (handle-expr-state (term (op:put 0)))))
(println (infer-type (term example-2)))
(println (infer-type (term (example-3 5))))
(println (infer-type (term (+ 5 7))))

;(traces red (term example-2))

;(define (reduces? t)
;  (not (null? (apply-reduction-relation
;               red
;               (term (,t))))))

; no longer loops the typechecker
(define loops
  (term
   ((λ v:X
      ((op:y 0) 1))
    (lift op:b 1))))

(define cnt 0)
(define typed 0)

(define (progress-holds? t)
  (begin
    (when (= (modulo cnt 1000) 0)
      (printf "checked ~s, well typed: ~s\n" cnt typed))
    (set! cnt (+ cnt 1))
    (if (types? t)
        (begin
          (set! typed (+ typed 1))
          (or (reduces? t)
              (value? t)))
        #t)))

(define (run-check)
  (let ([c (make-coverage red)])
    (parameterize ([relation-coverage (list c)])
      (redex-check Infer
                   e
                   (progress-holds? (term e))
                   #:attempts 100000)
      (covered-cases c)))
  (printf "well typed ~s\n" typed))