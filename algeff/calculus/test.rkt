#lang racket

(require redex
         "type.rkt"
         (prefix-in red: "eval.rkt")
         (prefix-in am: "abstract-machine.rkt")
         "lang.rkt"
         "examples.rkt")

(provide check-reduction check-am)

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

(define (check-reduction num-attempts #:verbose [verbose #f])
  (let ([c (make-coverage red:red)])
    (begin
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
                (when verbose
                  (println t))
                (or (red:reduce t)
                    (value? t)))
              #t)))
      (parameterize ([relation-coverage (list c)])
        (redex-check Infer
                     e
                     (progress-holds? (term e))
                     #:attempts num-attempts)
        (covered-cases c)))
    (printf "well typed ~s\n" typed)))

(define (check-am num-attempts #:verbose [verbose #f])
  (begin
    (define cnt 0)
    (define typed 0)
    (define (checks? e)
      (begin
        (when (= (modulo cnt 1000) 0)
          (printf "checked ~s, well typed: ~s\n" cnt typed))
        (set! cnt (+ cnt 1))
        (if (and (types-int? e) (not (number? e)))
            (begin
              (when verbose
                (println e))
              (set! typed (+ typed 1))
              (let ([r (red:reduce e)]
                    [m (am:reduce e)])
                (= r (cadr m))))
            #t)))
    (redex-check Infer
                 e
                 (checks? (term e))
                 #:attempts num-attempts)
    (printf "well typed ~s\n" typed)))