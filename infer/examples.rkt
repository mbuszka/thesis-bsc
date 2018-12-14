#lang racket

(require redex)

(provide handle-expr-read
         handle-expr-state
         example-1
         example-2
         example-3)

(define (handle-expr-read expr)
  (term
   (λ v:y
     (handle ,expr
             ([op:read (v:ignore v:r (v:r v:y))])
             (return v:x v:x)))))

(define (handle-expr-state expr)
  (term
   (handle ,expr
           ([op:get (v:ignore v:r (λ v:s ((v:r v:s) v:s)))]
            [op:put (v:new v:r (λ v:s ((v:r 0) v:new)))])
           (return v:x (λ v:s v:s)))))

(define-term example-1
  ,(handle-expr-read (term (op:read 0))))

(define-term example-2
  (,(handle-expr-state
     (term (op:get (op:put 42))))
   17))

(define-term example-3
  (λ v:x (λ v:y 5)))