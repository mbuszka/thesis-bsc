#lang racket

(require redex)

(provide handle-expr-read
         handle-expr-state
         example-1
         example-2
         example-3)

(define (handle-expr-read expr row)
  (term
   (λ [var:y Int]
     (handle ,expr ,row
             (return var:x var:x)
             ([op:read [var:ignore Int] [var:r Int]
                       (var:r var:y)])))))

(define (handle-expr-state expr row)
  (term
   (λ [var:init Int]
     ((handle ,expr ,row
              (return var:x (λ [var:s Int] var:s))
              ([op:get [var:ignore Int] [var:r Int]
                       (λ [var:s Int] ((var:r var:s) var:s))]
               [op:put [var:new Int] [var:r Int]
                       (λ [var:s Int] ((var:r 0) var:new))]))
      var:init))))

(define-term example-1
  ,(handle-expr-read (term (do op:read Int 0)) '()))

(define-term example-2
  (,(handle-expr-state
     (term (do op:get Int (do op:put Int 42)))
     '())
   17))

(define-term example-3
  (λ [var:x Int] (λ [var:y Int] 5)))