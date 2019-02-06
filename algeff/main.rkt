#lang racket/base

(require racket/pretty
         syntax/strip-context
         "calculus/abstract-machine.rkt"
         "calculus/eval.rkt"
         (for-syntax "calculus/type.rkt")
         (for-syntax racket/base)
         "lang/parser.rkt"
         "lang/tokenizer.rkt")

(module+ reader
  ; Converts program text into a simple module which contains a calculus expression
  (define (read-syntax path port)
    (define tokens (tokenize path port))
    (define tree (parse tokens path))
    (strip-context
     #`(module algeff-tokenizer-mod algeff
         #,tree)))
  (provide read-syntax))

; Expands the calculus expression into call to run function
;(define-macro (-#%module-begin STX)
;  #`(#%module-begin
;   (define #,(datum->syntax caller-stx 'foo) 1)
;   5))
(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(_ tree)
     (let* ([e (syntax->datum #'tree)]
            [t (infer-type e)])
       (with-syntax
           ([expression (datum->syntax stx 'expression)]
            [type (datum->syntax stx 'type)]
            [traces-reduction (datum->syntax stx 'traces-reduction)]
            [traces-machine (datum->syntax stx 'traces-machine)])
         (if t
             #`(#%module-begin
                (require redex)
                (define expression (quote tree))
                (define type (quote #,t))
                (define (traces-reduction)
                  (traces red expression))
                (define (traces-machine)
                  (traces abstract-machine (term (initial-conf tree))))
                (provide expression type))
             (raise "does-not-typecheck"))))]
    ))

(provide (rename-out [-#%module-begin #%module-begin])
         #%top #%app #%datum #%top-interaction
         )