#lang racket/base

(require syntax/strip-context
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

; If a calculus expression type checks convert it into module:
; Module defines and exports:
; - expression - the calculus expression
; - type - its type
; - (trace) - a procedure displaying trace of the reduction
; - (trace-machine) - a procedure displaying trace of abstract machine
; - (reduce) - a procedure reducing the expression
; - (reduce-machine) - a procedure reducing the expression with the abstract machine
(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(_ tree)
     (let* ([e (syntax->datum #'tree)]
            [t (infer-type e)])
       (with-syntax
           ([i-expr (datum->syntax stx 'expression)]
            [i-type (datum->syntax stx 'type)]
            [i-trace (datum->syntax stx 'trace)]
            [i-trace-machine (datum->syntax stx 'trace-machine)]
            [i-reduce (datum->syntax stx 'reduce)]
            [i-reduce-machine (datum->syntax stx 'reduce-machine)])
         (if t
             #`(#%module-begin
                (require redex)
                (define i-expr (quote tree))
                (define i-type (quote #,t))
                (define (i-trace)
                  (traces red i-expr))
                (define (i-trace-machine)
                  (traces abstract-machine (term (initial-conf tree))))
                (define (i-reduce)
                  (reduce i-expr))
                (define (i-reduce-machine)
                  (am-reduce i-expr))
                (i-reduce)
                (provide (all-defined-out)))
             (raise 'does-not-typecheck))))]
    ))

(provide (rename-out [-#%module-begin #%module-begin])
         #%top #%app #%datum #%top-interaction)