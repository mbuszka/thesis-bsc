#lang racket/base

(require syntax/strip-context
         "lang/tokenizer.rkt"
         "lang/parser.rkt"
         "calculus/type.rkt"
         (prefix-in ev: "calculus/eval.rkt")
         (prefix-in am: "calculus/abstract-machine.rkt")
         )

(define (read-syntax path port)
  (define tokens (tokenize path port))
  (define tree (parse tokens path))
  (strip-context
   #`(module algeff-tokenizer-mod algeff
       #,tree)))

(module+ reader (provide read-syntax))

(define-syntax-rule (-#%module-begin tree)
  (#%module-begin
   (run (quote tree))))

(define (run t)
  (begin
    (println t)
    (if (types? t)
        (list (ev:reduce t) (am:reduce t))
        '(error does not typecheck))))

(provide (rename-out [-#%module-begin #%module-begin])
         #%app #%datum #%top #%top-interaction)