#lang racket/base

(require syntax/strip-context
         racket/pretty
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

(define (run e)
  (begin
    (printf "desugared expression:\n")
    (pretty-print e)
    (define t (infer-type e))
    (if t
        (begin
          (printf "\nhas type:\n")
          (pretty-print t)
          (printf "\nreduction result:\n")
          (pretty-print (ev:reduce e))
          (printf "\nabstract machine result:\n")
          (pretty-print (am:reduce e)))
        (printf "does not type-check\n"))))

(provide (rename-out [-#%module-begin #%module-begin])
         #%app #%datum #%top #%top-interaction)