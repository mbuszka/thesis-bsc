#lang racket/base

(require racket/pretty
         syntax/strip-context
         "calculus/abstract-machine.rkt"
         "calculus/eval.rkt"
         "calculus/type.rkt"
         "lang/parser.rkt"
         "lang/tokenizer.rkt")

; Converts program text into a simple module which contains a calculus expression
(define (read-syntax path port)
  (define tokens (tokenize path port))
  (define tree (parse tokens path))
  (strip-context
   #`(module algeff-tokenizer-mod algeff
       #,tree)))

(module+ reader (provide read-syntax))

; Expands the calculus expression into call to run function
(define-syntax-rule (-#%module-begin tree)
  (#%module-begin
   (run (quote tree))))

; Infers the type of an expression, then reduces it using both reduction relation and abstract machine
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
          (pretty-print (reduce e))
          (printf "\nabstract machine result:\n")
          (pretty-print (am-reduce e)))
        (printf "does not type-check\n"))))

(provide (rename-out [-#%module-begin #%module-begin])
         #%app #%datum #%top #%top-interaction)