#lang racket

(require syntax/strip-context
;         megaparsack
;         megaparsack/parser-tools/lex
         "lang/tokenizer.rkt"
         "lang/parser.rkt")

(define (read-syntax path port)
  (define tokens (tokenize path port))
  (define tree (parse tokens path))
  (strip-context
   #`(module algeff-tokenizer-mod algeff/parse-only
       #,tree)))

(module+ reader (provide read-syntax))

(define-syntax-rule (-#%module-begin tree)
  (#%module-begin
     tree))

(provide (rename-out [-#%module-begin #%module-begin])
         #%app #%datum #%top #%top-interaction)