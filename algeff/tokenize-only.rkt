#lang racket/base

(require parser-tools/lex
         syntax/strip-context
         "lang/tokenizer.rkt")

(define (read-syntax path port)
  (define tokens (tokenize path port))
  (strip-context
   #`(module algeff-tokenizer-mod algeff/tokenize-only
       #,@tokens)))

(module+ reader (provide read-syntax))

(define-syntax-rule (tokenize-only-mb token ...)
  (#%module-begin
     (list token ...)))

(provide (rename-out [tokenize-only-mb #%module-begin])
         #%app #%datum #%top #%top-interaction)