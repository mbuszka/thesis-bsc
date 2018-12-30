#lang br

(require "lexer.rkt" brag/support)
(provide make-tokenizer)

(define (make-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)
  (define (next-token) (the-lexer port))
  next-token)
