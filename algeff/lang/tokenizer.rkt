#lang racket

(require "lexer.rkt" parser-tools/lex)
(provide make-tokenizer tokenize)

(define (make-tokenizer port [path #f])
  (port-count-lines! port)
  (file-path path)
  (define (next-token) (the-lexer port))
  next-token)

(define (tokenize path port)
  (define next-token (make-tokenizer port path))
  (define (loop v)
    (cond [(void? (position-token-token v)) (loop (next-token))]
          [(eq? 'EOF (token-name (position-token-token v))) '()]
          [else (cons v (loop (next-token)))]))
  (loop (next-token)))
