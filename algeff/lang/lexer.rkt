#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide the-lexer base base* keywords)

(define-empty-tokens keywords
  (LAMBDA HANDLE WITH END LIFT RETURN))

(define-tokens base
  (PRIM NUMBER VAR OP))

(define-empty-tokens base*
  (ARR PIPE SEMICOLON EOF LPAREN RPAREN))

(define the-lexer
  (lexer-src-pos
   [(:or "λ"
         "fun")
    (token-LAMBDA)]
   ["lift" (token-LIFT)]
   ["handle" (token-HANDLE)]
   ["with" (token-WITH)]
   ["end" (token-END)]
   ["return" (token-RETURN)]
   ["|" (token-PIPE)]
   [";" (token-SEMICOLON)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   [(:or "->"
         "→")
    (token-ARR)]
   [(:or #\+ #\- #\*)
    (token-PRIM lexeme)]
   [(:+ (:/ #\0 #\9))
    (token-NUMBER (string->number lexeme))]
   [(:: lower-case (:* alphabetic))
    (token-VAR lexeme)]
   [(:: upper-case (:* alphabetic))
    (token-OP lexeme)]
   [(:or whitespace blank iso-control) (return-without-pos (the-lexer input-port))]
   [(eof) (token-EOF)]))

(define (lex-str str)
  (define input (open-input-string str))
  (port-count-lines! input)
  (define (loop v)
    (cond [(void? (position-token-token v)) (loop (the-lexer input))]
          [(eq? 'EOF (token-name (position-token-token v))) (list v)]
          [else (cons v (loop (the-lexer input)))]))
  (loop (the-lexer input)))