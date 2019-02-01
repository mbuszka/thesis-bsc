#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide the-lexer base base* keywords)

(define-empty-tokens keywords
  (LAMBDA HANDLE WITH END LIFT RETURN LETREC LET IN IF THEN ELSE TRUE FALSE))

(define-tokens base
  (PRIM NUMBER VAR OP))

(define-empty-tokens base*
  (ARR PIPE SEMICOLON EOF LPAREN RPAREN EQUALS COMMA))

(define the-lexer
  (lexer-src-pos
   [(:: "--" (:* (:~ #\newline)) #\newline) (return-without-pos (the-lexer input-port))]
   [(:or "λ"
         "fun")
    (token-LAMBDA)]
   ["lift" (token-LIFT)]
   ["handle" (token-HANDLE)]
   ["with" (token-WITH)]
   ["end" (token-END)]
   ["return" (token-RETURN)]
   ["letrec" (token-LETREC)]
   ["let" (token-LET)]
   ["in" (token-IN)]
   ["if" (token-IF)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["true" (token-TRUE)]
   ["false" (token-FALSE)]
   ["|" (token-PIPE)]
   [";" (token-SEMICOLON)]
   ["," (token-COMMA)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["=" (token-EQUALS)]
   [(:or "->"
         "→")
    (token-ARR)]
   [(:or #\+ #\- #\* "==" "<=" ">=" "cons" "nil" "hd" "tl" "cons?" "nil?")
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