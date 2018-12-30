#lang br

(require brag/support)
(provide the-lexer)

(define the-lexer
  (lexer-srcloc
   [(:or "λ"
         "fun")
    (token 'LAMBDA)]
   ["handle" (token 'HANDLE)]
   ["with" (token 'WITH)]
   ["end" (token 'END)]
   [(:or "->"
         "→")
    (token 'ARR)]
   [(:or #\+ #\- #\*)
    (token 'PRIM lexeme)]
   [(:+ (:/ #\0 #\9))
    (token 'NUMBER (string->number lexeme))]
   [(:: lower-case (:* alphabetic))
    (token 'VAR lexeme)]
   [(:: upper-case (:* alphabetic))
    (token 'OP lexeme)]
   [(:or whitespace blank iso-control) (token lexeme #:skip? #t)]))

(define (lex-str str)
  (apply-lexer the-lexer str))