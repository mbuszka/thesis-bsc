#lang racket

(require
  data/functor
  data/monad
  data/applicative
  megaparsack
  megaparsack/parser-tools/lex
  redex)

(provide parse)

;(define parse
;  (parser
;   (tokens base base* keywords)
;   
;   (grammar
;    (expr [(expr term) (term ($1 $2))]
;           [(PRIM term term) (term ($1 $2 $3))]
;           [(term) $1])
;
;     (term [(LAMBDA VAR ARR expr) (term (λ $2 $4))]
;           [(NUMBER) $1]
;           [(var) $1])
;
;     (var [(VAR) (string->symbol (string-append "v:" $1))]))
;
;   (src-pos)
;   (start expr)
;   (end EOF)
;   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos) (void)))))


;expr : expr term
;     | PRIM term term
;     | HANDLE expr WITH handlers END
;     | LIFT OP expr
;     | term
;
;handlers : (PIPE handler SEMICOLON)* PIPE return SEMICOLON
;
;return  : RETURN VAR ARR expr
;
;handler : OP VAR VAR ARR expr
;
;term : LAMBDA VAR ARR expr
;     | OP expr
;     | NUMBER
;     | LPAREN expr RPAREN
;     | VAR
;

(define (parse tokens path)
  (parse-tokens expr/p tokens path))

(define number/p (syntax/p (token/p 'NUMBER)))

(define var/p (map (lambda (str) (string->symbol (string-append "v:" str)))
                   (syntax/p (token/p 'VAR))))

(define op/p (map (lambda (str) (string->symbol (string-append "op:" str)))
                  (syntax/p (token/p 'OP))))

(define prim/p (map string->symbol (syntax/p (token/p 'PRIM))))

(define prim-call/p
  (do (op <- op/p)
    (l <- term/p)
    (r <- term/p)
    (pure (term (,op ,l ,r)))))

(define lambda/p
  (do (token/p 'LAMBDA)
    (v <- var/p)
    (token/p 'ARR)
    (e <- expr/p)
    (pure (term (λ ,v ,e)))))

(define term/p
  (syntax/p
   (or/p
    ;    handle/p
    ;    op-call/p
    prim-call/p
    lambda/p
    number/p
    var/p)))

(define expr/p
  (syntax/p
   (many+/p term/p)))

(define handle/p (void))
     
     
