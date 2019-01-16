#lang racket/base

(require
  data/functor
  data/monad
  data/applicative
  megaparsack
  megaparsack/parser-tools/lex
  racket/match
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
  (parse-result! (parse-tokens expr/p tokens path)))

(define number/p (token/p 'NUMBER))

(define var/p (map (lambda (str) (string->symbol (string-append "v:" str)))
                   (token/p 'VAR)))

(define op/p (map (lambda (str) (string->symbol (string-append "op:" str)))
                  (token/p 'OP)))

(define prim/p (map string->symbol (token/p 'PRIM)))

(define prim-call/p
  (do (op <- prim/p)
    (l <- term/p)
    (r <- term/p)
    (pure (term (,op ,l ,r)))))

(define lambda/p
  (do (token/p 'LAMBDA)
    (v <- var/p)
    (e <- expr/p)
    (pure (term (λ ,v ,e)))))

(define op-call/p
  (do (op <- op/p)
    (t <- term/p)
    (pure (list op t))))

(define lift/p
  (do (token/p 'LIFT)
    (op <- op/p)
    (t <- term/p)
    (pure (list 'lift op t))))

(define handler/p
  (do (op <- op/p)
    (x <- var/p)
    (r <- var/p)
    (token/p 'ARR)
    (e <- expr/p)
    (pure (list op (list x r e)))))

(define pipe/p (token/p 'PIPE))

(define handlers/p
  (do pipe/p
    (or/p
     (try/p (do (h <- handler/p)
              (rest <- handlers/p)
              (pure (cons (car rest) (cons h (cdr rest))))))
     (do (ret <- return/p)
       (pure (cons ret '()))))))

(define return/p
  (do (token/p 'RETURN)
    (x <- var/p)
    (token/p 'ARR)
    (e <- expr/p)
    (pure (term (return ,x ,e)))))

(define handle/p
  (do (token/p 'HANDLE)
    (e <- expr/p)
    (token/p 'WITH)
    (pair <- handlers/p)
    (token/p 'END)
    (pure (list 'handle e (cdr pair) (car pair)))))

(define if/p
  (do (token/p 'IF)
    (cond <- expr/p)
    (token/p 'THEN)
    (then <- expr/p)
    (token/p 'ELSE)
    (else <- expr/p)
    (token/p 'END)
    (pure (term (if ,cond ,then ,else)))))

(define let/p
  (do (token/p 'LET)
    (x <- var/p)
    (token/p 'EQUALS)
    (body <- expr/p)
    (token/p 'IN)
    (rest <- expr/p)
    (pure (term ((λ ,x ,rest) ,body)))))

(define letrec/p
  (do (token/p 'LETREC)
    (f <- var/p)
    (x <- var/p)
    (token/p 'EQUALS)
    (body <- expr/p)
    (token/p 'IN)
    (rest <- expr/p)
    (pure (term ((λ ,f ,rest) (fix (λ ,f (λ ,x ,body))))))))

(define term/p
  (or/p
   handle/p
   if/p
   op-call/p
   lift/p
   prim-call/p
   lambda/p
   number/p
   var/p
   (do (token/p 'LPAREN)
     (e <- expr/p)
     (token/p 'RPAREN)
     (pure e))))

(define (applications xs)
  (define (aux acc xs)
    (match xs
      ['() acc]
      [(list-rest x xs) (aux (list acc x) xs)]))
  (match xs
    [(list x) x]
    [(list-rest x ys) (aux x ys)]))

(define expr/p
  (or/p
   letrec/p
   let/p
   (do (terms <- (many+/p term/p))
     (pure (applications terms)))))
