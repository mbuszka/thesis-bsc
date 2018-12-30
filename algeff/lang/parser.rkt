#lang brag

expr : expr term
     | PRIM term term
     | HANDLE expr WITH handlers END
     | LIFT OP expr
     | term

handlers : (PIPE handler SEMICOLON)* PIPE return SEMICOLON

return  : RETURN VAR ARR expr

handler : OP VAR VAR ARR expr

term : LAMBDA VAR ARR expr
     | OP expr
     | NUMBER
     | LPAREN expr RPAREN
     | VAR

;(define number/p (syntax/p (token/p 'NUMBER)))
;
;(define var/p (syntax/p (token/p 'VAR)))
;
;(define op/p (syntax/p (token/p 'OP)))
;
;(define prim/p (syntax/p (token/p 'PRIM)))
;
;(define expr/p
;  (syntax/p
;   (do [f <- expr/p]
;     [a <- term/p])
     
     
