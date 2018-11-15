#lang racket
(require redex)
(provide Base find split uncons ext ext* not-in)

(define-language Base)

(define-relation Base
  not-equal ⊂ any × any
  [(not-equal any_!_x any_!_x)])

(define-relation Base
  is-pair ⊂ any
  [(is-pair any_x)
   (side-condition (pair? (term any_x)))])

(define-judgment-form Base
  #:mode (uncons I O O)

  [(uncons any_x any_hd any_tl)
   (side-condition (is-pair any_x))
   (where (any_hd any_tl) (,(car (term any_x)) ,(cdr (term any_x))))]

  [(uncons any_x any_x ())
   (side-condition ,(not (pair? (term any_x))))])

(define-judgment-form Base
  #:mode (find I I O)
  [(find (any_hd any_tl ...) any_x any_v)
   (uncons any_hd any_x any_v)]
  
  [(find (any_hd any_tl ...) any_x any_v)
   (uncons any_hd any_y _)
   (side-condition (not-equal any_x any_y))
   (find (any_tl ...) any_x any_v)])

(define-judgment-form Base
  #:mode (not-in I I)
  [(not-in () any)]

  [(not-in (any_hd any_tl ...) any_x)
   (uncons any_hd any_y _)
   (side-condition (not-equal any_x any_y))
   (not-in (any_tl ...) any_x)])

(define-judgment-form Base
  #:mode (split I I O O)
  [(split (any_hd any_tl ...) any_x any_v (any_tl ...))
   (uncons any_hd any_x any_v)]
  
  [(split (any_hd any_tl ...) any_x any_v (any_hd any_rest ...))
   (uncons any_hd any_y _)
   (side-condition (not-equal any_x any_y))
   (split (any_tl ...) any_x any_v (any_rest ...))])

(define-metafunction Base
  ext : any (any ...) -> any
  [(ext any_x (any_y ...)) (any_x any_y ...)])

(define-metafunction Base
  ext* : (any ...) (any ...) -> (any ...)
  [(ext* (any_x ...) (any_y ...)) (any_x ... any_y ...)])
