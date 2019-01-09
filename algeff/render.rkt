#lang racket
(require redex
         "calculus/lang.rkt"
         "calculus/type.rkt"
         (only-in "calculus/eval.rkt" red)
         (only-in "calculus/abstract-machine.rkt" abstract-machine))

(define (render)
  (render-language Infer "../thesis/language.eps")
  (render-judgment-form infer "../thesis/infer.eps")
  (render-reduction-relation red "../thesis/reduction.eps"))