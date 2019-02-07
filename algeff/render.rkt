#! /usr/bin/racket
#lang racket/base
(require racket/match
         redex
         "calculus/abstract-machine.rkt"
         "calculus/eval.rkt"
         "calculus/lang.rkt"
         "calculus/type.rkt"
         "calculus/unify.rkt"
         (prefix-in lc: "../lc/lc.rkt"))

(define (render path)
  (define (mk-path str)
    (string-append path "/" str))

  (parameterize
      ([default-font-size 11]
       [metafunction-font-size 11]
       [label-font-size 9]
       [rule-pict-style 'horizontal])
    (with-compound-rewriters
        (['infer (lambda (lws)
                   (match lws
                     [(list _ _ Γ sn e t row sn-out _ ...)
                      (list "" Γ " | " sn " ⊢ " e " : "  t " ! " row " | " sn-out)]))]
         ['unify (lambda (lws)
                   (match lws
                     [(list _ _ sn-in lhs rhs sn-out _ ...)
                      (list "" sn-in lhs " ~ " rhs sn-out "")]))])
      (begin
        (render-language Infer (mk-path "algeff-syntax.eps"))
        (render-judgment-form infer (mk-path "algeff-infer.eps"))
        (render-judgment-form infer-handlers (mk-path "algeff-infer-handlers.eps"))
        (render-judgment-form free (mk-path "algeff-free.eps"))
        (render-reduction-relation red (mk-path "algeff-red.eps"))
        (render-language AM (mk-path "algeff-am-syntax.eps"))
        (render-reduction-relation am-a (mk-path "algeff-am-a.eps"))
        (render-reduction-relation am-b (mk-path "algeff-am-b.eps"))
        (parameterize ([rule-pict-style 'vertical])
          (begin
            (render-reduction-relation am-c (mk-path "algeff-am-c.eps"))
            (render-reduction-relation am-e (mk-path "algeff-am-e.eps"))))
        (render-metafunction initial-conf (mk-path "algeff-am-initial-conf.eps"))
        (render-language lc:LC (mk-path "lc-syntax.eps"))
        (render-reduction-relation lc:red (mk-path "lc-red.eps"))
        (render-language lc:LC-am (mk-path "lc-am-syntax.eps"))
        (render-reduction-relation lc:abstract-machine (mk-path "lc-am.eps"))
        (render-judgment-form unify (mk-path "algeff-unify.eps"))
        (render-judgment-form unify-row (mk-path "algeff-unify-row.eps"))
        ))))

(render "figures")