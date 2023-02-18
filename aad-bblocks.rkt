#lang racket
;; Helper functions for the algorithmic differentiation of polynomial expressions.

(provide grad)
(require (only-in "utils.rkt" eq-sym?))

;; Gradient of the polynomial expression expr with respect to
;; the variable var.
(define (grad expr var)
  (cond
    [(number? expr) 0]
    [(symbol? expr)
     (if (eq-sym? expr var) 1 0)]
    [else
     (match expr
       ;; Terminate the recursion
       [(list ex) (grad ex var)]
       ;; Definitions for + - * ^.
       [(list '^ x 1)
        (grad x var)]
       [(list '^ x (? number? n))
        #:when (eq-sym? x var)
        `(* ,n (^ ,x ,(sub1 n)))]
       [(list-rest '+ ex0 exs)
        `(+ ,(grad ex0 var)
            ,(grad `(+ ,@exs) var))]
       ; - is misleading: better to use + negate x instead.
       ;[(list-rest - ex0 exs)
       ; `(- ,(grad ex0 var)
       ;     ,(grad `(- ,@exs var))]
       [(list-rest '* exs)
        `(+ ,@(map (lambda (ex)
                     (let ([rest (remove ex exs)])
                       `(* ,(grad ex var) ,@rest)))
                   exs))])]))

