#lang racket
;; Building blocks for the simplification/reduction to normal form
;; of polynomial expressions.
(provide (all-defined-out))
(require "utils.rkt")


;; rec-with recursively applies the function f to the expression expr.
;; This is useful to apply a simplification/rewriting function that
;; is defined on a single term to a general expression composed of
;; many terms. Contract: f is supposed to be a function that accepts
;; a term (or list), and can return anything.
;; Most of the functions defined in this file are meant to be applied
;; to a polynomial expression by using rec-with.
(define (rec-with f expr)
  (cond [(null? expr) '()]
        [(not-list? expr) expr]
        ;; The input expression does not have any sub-expression as
        ;; a constituent.
        [(andmap not-list? expr) (f expr)]
        [else  ;; recur
         (cons (rec-with f (car expr))
               (rec-with f (cdr expr)))]))


;; helper to reorder the term symbols at compile-time.
;; expected behaviour:
;;    (reorder-term (op 1 2 3)) -> (op 1 2 3)
;;    (reorder-term (op 1 2 x 3 x x)) -> `(op ,(op 1 2 3) x x x)
;; TODO this is not going to work well e.g. with (- 8 3 2) or (/ 8 3 2)
(define (reorder-term expr)
  ;; op: the operation applied in this expression
  ;; vals: operands
  ;; r: output term (recursively builds a reordered copy of expr)
  (define (rt-aux op vals r)
    (cond [(and (null? vals)
                (null? (cdr r)))
           ;; r must be a single number or symbol: just return it.
           (car r)]
          ;; r is a (reordered) list: prepend the operation and return.
          [(null? vals) (cons op r)]
          [(null? r) (rt-aux op (cdr vals) (list (car vals)))]
          [else
           (match* (vals r)
             ;; Recursive case, next element is a number, we can
             ;; apply op to update the head of r.
             [((list-rest (? number? arg0) args)
               (list-rest (? number? r0) rs))
              (rt-aux op args (cons (l-eval `(,op ,arg0 ,r0)) rs))]
             ;; Recursive case, the next element is the first time we see a number.
             ;; Insert it at the head of the term.
             [((list-rest (? number? arg0) args) _)
              (rt-aux op args (cons arg0 r))]
             ;; Recursive case, next element is a symbolic variable.
             ;; Append it to (car r).
             [((list-rest (? symbol? arg0) args)
               (list-rest r0 rs))
              (rt-aux op args (cons r0 (cons arg0 rs)))])]))
  ;; Initialise the recursion.
  (cond [(null? expr) '()]
        ;; Expression with a single element: not a valid term, but can happen
        ;; while moving down an expression after composing this function with
        ;; rec-with.
        [(null? (cdr expr)) expr]
        [else (rt-aux (car expr) (cdr expr) '())]))


;; mult->expt rewrites a term in the form (4 x x x) to (4 (^ x 3)).
;; As it expands, mult->expt can have the following arguments:
;; () -> 0
;; n:number -> n
;; () n:number -> n
;; (s:symbol ...) -> (m->e (1 s ...))
;; (n:number s0 s1 ...) -> `(n (^ s0 ,(m->e (s1 ...) 1)))
;; (s0 s1 ...) n:number -> (m->e (s1 ...) (add1 n))
(define (mult->expt term)
  (define (me-aux t r)
    (match* (t r)
      ;; Exit condition: just append the exponent at the end
      ;; of the term.
      [('() _) r]
      ;; Leading coefficient is a number: start the recursion.
      [((list-rest (? number? t0) (? symbol? t1) ts) _)
       ;; TODO check that the symbols s0, s1, ... are equal?
       `(,t0 (^ ,t1 ,(me-aux ts (add1 r))))]
      ;; Recursive case.
      [((list-rest t0 ts) _)
       (me-aux ts (add1 r))]))
  ;; mult->expt implementation.
  (cond
    ;; Empty list: return 0.
    [(null? term) 0]
    ;; Constant.
    [(number? term) term]
    [(symbol? term) term]
    ;; No leading coefficient: set it to 1 and start the recursion.
    [(not (number? (car term)))
     (me-aux (cons 1 term) 0)]
    ;; Start the recursion.
    [else (me-aux term 0)]))


;; expt->mult does the inverse transformation of mult->expt.
;; Examples:
;;   (* 2 x) -> (* 2 x) (unchanged)
;;   (^ x 5) -> (* x x x x x)
(define (expt->mult term)
  (cond [(null? term) '()]
        [(not-list? term) term]
        [(eq? (car term) '^)
         ;; Rewrite.
         (let ([var (cadr term)]
               [n (caddr term)])
           `(* ,@(make-list n var)))]
        ;; Nothing to do.
        [else term]))


