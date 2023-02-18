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
;;    (reorder-term (op 1 2 x 3 x x)) -> (cons (op 1 2 3) (list x x x))
;; TODO this is not going to work well e.g. with (- 8 3 2) or (/ 8 3 2)
(define (reorder-term expr)
  (define (rt-aux op vals r)
    ;; op: the operation applied in this expression
    ;; vals: operands
    ;; r: output term (recursively builds a reordered copy of expr)
    (cond [(null? vals) r]
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
  (if (null? expr) '()
      (rt-aux (car expr) (cdr expr) '())))


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
           `(* ,(make-list n var)))]
        ;; Nothing to do.
        [else term]))


;; simpl-zmul simplifies an expression that has a multiplication
;; by zero. Example: (+ 2 (* 3 (^ x 2) 0)) -> (+ 2 0)
(define (simpl-zmul expr)
  ;; The helper function accepts an expression, a boolean that is
  ;; true if we are in the context of a multiplication, and returns
  ;; a lambda accepting a continuation.
  (define (sz-aux ex is-in-mult)
    (lambda (k)
      (cond [(null? ex) '()]
            [(number? ex) ex]
            [(symbol? ex) ex]
            ;; expr is a multiplication, and one of the sub-expressions
            ;; is zero. The expression will evaluate to with zero.
            [(and (eq? (car ex) '*)
                  (ormap num-zero? (cdr ex)))
             (if is-in-mult
                 (k 0)  ;; propagate up to the multiplication context
                 ;; that we found a zero.
                 0)]    ;; else just return zero and keep recurring.
            ;; expr is a multiplication, and we need to keep recurring.
            [(eq? (car ex) '*)
             (call/cc
              (lambda (k2)
                (let ([res
                       (map (lambda (e) ((sz-aux e #t) k2))
                            (cdr ex))])
                  `(* ,@res))))]
            ;; Regardless of whether expr is a multiplication, we need
            ;; to recur.
            [else
             (let ([res
                    (map (lambda (e) ((sz-aux e is-in-mult) k))
                         (cdr ex))])
               `(,(car ex) ,@res))])))
  ;; Launch the helper function.
  (call/cc (sz-aux expr #f)))


;; simpl-1mul simplifies an expression by removing any factors
;; equal to 1 in a multiplication expression.
(define (simpl-1mul expr)
  (cond [(null? expr) '()]
        [(not-list? expr) expr]
        [(eq? (car expr) '*)
         (let* ([s-expr
                 ;; recursively apply simpl-1mul...
                 (map simpl-1mul (cdr expr))]
                ;; ... then keep only the sub-expressions that did
                ;; not return one.
                [not1-expr `(* ,@(filter not-one? s-expr))]
                [all-one (andmap num-one? (cdr not1-expr))])
           ;; If all the sub-terms of not1-expr are 1, just
           ;; return 1.
           (if all-one 1 not1-expr))]
        [else
         ;; recursively apply to each term of the expression.
         (map simpl-1mul expr)]))


;;simpl-zadd simplifies an expression by removing any zeroes
;; in a summation expression.
(define (simpl-zadd expr)
  (cond [(null? expr) '()]
        [(not-list? expr) expr]
        [(eq? (car expr) '+)
         (let* ([s-expr
                 ;; recursively apply simpl-zadd...
                 (map simpl-zadd (cdr expr))]
                ;; ... then keep only the sub-expressions that did
                ;; not return zero.
                [nnz-expr `(+ ,@(filter not-zero? s-expr))]
                [all-zero (andmap num-zero? (cdr nnz-expr))])
           ;; If all the subterms of nnz-expr are zero, just
           ;; return zero.
           (if all-zero 0 nnz-expr))]
        [else
         ;; recursively apply to each term of the expression.
         (map simpl-zadd expr)]))


;; simpl-single fixes the nesting level for an expression where:
;; - there is a single operand after an operator + or *. For example:
;;   (simpl-nesting '(+ 2 (+ x))) -> '(+ 2 x)
;; - there is an unnecessary nesting of a list within a list,
;;   for example: (simpl-nesting '((+ 2 3)) -> '(+ 2 3)
(define (simpl-nesting expr)
  (cond [(null? expr) '()]
        [(not-list? expr) expr]
        ;; list with a single number or symbol. Remove one layer of nesting
        ;; and recur. TODO maybe this needs to be redefined in its own function.
        [(and (null? (cdr expr))
              (not-list? (car expr)))
         (car expr)]
        ;; list within a list. TODO maybe this needs to be redefined in its
        ;; own function.
        [(and (null? (cdr expr))
              (list? (car expr)))
         (simpl-nesting (car expr))]
        [(member (car expr) '(+ *))
         (if (null? (cddr expr))
             ;; List with an operator + or * followed by a single element.
             ;; e.g. '(+ (3))
             ;; We can remove one level of nesting.
             (simpl-nesting (cdr expr))
             ;; else recur. We need to apply this simplification action
             ;; to each term of (cdr expr).
             `(,(car expr) ,@(map simpl-nesting (cdr expr))))]
        [else ;; recur
         (map simpl-nesting expr)]))
