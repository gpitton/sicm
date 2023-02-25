#lang racket
;; Building blocks for the simplification/reduction to normal form
;; of polynomial expressions.
(provide (all-defined-out))
(require (only-in "utils.rkt" atom? l-eval not-one? one? not-zero? num-zero?))


;; rec-with recursively applies the function f to the expression expr.
;; This is useful to apply a simplification/rewriting function that
;; is defined on a single term to a general expression composed of
;; many terms. Contract: f is supposed to be a function that accepts
;; a term (or list), and can return anything.
;; Most of the functions defined in this file are meant to be applied
;; to a polynomial expression by using rec-with.
(define (rec-with f expr)
  (cond [(null? expr) '()]
        [(atom? expr) expr]
        ;; The input expression does not have any sub-expression as
        ;; a constituent.
        [(andmap atom? expr) (f expr)]
        [else  ;; recur
         (cons (rec-with f (car expr))
               (rec-with f (cdr expr)))]))


;; rec-with* is similar to rec-with in that it recursively parses an
;; expression and it applies a given function f to this expression.
;; In addition to that, it then re-applies f to the result of the
;; recursive application. Note that f is required to act on lists.
;; Examples:
;;   (rec-with* f (1 (2 3)) -> (f (1 (f 2 3)))
;;   (rec-with* f ((1 2) (3 4)) -> (f (f 1 2) (f 3 4))
(define (rec-with* f expr)
  (cond [(null? expr) '()]
        [(atom? expr) expr]
        [else  ;; recursively apply f to the arguments and then
         ;; apply f to the result of that.
         (f (map (lambda (ex) (rec-with* f ex)) expr))]))


;; reorder-term is a helper to reorder the term symbols.
;; Examples:
;;    (op 1 2 3) -> (op 1 2 3)
;;    (op 1 2 x 3 x x) -> `(op ,(op 1 2 3) x x x)
;; !Note! that this is not going to work well e.g. with (- 8 3 2) or (/ 8 3 2)
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
             [((list* (? number? arg0) args)
               (list* (? number? r0) rs))
              (rt-aux op args (cons (l-eval `(,op ,arg0 ,r0)) rs))]
             ;; Recursive case, the next element is the first time we see a number.
             ;; Insert it at the head of the term.
             [((list* (? number? arg0) args) _)
              (rt-aux op args (cons arg0 r))]
             ;; Recursive case, next element is a symbolic variable.
             ;; Append it to (car r).
             [((list* (? symbol? arg0) args)
               (list* r0 rs))
              (rt-aux op args (cons r0 (cons arg0 rs)))]
             ;; Recursive case, next element is a list.
             ;; Append it to the end of rs.
             [((list* arg0 args) _)
              (rt-aux op args (append r (list arg0)))])]))
  ;; Initialise the recursion.
  (cond [(null? expr) '()]
        ;; Expression with a single element: not a valid term, but can happen
        ;; while moving down an expression after composing this function with
        ;; rec-with.
        [(null? (cdr expr)) expr]
        [else (rt-aux (car expr) (cdr expr) '())]))


;; reorder-sublists reorders a term containing lists in such a way
;; that all the atoms come before the lists, and the first element
;; of the term is not moved. This function acts only at the outer-
;; most layer of the expression; it does not reorder any sub-terms
;; that may be inside the input term.
;; Examples:
;;   (* 2 (+ 3 4) 5) -> (* 2 5 (+ 3 4))
;;   (+ (* 2 x) 2) -> (+ 2 (* 2 x))
(define (reorder-sublists term)
  (cond [(null? term) '()]
        [(atom? term) term]
        [else
         (let ([op (car term)]
               [args (cdr term)])
           `(,op ,@(filter atom? args)
                 ,@(filter list? args)))]))


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
;;   (^ x 1) -> 'x
;;   (^ x 5) -> (* x x x x x)
(define (expt->mult term)
  (cond [(null? term) 0]
        [(atom? term) term]
        [(eq? (car term) '^)
         ;; Rewrite.
         (let ([var (cadr term)]
               [n (caddr term)])
           ;; Special handling for (^ x 1) -> x.
           (if (one? n) var
               `(* ,@(make-list n var))))]
        ;; Nothing to do.
        [else term]))


;; simpl-add simplifies a term that contains an addition with zero.
;; Examples:
;;   (+ 0 x) -> x
;;   (+ 0 x y) -> (+ x y)
(define (simpl-add term)
  (cond [(null? term) term]
        [(atom? term) term]
        [(eq? (car term) '+)
         ;; This term is an addition.
         (let ([simpl-term (filter not-zero? (cdr term))])
           (cond [(null? simpl-term)
                  ;; term was: (+ 0). This is a malformed expression.
                  (error 'simpl-add "Illegal expression: ~a" term)]
                 [(null? (cdr simpl-term))
                  ;; Term was: (+ 0 x). Just return x.
                  (car simpl-term)]
                 ;; General case: return the original term with the
                 ;; zeros removed.
                 [else `(+ ,@simpl-term)]))]
        [else  ;; This term is not a summation.
         term]))


;; simpl-mul simplifies a term that contains a multiplication by zero or by one.
;; Examples:
;;   (* 0 2 (+ x 3)) -> 0
;;   (* x (+ 2 x) 1) -> (* x (+ 2 x))
(define (simpl-mul term)
  (cond [(null? term) '()]
        [(atom? term) term]
        [(eq? (car term) '*)
         ;; This term is a multiplication.
         (let ([has-zeros
                (ormap num-zero? (cdr term))])
           (if has-zeros 0
               ;; Else just remove the ones. If the result
               ;; of removing the ones is a single number of symbol,
               ;; get that atom out of the list and return it.
               (let ([simpl-term
                      (filter not-one? (cdr term))])
                 (cond [(null? simpl-term)
                        ;; term was: (* 1). This is a malformed expression.
                        (error 'simpl-mul "Illegal expression: ~a" term)]
                       [(null? (cdr simpl-term))
                        ;; term was: (* 1 x). Just return x.
                        (car simpl-term)]
                       ;; General case: return the original term with the ones
                       ;; removed.
                       [else `(* ,@simpl-term)]))))]
        [else  ;; This term is not a multiplication.
         term]))


;; simpl-nested-op simplifes a symbolic expression that has nested
;; sub-expressions with the same operator.
;; Example:
;;   (* 1 (* 2 3) (+ 4 5)) -> (* 1 2 3 (+ 4 5))
;;   (+ (+ 1 2) 3 (+ 4 5)) -> (+ 1 2 3 4 5)
;; We do not guarantee that the order of the arguments is preserved.
;; For instance both transformations below (and more) are acceptable:
;;   (* 1 (* 2 3) (+ 4 5)) -> (* 1 2 3 (+ 4 5))
;;   (* 1 (* 2 3) (+ 4 5)) -> (* (+ 4 5) 3 2 1)
(define (simpl-nested-op term)
  (define (nested-op-aux op expr)
    ;; Helper function for the fold operation.
    (let ([compare-and-splice
           (lambda (x acc)
             (cond [(null? x) acc]
                   [(atom? x) (append acc (list x))]
                   ;; x is a list. Does it have the same operand as the main
                   ;; s-expression?
                   [(eq? (car x) op) (append acc (cdr x))]
                   ;; x is a list, but it has a different operand than the main
                   ;; s-expression.
                   [else (cons x acc)]))])
      (foldl compare-and-splice
             '()
             expr)))
  ;; Initialisation logic.
  (cond [(null? term) '()]
        [(atom? term) term]
        [(member (car term) '(+ *))
         (cons (car term)
               (nested-op-aux (car term) (cdr term)))]
        [else term]))
