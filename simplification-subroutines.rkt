#lang racket
;; This file has some term simplification routines that are
;; meant to be applied to a full polynomial expression. There
;; are implementations of these same subroutines that act on a
;; single term of an expression (and are meant to be combined
;; with rec-with) in simpl.bblocks.rkt

(provide simplify-zmul simplify-1mul simplify-zadd simplify-nesting)
(require (only-in "utils.rkt" not-list? not-one? num-one? not-zero? num-zero?))


;; simplify-zmul simplifies an expression that has a multiplication
;; by zero. Example: (+ 2 (* 3 (^ x 2) 0)) -> (+ 2 0).
;; This is a function that is meant to be applied to a full
;; expression. There is a version that acts on a single term
;; (and is meant to be used with "rec-with") called "simpl-zmul".
(define (simplify-zmul expr)
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


;; simplify-1mul simplifies an expression by removing any factors
;; equal to 1 in a multiplication expression.
(define (simplify-1mul expr)
  (cond [(null? expr) '()]
        [(not-list? expr) expr]
        [(eq? (car expr) '*)
         (let* ([s-expr
                 ;; recursively apply simplify-1mul...
                 (map simplify-1mul (cdr expr))]
                ;; ... then keep only the sub-expressions that did
                ;; not return one.
                [not1-expr `(* ,@(filter not-one? s-expr))]
                [all-one (andmap num-one? (cdr not1-expr))])
           ;; If all the sub-terms of not1-expr are 1, just
           ;; return 1.
           (if all-one 1 not1-expr))]
        [else
         ;; recursively apply to each term of the expression.
         (map simplify-1mul expr)]))


;; simplify-zadd simplifies an expression by removing any zeroes
;; in a summation expression.
(define (simplify-zadd expr)
  (cond [(null? expr) '()]
        [(not-list? expr) expr]
        [(eq? (car expr) '+)
         (let* ([s-expr
                 ;; recursively apply simpl-zadd...
                 (map simplify-zadd (cdr expr))]
                ;; ... then keep only the sub-expressions that did
                ;; not return zero.
                [nnz-expr `(+ ,@(filter not-zero? s-expr))]
                [all-zero (andmap num-zero? (cdr nnz-expr))])
           ;; If all the subterms of nnz-expr are zero, just
           ;; return zero.
           (if all-zero 0 nnz-expr))]
        [else
         ;; recursively apply to each term of the expression.
         (map simplify-zadd expr)]))


;; simplify-single fixes the nesting level for an expression where:
;; - there is a single operand after an operator + or *. For example:
;;   (simpl-nesting '(+ 2 (+ x))) -> '(+ 2 x)
;; - there is an unnecessary nesting of a list within a list,
;;   for example: (simpl-nesting '((+ 2 3)) -> '(+ 2 3)
(define (simplify-nesting expr)
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
         (simplify-nesting (car expr))]
        [(member (car expr) '(+ *))
         (if (null? (cddr expr))
             ;; List with an operator + or * followed by a single element.
             ;; e.g. '(+ (3))
             ;; We can remove one level of nesting.
             (simplify-nesting (cdr expr))
             ;; else recur. We need to apply this simplification action
             ;; to each term of (cdr expr).
             `(,(car expr) ,@(map simplify-nesting (cdr expr))))]
        [else ;; recur
         (map simplify-nesting expr)]))
