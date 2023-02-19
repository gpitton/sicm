;; Unit tests for the AAD helper functions
#lang racket

(require "aad-bblocks.rkt")
(require "simpl-bblocks.rkt")
(require "simplification-subroutines.rkt")
(require rackunit rackunit/text-ui)


;; Required to use eval (racket-only).
(define t-eval    ;; stands for test-eval
  (let ([ns (make-base-namespace)])
    (lambda (expr) (eval expr ns))))


(define-test-suite aad-helpers
  (test-case "algorithmic differentiation"
             (check-equal? (grad 2 'c) 0)
             (check-equal? (grad 'x 'c) 0)
             (check-equal? (grad 'c 'c) 1)
             (let ([res (grad '(+ c 2) 'c)])
               (check-equal? (t-eval res) 1))
             (let ([res (grad '(+ c c) 'c)])
               (check-equal? (t-eval res) 2))
             (let ([res (grad '(+ c c c c c 5) 'c)])
               (check-equal? (t-eval res) 5))
             ;; Test the full output without evaluating.
             (check-equal? (grad '(* c c c c c 5) 'c)
                           '(+ (* 1 c c c c 5) (* 1 c c c c 5)
                               (* 1 c c c c 5) (* 1 c c c c 5)
                               (* 1 c c c c 5) (* 0 c c c c c)))
             (let ([res (grad '(+ (+ c c c) c 2) 'c)])
               (check-equal? (t-eval res) 4))
             (let ([res (grad '(^ c 1) 'c)])
               (check-equal? (t-eval res) 1))
             (check-equal? (grad '(^ c 5) 'c) '(* 5 (^ c 4)))
             (check-equal? (grad '(* a (* x x)) 'x)
                           '(+ (* 0 (* x x))
                               (* (+ (* 1 x) (* 1 x)) a)))
             (check-equal? (grad '(* 2 a (* x x)) 'x)
                           '(+  (* 0 a (* x x))
                                (* 0 2 (* x x))
                                (* (+ (* 1 x) (* 1 x)) 2 a)))
             (check-equal? (grad '(+ (* 4 x x x) (* 2 x (^ x 6)) -2) 'x)
                           '(+ (+ (* 0 x x x) (* 1 4 x x)
                                  (* 1 4 x x) (* 1 4 x x))
                               (+ (+ (* 0 x (^ x 6)) (* 1 2 (^ x 6))
                                     (* (* 6 (^ x 5)) 2 x)) (+ 0 0))))
             (check-equal? (grad '(+ (* 5 b (^ x 2)) (* 2 c x) -2) 'x)
                           '(+ (+ (* 0 b (^ x 2)) (* 0 5 (^ x 2))
                                  (* (* 2 (^ x 1)) 5 b))
                               (+ (+ (* 0 c x) (* 0 2 x) (* 1 2 c)) (+ 0 0)))))
  )


(define-test-suite simplify-expression
  ;; Tests for the expression simplification subroutines that are
  ;; meant to be applied to a full polynomial expression.
  (test-case "simplify multiplication by zero"
             (check-equal? (simplify-zmul '()) '())
             (check-equal? (simplify-zmul '(+ 2 3)) '(+ 2 3))
             (check-equal? (simplify-zmul '(* 1 2)) '(* 1 2))
             (check-equal? (simplify-zmul '(* 1 2 3 4 0)) 0)
             (check-equal? (simplify-zmul '(+ (* 2 3 (+ 1 1)) (* 4 (* 1 0) 5)))
                           '(+ (* 2 3 (+ 1 1)) 0))
             (check-equal? (simplify-zmul '(+ (* 2 4 (^ x 4) (* (+ 1 2) 0)) 2))
                           '(+ 0 2))
             (check-equal? (simplify-zmul '(+ (* 1 (c c c c 5)) (* 0 (c c c c c))))
                           '(+ (* 1 (c c c c 5)) 0))
             (check-equal? (simplify-zmul '(+ (+ 2 3 (+ 1 0)) (* 2 (+ 1 1 0))))
                           '(+ (+ 2 3 (+ 1 0)) (* 2 (+ 1 1 0)))))
  (test-case "simplify multiplication by one"
             (check-equal? (simplify-1mul '()) '())
             (check-equal? (simplify-1mul '(* 1 2 3)) '(* 2 3))
             (check-equal? (simplify-1mul '(* 1 (* 2 3 1) 4 1)) '(* (* 2 3) 4))
             (check-equal? (simplify-1mul '(* x (+ 3 (* x 1 3) 2) 1))
                           '(* x (+ 3 (* x 3) 2)))
             (check-equal? (simplify-1mul '(1 (^ x 3))) '(1 (^ x 3)))
             (check-equal? (simplify-1mul '(* 1 1)) 1)
             (check-equal? (simplify-1mul '(* 1 1 (* 1 1) 1)) 1))
  (test-case "simplify summations with zero"
             (check-equal? (simplify-zadd '()) '())
             (check-equal? (simplify-zadd '(+ 0 1 2 3)) '(+ 1 2 3))
             (check-equal? (simplify-zadd '(+ 0 (* x 2 0) (+ 2 3 0) 1 0))
                           '(+ (* x 2 0) (+ 2 3) 1))
             (check-equal? (simplify-zadd '(+ 0 0)) 0)
             (check-equal? (simplify-zadd '(+ 0 0 (+ 0 0))) 0))
  (test-case "simplify terms with deeper nesting than necessary"
             (check-equal? (simplify-nesting '(+ (+ 1))) 1)
             (check-equal? (simplify-nesting '(+ (+ (* 2 3)))) '(* 2 3))
             (check-equal? (simplify-nesting '(+ (+ (* (* 2 (^ x 1)) 5 b)) (+ (+ (* 2 c)))))
                           '(+ (* (* 2 (^ x 1)) 5 b) (* 2 c))))
  )


(define-test-suite simpl-helpers
  (test-case "expression walker"
             (let ([term-add1 (lambda (xs) (map add1 xs))])
               (check-equal? (rec-with term-add1 '()) '())
               (check-equal? (rec-with term-add1 1) 1)
               (check-equal? (rec-with term-add1 '(1 2 3)) '(2 3 4))
               (check-equal? (rec-with term-add1 '(1 (2 3) (4 (5 6))))
                             '(1 (3 4) (4 (6 7))))))
  (test-case "reorder-term"
             (check-equal? (reorder-term '(+ 1 2 3 4)) 10)
             (check-equal? (reorder-term '(* 1 2 3 4)) 24)
             ;(check-equal? (reorder-term '(c)) '(c))
             ;(check-equal? (reorder-term '((2))) '((2)))  ;; TODO check this
             (check-equal? (reorder-term '(+ 2 3 c 3)) '(+ 8 c))
             (check-equal? (reorder-term '(+ c 2)) '(+ 2 c))
             (check-equal? (reorder-term '(+ 2 2 5 x)) '(+ 9 x))
             (check-equal? (reorder-term '(+ c c 8 c c 4 c 1 c c))
                           '(+ 13 c c c c c c c))
             (check-equal? (reorder-term '(* c c 8 c c 4 c 1 c c))
                           '(* 32 c c c c c c c)))
  (test-case "convert a multiplication to exponential notation"
             (check-equal? (mult->expt '()) 0)
             (check-equal? (mult->expt 5) 5)
             (check-equal? (mult->expt '(x)) '(1 (^ x 1)))
             (check-equal? (mult->expt '(3 x)) '(3 (^ x 1)))
             (check-equal? (mult->expt '(x x)) '(1 (^ x 2)))
             (check-equal? (mult->expt '(1 x x x)) '(1 (^ x 3)))
             (check-equal? (mult->expt '(6 x x x x x x)) '(6 (^ x 6))))
  (test-case "simplify-add"
             (check-equal? (simpl-add '()) '())
             (check-equal? (simpl-add 2) 2)
             (check-equal? (simpl-add 'x) 'x)
             (check-equal? (simpl-add '(+ 2 3 x)) '(+ 2 3 x))
             (check-equal? (simpl-add '(+ 0 x x)) '(+ x x)))
  (test-case "simplify-mul"
             (check-equal? (simpl-mul '()) '())
             (check-equal? (simpl-mul 2) 2)
             (check-equal? (simpl-mul 'x) 'x)
             (check-equal? (simpl-mul '(* 2 3 x)) '(* 2 3 x))
             (check-equal? (simpl-mul '(* 0 x x)) 0)
             (check-equal? (simpl-mul '(* 1 x x)) '(* x x))
             (check-equal? (simpl-mul '(* 1 x 0 x)) 0))
  )


(define-test-suite simpl-integration
  (test-case "integration tests for rec-with"
             (check-equal? (rec-with expt->mult '(+ (^ x 3) 2 (^ x 5)))
                           '(+ (* x x x) 2 (* x x x x x)))
             (check-equal? (rec-with reorder-term '(+ (* 1 x) 2)) '(+ (* 1 x) 2)))
  )


(run-tests aad-helpers)
(run-tests simplify-expression)
(run-tests simpl-helpers)
(run-tests simpl-integration)
