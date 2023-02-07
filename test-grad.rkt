;; Unit tests for the AAD helper functions
#lang racket

(require "aad-blocks.rkt")
(require rackunit rackunit/text-ui)

;; Required to use eval (racket-only).
(define teval    ;; stands for test-eval
  (let ([ns (make-base-namespace)])
    (lambda (expr) (eval expr ns))))

(define-test-suite aad-helpers
  (test-case "grad"
             (check-equal? (grad 2 'c) 0)
             (check-equal? (grad 'x 'c) 0)
             (check-equal? (grad 'c 'c) 1)
             (let ([res (grad '(+ c 2) 'c)])
               (check-equal? (teval res) 1))
             (let ([res (grad '(+ c c) 'c)])
               (check-equal? (teval res) 2))
             (let ([res (grad '(+ c c c c c 5) 'c)])
               (check-equal? (teval res) 5))
             ;; Test the full output without evaluating.
             (check-equal? (grad '(* c c c c c 5) 'c)
                           '(+ (* 1 (c c c c 5)) (* 1 (c c c c 5))
                               (* 1 (c c c c 5)) (* 1 (c c c c 5))
                               (* 1 (c c c c 5)) (* 0 (c c c c c))))
             (let ([res (grad '(+ (+ c c c) c 2) 'c)])
               (check-equal? (teval res) 4))
             (let ([res (grad '(^ c 1) 'c)])
               (check-equal? (teval res) 1))
             (check-equal? (grad '(^ c 5) 'c) '(* 5 (^ c 4)))
             (check-equal? (grad '(+ (* 4 x x x) (* 2 x (^ x 6)) -2) 'x)
                           '(+ (+ (* 0 (x x x)) (* 1 (4 x x))
                                  (* 1 (4 x x)) (* 1 (4 x x)))
                               (+ (+ (* 0 (x (^ x 6))) (* 1 (2 (^ x 6)))
                                     (* (* 6 (^ x 5)) (2 x))) (+ 0 0))))
             (check-equal? (grad '(+ (* 5 b (^ x 2)) (* 2 c x) -2) 'x)
                           '(+ (+ (* 0 (b (^ x 2))) (* 0 (5 (^ x 2)))
                                  (* (* 2 (^ x 1)) (5 b)))
                               (+ (+ (* 0 (c x)) (* 0 (2 x)) (* 1 (2 c))) (+ 0 0)))))
  (test-case "reorder-term"
             (check-equal? (reorder-term + '(1 2 3 4)) '(10))
             (check-equal? (reorder-term * '(1 2 3 4)) '(24))
             (check-equal? (reorder-term + '(c)) '(c))
             (check-equal? (reorder-term + '((2))) '((2)))  ;; TODO check this
             (check-equal? (reorder-term + '(2 3 c 3)) '(8 c))
             (check-equal? (reorder-term + '(c 2)) '(2 c))
             (check-equal? (reorder-term + '(c c 8 c c 4 c 1 c c))
                           '(13 c c c c c c c))
             (check-equal? (reorder-term * '(c c 8 c c 4 c 1 c c))
                           '(32 c c c c c c c)))
  (test-case "mult->expt"
             (check-equal? (mult->expt '()) 0)
             (check-equal? (mult->expt 5) 5)
             (check-equal? (mult->expt '(x)) '(1 (^ x 1)))
             (check-equal? (mult->expt '(3 x)) '(3 (^ x 1)))
             (check-equal? (mult->expt '(x x)) '(1 (^ x 2)))
             (check-equal? (mult->expt '(1 x x x)) '(1 (^ x 3)))
             (check-equal? (mult->expt '(6 x x x x x x)) '(6 (^ x 6))))
  (test-case "simpl-zmul"
             (check-equal? (simpl-zmul '()) '())
             (check-equal? (simpl-zmul '(+ 2 3)) '(+ 2 3))
             (check-equal? (simpl-zmul '(* 1 2)) '(* 1 2))
             (check-equal? (simpl-zmul '(* 1 2 3 4 0)) 0)
             (check-equal? (simpl-zmul '(+ (* 2 3 (+ 1 1)) (* 4 (* 1 0) 5)))
                           '(+ (* 2 3 (+ 1 1)) 0))
             (check-equal? (simpl-zmul '(+ (* 2 4 (^ x 4) (* (+ 1 2) 0)) 2))
                           '(+ 0 2))
             (check-equal? (simpl-zmul '(+ (+ 2 3 (+ 1 0)) (* 2 (+ 1 1 0))))
                                       '(+ (+ 2 3 (+ 1 0)) (* 2 (+ 1 1 0)))))
  )

(run-tests aad-helpers)
