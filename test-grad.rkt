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
                               (+ (+ (* 0 (c x)) (* 0 (2 x)) (* 1 (2 c))) (+ 0 0))))
             ))

(run-tests aad-helpers)
