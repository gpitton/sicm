#lang racket
;; Some very simple functions of general utility.

(provide (all-defined-out))

(define (num-zero? x) (and (number? x) (zero? x)))

(define (not-list? x) (not (list? x)))
(define (one? x) (eq? x 1))
(define (num-one? x) (and (number? x) (one? x)))
(define (not-zero? x) (or (not (number? x))
                          (not (zero? x))))
(define (not-one? x) (or (not (number? x))
                         (not (one? x))))

;; Required to use eval (racket-only).
(define l-eval    ;; stands for local-eval
  (let ([ns (make-base-namespace)])
    (lambda (expr) (eval expr ns))))


(define (eq-sym? a b)
  (and (symbol? a)
       (symbol? b)
       (eq? a b)))
