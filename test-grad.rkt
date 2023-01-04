#lang racket


;; Test: automatic differentiation for polynomial expressions

(define poly-1
  '(+ (* c1 (expt c2 3) t) (* c2 (expt t 2))))

#|
(define (grad ex v)
  (match ex
    [(? number? n) 0]
    [(? symbol? c) #:when (eq? c v) 1]
    [(? symbol? c) 0]
    [(+ ex0 ex1) (+ (grad ex0 v) (grad ex1 v))]
    ))
|#

(define-syntax grad
  (syntax-rules ()
    [(_ (? number? n) _) 0]
    [(_ (? symbol? c) v) #:when (eq? c v) 1]
    [(_ (? symbol? c) _) 0]
    [(_ (+ ex0 ex1) v) (+ (grad ex0 v) (grad ex1 v))]
    ))

(displayln (grad 2 'c))
(displayln (grad 'c 'c))
(displayln (grad '(+ c 2) 'c))