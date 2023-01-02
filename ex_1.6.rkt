#lang racket
;; SICM exercise 1.6

(require racket/vector)
;(require racket/flonum)
;(require math/matrix)


;(define (make-grid a b n)
;  (let ([h (fl/ (fl- b a) (->fl n))])
;    (for/flvector ([i (in-range n)])
;      (fl+ a (fl* h (->fl i))))))

(define (make-grid a b n)
  (let ([h (/ (- b a) n)])
    (for/vector ([i (in-range n)])
      (+ a (* h i)))))

;; approximates \int_x0 ^x1 f with the trapezoid rule
(define (int f x0 x1 #:n-points [n 128])
  (let* ([xs (make-grid x0 x1 n)]
         [h (- (vector-ref xs 1) (vector-ref xs 0))]
         [fs (vector-map f xs)]
         [fs-head (in-vector fs)]
         [fs-tail (in-vector fs 1)]
         [int-helper (lambda (x0 x1) (* h (/ (+ x0 x1) 2)))])
    (for/sum ([x0 fs-head]
              [x1 fs-tail])
      (int-helper x0 x1))))


;; A path in the form: c0 + c1 t + c2 t^2.
;; This is implemented as a function that accepts a message
;; that can be 'eval or 'D to evaluate the polynomial or
;; its first derivative respectively.
;; TODO re-implement this to support a polynomial of arbitrary-order,
;;      and more complex expressions/basis functions.
;; TODO support anti-derivatives
(define (poly-path cs)
  (lambda (tag)
    (case tag  
      ['eval ;; evaluate
       (lambda (t)
         (letrec ([eval-aux
                   ;; TODO make it tail-recursive
                   (lambda (cs)
                     (if (null? cs) 0
                         (+ (car cs) (* t (eval-aux (cdr cs))))))])
           (eval-aux cs)))]
      ['D ;; first t-derivative
       (let ([ds (for/list ([i  (in-range (length cs))]
                            [ci (in-list cs)])
                   (* i ci))])
         (poly-path (cdr ds)))]
      ['grad ;; gradient with respect to the coefficients
       (lambda (index)  ;; which coefficient
         (lambda (t)
           (* (list-ref cs index) (expt t index))))]
      ['sqr  ;; square (Cauchy product)
       (let ([ds (for/list ([i (in-range (length cs))])
                   (for/sum ([j (in-range i)])
                     (* (list-ref cs j) (list-ref cs (- i j)))))])
         (poly-path ds))])))

 

;; Lagrangian for a free particle
;; TODO clunky notation: think of something more readable
(define (L x xp)
  (lambda (t) (* 1/2 (((xp 'sqr) 'eval) t))))

;; Action
(define (S L path x0 x1 #:n-points [n 100])
  (int (L (path 'eval) (path 'D)) x0 x1 #:n-points n))


;(display (make-grid 0.0 1.0 10.0))
;(int sqr 0.0 1.0 #:n-points 100000)  ;; expected: 1/3
(S L (poly-path '(1 2 1)) 0.0 1.0 #:n-points 1000)

;; gradient of the path at the point t = 0.4
(for/list ([i '(0 1 2)]) ((((poly-path '(1 2 1)) 'grad) i) 0.4))
