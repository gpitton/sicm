#lang racket

(require (for-syntax syntax/parse))

;; Test: automatic differentiation for polynomial expressions

;(define poly-1
;  '(+ (* c1 (expt c2 3) t) (* c2 (expt t 2))))

#|
(define (grad ex v)
  (match ex
    [(? number? n) 0]
    [(? symbol? c) #:when (eq? c v) 1]
    [(? symbol? c) 0]
    [(+ ex0 ex1) (+ (grad ex0 v) (grad ex1 v))]
    ))
|#


(define-syntax ^
  (lambda (stx)
    (raise-syntax-error #f "unexpected use of ^" stx)))

(define-for-syntax (eq-sym? a b)
  (let ([sa (syntax-e a)]
        [sb (syntax-e b)])
    (and (symbol? sa)
         (symbol? sb)
         (eq? sa sb))))


(define-syntax (grad stx)
  (syntax-parse stx
    #:literals (+ - * ^)
    [(_ n:number _) #'0]
    [(_ x:id v:id)
     #:when (eq-sym? #'x #'v)
     #'1]
    [(_ x:id v:id) #'0]
    [(_ (^ x 1) v:id) #'(grad x v)]
    [(_ (^ x:id n:number) v:id)
     #:when (eq-sym? #'x #'v)
     #'`(* n (^ x ,(sub1 n)))]
    [(_ (+ ex0 ex1 ...) v:id)
     #'`(+ ,(grad ex0 v)
           ,(grad ex1 v)
           ...)]
    [(_ (- ex0 ex1 ...) v:id)
     #'`(- ,(grad ex0 v)
           ,(grad ex1 v)
           ...)]
    [(_ (* ex0 ex1) v:id)
     #'`(+ (* ,(grad ex0 v) ex1)
           (* ex0 ,(grad ex1 v)))]
    [(_ (* ex0 ex1 ...) v:id)
     #'`(+ (* ,(grad ex0 v) ex1 ...)
           (* ex0 ,(grad (* ex1 ...) v)))]
    [_ #'"not matched"]  ;; later just return 0
    ))


;; Tests for the grad macro
(displayln (grad 2 c))
(displayln (grad x c))
(displayln (grad c c))
(displayln (grad (+ c 2) c))
(displayln (grad (+ c c) c))
(displayln (grad (+ c c c c c 5) c))
(displayln (grad (- (+ c c c) c 2) c))
(displayln (grad (^ c 1) c))
(displayln (grad (^ c 5) c))
(displayln (grad (+ (* 4 x x x) (* 2 x (^ x 6)) -2) x))
(displayln (grad (+ (* 5 b (^ x 2)) (* 2 c x) -2) x))


;; helper to reorder the term symbols.
(define-for-syntax (reorder-term op args)
  ;; t is the simplified term
  (define (r-aux args t)
    (cond [(null? args) '()]
          [(null? t) (r-aux (cdr args) (list (car args)))]
          [(number? (car args))
           (r-aux (cdr args)
                  (cons (op (car args) (car t))
                        (cdr t)))]
          [else (r-aux (cdr args)
                       (append t (list (car args))))]))
    (r-aux args '()))
    

;; simplifies a term, where a term is an S-expression in
;; the form: (op arg0 . args) -> (op natural? symbol)
(define-syntax simpl-term
  (syntax-rules (+)
    [(_ (+ . args)) `(+ ,(reorder-term + args))]))

(displayln (simpl-term (+ 0 x x 2 x 3)))

;; TODO simplify expressions like (* ... 0 ...) -> #'0
(define-syntax simplify
  (syntax-rules ()
    [(_ ex) ex]))


(define-syntax D
  (syntax-rules ()
    [(_ ex var)
     (simplify (grad ex var))]))


;; TODO we need a normal-form macro to rewrite a monomial
;;      in a form like: (* x y x) -> (* (expt x 2) y)