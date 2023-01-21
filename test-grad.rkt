#lang racket

(require (for-syntax syntax/parse
                     racket/syntax))

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


;; helper to reorder the term symbols at compile-time.
;; expected behaviour:
;;    (reorder-term op (1 2 3) ()) -> (cons (op 1 2 3) '())
;;    (reorder-term op (1 2 x 3 x x) ()) -> (cons (op 1 2 3) (list x x x))
(define-syntax (reorder-term stx)
  (syntax-case stx ()
    ;; args is empty: just return t
    [(_ op args t)
     (null? (syntax->datum #'args))
     (let ([term (syntax->datum #'t)])
       (if (eq? (length term) 1)
           #'(car 't)
           #''t))]
    ;; t is empty: initialise t
    [(_ op args t)
     (null? (syntax->datum #'t))
     (let ([term (syntax->datum #'args)])
       (with-syntax ([args-tail
                      (datum->syntax #'args (cdr term))]
                     [args-head
                      (datum->syntax #'args (list (car term)))])
         #'(reorder-term op args-tail args-head)))]
    ;; recursive case, next element is a number
    [(_ op args t)
     (and (number? (car (syntax->datum #'args)))
          (number? (car (syntax->datum #'t))))
     (let* ([op-d (syntax->datum #'op)]
            [args-d (syntax->datum #'args)]
            [term (syntax->datum #'t)])
       (with-syntax ([args-tail
                      (datum->syntax #'args
                                     (cdr args-d))]
                     [args-add
                      (datum->syntax #'args
                                     (cons (eval `(,op-d ,(car args-d)
                                                         ,(car term)))
                                           (cdr term)))])
         #'(reorder-term op
                         args-tail
                         args-add)))]
    ;; recursive case, we need to add a number at the head of the term
    [(_ op args term)
     (number? (car (syntax->datum #'args)))
     (let ([args-d (syntax->datum #'args)]
           [term-d (syntax->datum #'term)])
       (with-syntax ([args-tail
                      (datum->syntax #'args
                                     (cdr args-d))]
                     [num-term
                      (datum->syntax #'args
                                     (cons (car args-d)
                                           term-d))])
         #'(reorder-term op
                         args-tail
                         num-term)))]
    ;; recursive case, next element is a symbolic variable
    [(_ op args t)
     (let ([term (syntax->datum #'args)]
           ;; reordered term
           [r-term (syntax->datum #'t)])
       (with-syntax ([args-tail
                      (datum->syntax #'args (cdr term))]
                     [t-next
                      (datum->syntax #'t (append r-term
                                                 `(,(car term))))])
         #'(reorder-term op
                         args-tail
                         t-next)))]))




;; simplifies a term, where a term is an S-expression in
;; the form: (op arg0 . args) -> (op natural? symbol)
;(define-syntax simpl-term
;  (syntax-rules (+)
;    [(_ (+ . args)) `(+ ,@(reorder-term + args '()))]))

(displayln (reorder-term + (1 2 3 4) ()))
(displayln (reorder-term * (1 2 3 4) ()))
(displayln (reorder-term + (2 3 c 3) ()))
(displayln (reorder-term + (c 2) ()))
(displayln (reorder-term + (c 8 c c 4 c 1 c c) ()))

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