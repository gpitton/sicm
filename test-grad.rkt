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
    (raise-syntax-error #f "^ used as part of a parse-a form" stx)))

(define-for-syntax (eq-sym? a b)
  (let ([sa (syntax-e a)]
        [sb (syntax-e b)])
    (and (symbol? sa)
         (symbol? sb)
         (eq? sa sb))))

;; TODO simplify expressions like (* ... 0 ...) -> #'0
;; TODO support long products: (grad (* 5 x (expt x 2) (+ x x)) x)
(define-syntax (grad stx)
  (syntax-parse stx
    #:literals (+ - * ^)
    [(_ n:number _) #'0]
    [(_ x:id v:id)
     #:when (eq-sym? #'x #'v)
     #'1]
    [(_ x:id v:id) #'0]
    [(_ '(^ x 1) v) #'(grad x v)]
    [(_ '(^ x:id n:number) v:id)
     #:when (eq-sym? #'x #'v)
     #'`(* n (^ x ,(sub1 n)))]
    [(_ '(+ ex0 ex1 ...) v) #'`(+ ,(grad ex0 v) ,(grad ex1 v) ...)]
    [(_ '(- ex0 ex1 ...) v) #'`(- ,(grad ex0 v) ,(grad ex1 v) ...)]
    [(_ '(* ex0 ex1) v) #'`(+ (* ,(grad ex0 v) ex1) (* ex0 ,(grad ex1 v)))]
    ;[(_ (* ex0 ex1 ...) v) #'(+ (* (grad ex0 v) ex1 ...) (* (grad ex1 v) ex0 ...) ...)]
    [_ #'"not matched"]  ;; later just return 0
    ))

(displayln (grad 2 c))
(displayln (grad x c))
(displayln (grad c c))
(displayln (grad '(+ c 2) c))
(displayln (grad '(+ c c) c))
(displayln (grad '(+ c c c c c 5) c))
(displayln (grad '(- '(+ c c c) c 2) c))
(displayln (grad '(^ c 1) c))
(displayln (grad '(^ c 5) c))
(displayln (grad '(+ '(* 5 x x) '(* 2 x) -2)))
(displayln (grad '(+ '(* 5 '(^ x 2)) '(* 2 x) -2) x))


;; TODO we need a normal-form macro to rewrite a monomial
;;      in a form like: (* x y x) -> (* (expt x 2) y)