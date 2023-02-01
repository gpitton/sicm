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
  (let ([sa (syntax->datum a)]
        [sb (syntax->datum b)])
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
;; TODO this is not going to work well e.g. with (- 8 3 2) or (/ 8 3 2)
(define-syntax (reorder-term stx)
  (syntax-case stx ()
    ;; args is empty: just return t
    [(_ op () term)
     (let ([term-d (syntax->datum #'term)])
       (if (eq? (length term-d) 1)
           #'(car 'term)
           #''term))]
    ;; t is empty: initialise t
    [(_ op args ())
     (let ([args-d (syntax->datum #'args)])
       (with-syntax
           ([args-tail
             (datum->syntax #'args (cdr args-d))]
            [args-head
             (datum->syntax #'args (list (car args-d)))])
         #'(reorder-term op args-tail args-head)))]
    ;; recursive case, next element is a number
    [(_ op args term)
     (and (number? (car (syntax->datum #'args)))
          (number? (car (syntax->datum #'term))))
     (let* ([op-d (syntax->datum #'op)]
            [args-d (syntax->datum #'args)]
            [term-d (syntax->datum #'term)])
       (with-syntax
           ([args-tail
             (datum->syntax #'args
                            (cdr args-d))]
            [args-add
             (datum->syntax #'args
                            (cons (eval `(,op-d ,(car args-d)
                                                ,(car term-d)))
                                  (cdr term-d)))])
         #'(reorder-term op
                         args-tail
                         args-add)))]
    ;; recursive case, we need to add a number at the head of the term
    [(_ op args term)
     (number? (car (syntax->datum #'args)))
     (let ([args-d (syntax->datum #'args)]
           [term-d (syntax->datum #'term)])
       (with-syntax
           ([args-tail
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
    [(_ op args term)
     (let ([args-d (syntax->datum #'args)]
           [term-d (syntax->datum #'term)])
       (with-syntax
           ([args-tail
             (datum->syntax #'args (cdr args-d))]
            [next-term
             (datum->syntax #'term (append term-d
                                           `(,(car args-d))))])
         #'(reorder-term op
                         args-tail
                         next-term)))]))

;; simplifies a term, where a term is an S-expression in
;; the form: (op arg0 . args) -> (op natural? symbol)
;(define-syntax simpl-term
;  (syntax-rules (+)
;    [(_ (+ . args)) `(+ ,@(reorder-term + args '()))]))

(displayln (reorder-term + (1 2 3 4) ()))
(displayln (reorder-term * (1 2 3 4) ()))
(displayln (reorder-term + (c) ()))
(displayln (reorder-term + (2 3 c 3) ()))
(displayln (reorder-term + (c 2) ()))
(displayln (reorder-term + (c 8 c c 4 c 1 c c) ()))
(displayln (reorder-term * (c 8 c c 4 c 1 c c) ()))


;; simplifies a term in the form (4 x x x) -> (4 (^ x 3))
;; As it expands, mult->expt can have the following arguments:
;; () -> 0
;; n:number -> n
;; () n:number -> n
;; (s:symbol ...) -> (m->e (1 s ...))
;; (n:number s0 s1 ...) -> `(n (^ s0 ,(m->e (s1 ...) 1)))
;; (s0 s1 ...) n:number -> (m->e (s1 ...) (add1 n))
(define-syntax (mult->expt stx)
  (syntax-parse stx
    ;; Empty list: return 0.
    [(_ ()) #'0]
    ;; Constant.
    [(_ n:number) #'n]
    ;; Exit condition: just append the exponent at the end
    ;; of the term.
    [(_ () n:number) #'n]
    ;; Leading coefficient is a number: start the recursion.
    [(_ (n:number s0 s1 ...))
     ;; TODO check that the symbols s0, s1, ... are equal?
     #'`(n (^ s0 ,(mult->expt (s1 ...) 1)))]
    ;; No leading coefficient: set it to 1 and retry.
    [(_ (s0 s1 ...))
     #:when (not (number? (syntax->datum #'s0)))
     #'(mult->expt (1 s0 s1 ...))]
    ;; Recursive case.
    [(_ (s0 s1 ...) n:number)
     (with-syntax
         ([n1 (datum->syntax #'n (add1 (syntax->datum #'n)))])
       #'(mult->expt (s1 ...) n1))]
    [_ #'"unexpected syntax"]))


(displayln (mult->expt ()))
(displayln (mult->expt 5))
(displayln (mult->expt (x)))
(displayln (mult->expt (3 x)))
(displayln (mult->expt (x x)))
(displayln (mult->expt (1 x x x)))
(displayln (mult->expt (6 x x x x x x)))

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