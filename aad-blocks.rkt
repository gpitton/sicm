#lang racket

(require (for-syntax syntax/parse
                     racket/syntax))

(provide grad reorder-term mult->expt)

;; Helper functions for the algorithmic differentiation of polynomial expressions


;(define-syntax ^
;  (lambda (stx)
;    (raise-syntax-error #f "unexpected use of ^" stx)))


(define (eq-sym? a b)
  (and (symbol? a)
       (symbol? b)
       (eq? a b)))


;; Gradient of the polynomial expression expr with respect to
;; the variable var.
(define (grad expr var)
  (cond
    [(number? expr) 0]
    [(symbol? expr)
     (if (eq-sym? expr var) 1 0)]
    [else
     (match expr
       ;; Terminate the recursion
       [(list ex) (grad ex var)]
       ;; Definitions for + - * ^.
       [(list '^ x 1)
        (grad x var)]
       [(list '^ x (? number? n))
        #:when (eq-sym? x var)
        `(* ,n (^ ,x ,(sub1 n)))]
       [(list-rest '+ ex0 exs)
        `(+ ,(grad ex0 var)
            ,(grad `(+ ,@exs) var))]
       ; - is misleading: better to use + negate x instead.
       ;[(list-rest - ex0 exs)
       ; `(- ,(grad ex0 var)
       ;     ,(grad `(- ,@exs var))]
       [(list-rest '* exs)
        `(+ ,@(map (lambda (ex)
                     (let ([rest (remove ex exs)])
                       `(* ,(grad ex var) ,rest)))
                   exs))])]))


;; helper to reorder the term symbols at compile-time.
;; expected behaviour:
;;    (reorder-term op (1 2 3) ()) -> (cons (op 1 2 3) '())
;;    (reorder-term op (1 2 x 3 x x) ()) -> (cons (op 1 2 3) (list x x x))
;; TODO this is not going to work well e.g. with (- 8 3 2) or (/ 8 3 2)
(define (reorder-term op t)
  (define (rt-aux t r)
    ;; t: input term
    ;; r: output term (recursively builds a reordered copy of t)
    (cond [(null? t) r]
          [(null? r) (rt-aux (cdr t) (list (car t)))]
          [else
           (match* (t r)
             ;; Recursive case, next element is a number, we can
             ;; apply op to update the head of r.
             [((list-rest (? number? arg0) args)
               (list-rest (? number? r0) rs))
              (rt-aux args (cons (op arg0 r0) rs))]
             ;; Recursive case, the next element is the first time we see a number.
             ;; Insert it at the head of the term.
             [((list-rest (? number? arg0) args) _)
              (rt-aux args (cons arg0 r))]
             ;; Recursive case, next element is a symbolic variable.
             ;; Append it to (car r).
             [((list-rest (? symbol? arg0) args)
               (list-rest r0 rs))
              (rt-aux args (cons r0 (cons arg0 rs)))])]))
  ;; Initialise the recursion.
  (rt-aux t '()))


(displayln "reorder-term -------------------------------------------------------")
(displayln (reorder-term + '(1 2 3 4)))
(displayln (reorder-term * '(1 2 3 4)))
(displayln (reorder-term + '(c)))
;(displayln (reorder-term ((2))))  ;; this should fail
(displayln (reorder-term + '(2 3 c 3)))
(displayln (reorder-term + '(c 2)))
(displayln (reorder-term + '(c c 8 c c 4 c 1 c c)))
(displayln (reorder-term * '(c c 8 c c 4 c 1 c c)))


;; simplifies a term in the form (4 x x x) -> (4 (^ x 3))
;; As it expands, mult->expt can have the following arguments:
;; () -> 0
;; n:number -> n
;; () n:number -> n
;; (s:symbol ...) -> (m->e (1 s ...))
;; (n:number s0 s1 ...) -> `(n (^ s0 ,(m->e (s1 ...) 1)))
;; (s0 s1 ...) n:number -> (m->e (s1 ...) (add1 n))
(define (mult->expt term)
  (define (me-aux t r)
    (match* (t r)
      ;; Exit condition: just append the exponent at the end
      ;; of the term.
      [('() _) r]
      ;; Leading coefficient is a number: start the recursion.
      [((list-rest (? number? t0) (? symbol? t1) ts) _)
       ;; TODO check that the symbols s0, s1, ... are equal?
       `(,t0 (^ ,t1 ,(me-aux ts (add1 r))))]
      ;; Recursive case.
      [((list-rest t0 ts) _)
       (me-aux ts (add1 r))]))
  ;; mult->expt implementation.
  (cond
    ;; Empty list: return 0.
    [(null? term) 0]
    ;; Constant.
    [(number? term) term]
    [(symbol? term) term]
    ;; No leading coefficient: set it to 1 and start the recursion.
    [(not (number? (car term)))
     (me-aux (cons 1 term) 0)]
    ;; Start the recursion.
    [else (me-aux term 0)]))


(displayln "mult->expt ---------------------------------------------------------")
(displayln (mult->expt '()))
(displayln (mult->expt 5))
(displayln (mult->expt '(x)))
(displayln (mult->expt '(3 x)))
(displayln (mult->expt '(x x)))
(displayln (mult->expt '(1 x x x)))
(displayln (mult->expt '(6 x x x x x x)))

#|(define-for-syntax (notlist? x) (not (list? x)))

;; Applies a macro f to each node in a (possibly nested) S-expression
;; (interpreted as a tree here). This is a useful tool to simplify a
;; polynomial expression by applying a term-reordering or a simplifying
;; macro to each sub-expression.
(define-syntax (map-tree stx)
  (syntax-case stx ()
    ;; Empty tree. End of recursion.
    [(_ f ()) #''()]
    ;; List with a single element. Nothing to do.
    [(_ f (s))
     (not (list? (syntax->datum #'s)))
     #''(s)]
    ;; No element of ex is a list. We can just apply f.
    [(_ f ex)
     (andmap notlist? (syntax->datum #'ex))
     #'(f ex)]
    ;; (operand list ...) -> (op (recur list) (recur ...))
    [(_ f (op ex0 ex1 ...))
     (and (member (syntax->datum #'op) '(+ - * ^))
          (list? (syntax->datum #'ex0)))
     #'`(op ,(map-tree f ex0) ,@(map-tree f (ex1 ...)))]
    ;; (operand symbol ...) -> (op symbol (recur ...))
    [(_ f (op ex0 ex1 ...))
     (member (syntax->datum #'op) '(+ - * ^))
     #'`(op ex0 ,@(map-tree f (ex1 ...)))]
    ;; Tree that does not start with an operator. This is the recursive
    ;; case started by one of the two steps above.
    ;; (list ...) -> ((recur list) (recur ...))
    [(_ f (ex0 ex1 ...))
     (list? (syntax->datum #'ex0))
     #'`(,(map-tree f ex0) ,@(map-tree f (ex1 ...)))]
    ;; Tree that does not start with an operator. This is also a recursive
    ;; step, like the one above.
    ;; (symbol ...) -> (symbol (recur ...))
    [(_ f (ex0 ex1 ...))
     #'`(ex0 ,@(map-tree f (ex1 ...)))]
    [_ #'"unexpected syntax"]))

(displayln "map-tree -----------------------------------------------------------")
(displayln (map-tree reorder-term (2)))
(displayln (map-tree reorder-term (x)))
(displayln (map-tree reorder-term (+ 'x 2)))
(displayln (map-tree reorder-term (+ 2 'c 3)))
(displayln (map-tree reorder-term (+ 6 (* 5 4) (+ 2 3) 1)))
(displayln (map-tree reorder-term (+ (+ 2 3) (+ 3 'x 'x 6))))
;; 4x^3 + 2x*x^6 - 2
;(displayln (map-tree reorder-term (+ (* x 4 x x) (* 2 x (^ x 6)) -2)))
;(displayln (map-tree reorder-term (+ (* x x) (+ 2 x (+ 3 (* 4 x x 2 x))))))


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
|#