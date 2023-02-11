#lang racket

(provide grad reorder-term mult->expt simpl-zmul simpl-1mul)

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


;; mult->expt rewrites a term in the form (4 x x x) to (4 (^ x 3)).
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


(define (num-zero? x) (and (number? x) (zero? x)))

;; simpl-zmul simplifies an expression that has a multiplication
;; by zero. Example: (+ 2 (* 3 (^ x 2) 0)) -> (+ 2 0)
(define (simpl-zmul expr)
  ;; The helper function accepts an expression, a boolean that is
  ;; true if we are in the context of a multiplication, and returns
  ;; a lambda accepting a continuation.
  (define (sz-aux ex is-in-mult)
    (lambda (k)
      (cond [(null? ex) '()]
            [(number? ex) ex]
            [(symbol? ex) ex]
            ;; expr is a multiplication, and one of the sub-expressions
            ;; is zero. The expression will evaluate to with zero.
            [(and (eq? (car ex) '*)
                  (ormap num-zero? (cdr ex)))
             (if is-in-mult
                 (k 0)  ;; propagate up to the multiplication context
                 ;; that we found a zero.
                 0)]    ;; else just return zero and keep recurring.
            ;; expr is a multiplication, and we need to keep recurring.
            [(eq? (car ex) '*)
             (call/cc
              (lambda (k2)
                (let ([res
                       (map (lambda (e) ((sz-aux e #t) k2))
                            (cdr ex))])
                  `(* ,@res))))]
            ;; Regardless of whether expr is a multiplication, we need
            ;; to recur.
            [else
             (let ([res
                    (map (lambda (e) ((sz-aux e is-in-mult) k))
                         (cdr ex))])
               `(,(car ex) ,@res))])))
  ;; Launch the helper function.
  (call/cc (sz-aux expr #f)))


(define (not-list? x) (not (list? x)))
(define (one? x) (eq? x 1))
(define (not-one? x) (or (not (number? x))
                         (not (one? x))))
;; simpl-1mul simplifies an expression by removing any factors
;; equal to 1 in a multiplication expression.
(define (simpl-1mul expr)
  (cond [(null? expr) '()]
        [(not-list? expr) expr]
        [(eq? (car expr) '*)
         (let ([s-expr
                ;; recursively apply simpl-1mul...
                (map simpl-1mul (cdr expr))])
           ;; ... then keep only the sub-expressions that did
           ;; not evaluate to one.
           `(* ,@(filter not-one? s-expr)))]
        [else
         ;; recursively apply to each term of the expression.
         (map simpl-1mul expr)]))


;; TODO we need a normal-form macro to rewrite a monomial
;;      in a form like: (* x y x) -> (* (expt x 2) y)
