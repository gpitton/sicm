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
    [_ #'"not matched"]))  ;; later just return 0


;; Tests for the grad macro
(displayln "grad ---------------------------------------------------------------")
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
  (syntax-parse stx
    ;; args is empty: recursion has ended. Just return the reodered term.
    [(_ (op) (term:number)) #'term]  ;; number specialisation.
    [(_ (op) (term)) #''term]        ;; single variable specialisation.
    [(_ (op) (term ...))             ;; general case.
     #''(op term ...)]
    ;; Term with a single element: just return the element.
    [(_ (n:number)) #'n]             ;; with brackets
    [(_ n:number) #'n]               ;; without brackets
    [(_ (s:id)) #''s]                ;; quote the symbol
    [(_ s:id) #''s]                  ;; without brackets
    ;; Proper term: initialise the recursion.
    [(_ (op arg0 arg1 ...))
     #'(reorder-term (op arg1 ...) (arg0))]
    ;; Recursive case, next element is a number, we can
    ;; perform op to update the head of term.
    [(_ (op arg0:number arg1 ...) (term0:number term1 ...))
     (let ([op-d (syntax->datum #'op)]
           [arg0-d (syntax->datum #'arg0)]
           [term0-d (syntax->datum #'term0)])
       (with-syntax ([hd (eval `(,op-d ,arg0-d ,term0-d))])
         #'(reorder-term (op arg1 ...) (hd term1 ...))))]
    ;; Recursive case, the next element is the first time we see a number.
    ;; Insert it at the head of the term.
    [(_ (op arg0:number arg1 ...) (t0 t1 ...))
     #'(reorder-term (op arg1 ...) (arg0 t0 t1 ...))]
    ;; Recursive case, next element is a symbolic variable.
    ;; Append it at the end of terms.
    [(_ (op arg0 arg1 ...) (t0 t1 ...))
     #'(reorder-term (op arg1 ...) (t0 t1 ... arg0))]))


(displayln "reorder-term -------------------------------------------------------")
(displayln (reorder-term (+ 1 2 3 4)))
(displayln (reorder-term (* 1 2 3 4)))
(displayln (reorder-term (c)))
;(displayln (reorder-term ((2))))  ;; this should fail
(displayln (reorder-term (+ 2 3 c 3)))
(displayln (reorder-term (+ c 2)))
(displayln (reorder-term (+ c c 8 c c 4 c 1 c c)))
(displayln (reorder-term (* c c 8 c c 4 c 1 c c)))

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
         ([n1 (add1 (syntax->datum #'n))])
       #'(mult->expt (s1 ...) n1))]
    [_ #'"unexpected syntax"]))


(displayln "mult->expt ---------------------------------------------------------")
(displayln (mult->expt ()))
(displayln (mult->expt 5))
(displayln (mult->expt (x)))
(displayln (mult->expt (3 x)))
(displayln (mult->expt (x x)))
(displayln (mult->expt (1 x x x)))
(displayln (mult->expt (6 x x x x x x)))

(define-for-syntax (notlist? x) (not (list? x)))

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
(displayln (map-tree reorder-term (+ x 2)))
(displayln (map-tree reorder-term (+ 2 c 3)))
(displayln (map-tree reorder-term (+ 6 (* 5 4) (+ 2 3) 1)))
(displayln (map-tree reorder-term (+ (+ 2 3) (+ 3 x x 6))))
;; 4x^3 + 2x*x^6 - 2
(displayln (map-tree reorder-term (+ (* x 4 x x) (* 2 x (^ x 6)) -2)))
(displayln (map-tree reorder-term (+ (* x x) (+ 2 x (+ 3 (* 4 x x 2 x))))))


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