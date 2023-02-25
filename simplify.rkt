#lang racket

(provide simplify)
(require "simpl-bblocks.rkt")

;; simplify combines the building blocks defined in simpl-bblocks.rkt
;; to apply all the reordering/simplification utilities that we have
;; available to a given polynomial expression.
;; This works by first using expt->mult to bring all terms to the most
;; basic form that we can deal with, namely an s-expression containing
;; only + and * operations. Then we can apply the simplifcation/rewriting
;; building blocks. Finally we return to a human-readable form with
;; mult->expt.
(define (simplify expr)
  (let* ([mult-form (rec-with expt->mult expr)]
         [reordered-form (rec-with* reorder-sublists mult-form)]
         [simpl-ops (compose simpl-add simpl-mul reorder-term simpl-nested-op)])
    (rec-with* simpl-ops reordered-form)))

#|
(define s0 '(+
  (+ (* 0 x x x) (* 1 4 x x) (* 1 4 x x) (* 1 4 x x))
  (+
   (+ (* 0 x (^ x 6)) (* 1 2 (^ x 6)) (* (* 6 (^ x 5)) 2 x))
   (+ 0 0))))

(define s1 (rec-with expt->mult s0))
(define s2 (rec-with* reorder-sublists s1))
(define s3 (rec-with* reorder-term s2))
(define s4 (rec-with* simpl-mul s3))
(displayln s3)
(displayln "")
(displayln s4)
(define s5 (rec-with* simpl-add s4))
(displayln "")
(displayln s5)
(define s6 (rec-with* simpl-nested-op s5))
(displayln "")
(displayln s6)
|#