This repository collects my exercises from the book _Structure and Interpretation of Classical Mechanics_ by Gerald Jay Sussman and Jack Wisdom with Meinhard E. Mayer.

## References
- [Online edition](https://tgvaughan.github.io/sicm/)
- [Wikipedia entry](https://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Classical_Mechanics)

### To do
- We need a reduction rule to merge the monomials with the same power: `(+ (* x x) (* 2 x x) (* 3 x x x)) -> (+ (* 3 x x) (* 3 x x x))`.
- The rewriting function is complete only when the final s-expression has the form: `(+ (* num (^ var num)) ...)`.
- It could be useful to add contracts to each term-rewriting function. Most
contracts would be in the form: the output of this function cannot have a
subexpression such that ...

- It would be particularly useful to put a contract that checks the signatures of the functions passed to `rec-with` and `rec-with*`.

### Lessons learned
- use pattern-matching and ellipses (`...`) instead of doing
  `datum->syntax` . `car` or `cdr` . `syntax->datum` all the time.
- use splicing (`,@`) when consuming a list with a macro:
```scheme
;; This one usually gives the wrong results:
(macro (ex0 ex1 ...)) -> (ex0 (macro ex1 ...))
;; too many parenthesis. Instead, do:
(macro (ex0 ex1 ...)) -> `(ex0 ,@(macro ex1 ..))
```
