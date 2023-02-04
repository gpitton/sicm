This repository collects my exercises from the book _Structure and Interpretation of Classical Mechanics_ by Gerald Jay Sussman and Jack Wisdom with Meinhard E. Mayer.

## References
- [Online edition](https://tgvaughan.github.io/sicm/)
- [Wikipedia entry](https://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Classical_Mechanics)

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
