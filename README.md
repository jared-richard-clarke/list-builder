# list-builder

- **Description**: Library provides `for-list` — a simplified list comprehension, pulled from Haskell, 
  implemented in Scheme.
- **Syntax**: `(for-list expression [(x <- mx) ...] predicate?)`

## Example

```scheme
(import (rnrs)
        (list-builder))
        
(define py-triple
  (lambda (n)
    (for-list (list x y z)
              [(x <- (range 1 n))
               (y <- (range x n))
               (z <- (range y n))]
              (= (+ (sqr x) (sqr y))
                 (sqr z)))))
;; - expands ->
(define py-triple
  (lambda (n)
    (bind (range 1 n)
          (lambda (x)
            (bind (range x n)
                  (lambda (y)
                    (bind (range y n)
                          (lambda (z)
                            (if (= (+ (sqr x) (sqr y))
                                   (sqr z))
                                (return (list x y z))
                                empty)))))))))
;; - so that ->
(py-triple 21)
;; - evaluates ->
'((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20))
```
