# `list-builder.scm`

- **Description**: Library provides `for-list` — a simplified list comprehension I copied from Haskell
  and implemented in Scheme.
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

## List Comprehensions

List comprehensions are syntactic sugar for processing lists in a monadic context.
The monadic context for lists is non-determinism.

## Haskell

```haskell
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  fail _   = []

[(x, y) | x <- ['a', 'b'], y <- [1, 2]]

-- equivalent ->

do x <- ['a', 'b']
   y <- [1, 2]
   return (x, y)
   
-- equivalent ->

['a', 'b'] >>= \x ->
[1, 2]     >>= \y ->
return (x, y)

-- evaluates ->

[('a', 1), ('a', 2), ('b', 1), ('b', 2)]
```

## Process

> "When you have non-deterministic values interacting, you can view their computation as 
>  a tree where every possible result in a list represents a separate branch."
>
> — **Learn You A Haskell For Great Good** by Miran Lipovača

```text
                 ['a', 'b']
                 /        \
       [1, 2]                 [1, 2]
       /    \                 /    \
['a', 1]    ['a', 2]   ['b', 1]    ['b', 2]
```
