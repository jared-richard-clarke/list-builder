# `list-builder.scm`

- **Description**: Library provides `for-list` — a simplified list comprehension I copied from Haskell
  and implemented as a Scheme macro.
- **Syntax**: `(for-list expression ([x <- mx] ...) predicate?)`

## Example

```scheme
(import (rnrs)
        (list-builder))
        
(define py-triple
  (lambda (n)
    (for-list (list x y z)
              ([x <- (range 1 n)]
               [y <- (range x n)]
               [z <- (range y n)])
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

The List monad embodies the strategy of combining a chain of non-deterministic computations 
by applying the operations to all possible values at each step. A list comprehension
is syntactic sugar over a list in a monadic context.

> One use of functions which return lists is to represent ambiguous computations — that is computations 
> which may have 0, 1, or more allowed outcomes. In a computation composed from ambigous subcomputations, 
> the ambiguity may compound, or it may eventually resolve into a single allowed outcome or no allowed 
> outcome at all. During this process, the set of possible computational states is represented as a list. 
> The List monad thus embodies a strategy for performing simultaneous computations along all allowed 
> paths of an ambiguous computation. 
>
> — [All About Monads - Haskell Wiki](https://wiki.haskell.org/All_About_Monads)

## Haskell

```haskell
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  fail _   = []

pyTriple n =
  [ (x, y, z)
    | x <- [1 .. n],
      y <- [x .. n],
      z <- [y .. n],
      x ^ 2 + y ^ 2 == z ^ 2
  ]

-- equivalent ->

pyTriple n = do
  x <- [1 .. n]
  y <- [x .. n]
  z <- [y .. n]
  if x ^ 2 + y ^ 2 == z ^ 2
    then [(x, y, z)]
    else []
   
-- equivalent ->

pyTriple n =
  [1 .. n] >>= \x ->
    [x .. n] >>= \y ->
      [y .. n] >>= \z ->
        ( if x ^ 2 + y ^ 2 == z ^ 2
            then [(x, y, z)]
            else []
        )
             
-- equivalent ->

pyTriple n =
  concatMap
    ( \x ->
        concatMap
          ( \y ->
              concatMap
                ( \z ->
                    if x ^ 2 + y ^ 2 == z ^ 2
                      then [(x, y, z)]
                      else []
                )
                [y .. n]
          )
          [x .. n]
    )
    [1 .. n]

-- so that ->

pyTriple 21

-- evaluates ->

[(3,4,5), (5,12,13), (6,8,10), (7,24,25), (8,15,17), (9,12,15), (12,16,20)]
```

## Process

> "When you have non-deterministic values interacting, you can view their computation as 
>  a tree where every possible result in a list represents a separate branch."
>
> — **Learn You A Haskell For Great Good** by Miran Lipovača

```text
                 ['a', 'b']
                 /        \
       [1, 2]                  [1, 2]
       /    \                  /    \
['a', 1]    ['a', 2]    ['b', 1]    ['b', 2]
```
