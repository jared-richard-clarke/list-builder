# `list-builder.scm`

- **Description**: Library provides `yield` — a list comprehension syntactic form
  that I copied from Haskell and implemented as a Scheme macro.
- **Scheme Syntax**: `(yield expression ([x <- xs] ...) predicate?)`
- **Haskell Syntax**: `[expression | x <- xs, ... predicate?]`

**Side Note**: Haskell is a **non-strict** language whereas Scheme is a **strict** language. A Scheme
list comprehension will build an entire list once invoked, whereas a Haskell list comprehension
will only build as far as a program pattern matches on its contents. A Haskell list comprehension,
once declared, might not produce anything at all.

## Scheme: Count in Binary

```scheme
(define binary '(0 1))

(define count-to-fifteen
  (yield (list bit-4 bit-3 bit-2 bit-1)
         ([bit-4 <- binary]
          [bit-3 <- binary]
          [bit-2 <- binary]
          [bit-1 <- binary])))

;; - expands ->

(define count-to-fifteen
  (concat-map binary
              (lambda (bit-4)
                (concat-map binary
                            (lambda (bit-3)
                              (concat-map binary
                                          (lambda (bit-2)
                                            (concat-map binary
                                                        (lambda (bit-1)
                                                          (list (list bit-4 bit-3 bit-2 bit-1)))))))))))

;; - evaluates ->

'((0 0 0 0) (0 0 0 1) (0 0 1 0) (0 0 1 1)
  (0 1 0 0) (0 1 0 1) (0 1 1 0) (0 1 1 1)
  (1 0 0 0) (1 0 0 1) (1 0 1 0) (1 0 1 1)
  (1 1 0 0) (1 1 0 1) (1 1 1 0) (1 1 1 1))
```

## Haskell: Count in Binary

```haskell
binary = ['0', '1']
countToFifteen = [[bit4, bit3, bit2, bit1] | bit4 <- binary,
                                             bit3 <- binary,
                                             bit2 <- binary,
                                             bit1 <- binary]

-- equivalent ->

countToFifteen = do
  bit4 <- binary
  bit3 <- binary
  bit2 <- binary
  bit1 <- binary
  return [bit4, bit3, bit2, bit1]

-- equivalent ->

countToFifteen =
  binary >>= \bit4 ->
  binary >>= \bit3 ->
  binary >>= \bit2 ->
  binary >>= \bit1 ->
  return [bit4, bit3, bit2, bit1]

-- equivalent ->

countToFifteen =
  concatMap (\bit4 ->
              concatMap (\bit3 ->
                          concatMap (\bit2 ->
                                      concatMap (\bit1 -> [[bit4, bit3, bit2, bit1]])
                                                 binary)
                                     binary)
                        binary)
            binary

-- evaluates ->

["0000","0001","0010","0011",
 "0100","0101","0110","0111",
 "1000","1001","1010","1011",
 "1100","1101","1110","1111"]
```

## Scheme: Pythagorean Triples

```scheme       
(define (py-triple n)
  (yield (list x y z)
         ([x <- (range 1 n)]
          [y <- (range x n)]
          [z <- (range y n)])
         (= (+ (sqr x) (sqr y))
            (sqr z))))

;; - expands ->

(define py-triple
  (lambda (n)
    (concat-map (range 1 n)
                (lambda (x)
                  (concat-map (range x n)
                              (lambda (y)
                                (concat-map (range y n)
                                            (lambda (z)
                                              (if (= (+ (sqr x) (sqr y))
                                                     (sqr z))
                                                  (list (list x y z))
                                                  empty)))))))))
;; - so that ->

(py-triple 21)

;; - evaluates ->

'((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20))
```

## Haskell: Pythagorean Triples

```haskell
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
    then return (x, y, z)
    else mzero
   
-- equivalent ->

pyTriple n =
  [1 .. n] >>= \x ->
    [x .. n] >>= \y ->
      [y .. n] >>= \z ->
        ( if x ^ 2 + y ^ 2 == z ^ 2
            then return (x, y, z)
            else mzero
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

### Monadic Definition

```haskell
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  fail _   = []
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
