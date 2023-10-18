(import (rnrs base)
        (list-builder)
        (utils))

(define (filter f xs)
  (yield x ([x <- xs]) (f x)))

(define (py-triple n)
  (yield (list x y z)
         ([x <- (range 1 n)]
          [y <- (range x n)]
          [z <- (range y n)])
         (= (+ (sqr x) (sqr y))
            (sqr z))))

;; === tests ===

(assert equal? (filter even? '(1 2 3 4 5 6 7 8 9 10)) '(2 4 6 8 10))

(assert equal? (py-triple 21) '((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20)))
