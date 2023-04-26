(import (rnrs)
        (list-builder)
        (utils))

(define py-triple
  (lambda (n)
    (for-list (list x y z)
              [(x <- (range 1 n))
               (y <- (range x n))
               (z <- (range y n))]
              (= (+ (sqr x) (sqr y))
                 (sqr z)))))

;; === tests ===

(assert equal? (py-triple 21) '((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20)))
