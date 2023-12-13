(import (rnrs base)
        (list-builder)
        (utils))

(define binary '("0" "1"))

(define count-to-fifteen
  (yield (string-append bit-4 bit-3 bit-2 bit-1)
         ([bit-4 <- binary]
          [bit-3 <- binary]
          [bit-2 <- binary]
          [bit-1 <- binary])))

(define (py-triple n)
  (yield (list x y z)
         ([x <- (range 1 n)]
          [y <- (range x n)]
          [z <- (range y n)])
         (= (+ (sqr x) (sqr y))
            (sqr z))))

;; === tests ===
(assert equal? count-to-fifteen '("0000" "0001" "0010" "0011" "0100" "0101" "0110" "0111" "1000" "1001" "1010" "1011" "1100" "1101" "1110" "1111"))

(assert equal? (py-triple 21) '((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20)))
