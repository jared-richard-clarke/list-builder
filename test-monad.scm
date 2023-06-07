(import (rnrs base)
        (rnrs lists)
        (utils))

;; Side Note: Since "return-list", "bind-list", and "concat" are private
;; to "list-builder", I explicitly added them to this file for testing.

(define return-list
  (lambda (x) (list x)))

(define bind-list
  (lambda (xs f)
    (concat (map f xs))))

(define concat
  (lambda (xs)
    (fold-right append '() xs)))

;; === monad laws ===

;; left identity: return x >>= f = f x

(let ([add1-list (lambda (x) (list (add1 x)))])
  (assert equal? (bind (return 6) add1-list) (add1-list 6)))

;; right identity: m >>= return = m

(assert equal? (bind (list 7) return) (list 7))

;; associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)

(let ([sub1-list (lambda (x) (list (sub1 x)))]
      [add1-list (lambda (x) (list (add1 x)))])
  (assert equal?
          (bind (bind (list 7) sub1-list) add1-list)
          (bind (list 7) (lambda (x) (bind (sub1-list x) add1-list)))))
