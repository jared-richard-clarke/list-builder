(import (rnrs)
        (list-builder))

;; === test utilities ===

(define-syntax assert
  (syntax-rules ()
    [(_ compare x y)
     (let ([computed-x x]
           [computed-y y])
       (unless (compare computed-x computed-y)
         (printf "Test failed:\nlhs: ~a -> ~a, rhs: ~a -> ~a\n"
                 (quote x)
                 x
                 (quote y)
                 y)))]))

;; === monad laws ===

;; left identity: return x >>= f = f x

(let ([add1 (lambda (x) (list (add1 x)))])
  (assert equal? (bind (return 6) add1) (add1 6)))

;; right identity: m >>= return = m

(assert equal? (bind (list 7) return) (list 7))

;; associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)

(let ([sub1 (lambda (x) (list (sub1 x)))]
      [add1 (lambda (x) (list (add1 x)))])
  (assert equal?
          (bind (bind (list 7) sub1) add1)
          (bind (list 7) (lambda (x) (bind (sub1 x) add1)))))
