(library (utils)
         (export assert
                 range)
         (import (rnrs))
         
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

         (define range
           (case-lambda
             [(stop)
              (range 0 stop 1)]
             [(start stop)
              (range start stop 1)]
             [(start stop step)
              (if (<= step 0)
                  '()
                  (let loop ([number stop]
                             [result '()])
                    (if (> start number)
                        result
                        (loop (- number step)
                              (cons number result)))))]))
         )
