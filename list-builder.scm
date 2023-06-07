(library (list-builder)
         (export for-list)
         (import (rnrs base)
                 (rnrs lists))
         
         (define-syntax for-list
           (syntax-rules (<-)
             [(_ expression [(x <- mx)])
              (bind-list mx (lambda (x)
                              (return-list expression)))]
             [(_ expression [(x <- mx) (y <- my) ...])
              (bind-list mx (lambda (x)
                              (for-list expression [(y <- my) ...])))]
             [(_ expression [(x <- mx)] predicate)
              (bind-list mx (lambda (x)
                              (if predicate
                                  (return-list expression)
                                  empty-list)))]
             [(_ expression [(x <- mx) (y <- my) ...] predicate)
              (bind-list mx (lambda (x)
                              (for-list expression [(y <- my) ...] predicate)))]))

         ;; === monad ===

         (define return-list
           (lambda (x) (list x)))

         (define bind-list
           (lambda (xs f)
             (concat (map f xs))))

         (define empty-list '())

         ;; === utils ===

         (define concat
           (lambda (xs)
             (fold-right append '() xs)))
         )
