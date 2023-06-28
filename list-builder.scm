(library (list-builder)
         (export for-list)
         (import (rnrs base)
                 (rnrs lists))
         
         (define-syntax for-list
           (syntax-rules (<-)
             [(_ expression ([x <- mx]))
              (bind mx (lambda (x)
                         (return expression)))]
             [(_ expression ([x <- mx] [y <- my] ...))
              (bind mx (lambda (x)
                         (for-list expression ([y <- my] ...))))]
             [(_ expression ([x <- mx]) predicate)
              (bind mx (lambda (x)
                         (if predicate
                             (return expression)
                             empty)))]
             [(_ expression ([x <- mx] [y <- my] ...) predicate)
              (bind mx (lambda (x)
                         (for-list expression ([y <- my] ...) predicate)))]))

         ;; === monad ===

         (define return
           (lambda (x) (list x)))

         (define bind
           (lambda (xs f)
             (concat (map f xs))))

         (define empty '())

         (define concat
           (lambda (xs)
             (fold-right append '() xs)))
         
         )
