(library (list-builder)
         (export for-list)
         (import (rnrs base)
                 (rnrs lists))
         
         (define-syntax yield
           (syntax-rules (<-)
             ;; base case
             [(yield expression ([x <- mx]))
              (bind mx (lambda (x)
                         (return expression)))]
             ;; recursive case
             [(yield expression ([x <- mx] [y <- my] ...))
              (bind mx (lambda (x)
                         (yield expression ([y <- my] ...))))]
             ;; base case with predicate
             [(yield expression ([x <- mx]) predicate)
              (bind mx (lambda (x)
                         (if predicate
                             (return expression)
                             empty)))]
             ;; recursive case with predicate
             [(yield expression ([x <- mx] [y <- my] ...) predicate)
              (bind mx (lambda (x)
                         (yield expression ([y <- my] ...) predicate)))]))

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
