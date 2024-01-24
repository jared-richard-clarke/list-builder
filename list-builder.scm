(library (list-builder)
         (export yield)
         (import (rnrs base)
                 (rnrs lists))

         (define-syntax yield
           (syntax-rules (<-)
             ;; base case
             [(yield expression ([x <- mx]))
              (concat-map mx (lambda (x)
                               (list expression)))]
             ;; recursive case
             [(yield expression ([x <- mx] [y <- my] ...))
              (concat-map mx (lambda (x)
                               (yield expression ([y <- my] ...))))]
             ;; base case with predicate
             [(yield expression ([x <- mx]) predicate)
              (concat-map mx (lambda (x)
                               (if predicate
                                   (list expression)
                                   empty)))]
             ;; recursive case with predicate
             [(yield expression ([x <- mx] [y <- my] ...) predicate)
              (concat-map mx (lambda (x)
                               (yield expression ([y <- my] ...) predicate)))]))

         ;; === monad ===

         (define empty '())

         ;; (define concat
         ;;   (lambda (xs)
         ;;     (fold-right append empty xs)))
         ;;
         ;; Calling map between concatenations produces intermediary lists.
         ;; Avoid extra allocations by integrating map directly into concat.
         ;;
         ;; (define concat-map
         ;;   (lambda (xs f)
         ;;     (concat (map f xs))))

         (define concat-map
           (lambda (xs f)
             (if (null? xs)
                 empty
                 (append (f (car xs)) (concat-map (cdr xs) f)))))

         )
