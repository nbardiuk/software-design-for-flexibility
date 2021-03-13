(define (parallel-combine h f g)
  (define (the-combination . args)
    (let-values ((fv (apply f args))
                 (gv (apply g args)))
      (apply h (append fv gv))))
  the-combination)

(assert (equal? (list 'a 'b 'c 'a 'b 'c)
                ((parallel-combine
                   list
                   (lambda (x y z) (values x y z))
                   (lambda (u v w) (values u v w)))
                 'a 'b 'c)))

(assert (equal? (list (list 'foo 'a 'b 'c)
                      (list 'bar 'c 'b 'a))
                ((parallel-combine
                   list
                   (lambda (x y z) (list 'foo x y z))
                   (lambda (u v w) (list 'bar w v u)))
                 'a 'b 'c)))
