
; Exercise 2.1: Arity repair

(define arity-table
  (make-key-weak-eqv-hash-table))

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc)))
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (compose f g)
  (let ((f-arity (get-arity f))
        (g-arity (get-arity g)))
    (assert (= 1 f-arity))
    (define (the-composition . args)
      (f (apply g args)))
    (restrict-arity the-composition g-arity)))

((compose
   (lambda (u) (list 'bar u))
   (lambda (x y z) (list 'foo x y z))) 'a 'b 'c)

(get-arity (compose
             (lambda (u) (list 'bar u))
             (lambda (x y z) (list 'foo x y z))))

(define (parallel-combine h f g)
  (let ((f-arity (get-arity f))
        (g-arity (get-arity g)))
    ; (assert (= 2 (get-arity h))) this is too naive - does not allow to use 'list
    (assert (= f-arity g-arity))
    (define (the-combination . args)
      (h (apply f args) (apply g args)))
    (restrict-arity the-combination f-arity)))

((parallel-combine
   list
   (lambda (x y z) (list 'foo x y z))
   (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c)
