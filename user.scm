; 2.1.1 Function combinators

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define (identity x) x)

(define ((iterate n) f)
  (if (= n 0)
    identity
    (compose f ((iterate (- n 1)) f))))

(((iterate 3) square) 5)

(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

((parallel-combine
   list
   (lambda (x y z) (list 'foo x y z))
   (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c)

;; interesting that clojure's (juxt f g) ~= (parallel-combine list f g)

; Arity

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

(define (spread-combine h f g)
  (let ((n (get-arity f))
        (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (h (apply f (list-head args n))
           (apply g (list-tail args n))))
      (restrict-arity the-combination t))))

((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

; Multiple Values

(define (spread-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (values (apply f (list-head args n))
                (apply g (list-tail args n))))
      (restrit-arity the-combination t))))

(define (spread-combine h f g)
  (compose h (spread-apply f g)))

(define (compose f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args)) f))
  (restrict-arity the-composition (get-arity g)))

((compose (lambda (a b) (list 'foo a b))
          (lambda (x) (values (list 'bar x)
                              (list 'baz x))))
 'z)

(define (spread-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (let-values ((fv (apply f (list-head args n)))
                     (gv (apply g (list-tail args n))))
          (apply values (append fv gv))))
      (restrict-arity the-combination t))))

((spread-combine list
                 (lambda (x y) (values x y))
                 (lambda (u v w) (values u v w)))
 'a 'b 'c 'd 'e)

#; (restart 1)
