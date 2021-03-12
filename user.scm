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


; Exercise 2.1: Arity repair

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

; Exercise 2.2: Arity extension


;; (RESTART 1)
