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

; A small library

(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (let ((m (+ (get-arity f) 1)))
      (define (the-combination . args)
        (assert (= (length args) m))
        (apply f (list-remove args i)))
      (assert (< i m))
      (restrict-arity the-combination m))))

(define (list-remove lst index)
  (let loop ((lst lst)
             (index index))
    (if (= index 0)
      (cdr lst)
      (cons (car lst) (loop (cdr lst) (- index 1))))))

(((discard-argument 2)
  (lambda (x y z) (list 'foo x y z)))
 'a 'b 'c 'd)


(define ((curry-argument i) . args)
  (lambda (f)
    (assert (= (length args) (- (get-arity f) 1)))
    (lambda (x)
      (apply f (list-insert args i x)))))

(define (list-insert lst index value)
  (let loop ((lst lst)
             (index index))
    (if (= index 0)
      (cons value lst)
      (cons (car lst) (loop (cdr lst) (- index 1))))))

;; clojure's #(f 'a 'b % 'c) ~= (((curry-argument 2) 'a 'b 'c) f)
((((curry-argument 2) 'a 'b 'c)
  (lambda (x y z w) (list 'foo x y z w)))
 'd)


(define (permute-arguments . permspec)
  (let ((permute (make-permutation permspec)))
    (lambda (f)
      (define (the-combination . args)
        (apply f (permute args)))
      (let ((n (get-arity f)))
        (assert (= n (length permspec)))
        (restrict-arity the-combination n)))))

(define (make-permutation permspec)
  (define (the-permuter lst)
    (map (lambda (p) (list-ref lst p)) permspec))
  the-permuter)

;; clojure's #(f %2 %3 %1 %4) ~= ((permute-arguments 1 2 0 3) f)
(((permute-arguments 1 2 0 3)
  (lambda (x y z w) (list 'foo x y z w)))
 'a 'b 'c 'd)

; exercise 2.5 Useful combinators

(define (compose . procs)
  (cond
    ((null? procs) identity)
    ((null? (cdr procs)) (car procs))
    (else (let ((f (car procs))
                (g (apply compose (cdr procs))))
            (define (the-composition . args)
              (call-with-values (lambda () (apply g args)) f))
            (restrict-arity the-composition (get-arity g))))))

((compose) 'z)

((compose (lambda (a b) (list 'foo a b))) 'z 'w)

((compose list
          (lambda (a b) (list 'foo a b))
          (lambda (x) (values (list 'bar x)
                              (list 'baz x))))
 'z)

#; (restart 1)
