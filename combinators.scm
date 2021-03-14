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


(define (compose . procs)
  (cond
    ((null? procs) identity)
    ((null? (cdr procs)) (car procs))
    (else (let ((f (car procs))
                (g (apply compose (cdr procs))))
            (define (the-composition . args)
              (call-with-values (lambda () (apply g args)) f))
            (restrict-arity the-composition (get-arity g))))))


(define (identity x) x)


(define ((iterate n) f)
  (if (= n 0)
    identity
    (compose f ((iterate (- n 1)) f))))


(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)


(define (spread-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (let-values ((fv (apply f (list-head args n)))
                     (gv (apply g (list-tail args n))))
          (apply values (append fv gv))))
      (restrict-arity the-combination t))))


(define (spread-combine h f g)
  (compose h (spread-apply f g)))


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

(define (list-insert lst index value)
  (let loop ((lst lst)
             (index index))
    (if (= index 0)
      (cons value lst)
      (cons (car lst) (loop (cdr lst) (- index 1))))))

(define ((curry-argument i) . args)
  (lambda (f)
    (assert (= (length args) (- (get-arity f) 1)))
    (lambda (x)
      (apply f (list-insert args i x)))))

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

(assert (eq? 'z
             ((compose) 'z)))

(assert (equal? '(foo z w)
                ((compose (lambda (a b) (list 'foo a b)))
                 'z 'w)))

(assert (equal? '((foo (bar z) (baz z)))
                ((compose list
                          (lambda (a b) (list 'foo a b))
                          (lambda (x) (values (list 'bar x)
                                              (list 'baz x))))
                 'z)))

(assert (= 390625
           (((iterate 3) square) 5)))

(assert (equal? '(foo b c a d)
                (((permute-arguments 1 2 0 3)
                  (lambda (x y z w) (list 'foo x y z w)))
                 'a 'b 'c 'd) ) )

(assert (equal? '((foo a b) (bar c d e))
                ((spread-combine list
                                 (lambda (x y) (list 'foo x y))
                                 (lambda (u v w) (list 'bar u v w)))
                 'a 'b 'c 'd 'e)))

(assert (equal? '(a b c d e)
                ((spread-combine list
                                 (lambda (x y) (values x y))
                                 (lambda (u v w) (values u v w)))
                 'a 'b 'c 'd 'e)))

(assert (equal? '(foo a b d c)
                ((((curry-argument 2) 'a 'b 'c)
                  (lambda (x y z w) (list 'foo x y z w)))
                 'd)))

(assert (equal? '(foo a b d)
                (((discard-argument 2)
                  (lambda (x y z) (list 'foo x y z)))
                 'a 'b 'c 'd)))
