; Exercise 2.2: Arity extension

(define arity-table
  (make-key-weak-eqv-hash-table))

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (procedure-arity proc)))

(define (restrict-arity proc arity)
  (hash-table-set! arity-table proc arity)
  proc)

(define (compose f g)
  (let ((f-arity (get-arity f))
        (g-arity (get-arity g)))
    (guarantee-procedure-of-arity f (make-procedure-arity 1) 'f)
    (define (the-composition . args)
      (guarantee-procedure-of-arity g (make-procedure-arity (length args)) 'g)
      (f (apply g args)))
    (restrict-arity the-composition g-arity)))

((compose
   (lambda (u) (list 'bar u))
   (lambda (x y z) (list 'foo x y z))) 'a 'b 'c)

(get-arity (compose
             (lambda (u) (list 'bar u))
             (lambda (x y z) (list 'foo x y z))))

(define (intersect-arities a b)
  (let ((min-arity (max (procedure-arity-min a)
                        (procedure-arity-min b)))
        (max-arity (or (and (procedure-arity-max a)
                            (procedure-arity-max b)
                            (min (procedure-arity-max a)
                                 (procedure-arity-max b)))
                       (procedure-arity-max a)
                       (procedure-arity-max b))))
    (and max-arity (<= min-arity max-arity)
         (make-procedure-arity min-arity max-arity))))

(define (parallel-combine h f g)
  (guarantee-procedure-of-arity h (make-procedure-arity 2) 'h)
  (let ((common-arity (intersect-arities (get-arity f) (get-arity g))))
    (assert common-arity)
    (define (the-combination . args)
      (let ((args-arity (make-procedure-arity (length args))))
        (guarantee-procedure-of-arity f args-arity 'f)
        (guarantee-procedure-of-arity g args-arity 'g)
        (h (apply f args) (apply g args))))
    (restrict-arity the-combination common-arity)))


((parallel-combine
   list
   (lambda (x y z) (list 'foo x y z))
   (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c)

(get-arity (parallel-combine
             list
             list
             (lambda (u v w) (list 'bar u v w))))

(define (spread-combine h f g)

  (define (fixed-arity? a)
    (eq? (procedure-arity-min a)
         (procedure-arity-max a)))

  (define (join-arities a b)
    (let ((min-arity (+ (procedure-arity-min a)
                        (procedure-arity-min b)))
          (max-arity (and (procedure-arity-max b)
                          (+ (procedure-arity-min a)
                             (procedure-arity-max b)))))
      (make-procedure-arity min-arity max-arity)))

  (define (in-arity? a n)
    (let ((min-arity (procedure-arity-min a))
          (max-arity (procedure-arity-max a)))
      (and (<= min-arity n) (or (not max-arity) (<= n max-arity)))))

  (guarantee-procedure-of-arity h (make-procedure-arity 2) 'h)
  (assert (fixed-arity? (get-arity f)))

  (let ((n (procedure-arity-min (get-arity f)))
        (common-arity (join-arities (get-arity f) (get-arity g))))
    (define (the-combination . args)
      (assert (in-arity? common-arity (length args)))
      (h (apply f (list-head args n))
         (apply g (list-tail args n))))
    (restrict-arity the-combination common-arity)))

((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list)
 'a 'b 'c 'd 'e 'd)

(get-arity (spread-combine list
                           (lambda (x y) (list 'foo x y))
                           (lambda (u v w) (list 'bar u v w))))
