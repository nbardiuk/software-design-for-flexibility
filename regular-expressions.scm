(define (r:dot) ".")
(define (r:bol) "^")
(define (r:eol) "$")

(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

(define (r:quote string)
  (r:seq
    (list->string
      (append-map (lambda (char)
                    (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                  (string->list string)))))

(define chars-needing-quoting
  '(#\. #\[ #\\ #\^ #\$ #\*))

(define (r:alt . exprs)
  (if (pair? exprs)
    (apply r:seq (cons (car exprs)
                       (append-map (lambda (expr) (list "\\|" expr))
                                   (cdr exprs))))
    (r:seq)))

(define (r:repeat min max expr)
  (apply
    r:seq
    (append (list expr "\\{" (number->string min))
            (cond ((and max (= min max)) '())
                  (max (list "," (number->string max)))
                  (else (list ",")))
            (list "\\}"))))

(define (r:* expr) (r:repeat 0 #f expr))
(define (r:+ expr) (r:repeat 1 #f expr))

(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quite string))
    (else (bracket string
                   (lambda (members)
                     (if (lset= eqv? '(#\- #\^) members)
                       '(#\- #\^)
                       (quote-bracketed-contents members)))))))


(define (r:char-not-from string)
  (bracket string
           (lambda (members)
             (cons #\^ (quite-bracketed-contents members)))))

(define (bracket string procedure)
  (list->string
    (append '(#\[)
            (procedure (string->list string))
            '(#\]))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members)
      (list char)
      '()))
  (append (optional #\])
          (remove (lambda (c) (memv c chars-needing-quoting-in-bracktes))
                  members)
          (optional #\^)
          (optional #\-)))

(define chars-needing-quoting-in-brackets '(#\] #\^ #\-))

(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

(define (bourne-shell-quote-string string)
  (list->string
    (append (list #\')
            (append-map (lambda (char)
                          (if (char=? char #\')
                            (list #\' #\\ char #\')
                            (list char)))
                        (string->list string))
            (list #\'))))

(assert (equal?
          "\\(\\(a\\).\\(c\\)\\)"
          (r:seq (r:quote "a") (r:dot) (r:quote "c"))))

(assert (equal?
          "\\(\\(foo\\)\\\|\\(bar\\)\\\|\\(baz\\)\\)"
          (r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))))

(assert (equal?
          "\\(\\(a\\)\\{3,\\}\\)"
          (r:repeat 3 #f (r:quote "a"))))

(assert (equal?
          "\\(\\(a\\)\\{3,5\\}\\)"
          (r:repeat 3 5 (r:quote "a"))))

(assert (equal?
          "\\(\\(a\\)\\{3\\}\\)"
          (r:repeat 3 3 (r:quote "a"))))

(assert (equal?
          "\\(\\(a\\)\\{0,1\\}\\)"
          (r:repeat 0 1 (r:quote "a"))))

(assert (equal?
          "\\(\\(\\(cat\\)\\\|\\(dog\\)\\)\\{3,5\\}\\)"
          (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))))

(assert (equal?
          "\\(\\(a\\)\\{0,\\}\\)"
          (r:* (r:quote "a"))))

(assert (equal?
          "\\(\\(a\\)\\{1,\\}\\)"
          (r:+ (r:quote "a"))))

