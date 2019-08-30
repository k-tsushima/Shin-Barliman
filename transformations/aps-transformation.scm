;;; Accumulator Passing Style transformation

;;; Direct style
(define !
  (lambda (n)
    (if (zero? n)
        1
        (* n (! (sub1 n))))))

(! 5)

(define fib
  (lambda (n)
    (if (= n 0)
        0
        (if (= n 1)
            1
            (+ (fib (- n 1)) (fib (- n 2)))))))

(fib 6)

(define reverse
  (lambda (l)
    (if (null? l)
        '()
        (append (reverse (cdr l)) (cons (car l) '())))))

(reverse '(a b c))

;;; APS
(define !-aps
  (lambda (n a)
    (if (zero? n)
        a
        (!-aps (sub1 n) (* n a)))))

(!-aps 5 1)

(define reverse-aps
  (lambda (l a)
    (if (null? l)
        a
        (reverse-aps (cdr l) (cons (car l) a)))))

(define reverse-aps
  (lambda (l a)
    (if (null? l)
        a
        (let ((cl (car l)))
          (reverse-aps (cdr l) ,A)))))

(reverse-aps '(a b c) '())

(define fib-aps
  (lambda (n a1 a2)
    (if (= n 0)
        a1
        (if (= n 1)
            a2
            (fib-aps (- n 1) a2 (+ a1 a2))))))

(fib-aps 6 0 1)
