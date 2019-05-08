(check-equal?
 (construct-pattern 'reverse (list (cons (list (list 1 2)) (list 2 1))))
 '(define reverse
    (lambda (l)
      (if (null? l)
          ,B
          (,C (car l) (reverse (cdr l)))))))

(check-equal?
 (construct-pattern 'reverse (list (cons (list (list 1 #f)) (list #f 1))))
 '(define reverse
    (lambda (l)
      (if (null? l)
          ,B
          (,C (car l) (reverse (cdr l)))))))

(check-equal?
 (construct-pattern 'reverse '((((1 2)) . (2 1))))
 '(define reverse
    (lambda (l)
      (if (null? l)
          ,B
          (,C (car l) (reverse (cdr l)))))))

(check-equal?
 (construct-pattern 'reverse '(((1 (4 5 6) 3) . 4)))
 '(define reverse
    (lambda (n1 l n2)
      (if (null? l)
          ,B
          (,C (car l) (reverse n1 (cdr l) n2))))))

(check-equal?
 (construct-pattern 'listthru '((((1 2 3 4)) . ((1) (2) (3) (4)))))
 '(define listthru
    (lambda (l)
      (if (null? l)
          ,B
          (,C (car l) (listthru (cdr l)))))))

(check-equal?
 (construct-pattern 'double '((((1 2 3 4)) . (1 1 2 2 3 3 4 4))))
 '(define double
    (lambda (l)
      (if (null? l)
          ,B
          (,C (car l) (double (cdr l)))))))

; This function needs an auxiliary function
(check-equal?
 (construct-pattern 'listofcombs '((((1 2 3 4)) . ((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))))
 '(define listofcombs
    (lambda (l)
      (if (null? l)
          ,B
          (,C (car l) (listofcombs (cdr l)))))))

(check-equal?
 (construct-pattern 'telescope '((((1 2 3 4)) . ((1 2 3 4 2 3 4 3 4 4)))))
 '(define telescope
    (lambda (l)
      (if (null? l)
          ,B
          (,C (car l) (telescope (cdr l)))))))


(check-equal?
 (construct-pattern 'revdouble '((((1 2 3 4)) . ((4 4 3 3 2 2 1 1)))))
 '(define revdouble
    (lambda (l)
      (if (null? l)
          ,B
          (,C (car l) (revdouble (cdr l)))))))

