#lang racket

(require rackunit
         "construct-templates.rkt")

(provide
  (all-defined-out))

(check-equal?
 (const-pattern 'reverse (list (cons (list (list 1 2)) (list 2 1))))
 '(define reverse
    (lambda (l)
      (if (null? l)
          ,B
          (,C (car l) (reverse (cdr l)))))))

(check-equal?
 (const-pattern 'reverse '((((1 2)) . (2 1))))
 '(define reverse
    (lambda (l)
      (if (null? l)
          ,B
          (,C (car l) (reverse (cdr l)))))))

(check-equal?
 (const-pattern 'reverse '(((1 (4 5 6) 3) . 4)))
 '(define reverse
    (lambda (n1 l n2)
      (if (null? l)
          ,B
          (,C (car l) (reverse n1 (cdr l) n2))))))
