#lang racket

(require
 )

(provide
  (all-defined-out))


;;; Construct quasiquoted template for functions that recur over
;;; lists, with place holders for logic variables.

;;; TODO
;;;
;;; add test macro
;;;
;;; use pattern matcher
;;;
;;; add recursive parameters that aren't lists to the recursive call
;;;
;;; handle multiple list args

(define get-args-that-are-lists
  (lambda (inputs arg-names)
    (match `(,inputs ,arg-names)
      ['(() ()) '()]
      [`((,in-a . ,in-d) (,arg-a . ,arg-d))
       (if (list? in-a)
           (cons arg-a (get-args-that-are-lists in-d arg-d))
           (get-args-that-are-lists in-d arg-d))])))

(define generate-pretty-arg-names
  (lambda (args)
    (let loop ((args args)
               (count 1))
      (cond
        ((null? args) '())
        (else (cons (string->symbol (format "a~s" count))
                    (loop (cdr args) (+ 1 count))))))))

; 知りたい情報、なんばんめでリストにマッチングするか
; ios = pairs of input and output
(define const-pattern
  (lambda (fname ios)
    (match ios
      [`((,inputs . ,output) . ,ios-rest)
       (let ((arg-names (generate-pretty-arg-names inputs)))
         (let ((list-args (get-args-that-are-lists inputs arg-names)))
           (match list-args
             ['()
              `(define ,fname
                 (lambda ,arg-names
                   ,',A))]
             [`(,la)
              `(define ,fname
                 (lambda ,arg-names
                   (if (null? ,la)
                       ,',B
                       ;; To do: fix fname (cdr first-list-arg) .. it might have other arguments.
                       (,',C (car ,la) (,fname (cdr ,la))))))]
             [else (error 'const-pattern (format "more than one list argument in ~s" inputs))])))])))




(const-pattern 'reverse (list (cons (list (list 1 2)) (list 2 1))))
#|
(define reverse
  (lambda (a)
    (if (null? a)
        ,B
        (,C (car a) (reverse (cdr a))))))
|#
(const-pattern 'reverse '((((1 2)) . (2 1))))

(let ((input-list-1 '((1 2)))
      (output-1 '(2 1)))
  (let ((example-1 (cons input-list-1 output-1)))
    (const-pattern 'reverse (list example-1))))

(let ((input-list-1 '((1 2)))
      (output-1 '(2 1)))
  (let ((example-1 `(,input-list-1 . ,output-1)))
    (const-pattern 'reverse `(,example-1))))


;; (const-pattern 'reverse (list (cons (list 1 (list 4 5 6) 3) 4)))
(const-pattern 'reverse '(((1 (4 5 6) 3) . 4)))

#|
(define reverse
  (lambda (a1 a2 a3)
    (if (null? a2)
        ,B
        (,C (car a2) (reverse (cdr a2))))))
|#
