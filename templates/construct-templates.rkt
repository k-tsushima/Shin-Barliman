#lang racket

(require
 )

(provide
  (all-defined-out))


;;; Construct quasiquoted template for functions that recur over
;;; lists, with place holders for logic variables.

;;; TODO
;;;
;;; add recursive parameters that aren't lists to the recursive call
;;;
;;; handle multiple list args
;;;
;;; refactor generate-pretty-arg-names to avoid duplicate code, etc.

(define get-args-that-are-lists
  (lambda (inputs arg-names)
    (match `(,inputs ,arg-names)
      ['(() ()) '()]
      [`((,in-a . ,in-d) (,arg-a . ,arg-d))
       (if (list? in-a)
           (cons arg-a (get-args-that-are-lists in-d arg-d))
           (get-args-that-are-lists in-d arg-d))])))


;;; simple and easy to explain!
(define simple-generate-pretty-arg-names
  (lambda (args)
    (let loop ((args args)
               (count 1))
      (cond
        ((null? args) '())
        (else (cons (string->symbol (format "a~s" count))
                    (loop (cdr args) (+ 1 count))))))))

;;; produces smarter variables names, but is complex!
(define smart-generate-pretty-arg-names
  (lambda (args)
    (let ((list-count (length (filter list? args)))
          (num-count (length (filter number? args)))
          (sym-count (length (filter symbol? args))))
      (let ((other-count (- (length args) list-count num-count sym-count)))
        (let ((pretty-arg-name/counts (lambda (arg counts)
                                        (match counts
                                          [`(,lc ,nc ,sc ,oc)
                                           (cond
                                             ((list? arg)
                                              (let ((counts `(,(add1 lc) ,nc ,sc ,oc)))
                                                (if (> list-count 1)
                                                    (cons (format "l~s" lc) counts)
                                                    (cons (format "l") counts))))
                                             ((number? arg)
                                              (let ((counts `(,lc ,(add1 nc) ,sc ,oc)))
                                                (if (> num-count 1)
                                                    (cons (format "n~s" nc) counts)
                                                    (cons (format "n") counts))))
                                             ((symbol? arg)
                                              (let ((counts `(,lc ,nc ,(add1 sc) ,oc)))
                                                (if (> sym-count 1)
                                                    (cons (format "x~s" sc) counts)
                                                    (cons (format "x") counts))))
                                             (else
                                              (let ((counts `(,lc ,nc ,sc ,(add1 oc))))
                                                (if (> other-count 1)
                                                    (cons (format "a~s" oc) counts)
                                                    (cons (format "a") counts)))))]))))
          (let loop ((args args)
                     (counts '(1 1 1 1)))
            (cond
              ((null? args) '())
              (else
               (let ((name/counts (pretty-arg-name/counts (car args) counts)))
                 (let ((name (car name/counts))
                       (counts (cdr name/counts)))
                   (cons (string->symbol name)
                         (loop (cdr args) counts))))))))))))


; 知りたい情報、なんばんめでリストにマッチングするか
; ios = pairs of input and output
(define const-pattern
  (lambda (fname ios)
    (match ios
      [`((,inputs . ,output) . ,_)
       (let ((arg-names (smart-generate-pretty-arg-names inputs)))
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
