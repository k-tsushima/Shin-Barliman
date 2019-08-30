(load "../templates/chez/mk/mk-vicare.scm")
(load "../templates/chez/mk/mk.scm")
(load "../templates/chez/interp.scm")
(load "../templates/chez/mk/test-check.scm")

;; Peano arithmetic
;; n ::= z
;;     | (s n)


(time
  (test "Peano factorial-aps and initial accumulator synthesis from template"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns ACC1 ACC2)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))
                 
                 ;; skeleton
                 (== `((define zero?
                         (lambda (n)
                           (equal? 'z n)))

                       (define add1
                         (lambda (n)
                           (cons 's n)))

                       (define sub1
                         (lambda (n)
                           (and (equal? (car n) 's)
                                (cdr n))))
                       
                       (define +
                         (lambda (n m)
                           (if (zero? n)
                               m
                               (add1 (+ (sub1 n) m)))))
                       
                       (define -
                         (lambda (n m)
                           (if (zero? m)
                               n
                               (sub1 (- n (sub1 m))))))


                       #|
                       ;; 4 seconds
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))
                       |#


                       #|
                       ;; guessing wrong seems bad!
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) (,A . ,B) (+ a1 a2))))))
                       |#

                       #|
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))
                       |#

                       #|
                       ;;; 145 seconds
                       ;;
                       ;; Having to guess the initial accumulator values,
                       ;; without knowing the base case values, is apparently hard!
                       ;;
                       ;; If we started from direct-style fib, we
                       ;; should be able to do a better job
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,A
                               (if (zero? (sub1 n))
                                   ,B
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))
                       |#


                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,A
                               (if (zero? (sub1 n))
                                   ,B
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))

                       #|
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               (let ((f (lambda (x y) ,A)))
                                 (f a1 a2))                               
                               (if (zero? (sub1 n))
                                   (let ((g (lambda (x y) ,A)))
                                     (g a1 a2))
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))
                       |#
                       
                       #|
                       ;;; 
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A (,B . ,E))))))
                       |#
                       

                       #|
                       ;; 8 seconds
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#

                       
                       #|
                       ;; 
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#


                       #|
                       ;; 
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   ,D
                                   (fib-aps (- n '(s . z)) ,A (,B . ,E))))))
                       |#

                       #|
                       ;; does this come back?  would have to guess the identity function for F
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   ,D
                                   (fib-aps (- n '(s . z)) (,F . ,G) (,B . ,E))))))
                       |#


                       #|
                       ;; Takes a long time
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   ,D
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#

                       #|
                       ;; removing concrete variable names makes synthesis muuuch slower!
                       (define fib-aps
                         (lambda (n ,E ,F)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   ,D
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#

                       #|
                       ;; 
                       (define fib-aps
                         (lambda (n ,Y . ,Z)
                           (if (zero? n)
                               ,A
                               (if (zero? (sub1 n))
                                   ,B
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))
                       |#

                       #|
                       ;; 
                       (define fib-aps
                         (lambda (n ,Y . ,Z)
                           (if (zero? n)
                               ,A
                               (if (zero? (sub1 n))
                                   ,B
                                   (fib-aps (- n '(s . z)) ,C . ,D)))))
                       |#

                       #|
                       ;; 
                       (define fib-aps
                         (lambda (n ,W ,Y . ,Z)
                           (if (zero? n)
                               ,A
                               (if (zero? (sub1 n))
                                   ,B
                                   (fib-aps (- n '(s . z)) ,C ,D . ,E)))))
                       |#
                       
                       )
                     defns)

                 #|
                 (conde
                   ((== 'z ACC1))
                   ((== '(s . z) ACC1))
                   ((fresh (my-num)
                      (== `(s s . ,my-num) ACC1))))

                 (conde
                   ((== 'z ACC2))
                   ((== '(s . z) ACC2))
                   ((fresh (my-num)
                      (== `(s s . ,my-num) ACC2))))
                 |#

                 #|
                 (absento 'lambda A)
                 (absento 'lambda B)
                 |#
                 
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs

                             (fib-aps 'z ',ACC1 ',ACC2)
                             (fib-aps '(s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s s s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s s s s . z) ',ACC1 ',ACC2)
  
                            
                             ;;(fib-aps '(s s s s s s . z) ,ACC1 ,ACC2)
                             
                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs

                         'z
                         '(s . z)
                         '(s . z)
                         '(s s . z)
                         '(s s s . z)                   
                         '(s s s s s . z)


                         ;; '(s s s s s s s s . z)

                         
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '???)
  )

#!eof

(time
  (test "Peano factorial-aps synthesis from template"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns FOO)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))
                 
                 ;; skeleton
                 (== `((define zero?
                         (lambda (n)
                           (equal? 'z n)))

                       (define add1
                         (lambda (n)
                           (cons 's n)))

                       (define sub1
                         (lambda (n)
                           (and (equal? (car n) 's)
                                (cdr n))))
                       
                       (define +
                         (lambda (n m)
                           (if (zero? n)
                               m
                               (add1 (+ (sub1 n) m)))))
                       
                       (define -
                         (lambda (n m)
                           (if (zero? m)
                               n
                               (sub1 (- n (sub1 m))))))

                                              
                       #|
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))
                       |#

                       #|
                       ;; guessing wrong seems bad!
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) (,A . ,B) (+ a1 a2))))))
                       |#

                       #|
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))
                       |#

                       #|
                       ;;; 6 seconds
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A (,B . ,E))))))
                       |#
                       
                       #|
                       ;; 8 seconds
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#
                       
                       #|
                       ;; 16 seconds
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#


                       #|
                       ;; 33 seconds
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   ,D
                                   (fib-aps (- n '(s . z)) ,A (,B . ,E))))))
                       |#

                       #|
                       ;; does this come back?  would have to guess the identity function for F
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   ,D
                                   (fib-aps (- n '(s . z)) (,F . ,G) (,B . ,E))))))
                       |#

                       #|
                       ;; 38 seconds
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   ,D
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#

                       #|
                       ;; removing concrete variable names makes synthesis muuuch slower!
                       (define fib-aps
                         (lambda (n ,E ,F)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   ,D
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#

                       #|
                       ;; 23 seconds
                       (define fib-aps
                         (lambda (n ,Y . ,Z)
                           (if (zero? n)
                               ,A
                               (if (zero? (sub1 n))
                                   ,B
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))
                       |#

                       #|
                       ;; 123 seconds
                       (define fib-aps
                         (lambda (n ,Y . ,Z)
                           (if (zero? n)
                               ,A
                               (if (zero? (sub1 n))
                                   ,B
                                   (fib-aps (- n '(s . z)) ,C . ,D)))))
                       |#

                       ;; 123 seconds
                       (define fib-aps
                         (lambda (n ,W ,Y . ,Z)
                           (if (zero? n)
                               ,A
                               (if (zero? (sub1 n))
                                   ,B
                                   (fib-aps (- n '(s . z)) ,C ,D . ,E)))))
                       
                         )
                     defns)
                 
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs

                             (fib-aps 'z 'z '(s . z))
                             (fib-aps '(s . z) 'z '(s . z))
                             (fib-aps '(s s . z) 'z '(s . z))
                             (fib-aps '(s s s . z) 'z '(s . z))
                             (fib-aps '(s s s s . z) 'z '(s . z))
                             (fib-aps '(s s s s s . z) 'z '(s . z))
  
                            
                             ;;(fib-aps '(s s s s s s . z) 'z '(s . z))
                             
                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs

                         'z
                         '(s . z)
                         '(s . z)
                         '(s s . z)
                         '(s s s . z)                   
                         '(s s s s s . z)


                         ;; '(s s s s s s s s . z)

                         
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '???)
  )

#!eof

(time
  (test "Peano factorial-aps synthesis from template"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns FOO)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))
                 
                 ;; skeleton
                 (== `((define zero?
                         (lambda (n)
                           (equal? 'z n)))

                       (define add1
                         (lambda (n)
                           (cons 's n)))

                       (define sub1
                         (lambda (n)
                           (and (equal? (car n) 's)
                                (cdr n))))

                       (define =
                         (lambda (n m)
                           (if (and (zero? n) (zero? m))
                               #t
                               (if (zero? n)
                                   #f
                                   (if (zero? m)
                                       #f
                                       (= (sub1 n) (sub1 m)))))))

                       (define +
                         (lambda (n m)
                           (if (zero? n)
                               m
                               (add1 (+ (sub1 n) m)))))
                       
                       (define -
                         (lambda (n m)
                           (if (zero? m)
                               n
                               (sub1 (- n (sub1 m))))))

                       (define *
                         (lambda (n m)
                           (if (zero? n)
                               (zero)
                               (+ (* (sub1 n) m) m))))

                       (define zero
                         (lambda ()
                           'z))
                       (define one
                         (lambda ()
                           (add1 (zero))))                       

                       #|
                       (define !-aps
                         (lambda (n a)
                           (if (zero? n)
                               a
                               (!-aps (sub1 n) (* n a)))))
                       |#

                       #|
                       (define !-aps
                         (lambda (n a)
                           (if (zero? n)
                               a
                               (!-aps (sub1 n) (* ,A ,B)))))
                       |#

                       #|
                       (define !-aps
                         (lambda (n a)
                           (if (zero? n)
                               a
                               (!-aps (sub1 n) (* . ,A)))))
                       |#

                       #|
                       ;;; slow or doesn't come back
                       (define !-aps
                         (lambda (n a)
                           (if (zero? n)
                               a
                               (let ((f (lambda (m) ,B)))
                                 (!-aps (sub1 n) (f (* . ,A)))))))
                       |#

                       #|
                       (define !-aps
                         (lambda (n a)
                           (if (zero? n)
                               a
                               (!-aps (sub1 n) (* . ,A)))))
                       |#

                       #|
                       ;; 15 seconds
                       (define !-aps
                         (lambda (n a)
                           (if (zero? n)
                               a
                               (!-aps (sub1 n) (,B . ,A)))))
                       |#

                       #|
                       ;; sloooowwwwww!
                       (define !-aps
                         (lambda (n a)
                           (if (zero? n)
                               a
                               (!-aps (sub1 n) ,A))))
                       |#

                       
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (= n 'z)
                               a1
                               (if (= n '(s . z))
                                   a2
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))
                       

                       #|
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) a2 (+ a1 a2))))))
                       |#

                       #|
                       ;; 12 seconds
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#

                       #|
                       ;;; ???
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A (,B . ,E))))))
                       |#
                       
                       #|
                       ;; 21 seconds
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#

                       #|
                       ;; slooow
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   ,D
                                   (fib-aps (- n '(s . z)) ,A (,B . ,E))))))
                       |#                       
                       
                       #|
                       ;; 45 seconds
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,C
                               (if (zero? (sub1 n))
                                   ,D
                                   (fib-aps (- n '(s . z)) ,A ,B)))))
                       |#
                       
                         )
                     defns)
                 
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs

                             #|
                             (!-aps 'z '(s . z))
                             (!-aps '(s . z) '(s . z))
                             (!-aps '(s s . z) '(s . z))
                             (!-aps '(s s s . z) '(s . z))
                             |#
                             

                             (fib-aps 'z 'z '(s . z))
                             (fib-aps '(s . z) 'z '(s . z))
                             (fib-aps '(s s . z) 'z '(s . z))
                             (fib-aps '(s s s . z) 'z '(s . z))
                             (fib-aps '(s s s s . z) 'z '(s . z))
                             (fib-aps '(s s s s s . z) 'z '(s . z))
  
                            
                             ;;(fib-aps '(s s s s s s . z) 'z '(s . z))
                             
                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs

                         #|
                         '(s . z)
                         '(s . z)
                         '(s s . z)
                         '(s s s s s s . z)
                         |#
                         

                         'z
                         '(s . z)
                         '(s . z)
                         '(s s . z)
                         '(s s s . z)                   
                         '(s s s s s . z)


                         ;; '(s s s s s s s s . z)

                         
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '???)
  )

#!eof

(time
  (test "Peano factorial and fib"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns FOO BAR BAZ QUUX)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))
                 
                 ;; skeleton
                 (== `((define zero?
                         (lambda (n)
                           (equal? 'z n)))

                       (define add1
                         (lambda (n)
                           (cons 's n)))

                       (define sub1
                         (lambda (n)
                           (and (equal? (car n) 's)
                                (cdr n))))

                       (define =
                         (lambda (n m)
                           (if (and (zero? n) (zero? m))
                               #t
                               (if (zero? n)
                                   #f
                                   (if (zero? m)
                                       #f
                                       (= (sub1 n) (sub1 m)))))))

                       (define +
                         (lambda (n m)
                           (if (zero? n)
                               m
                               (add1 (+ (sub1 n) m)))))

                       (define -
                         (lambda (n m)
                           (if (zero? m)
                               n
                               (sub1 (- n (sub1 m))))))

                       (define *
                         (lambda (n m)
                           (if (zero? n)
                               (zero)
                               (+ (* (sub1 n) m) m))))


                       (define zero
                         (lambda ()
                           'z))
                       (define one
                         (lambda ()
                           (add1 (zero))))
                       (define two
                         (lambda ()
                           (add1 (add1 (zero)))))


                       (define !
                         (lambda (n)
                           (if (zero? n)
                               (one)
                               (* n (! (sub1 n))))))

                       (define !-aps
                         (lambda (n a)
                           (if (zero? n)
                               a
                               (!-aps (sub1 n) (* n a)))))

                       (define fib
                         (lambda (n)
                           (if (= (zero) n)
                               (zero)
                               (if (= (one) n)
                                   (one)
                                   (+ (fib (- n (one))) (fib (- n (two)))))))))
                     defns)
                 
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (! 'z)
                             (! ',BAR)
                             (!-aps '(s s s . z) '(s . z))
                             (fib '(s s s . z))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         FOO
                         '(s s s s s s . z)
                         BAZ
                         QUUX
                         
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '???)
  )
