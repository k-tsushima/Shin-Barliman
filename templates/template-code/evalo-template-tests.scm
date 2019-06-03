(check-equal?
 (let ((template '((define double
                     (lambda (l)
                       (match l
                         (`() '())
                         (`(,a . ,d)
                          (cons ?C (double d))))))))
       (input* `(
                 (double '())
                 (double '(1))
                 (double '(2 1))
                 (double '(2 3 4 1))
                 ))
       (output* `(
                  ()
                  ((1 1))
                  ((2 2) (1 1))
                  ((2 2) (3 3) (4 4) (1 1))
                  )))
   (let ((f (lambda (template input* output*)
              (define (ans-allTests)
                (define (results)
                  (run 1 (defns)                    
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
                          (absento g20 defn-list)
                          )

                        (letrec ((replace-?-vars-in-template
                                  (lambda (template)
                                    (cond
                                      [(null? template)
                                       template]
                                      [(symbol? template)
                                       (let ((sym (get-raw-?-symbol template)))
                                         (if sym
                                             (raw-?-symbol->logic-var sym)
                                             template))]
                                      [(pair? template)
                                       (cons (replace-?-vars-in-template (car template))
                                             (replace-?-vars-in-template (cdr template)))]
                                      [else template])))
                                 (get-raw-?-symbol
                                  (lambda (sym)
                                    (let ((str (symbol->string sym)))
                                      (and
                                        (= (string-length str) 2)
                                        (equal? (string-ref str 0) #\?)
                                        (let ((c (string-ref str 1)))
                                          (and (>= (char->integer c) (char->integer #\A))
                                               (<= (char->integer c) (char->integer #\Z))
                                               (string->symbol (list->string (list c)))))))))
                                 (raw-?-symbol->logic-var
                                  (lambda (raw-sym)
                                    (cdr (assq raw-sym
                                               `((A . ,A)
                                                 (B . ,B)
                                                 (C . ,C)
                                                 (D . ,D)
                                                 (E . ,E)
                                                 (F . ,F)
                                                 (G . ,G)
                                                 (H . ,H)
                                                 (I . ,I)
                                                 (J . ,J)
                                                 (K . ,K)
                                                 (L . ,L)
                                                 (M . ,M)
                                                 (N . ,N)
                                                 (O . ,O)
                                                 (P . ,P)
                                                 (Q . ,Q)
                                                 (R . ,R)
                                                 (S . ,S)
                                                 (T . ,T)
                                                 (U . ,U)
                                                 (V . ,V)
                                                 (W . ,W)
                                                 (X . ,X)
                                                 (Y . ,Y)
                                                 (Z . ,Z)))))))

                          (fresh ()


                            (let ((t (replace-?-vars-in-template template)))
                              (printf "*** original template:\n ~s\n\n" template)
                              (printf "*** updated template:\n ~s\n\n" t)
                              
                              (fresh ()
                                (== t defns)))
                                                   
                            (appendo defns
                                     `(((lambda x x) . ,input*))
                                     begin-body)
                            (evalo `(begin . ,begin-body)
                                   output*)
                          
                            )

                          )))))
                (results))
              
              (ans-allTests))))
     (f template input* output*)))
;;; answer
 '((((define double
       (lambda (l)
         (match l
           (`() '())
           (`(,a unquote d)
            (cons (list a a) (double d))))))))))

(check-equal?
 (let ()
   (define (ans-allTests)
     (define (results)
       (run 1 (defns)
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
             (== `((define double
                     (lambda (l)
                       (match l
                         (`() '())
                         (`(,a . ,d)
                          (cons ,C (double d)))))))
                 defns)

             (appendo defns
                      `(((lambda x x)

                         ;; example inputs
                         (double '())
                         (double '(1))
                         (double '(2 1))
                         (double '(2 3 4 1))

                         ))
                      begin-body)
             (evalo `(begin . ,begin-body)
                    (list
                         
                     ;; example outputs
                     '()
                     '((1 1))
                     '((2 2) (1 1))
                     '((2 2) (3 3) (4 4) (1 1))
                     ))
             ))))
     (results))

   (ans-allTests))
 '((((define double
       (lambda (l)
         (match l
           (`() '())
           (`(,a unquote d)
            (cons (list a a) (double d))))))))))

(check-equal?
 (let ()
   (define (ans-allTests)
     (define (results)
       (run 1 (defns)
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
             (== `((define append
                     (lambda (l s)
                       (if (null? l)
                           s
                           (cons (car l) (append (cdr l) s)))))
                   (define reverse
                     (lambda (l)
                       (if (null? l)
                           '()
                           (append (reverse (cdr l)) ,B)))))
                 defns)

             ;; hint
             (== `(list ,C) B)
                 
             (fresh (_0 _1 _2 _3 _4 _5 _6)
               (numbero _0)
               (numbero _1)
               (numbero _2)
               (numbero _3)
               (numbero _4)
               (numbero _5)
               (numbero _6)
               (appendo defns
                        `(((lambda x x)
                               
                           ;; example inputs
                           (reverse '())
                           (reverse '(,_0))
                           (reverse '(,_1 ,_2))
                           (reverse '(,_3 ,_4 ,_5 ,_6))
                               
                           ))
                        begin-body))
                 
             (fresh (_0 _1 _2 _3 _4 _5 _6)
               (numbero _0)
               (numbero _1)
               (numbero _2)
               (numbero _3)
               (numbero _4)
               (numbero _5)
               (numbero _6)
               (evalo `(begin . ,begin-body)
                      (list
                         
                       ;; example outputs
                       `()
                       `(,_0)
                       `(,_1 ,_2)
                       `(,_3 ,_4 ,_5 ,_6)
                           
                       )))
             ))))
     (results))
   ;;
   (ans-allTests))
 '((((define append
       (lambda (l s)
         (if (null? l) s (cons (car l) (append (cdr l) s)))))
     (define reverse
       (lambda (l)
         (if (null? l) '() (append (reverse (cdr l)) (list _.0))))))
    ;; haha!
    (num _.0))))

#|
(check-equal?
 (let ()
   (define (ans-allTests)
     (define (results)
       (run 1 (defns)
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
             (== `((define append
                     (lambda (l s)
                       (if (null? l)
                           s
                           (cons (car l) (append (cdr l) s)))))
                   (define reverse
                     (lambda (l)
                       (if (null? l)
                           '()
                           (append (reverse (cdr l)) ,B)))))
                 defns)

             ;; hint
             (== `(list ,C) B)
                 
             (fresh (_0 _1 _2 _3 _4 _5 _6)
               (numbero _0)
               (numbero _1)
               (numbero _2)
               (numbero _3)
               (numbero _4)
               (numbero _5)
               (numbero _6)
               (appendo defns
                        `(((lambda x x)
                               
                           ;; example inputs
                           (reverse '())
                           (reverse '(,_0))
                           (reverse '(,_1 ,_2))
                           (reverse '(,_3 ,_4 ,_5 ,_6))
                               
                           ))
                        begin-body))
                 
             (fresh (_0 _1 _2 _3 _4 _5 _6)
               (numbero _0)
               (numbero _1)
               (numbero _2)
               (numbero _3)
               (numbero _4)
               (numbero _5)
               (numbero _6)
               (evalo `(begin . ,begin-body)
                      (list
                         
                       ;; example outputs
                       `()
                       `(,_0)
                       `(,_1 ,_2)
                       `(,_3 ,_4 ,_5 ,_6)
                           
                       )))
             ))))     
     (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
       (if (null? results-fast)
           (begin (set! allow-incomplete-search? #f) (results))
           results-fast)))
   ;;
   (ans-allTests))
 '((((define append
       (lambda (l s)
         (if (null? l) s (cons (car l) (append (cdr l) s)))))
     (define reverse
       (lambda (l)
         (if (null? l) '() (append (reverse (cdr l)) (list _.0))))))
    ;; haha!
    (num _.0))))
|#
