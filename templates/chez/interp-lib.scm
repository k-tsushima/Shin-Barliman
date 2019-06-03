(library (interp-lib)
  (export parseo
          evalo
          extract-nameso
          appendo
          synthesize-from-template/input*/output*)
  (import (except (rnrs) condition)
          (mk-lib)
          (only (chezscheme) include printf gensym))

  (include "interp.scm")

  (define synthesize-from-template/input*/output*
    (lambda (template input* output*)
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
              (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body )
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
                                         (Z . ,Z))))))
			 (replace-G-vars-in-inputs/outputs
                          (lambda (in/outputs)
                            (cond
                              [(null? in/outputs)
                               in/outputs]
                              [(symbol? in/outputs)
                               (let ((sym (get-raw-G-symbol in/outputs)))
                                 (if sym
                                     (raw-G-symbol->logic-var sym)
                                     in/outputs))]
                              [(pair? in/outputs)
                               (cons (replace-G-vars-in-inputs/outputs (car in/outputs))
                                     (replace-G-vars-in-inputs/outputs (cdr in/outputs)))]
                              [else in/outputs])))
			 (get-raw-G-symbol
                          (lambda (sym)
                            (let ((str (symbol->string sym)))
			      (cond
			       [(and (= (string-length str) 2)
				     (equal? (string-ref str 0) #\G))
				(let ((c (string-ref str 1)))
				  (and (>= (char->integer c) (char->integer #\1))
				       (<= (char->integer c) (char->integer #\9))
				       (string->symbol (string-append "g" (list->string (list c))))))]
			       [(and (= (string-length str) 1)
				     (equal? (string-ref str 0) #\G))
				(let ((c (string (string-ref str 1) (string-ref str 2))))
				  (and (>= (string->number c) 10)
				       (<= (string->number c) 20)
				       (string->symbol (string-append "g" c))))]
			       [else #f]))))
			 (raw-G-symbol->logic-var
                          (lambda (raw-sym)
                            (cdr (assq raw-sym
                                       `((g1 . ,g1)
                                         (g2 . ,g2)
                                         (g3 . ,g3)
                                         (g4 . ,g4)
                                         (g5 . ,g5)
                                         (g6 . ,g6)
                                         (g7 . ,g7)
                                         (g8 . ,g8)
                                         (g9 . ,g9)
                                         (g10 . ,g10)
                                         (g11 . ,g11)
                                         (g12 . ,g12)
                                         (g13 . ,g13)
                                         (g14 . ,g14)
                                         (g15 . ,g15)
                                         (g16 . ,g16)
                                         (g17 . ,g17)
                                         (g18 . ,g18)
                                         (g19 . ,g19)
                                         (g20 . ,g20)
                                         )))))
			      
                             
			 )

                  (fresh ()


			 (let ((t (replace-?-vars-in-template template))
			       (output* (map replace-G-vars-in-inputs/outputs output*))
			       (input* (map replace-G-vars-in-inputs/outputs input*)))
                      (printf "*** original template:\n ~s\n\n" template)
                      (printf "*** updated template:\n ~s\n\n" t)
		      
		      (printf "*** updated outputs:\n ~s\n\n" output*)
		      (printf "*** updated inputs:\n ~s\n\n" input*)
		      
                              
                      (fresh ()
                        (== t defns)))
                                                
                    (appendo defns
                             `(((lambda x x) . ,input*))
                             begin-body)
                    (evalo `(begin . ,begin-body)
			   output*
                           )
                          
                    )

                  )))))
        (results))
              
      (ans-allTests)))
  
  )
