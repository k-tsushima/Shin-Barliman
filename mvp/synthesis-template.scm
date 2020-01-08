;; TODO Is there a way to simplify the template's complicated use of quotate, quasiquote, and unquote?

(define (fill-in-template definitions inputs outputs)
  (let ((definitions (cons 'quasiquote (list definitions)))
        (inputs (cons 'quasiquote (list inputs)))
        (outputs (cons 'list outputs)))
    `(let ()
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

                 (== ,definitions defn-list)

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

               (== ,definitions defns)
               (appendo defns
                        (list (cons '(lambda x x) ,inputs))
                        begin-body)
               (evalo `(begin . ,begin-body)
                      ,outputs)))))
       (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
         (if (null? results-fast)
             (begin (set! allow-incomplete-search? #f) (results))
             results-fast)))))

#!eof

;; examples

(fill-in-template
 ;; definitions
 '((define ,A
     (lambda ,B
       ,C)))
 ;; inputs
 '((append '() '()))
 ;; outputs
 '(()))

(fill-in-template
 ;; definitions
 '((define append
     (lambda (l s)
       (if (null? l)
           ,A
           (cons ,B ,C)))))
 ;; inputs
 '((append '() '())
   (append '(a) '(b))
   (append '(c) '(d))
   (append '(e f) '(g h)))
 ;; outputs
 '(()
   (a b)
   (c d)
   (e f g h)))

(fill-in-template
 ;; definitions
 '((define append
     (lambda (l s)
       (if (null? l)
           ,A
           (cons ,B ,C)))))
 ;; inputs
 '((append '() '())
   (append '(,g1) '(,g2))
   (append '(,g3) '(,g4))
   (append '(,g5 ,g6) '(,g7 ,g8)))
 ;; outputs
 '(()
   (,g1 ,g2)
   (,g3 ,g4)
   (,g5 ,g6 ,g7 ,g8)))
