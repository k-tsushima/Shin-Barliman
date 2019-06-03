;;; please load this file from within
;;; Barliman/cocoa/Barliman/mk-and-rel-interp/

(load "chez-load-interp.scm")
(load "mk/test-check.scm")

#|
Start with smartest and most general skeleton (may need to use a gensym for 'v'):

(define double
  (lambda (l)
    (if (null? l)
        '()
        (let ((v (double (cdr l))))
          (,B . ,C)))))

as shown in the test "double with simplified structure and 'let' sekeleton with unknown arity and inferred base case"

The 'run' expression will generate a ground, overspecialized program:

(define double
  (lambda (l)
    (if (null? l)
        '()
        (let ((v (double (cdr l))))
          (cons '(1 1) v)))))

Then, we replace the overspecialized part of the answer, '(1 1), with a new logic variable:

(define double
  (lambda (l)
    (if (null? l)
        '()
        (let ((v (double (cdr l))))
          (cons ,A v)))))

and we can inline 'v' to avoid the interpretive overhead of having a let for no reason, now that we know the arity and argument order for the call:

(define double
  (lambda (l)
    (if (null? l)
        '()
        (cons ,A (double (cdr l))))))

Now, run the synthesis again on this new skeleton to produce the final program.



Can also use 'match' instead of 'if', which seems to give better performance for lists:

(define double
  (lambda (l)
    (match l
      (`() '())
      (`(,a . ,d)
       (let ((v (double d)))
         (,B . ,C))))))

which produces

(define double
  (lambda (l)
    (match l
      (`() '())
      (`(,a . ,d)
       (let ((v (double d)))
         (cons '(1 1) v))))))

which gives us the skeleton

(define double
  (lambda (l)
    (match l
      (`() '())
      (`(,a . ,d)
       (let ((v (double d)))
         (cons ,C v))))))

which we can inline (although it seems to very slightly degrade performance) to

(define double
  (lambda (l)
    (match l
      (`() '())
      (`(,a . ,d)
       (cons ,C (double d))))))

When we run a new query with this skeleton, we get the answer

(define double
  (lambda (l)
    (match l
      (`() '())
      (`(,a unquote d)
       (cons (list a a) (double d))))))

|#




#|
more thoughts

;; tail-recursive
(define double
  (lambda (l)
    (if (null? l)
        '()
        (double (cdr l)))))

;; not tail recusive
(define double
  (lambda (l)
    (if (null? l)
        '()
        (,op ,args1 ...  (double (cdr l)) ,args2 ...))))
        ;; and you know op must return a list, and you know the type of (double (cdr l)) is a list
        ;; Assume 'op' is one of the operators in the current environment
        ;; If we have type and arity information, should be able to pick the patterns that are consistent:
        (cons ,A (double (cdr l)))
        (cons (double (cdr l)) ,A)
        (append ,A (double (cdr l)))
        (append (double (cdr l)) ,A)
        ;; any more possibilities?

;; not tail recusive, with 'let'
(define double
  (lambda (l)
    (if (null? l)
        '()
        (let ((v (double (cdr l))))
          (,A . ,B)))))
|#


#|
;; reverse, tail recursive version.

(time
  (test "reverse 1st step"
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
                 (== `((define reverse
			 (lambda (l s)
			   (if (null? l)
			       ,A
			       (let ((k (cdr l)))
				 ,B)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (reverse '() '())
                             (reverse '(1) '())
                             (reverse '(1 1) '())
                             (reverse '(1 1 1) '())

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `(1)
                         `(1 1)
                         `(1 1 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define reverse
           (lambda (l s)
             (if (null? l)
                 s
                 (reverse (cdr l) (cons (car l) s)))))))))
  )

(time
  (test "reverse 2nd step"
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
                 (== `((define reverse
			 (lambda (l s)
			   (if (null? l)
			       s
			       ,B))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (reverse '() '())
                             (reverse '(1) '())
                             (reverse '(2 1) '())
                             (reverse '(1 2 3) '())

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `(1)
                         `(1 2)
                         `(3 2 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define reverse
           (lambda (l s)
             (if (null? l)
                 s
                 (reverse (cdr l) (cons (car l) s)))))))))
  )

(time
  (test "reverse one-step"
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
                 (== `((define reverse
			 (lambda (l s)
			   (if (null? l)
			       ,A
			       (let ((k (cdr l)))
				 ,B)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (reverse '() '())
                             (reverse '(1) '())
                             (reverse '(2 1) '())
                             (reverse '(1 2 3) '())

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `(1)
                         `(1 2)
                         `(3 2 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define reverse
           (lambda (l s)
             (if (null? l)
                 s
                 (reverse (cdr l) (cons (car l) s)))))))))
  )


#!eof

|#

#|

;; reverse, non-tail recursive version.

(time
  (test "reverse one step with actual examples"
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
			       ,A
			       (let ((k (reverse (cdr l))))
				 ,B)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (reverse '())
                             (reverse '(1))
                             (reverse '(2 1))
                             (reverse '(1 2 3))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `(1)
                         `(1 2)
                         `(3 2 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (cons (car l) (append (cdr l) s)))))
	 (define reverse
           (lambda (l)
             (if (null? l)
                 l
                 (append (reverse (cdr l)) (list (car l))))))))))
  )

(time
  (test "reverse 1st step"
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
			       ,A
			       (let ((k (reverse (cdr l))))
				 ,B)))))
		       defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (reverse '())
                             (reverse '(1))
                             (reverse '(1 1))
                             (reverse '(1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `(1)
                         `(1 1)
                         `(1 1 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (cons (car l) (append (cdr l) s)))))
	 (define reverse
           (lambda (l)
             (if (null? l)
                 l
                 (append (reverse (cdr l)) (list (car l))))))))))
  )

(time
  (test "reverse 2nd step"
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
			       l
			       (let ((k (reverse (cdr l))))
				 ,B)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (reverse '())
                             (reverse '(1))
                             (reverse '(2 1))
                             (reverse '(1 2 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `(1)
                         `(1 2)
                         `(1 2 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (cons (car l) (append (cdr l) s)))))
	 (define reverse
           (lambda (l)
             (if (null? l)
                 l
                 (append (reverse (cdr l)) (list (car l))))))))))
  )

|#

#|
(time
  (test "append one step"
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
			       ,A
			       (let ((k (cdr l)))
				 ,B)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(2))
                             (append '(3 4) '(5 6))
                             (append '(7 8 9) '(10 11 12))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '(1 2)
                         '(3 4 5 6)
                         '(7 8 9 10 11 12)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
		 ((let ((k (cdr l)))
		    (cons (car l) (append k s)))))))))))
  )


(time
  (test "append 1st step"
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
			       ,A
			       (let ((k (cdr l)))
				 ,B)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(1))
                             (append '(1 1) '(1 1))
                             (append '(1 1 1) '(1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '(1 1)
                         '(1 1 1 1)
                         '(1 1 1 1 1 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
		 (let
		     ((k (cdr l)))
		   (append k (cons '1 s))))))))))
  )


(time
  (test "append 2nd step"
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
			       (let ((k (cdr l)))
				 ,B)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(2))
                             (append '(3 4) '(5 6))
                             (append '(7 8 9) '(10 11 12))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '(1 2)
                         '(3 4 5 6)
                         '(7 8 9 10 11 12)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (cons (car l) (append k s)))))))))
  )

#!eof
|#

#|
(time
  (test "zip one-step with actual value"
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
                 (== `((define zip
			 (lambda (l s)
			   (if (and (null? l) (null? s))
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(1) '(2))
                             (zip '(3 4) '(5 6))
                             (zip '(7 8 9 10) '(11 12 13 14))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         `((1 2))
                         `((3 5) (4 6))
                         `((7 11) (8 12) (9 13) (10 14))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (and (null? l) (null? s))
                 s
                 (cons (list (car l) (car s)) (zip (cdr l) (cdr s))))))))))
  )
|#

(time
  (test "zip one-step with ,g1"
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
                 (== `((define zip
			 (lambda (l s)
			   (if (and (null? l) (null? s))
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(,g1) '(,g2))
                             (zip '(,g3 ,g4) '(,g5 ,g6))
                             (zip '(,g7 ,g8 ,g9 ,g10) '(,g11 ,g12 ,g13 ,g14))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         `((,g1 ,g2))
                         `((,g3 ,g5) (,g4 ,g6))
                         `((,g7 ,g11) (,g8 ,g12) (,g9 ,g13) (,g10 ,g14))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (and (null? l) (null? s))
                 s
                 (cons (list (car l) (car s)) (zip (cdr l) (cdr s))))))))))
  )

(time
  (test "zip 1st step"
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
                 (== `(
#|
		       (define zip
			 (lambda (l s)
			   (if (and (null? l) (null? s))
			       ,A
			       (let ((k (cdr l)) (j (cdr s)))
		       (,B . ,C)))))) |#
		       (define zip
           (lambda (l s)
             (if (null? l)
                 s
		 (let ((k (cdr l)) (j (cdr s)))
		   (cons `(list ,(car l) ,(car s)) (zip k j)))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(,g1) '(,g1))
                             (zip '(,g1 ,g1) '(,g1 ,g1))
                             (zip '(,g1 ,g1 ,g1) '(,g1 ,g1 ,g1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `((,g1 ,g1))
                         `((,g1 ,g1) (,g1 ,g1))
                         `((,g1 ,g1) (,g1 ,g1) (,g1 ,g1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (null? l)
                 s
		 (let ((k (cdr l)) (j (cdr s)))
		   (cons `(list ,(car l) ,(car s)) (zip k j))))))))))
  )


;; Why this does not return the correct one?
(time
  (test "zip 1st step"
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
                 (== `(
#|
		       (define zip
			 (lambda (l s)
			   (if (and (null? l) (null? s))
			       ,A
			       (let ((k (cdr l)) (j (cdr s)))
		       (,B . ,C)))))) |#
		       (define zip
           (lambda (l s)
             (if (null? l)
                 s
		 (let ((k (cdr l)) (j (cdr s)))
		   (cons `(list ,(car l) ,(car s)) (zip k j)))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(,g1) '(,g1))
                             (zip '(,g1 ,g1) '(,g1 ,g1))
                             (zip '(,g1 ,g1 ,g1) '(,g1 ,g1 ,g1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `((,g1 ,g1))
                         `((,g1 ,g1) (,g1 ,g1))
                         `((,g1 ,g1) (,g1 ,g1) (,g1 ,g1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (null? l)
                 s
		 (let ((k (cdr l)) (j (cdr s)))
		   (cons `(list ,(car l) ,(car s)) (zip k j))))))))))
  )

(time
  (test "zip 2nd step"
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
                 (== `((define zip
			    (lambda (l s)
			      (if (null? l)
				  ,A
				  (let ((k (cdr l)) (j (cdr s)))
				    (cons ,B (zip ,C ,D)))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(1) '(1))
                             (zip '(1 1) '(2 1))
                             (zip '(1 1 1) '(1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `((1 1))
                         `((1 2) (1 1))
                         `((1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (null? l)
                 s
		 (let ((k (cdr l)) (j (cdr s)))
		   (cons `(list ,(car l) ,(car s)) (zip k j))))))))))
  )


(time
  (test "zip 3rd step"
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
                 (== `((define zip
			    (lambda (l s)
			      (if (null? l)
				  s
				  (let ((k (cdr l)) (j (cdr s)))
				    (cons (list ,B (car s)) (zip ,C ,D)))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(1) '(1))
                             (zip '(3 1) '(2 4))
                             (zip '(1 1 1) '(1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `((1 1))
                         `((3 2) (1 4))
                         `((1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (null? l)
                 s
		 (let ((k (cdr l)) (j (cdr s)))
		   (cons `(list ,(car l) ,(car s)) (zip k j))))))))))
  )


#!eof



;; try to synthesize zip
(time
  (test "zip 2nd step"
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
                 (== `((define zip
			    (lambda (l s)
			      (if (null? l)
				  s
				  (cons ,A (zip (cdr l) (cdr s)))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(1) '(2))
                             (zip '(3 4) '(5 6))
                             (zip '(7 8 9) '(5 6 7))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         `()
                         `((1 2))
                         `((3 5) (4 6))
                         `((7 5) (8 6) (9 7))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (null? l)
                 s
                 (cons `(list ,(car l) ,(car s)) (zip (cdr l) (cdr s))))))))))
  )





;;; try with zip

(time
  (test "zip with simplified structure -- invents double, and ignores second argument!"
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
                 (== `((define zip
			 (lambda (l s)
			   (if (null? l)
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(1) '(1))
                             (zip '(1 1) '(1 1))
                             (zip '(1 1 1 1) '(1 1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 1))
                         '((1 1) (1 1))
                         '((1 1) (1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (null? l)
                 s
                 (cons '(1 1) (zip (cdr l) '())))))))))
  )

(time
  (test "zip with simplified structure, with double null? check -- invents double, and ignores first argument!"
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
                 (== `((define zip
			 (lambda (l s)
			   (if (and (null? l) (null? s))
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(1) '(1))
                             (zip '(1 1) '(1 1))
                             (zip '(1 1 1 1) '(1 1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 1))
                         '((1 1) (1 1))
                         '((1 1) (1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (and (null? l) (null? s))
                 s
                 (cons '(1 1) (zip '() (cdr s))))))))))
  )

(time
  (test "zip with simplified structure, with double null? check, and make the two lists contain different numbers -- still invents double!"
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
                 (== `((define zip
			 (lambda (l s)
			   (if (and (null? l) (null? s))
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(1) '(2))
                             (zip '(1 1) '(2 2))
                             (zip '(1 1 1 1) '(2 2 2 2))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 2))
                         '((1 2) (1 2))
                         '((1 2) (1 2) (1 2) (1 2))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (and (null? l) (null? s))
                 s
                 (cons '(1 2) (zip '() (cdr s))))))))))
  )

(time
  (test "zip with simplified structure, with double null? check, and make the two lists contain different numbers, and swap, match version -- with hint"
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
                 (== `((define zip
			 (lambda (l s)
                           (match (list l s)
                             (`(() ()) ,A)
                             (`((,la . ,ld) (,sa . ,sd))
                              (,B . ,C))))))
                     defns)

                 ;; hint
                 (== `(cons ,D (zip ld sd)) `(,B . ,C))                 
                 
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(1) '(2))
                             (zip '(2 2) '(1 1))
                             (zip '(1 1 1 1) '(2 2 2 2))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 2))
                         '((2 1) (2 1))
                         '((1 2) (1 2) (1 2) (1 2))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (match (list l s)
               (`(() ()) s)
               (`((,la . ,ld) (,sa . ,sd))
                (cons (list la sa) (zip ld sd))))))))))
  )

(time
  (test "zip with simplified structure, with double null? check, and make the two lists contain different numbers, and swap"
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
                 (== `((define zip
			 (lambda (l s)
			   (if (and (null? l) (null? s))
			       ,A
			       (,B . ,C)))))
                     defns)

                 ;; hint
                 (== `(cons ,D (zip (cdr l) (cdr s))) `(,B . ,C))                 
                 
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (zip '() '())
                             (zip '(1) '(2))
                             (zip '(2 2) '(1 1))
                             (zip '(1 1 1 1) '(2 2 2 2))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 2))
                         '((2 1) (2 1))
                         '((1 2) (1 2) (1 2) (1 2))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define zip
           (lambda (l s)
             (if (and (null? l) (null? s))
                 s
                 (cons (list (car l) (car s))
                       (zip (cdr l) (cdr s))))))))))
  )


#!eof



;;; try with append
(time
  (test "append with simplified structure, too simplified!!  gets a totally different template than we had intended, but actually kind of clever program!"
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
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(1))
                             (append '(1 1) '(1 1))
                             (append '(1 1 1) '(1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '(1 1)
                         '(1 1 1 1)
                         '(1 1 1 1 1 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (append (cdr l) (cons '1 s)))))))))
  )

(time
  (test "append with simplified structure, too simplified!!  once again gets a totally different template than we had intended, but clever!"
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
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(1))
                             (append '(1 1) '(1))
                             (append '(1) '(1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '(1 1)
                         '(1 1 1)
                         '(1 1 1 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (append (cdr l) (cons '1 s)))))))))
  )

(time
  (test "append with simplified structure, with two different numbers, but only one number per example, but still not as general as we would like"
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
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(1))
                             (append '(2 2) '(2))
                             (append '(1) '(1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '(1 1)
                         '(2 2 2)
                         '(1 1 1 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (append (cdr l) (cons (car s) s)))))))))
  )

(time
  (test "append with simplified structure, with two different numbers in each list, still not general enough!"
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
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(2))
                             (append '(1 1) '(2))
                             (append '(1) '(2 2 2))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '(1 2)
                         '(1 1 2)
                         '(1 2 2 2)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (append (cdr l) (cons '1 s)))))))))
  )

(time
  (test "append with simplified structure, with two different numbers in each list, reversing order, still not general enough!"
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
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(2))
                             (append '(2 2) '(1))
                             (append '(1) '(2 2 2))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '(1 2)
                         '(2 2 1)
                         '(1 2 2 2)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (append (cdr l) (cons (car l) s)))))))))
  )

(time
  (test "append with simplified structure, with two different numbers in each list, reversing order, with eigens in the second lists, still not general enough"
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
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(,g1))
                             (append '(2 2) '(,g2))
                             (append '(1) '(,g3 ,g4 ,g5))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         `(1 ,g1)
                         `(2 2 ,g2)
                         `(1 ,g3 ,g4 ,g5)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (append (cdr l) (cons (car l) s)))))))))
  )

(time
  (test "append with simplified structure, with two different numbers in each list, reversing order, with eigens in the second lists, with different numbers in the first list"
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
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(,g1))
                             (append '(1 2) '(,g2))
                             (append '(2 1) '(,g3 ,g4 ,g5))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         `(1 ,g1)
                         `(1 2 ,g2)
                         `(2 1 ,g3 ,g4 ,g5)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (cons (car l) (append (cdr l) s)))))))))
  )

(time
  (test "append with simplified structure, with two different numbers in each list, reversing order, with different numbers in the first list, fewer examples"
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
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(1) '(3))
                             (append '(1 2) '(4))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         `(1 3)
                         `(1 2 4)             
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (cons (car l) (append (cdr l) s)))))))))
  )







;;; Two step approach
;;; Part 1: test for double with simplified structure
;;; Aim: to find the structure of double.
(time
  (test "double with simplified structure"
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
			   (if (null? l)
			       ,A
			       (,B ,C ,D)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (double '())
                             (double '(1))
                             (double '(1 1))
                             (double '(1 1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 1))
                         '((1 1) (1 1))
                         '((1 1) (1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
	   (lambda (l)
	     (if (null? l)
		 l
		 (cons (list (car l) (car l)) (double (cdr l))))))
	 
         ))))
  )

;;; too slow!  arguments make the synthesis problem too hard for just generating a template:
;;; either use simple arguments, or go with eigen!
#|
(time
  (test "double with simplified structure with lists of numbers with no duplicate numbers"
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
			   (if (null? l)
			       ,A
			       (,B ,C ,D)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (double '())
                             (double '(1))
                             (double '(2 3))
                             (double '(4 5 6 7))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 1))
                         '((2 2) (3 3))
                         '((4 4) (5 5) (6 6) (7 7))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
	   (lambda (l)
	     (if (null? l)
		 l
		 (cons (list (car l) (car l)) (double (cdr l))))))
	 
         ))))
  )
|#

(time
  (test "double with simplified structure with eigens"
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
			   (if (null? l)
			       ,A
			       (,B ,C ,D)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (double '())
                             (double '(,g1))
                             (double '(,g2 ,g3))
                             (double '(,g4 ,g5 ,g6 ,g7))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         `((,g1 ,g1))
                         `((,g2 ,g2) (,g3 ,g3))
                         `((,g4 ,g4) (,g5 ,g5) (,g6 ,g6) (,g7 ,g7))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
	   (lambda (l)
	     (if (null? l)
		 l
		 (cons (list (car l) (car l)) (double (cdr l))))))
	 
         ))))
  )

(time
  (test "double with simplified structure, unknown arity for call"
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
			   (if (null? l)
			       ,A
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (double '())
                             (double '(1))
                             (double '(1 1))
                             (double '(1 1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 1))
                         '((1 1) (1 1))
                         '((1 1) (1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
	   (lambda (l)
	     (if (null? l)
		 l
		 (cons (list (car l) (car l)) (double (cdr l))))))
	 
         ))))
  )

(time
  (test "double with simplified structure, unknown arity for call, infer base case from examples"
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
			   (if (null? l)
			       '()
			       (,B . ,C)))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (double '())
                             (double '(1))
                             (double '(1 1))
                             (double '(1 1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 1))
                         '((1 1) (1 1))
                         '((1 1) (1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
	   (lambda (l)
	     (if (null? l)
		 l
		 (cons (list (car l) (car l)) (double (cdr l))))))
	 
         ))))
  )

(time
  (test "double with simplified structure, guessing recursion"
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
			   (if (null? l)
			       ,A
			       (,B ,C (double (cdr l)))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (double '())
                             (double '(1))
                             (double '(1 1))
                             (double '(1 1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 1))
                         '((1 1) (1 1))
                         '((1 1) (1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
	   (lambda (l)
	     (if (null? l)
		 l
		 (cons (list (car l) (car l)) (double (cdr l))))))
	 
         ))))
  )

(time
  (test "double with simplified structure and 'let' sekeleton"
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
			   (if (null? l)
			       ,A
                               (let ((v (double (cdr l))))
                                 (,B ,C ,D))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (double '())
                             (double '(1))
                             (double '(1 1))
                             (double '(1 1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 1))
                         '((1 1) (1 1))
                         '((1 1) (1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
	   (lambda (l)
	     (if (null? l)
		 l
		 (cons (list (car l) (car l)) (double (cdr l))))))
	 
         ))))
  )

(time
  (test "double with simplified structure and 'let' sekeleton with unknown arity and inferred base case"
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
			   (if (null? l)
			       '()
                               (let ((v (double (cdr l))))
                                 (,B . ,C))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (double '())
                             (double '(1))
                             (double '(1 1))
                             (double '(1 1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 1))
                         '((1 1) (1 1))
                         '((1 1) (1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
           (lambda (l)
             (if (null? l)
                 '()
                 (let ((v (double (cdr l))))
                   (cons '(1 1) v)))))))))
  )

(time
  (test "double with simplified structure and 'let' sekeleton with unknown arity and inferred base case, match version"
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
                              (let ((v (double d)))
                                 (,B . ,C)))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (double '())
                             (double '(1))
                             (double '(1 1))
                             (double '(1 1 1 1))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         '((1 1))
                         '((1 1) (1 1))
                         '((1 1) (1 1) (1 1) (1 1))
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
           (lambda (l)
             (match l
               (`() '())
               (`(,a . ,d)
                (let ((v (double d)))
                  (cons '(1 1) v))))))))))
  )

;;; Two step approach
;;; Part 2: Synthesis double code using actual values and
;;; the template that we obtained in part 1.
;;; Aim: to find the actual code of double.
;;; 
;;; Note!!: To obtain template, we need to work (compare the results and add holes in the programs).
;;; Currently it is made by hands.
(time
  (test "double with actual values and tamplate we obtained in part 1"
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
			   (if (null? l)
                               l
			       (cons ,C (double (cdr l)))))))
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
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
	   (lambda (l)
	     (if (null? l)
		 l
		 (cons (list (car l) (car l)) (double (cdr l))))))
	 
         ))))
 )


(time
  (test "double with actual values and tamplate we obtained in part 1, 'let' skeleton version"
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
                           (if (null? l)
                               l
                               (let ((v (double (cdr l))))
                                 (cons ,C v))))))
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
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
           (lambda (l)
             (if (null? l)
                 l
                 (let ((v (double (cdr l))))
                   (cons (if (null? v) '(1 1) (list (car l) (car l)))
                         v)))))))))
 )

(time
  (test "double with actual values and tamplate we obtained in part 1, 'let' skeleton version, match version"
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
                              (let ((v (double d)))
                                (cons ,C v)))))))
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
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
           (lambda (l)
             (match l
               (`() '())
               (`(,a unquote d)
                (let ((v (double d)))
                  (cons (list a a) v))))))))))
 )

(time
  (test "double with actual values and tamplate we obtained in part 1, 'let' skeleton version, inlined 'let', match version"
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
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
           (lambda (l)
             (match l
               (`() '())
               (`(,a unquote d)
                (cons (list a a) (double d))))))))))
 )

(time
  (test "double with actual values and 'let' skeleton"
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
                           (if (null? l)
                               l
                               (let ((v (double (cdr l))))
                                 (,B ,C v))))))
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
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
           (lambda (l)
             (if (null? l)
                 l
                 (let ((v (double (cdr l))))
                   (cons (list (car l) (car l)) v)))))))))
 )

;;; Another (one-step) approach: Synthesis double code without simplified example.
;;; Aim: to find the actual code of double
(time
  (test "double with actual values"
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
			   (if (null? l)
			       ,A
			       (,B ,C ,D)))))
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
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define double
	   (lambda (l)
	     (if (null? l)
		 l
		 (cons (list (car l) (car l)) (double (cdr l))))))
	 
         ))))
 )




;;; Kanae's idea:
;;;
;;; Try
;;; (reverse `(,_ ,_ ,_ ,_)) => `(,_ ,_ ,_ ,_)
;;; instead of
;;; (reverse `(1 2 3 4)) => `(4 3 2 1)
(time
  (test "reverse with simplified structure, part 1"
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
                               (append (reverse (cdr l)) (list (car l)))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (reverse '())
                             (reverse '(,g1))
                             (reverse '(,g3 ,g4))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         `(,g1)
                         `(,g4 ,g3)

                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (cons (car l) (append (cdr l) s)))))
         (define reverse
           (lambda (l)
             (if (null? l)
                 '()
                 (append (reverse (cdr l)) (list (car l))))))))))
 )


;;; part 2--use concrete numbers in the examples
;;;
;;; Kanae's idea:
;;;
;;; Try
;;; (reverse `(,_ ,_ ,_ ,_)) => `(,_ ,_ ,_ ,_)
;;; instead of
;;; (reverse `(1 2 3 4)) => `(4 3 2 1)
(time
  (test "reverse with simplified structure, part 2"
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
                               (append (reverse (cdr l)) (list (car l)))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (reverse '())
                             (reverse '(a))
                             (reverse '(b c))
                             (reverse '(1 2 3 4))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         `(a)
                         `(c b)
                         `(4 3 2 1)
                         ))
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (cons (car l) (append (cdr l) s)))))
         (define reverse
           (lambda (l)
             (if (null? l)
                 '()
                 (append (reverse (cdr l)) (list (car l))))))))))
 )

;;; part 3--uses examples with _'s
;;;
;;; Kanae's idea:
;;;
;;; Try
;;; (reverse `(,_ ,_ ,_ ,_)) => `(,_ ,_ ,_ ,_)
;;; instead of
;;; (reverse `(1 2 3 4)) => `(4 3 2 1)
(time
  (test "reverse with simplified structure, part 3"
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
                               (append (reverse (cdr l)) (list (car l)))))))
                     defns)

                 (fresh (_0 _1 _2 _3 _4 _5 _6)
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

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (cons (car l) (append (cdr l) s)))))
         (define reverse
           (lambda (l)
             (if (null? l)
                 '()
                 (append (reverse (cdr l)) (list (car l))))))))))
 )


;;; part 4--add holes to template, without being smart about
;;; generating template based on pattern of simplified input/output
;;; examples
;;;
;;; Kanae's idea:
;;;
;;; Try
;;; (reverse `(,_ ,_ ,_ ,_)) => `(,_ ,_ ,_ ,_)
;;; instead of
;;; (reverse `(1 2 3 4)) => `(4 3 2 1)
(time
  (test "reverse with simplified structure, part 4"
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
                               (append ,A ,B)))))
                     defns)

                 (fresh (_0 _1 _2 _3 _4 _5 _6)
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

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l) s (cons (car l) (append (cdr l) s)))))
         (define reverse
           (lambda (l)
             (if (null? l)
                 '()
                 ;;; haha!!
                 (append '() l))))))))
 )

;;; part 5--add holes to template, after being smarter about
;;; generating template based on pattern of simplified input/output
;;; examples
;;;
;;; Kanae's idea:
;;;
;;; Try
;;; (reverse `(,_ ,_ ,_ ,_)) => `(,_ ,_ ,_ ,_)
;;; instead of
;;; (reverse `(1 2 3 4)) => `(4 3 2 1)
(time
  (test "reverse with simplified structure, part 5"
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

                 (fresh (_0 _1 _2 _3 _4 _5 _6)
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

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l) s (cons (car l) (append (cdr l) s)))))
         (define reverse
           (lambda (l)
             (if (null? l)
                 '()
                 ;; haha!
                 (append (reverse (cdr l)) (car l)))))))))
 )


;;; interesting!
;;; a:  without a hint that B should be `(list ,C), synthesis takes a loong time.
;;; b:  even with that hint, the synthesized program is (list _.0), with the side condition
;;; (number _.0).

;;; part 6--add holes to template, after being smarter about
;;; generating template based on pattern of simplified input/output
;;; examples, and force the simplified input/output patterns to be numbers
;;;
;;; Kanae's idea:
;;;
;;; Try
;;; (reverse `(,_ ,_ ,_ ,_)) => `(,_ ,_ ,_ ,_)
;;; instead of
;;; (reverse `(1 2 3 4)) => `(4 3 2 1)
(time
  (test "reverse with simplified structure, part 6"
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

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l) s (cons (car l) (append (cdr l) s)))))
         (define reverse
           (lambda (l)
             (if (null? l) '() (append (reverse (cdr l)) (list _.0))))))
        ;; haha!
        (num _.0))))
 )
