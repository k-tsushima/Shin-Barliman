(load "../scheme-helpers/pmatch.scm")
;(load "../chez/interp-lib.scm")
(import (template-tests-lib))

(define tag-exp
  (lambda (e c)
    (pmatch e
      [,n (guard (number? n)) `((Num ,n) ,c)]
      [,x (guard (symbol? x)) `((Var ,x) ,c)]
      [(cons ,e1 ,e2)
       (let ((e1^/c1 (tag-exp e1 c)))
	 (pmatch e1^/c1
	   [(,e1^ ,c1)
	    (let* ((c1^ (+ c1 1))
		   (e2^/c2 (tag-exp e2 c1^)))
		(pmatch e2^/c2
		  [(,e2^ ,c2)
		   (let ((c2^ (+ c2 1)))
		     `((Cons ,e1^/c1 ,e2^/c2) ,c2^))]))]))]
      ['() `(Empty ,c)]
      [(lambda (,x) ,body)
       (let ((e1^/c1 (tag-exp body c)))
	 (pmatch e1^/c1
	   [(,e1^ ,c1)
	    (let ((c1^ (+ c1 1)))
	      `((Fun ,x ,e1^/c1) ,c1^))]))]
      [(,e1 ,e2)
       (let ((e1^/c1 (tag-exp e1 c)))
	 (pmatch e1^/c1
	   [(,e1^ ,c1)
	    (let* ((c1^ (+ c1 1))
		   (e2^/c2 (tag-exp e2 c1^)))
	      (pmatch e2^/c2
		[(,e2^ ,c2)
		 (let ((c2^ (+ c2 1)))
		   `((App ,e1^/c1 ,e2^/c2) ,c2^))]))]))])))


(define lookup
  (lambda (env x)
    (pmatch env
      [() (error 'lookup (format "unbound variable: ~s" x ))]
      [((,k ,v ,env) ,rest)
       (cond
	 [(eq? k x) `(,v ,env)]
	 [else (lookup rest x)])])))

(define eval
  (lambda (e env c*)
    (pmatch e
      [((Num ,n) ,c) `((Num ,n) (,c . ,c*))]
      [((Var ,x) ,c)
       (let ((v/c* (lookup env x)))
	 (pmatch v/c*
	   [(,v ,c^*) `(,v (,c . ,(append c* c^*)))]))]
      [((Cons ,e1 ,e2) ,c)
       (let ((v1/c1* (eval e1 env c*)))
	 (pmatch v1/c1*
	   [(,v1 ,c1*)
	    (let ((v2/c2* (eval e2 env c*)))
	      (pmatch v2/c2*
		[(,v2 ,c2*) `((Cons ,v1/c1* ,v2/c2*) (,c . ,c*))]))]))]
      [(Empty ,c) `(Empty (,c . ,c*))]
      [((Fun ,x ,body) ,c) `((Closure ,x ,body ,env) (,c . ,c*))]
      [((App ,e1 ,e2) ,c)
       (let ((v1/c1* (eval e1 env c*))
	     (v2/c2* (eval e2 env c*)))
	 (pmatch v1/c1*
           [((Closure ,x ,body ,env) ,c3)
	    (pmatch v2/c2*
	      [(,v2 ,c2*)
	       ; (eval body `((,x . (,v2 (,c . ,(append c* c2* c3)))) ,env) c*)
	       (eval body `((,x . (,v2 ,(append c* c2* c3))) ,env) c*)])]))]

      )))

(define call-eval
  (lambda (e)
    (eval (tag-exp e 1) '() '())))

(define compare
  (lambda (intend eval true* false*)
    (pmatch intend
      [((Num ,n1) ,_)
       (pmatch eval
	 [((Num ,n2) ,c) (guard (eq? n1 n2))
	  `(,(append c true*) . ,false*)]
	 [(,otherwise ,c*) `(,true* ,(append c* false*))])]
;      After evaluation we do not have Var.
;      [((Var ,x1) ,_)
;       (pmatch eval
;	 [((Var ,x2) ,c) (guard (eq? x1 x2))
;	  `(,(append c true*) . ,false*)]
;	 [(,otherwise ,c*) `(,true* ,(append c* false*))])]
      [(Empty ,_)
       (pmatch eval
	 [(Empty ,c)
	  `(,(append c true*) . ,false*)]
	 [(,otherwise ,c*) `(,true* ,(append c* false*))])]
      [((Cons ,v1 ,v2) ,_)
       (pmatch eval
	 [((Cons ,v3 ,v4) ,c)
	  (let ((posneg (compare v1 v3 (append c true*) false*)))
	    (pmatch posneg
	      [(,pos . ,neg) (compare v2 v4 pos neg)]))]
	 [(,otherwise ,c*) `(,true* ,(append c* false*))])])))

(define call-compare
  (lambda (intend eval)
    (compare intend eval `('true) `('false))))

(define include
  (lambda (c target)
    (pmatch target
      ['() #f]
      [(,a . ,rest)
       (cond
	[(eq? a c) #t]
	[else (include c rest)])]
      [,a (eq? a c)])))
	  

(define revise-prog
  (lambda (prog true* false*)
    (pmatch prog
      [((Num ,n) ,c)
       (cond
	[(include c false*) ''A]
	[else `(Num ,n)])]
      [((Var ,x) ,c)
       (cond
	[(include c false*) ''A]
	[else `(Var ,x)])]
      [((Cons ,e1 ,e2) ,c)
       (cond
	[(include c false*) ''A]
	[else 
	 (let ((e1* (revise-prog e1 true* false*))
	       (e2* (revise-prog e2 true* false*)))
	   `(Cons ,e1* ,e2*))])]
      [((Fun ,x ,body) ,c)
       (cond
	[(include c false*) ''A]
	[else
	 (let ((body* (revise-prog body true* false*)))
	   `(Fun ,x ,body*))])]
      [((App ,e1 ,e2) ,c)
       (cond
	[(include c false*) ''A]
	[else
	 (let ((e1* (revise-prog e1 true* false*))
	       (e2* (revise-prog e2 true* false*)))
	   `(App ,e1* ,e2*))])]
)))

(define update-template*
  (lambda (intend prog input)
    (let ((test-prog `(,prog ,input))
	  (intend-prog (tag-exp intend 1)))
    (pmatch (call-compare intend-prog (call-eval test-prog))
      [(,true* ,false*)
       `(tag ,(tag-exp test-prog 1) eval ,(call-eval test-prog)
	     compare ,(call-compare intend-prog (call-eval test-prog))
	     result ,(revise-prog (tag-exp test-prog 1) true* false*))]))))

      

(define update-template
  (lambda (intend prog input)
    (let ((test-prog `(,prog ,input))
	  (intend-prog (tag-exp intend 1)))

    (pmatch (call-compare intend-prog (call-eval test-prog))
	[(,true* ,false*)
	 (revise-prog (tag-exp test-prog 1) true* false*)])
      )))


(define generalized-example
  (lambda (ori-input* ori-output* input* output*)
    ;; TODO
    `(,ori-input* ,ori-output*)))

; tagged exp -> scheme's exp
(define remove-tag
  (lambda (e)
    (pmatch e
      [((Num ,n) ,c) n]
      [((Var ,x) ,c) x]
      [((Cons ,e1^/c1 ,e2^/c2) ,c2^) `(cons ,(remove-tag e1^/c1) ,(remove-tag e2^/c2))]
      [(Empty ,c) '()]
      [((Fun ,x ,e1^/c1) ,c1^) `(lambda (,x) ,(remove-tag e1^/c1))]
      [((App ,e1^/c1 ,e2^/c2) ,c2^) `(,(remove-tag e1^/c1) ,(remove-tag e2^/c2))]
      )))

;; lack several funcions: generalized-example, synthesizer 
(define loop-synthesis
  (lambda (template ori-input* ori-output* input* output*)
    (let ((gexams* (generalized-example ori-input* ori-output* input* output*)))
      (let ((input* (car gexams*)) (output* (cdr gexams*)))
	(let ((prog (synthesize-from-template/input*/output* (remove-tag template) input* output*)))
	   (cond [(and (eq? ori-input* input*) (eq? ori-output* output*)) prog]
		 [else
		  (let ((temp* (update-template ori-output* prog ori-input*)))
		    (loop-synthesis temp* ori-input* ori-output* input* output*))
		  
		  ]))))))

(define first-template-generator
  (lambda (ori-input* ori-output* other-info)
    ; TODO
    '(lambda (x) (cons x x)
    )))

(define first-generalized-example
  (lambda (ori-input* ori-output*)
    ;; TODO
    `(,ori-input* ,ori-output*)))

;; lack functions: first-template-generator, first-generalized-example
(define main
  (lambda (ori-input* ori-output* other-info)
    (let ((template (first-template-generator ori-input* ori-output* other-info)))
      (let ((gexams* (first-generalized-example ori-input* ori-output*)))
	(pmatch gexams*
	  [(input* output*)
	   (loop-synthesis (tag-exp template 1) ori-input* ori-output* input* output*)])))))


;; lack functions: first-template-generator, first-generalized-example
(define main*
  (lambda (ori-input* ori-output* other-info)
    (let ((template (first-template-generator ori-input* ori-output* other-info)))
      (let ((gexams* (first-generalized-example ori-input* ori-output*)))
	(let ((input* (car gexams*)) (output* (cdr gexams*)))
	  (loop-synthesis (tag-exp template 1) ori-input* ori-output* input* output*)
	  )))))
