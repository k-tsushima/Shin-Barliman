(eval '((lambda (y) (y y)) (lambda (z) z))
      (empty-env))

(define eval
  (lambda (expr env)
    (pmatch expr
      [,x (guard (symbol? x)) (apply-env env x)]
      [`(lambda (,x) ,body)
       (lambda (a)
         (eval body (extend-env x a env)))]
      [`(,e1 ,e2)
       ((eval e1 env) (eval e2 env))])))

(define eval
  (lambda (expr env)
    (pmatch expr
      [,x (guard (symbol? x)) (apply-env env x)]
      [`(lambda (,x) ,body)
       (lambda (a)
         (eval body (extend-env x a env)))]
      [`(,e1 ,e2)
       (let ((v1 (eval e1 env)))
         (v1 (eval e2 env)))])))

(define eval-anf
  (lambda (expr env)
    (pmatch expr
      [,x (guard (symbol? x)) (apply-env env x)]
      [`(lambda (,x) ,body)
       (lambda (a)
         (eval-anf body (extend-env x a env)))]
      [`(,e1 ,e2)
       (let ((v1 (eval-anf e1 env)))
         (let ((v2 (eval-anf e2 env)))
           (v1 v2)))])))


#!eof



;;; direct
(define !
  (lambda (n)
    (if (zero? n)
        1
        (* n (! (sub1 n))))))

;; ANF
(define !
  (lambda (n)
    (if (zero? n)
        1
        (let ((v (! (sub1 n))))
          (* n v)))))



#!eof

;;; trampolined style

(define trampoline
  (lambda (th)
    (trampoline (th))))

(define !-cps
  (lambda (n k)
    (lambda ()
      (if (zero? n)
          (k 1)
          (!-cps (sub1 n) (lambda (v)
                            (k (* n v))))))))
(call/cc (lambda (k)
           (trampoline (!-cps 5 k))))

#!eof

(define trampoline
  (lambda (th)
    (if (procedure? th)
        (trampoline (th))
        th)))

(define !-cps
  (lambda (n k)
    (lambda ()
      (if (zero? n)
          (k 1)
          (!-cps (sub1 n) (lambda (v)
                            (k (* n v))))))))

(trampoline (!-cps 5 (lambda (v) v)))

#!eof

(define !-cps
  (lambda (n k)
    (if (zero? n)
        (k 1)
        (!-cps (sub1 n) (lambda (v)
                          (k (* n v)))))))

(!-cps 5 (lambda (v) v))

#!eof

(define !
  (lambda (n)
    (lambda ()
      (if (zero? n)
          1
          (* n (! (sub1 n)))))))

(trampoline (! 5))

#!eof


;; direct style
(define !
  (lambda (n)
    (if (zero? n)
        1
        (* n (! (sub1 n))))))

#!eof


















(eval '((lambda (y) (y y)) (lambda (z) z))
      (lambda (x)
        (error 'lookup (format "unbound variable ~s" x))))

(define eval
  (lambda (expr env)
    (pmatch expr
      [,x (guard (symbol? x)) (env x)]
      [`(lambda (,x) ,body)
       (lambda (a)
         (eval body (lambda (y)
                      (if (eq? x y)
                          a
                          (env y)))))]
      [`(,e1 ,e2)
       ((eval e1 env) (eval e2 env))])))

;;; representation independence with respect environments

(define apply-env
  (lambda (env x)
    (env x)))

(define empty-env
  (lambda ()
    (lambda (x)
      (error 'lookup (format "unbound variable ~s" x)))))

(define extend-env
  (lambda (x a env)
    (lambda (y)
      (if (eq? x y)
          a
          (apply-env env y)))))

;;;;

(eval '((lambda (y) (y y)) (lambda (z) z))
      (empty-env))

(define eval
  (lambda (expr env)
    (pmatch expr
      [,x (guard (symbol? x)) (apply-env env x)]
      [`(lambda (,x) ,body)
       (lambda (a)
         (eval body (extend-env x a env)))]
      [`(,e1 ,e2)
       ((eval e1 env) (eval e2 env))])))


;;; registerization

(define *expr* 'whocares)
(define *env* 'whocares)

(define eval
  (lambda ()
    (pmatch expr
      [,x (guard (symbol? x)) (apply-env *env* x)]
      [`(lambda (,x) ,body)
       (lambda (a)
         (eval body (extend-env! x a *env*)))]
      [`(,e1 ,e2)
       ((eval e1 env) (eval e2 env))])))




















(define !
  (lambda (n)
    (if (zero? n)
        1
        (* n (! (sub 1))))))





(define !-cps
  (lambda (n k)
    (if (zero? n)
        (k 1)
        (!-cps (sub 1) (lambda (v)
                         (k (* n v)))))))

(!-cps 5 (lambda (v) v))



(define tramp
  (lambda ()
    (tramp)))

(define !-cps
  (lambda (n k)
    (if (zero? n)
        (k 1)
        (!-cps (sub 1) (lambda (v)
                         (k (* n v)))))))

(!-cps 5 (lambda (v) v))
