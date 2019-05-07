(define-syntax check-equal?
  (syntax-rules ()
    ((_ tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (or (equal? expected produced)
           (printf "~%check-equal? failed for expression:~%~a~%Expected value:~%~a~%Computed value:~%~a~%~%"
                   'tested-expression expected produced))))))
