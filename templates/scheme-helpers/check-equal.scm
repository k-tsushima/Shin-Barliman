;;; Simple Scheme test macro that is compatible with Racket's
;;; 'check-equal?'  syntax (but without supporting all of Racket's
;;; fancy testing infrastructure).
;;;
;;; This macro is adopted from an old test macro originally written by
;;; Oleg Kiselyov for Kanren, and modified by Dan Friedman and Will
;;; Byrd over the years.

(define-syntax check-equal?
  (syntax-rules ()
    ((_ tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (or (equal? expected produced)
           (printf "~%check-equal? failed for expression:~%~a~%Expected value:~%~a~%Computed value:~%~a~%~%"
                   'tested-expression expected produced))))))
