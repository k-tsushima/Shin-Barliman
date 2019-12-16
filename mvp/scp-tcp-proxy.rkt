#lang racket/base

; This is a code between mcp(client) and subprocess

(provide  
  (all-from-out racket/tcp)
  (all-defined-out))

(require racket/tcp)

; (define *program* (box #f))
; (define *tests* (box #f))
; (define *scm-files* (box #f))

(define *data* (box #f))

(print-as-expression #f)

(define (handle in out)
;  (printf "handle called\n")
  (let loop ((msg (read in)))
;    (printf "subprocess-client received message ~s\n" msg)
    (cond
      ((eof-object? msg)
       (write '(goodbye) out)
 ;      (printf "subprocess-client sent goodbye message\n")
 	)
      ((eq? msg 'finished)
       (loop (read in)))
      ((eq? (car msg) 'data-sending)
;       (set! *program* (cdr (car (cdr msg))))
;       (set! *tests* (cdr (car (cdr (cdr msg)))))
;       (set! *scm-files* (cdr (car (cdr (cdr (cdr msg))))))
        (set! *data* (cadr msg))
	(open-input-string "~s\n" (cdr *data*))
       (loop (read in)))
      (else 
;       (printf "error : ~s\n" msg)       
       (loop (read in))))))

(define (connect address port)
  (define-values (in out) (tcp-connect address port))
;  (printf "client writing hello message\n")
  (write '(hello) out)
  (flush-output out)
;  (printf "client wrote hello message\n")

  (handle in out)
;  (printf "program ~s\n" *program*)
;  (printf "tests ~s\n" *tests*)
;  (printf "scm-files ~s\n" *scm-files*)
  (printf "~s\n" *data*)
  (close-input-port in)
  (close-output-port out)
  )

(connect "localhost" 8081)

;; (connect "localhost" 8081)

;; > (require "tcp-client-for-subprocess.rkt")
;; > (connect "localhost" 8080)
