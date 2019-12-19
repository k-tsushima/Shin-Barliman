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

(define (handle tcp-in tcp-out)
  (printf "handle called\n")
  (let loop ((msg (read tcp-in)))
     (printf "subprocess-client received message from MCP ~s\n" msg)
     (write msg)
     (flush-output (current-output-port))
     (printf "subprocess-client sent message to SCP ~s\n" msg)
     (let ((msg (read (current-input-port))))
     	  (printf "subprocess-client received message from SCP ~s\n" msg)
     	  (write msg tcp-out)
	  (flush-output tcp-out)
	  (printf "subprocess-client sent message from MCP ~s\n" msg)
	       (loop (read tcp-in)))))


;   (cond
;     ((eof-object? msg)
;      (write '(goodbye) tcp-out)
;      (printf "subprocess-client sent goodbye message\n")
;	)	
;     ((eq? msg 'finished)
;      (loop (read tcp-in)))
;     ((eq? (car msg) 'data-sending)
;       (set! *program* (cdr (car (cdr msg))))
;       (set! *tests* (cdr (car (cdr (cdr msg)))))
;       (set! *scm-files* (cdr (car (cdr (cdr (cdr msg))))))
;        (set! *data* (cadr msg))
;	(open-input-string "~s\n" (cdr *data*))
;       (loop (read tcp-in)))
;      (else 
;       (printf "error : ~s\n" msg)       
;       (loop (read tcp-in))))))

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

;; > (require scp-tcp-proxy.rkt")
;; > (connect "localhost" 8080)
