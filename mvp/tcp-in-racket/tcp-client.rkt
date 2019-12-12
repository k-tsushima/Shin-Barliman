#lang racket/base

(provide  
  (all-from-out racket/tcp)
  (all-defined-out))

(require racket/tcp)

(print-as-expression #f)

(define (connect address port)
  (define-values (in out) (tcp-connect address port))
  (printf "client writing hello message\n")
  (write '(hello) out)
  (flush-output out)
  (printf "client wrote hello message\n")
  (define msg (read in))
  (printf "client received message ~s\n" msg)
  (close-input-port in)
  (close-output-port out)
  )

;; > (require "tcp-client.rkt")
;; > (connect "localhost" 8080)

