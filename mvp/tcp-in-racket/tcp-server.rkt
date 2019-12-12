#lang racket/base

(provide  
  (all-from-out racket/tcp)
  (all-defined-out))

(require racket/tcp)

(print-as-expression #f)

;; Adapted from https://docs.racket-lang.org/more/
;;
;; 4 “Hello World” Server and 5 which adds thread support
;;
;; The tutorial has other possible improvements, including
;; creating one thread per connection, and adding timeouts.
;; We may want to add these features later.

;; Might want to allow breaks using 'tcp-accept/enable-break' instead of 'tcp-accept', so the connection can be interrupted:

;; https://docs.racket-lang.org/reference/breakhandler.html?q=tcp

;; The `serve' function is revised to run the loop
;; in a thread, and it returns a function to shut down
;; down the server.

(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
      (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

;; The rest is the same as before.

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (printf "accepted listener connection\n")
  (handle in out)
  (close-input-port in)
  (close-output-port out))

(define (handle in out)
  (printf "handle called\n")
  (let loop ((msg (read in)))
    (printf "server received message ~s\n" msg)
    (cond
      ((eof-object? msg)
       (write '(goodbye) out)
       (printf "server sent goodbye message\n"))
      (else
       (loop (read in))))))

;; > (require "tcp-server.rkt")
;; > (define stop (serve 8081))
;; > (stop)
;; > (define stop (serve 8081))
;; > (stop)
;; > (define stop (serve 8081))
;; > (stop)
;; > (define stop (serve 8081))
