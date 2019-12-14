#lang racket/base

;; Shin-Barliman Main Control Program (MCP)

#|
Description of the Main Control Program.
----------------------------------------

The MCP is responsible for coordinating communication between the
user interface (UI) and the sub-processes responsible for synthesis.

The MCP is also responsible for the policies and strategies used for
efficient synthesis.
|#

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

(define MAXIMUM-SIMULTANEOUS-CONNECTIONS 5)

(define (serve port-no)
  (define listener (tcp-listen port-no MAXIMUM-SIMULTANEOUS-CONNECTIONS #t))
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
    (printf "mcp received message ~s\n" msg)
    (cond
      ((eof-object? msg)
       (write '(goodbye) out)
       (printf "mcp sent goodbye message\n"))
      (else
       (loop (read in))))))

;; > (require "mcp.rkt")
;; > (define stop (serve 8080))
;; > (stop)
;; > (define stop (serve 8080))
;; > (stop)
;; > (define stop (serve 8080))
;; > (stop)
;; > (define stop (serve 8080))
