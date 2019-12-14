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

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (printf "mcp: accepted listener connection\n")
  (handle in out)
  (close-input-port in)
  (close-output-port out))

(define (handle in out)
  (printf "mcp: handle called\n")
  (let loop ((msg (read in)))
    (printf "mcp: received message ~s\n" msg)
    (cond
      ((eof-object? msg)
       (write '(goodbye) out)
       (printf "mcp: sent goodbye message\n"))
      (else
       (loop (read in))))))

;; > (require "mcp.rkt")
;; > (define stop (serve 8080))
;; > (stop)
;; > (define stop (serve 8080))
;; > (stop)
