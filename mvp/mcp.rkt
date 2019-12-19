#lang racket

;; Shin-Barliman Main Control Process (MCP)

#|
Description of the Main Control Process.
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

(define *ui-table-box* (box #f))
(define *scp-table-box* (box #f))
(define *synthesis-task-table-box* (box #f))

(define (serve port-no)
  (define listener (tcp-listen port-no MAXIMUM-SIMULTANEOUS-CONNECTIONS #t))
  (define (loop)
    (when (tcp-accept-ready? listener)
      (accept listener))
    (handle)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

(define (accept listener)
  (define-values (in out) (tcp-accept listener))
  (printf "mcp: accepted listener connection\n")
  ;; add in & out to connection table
  )

(define (handle)
  (printf "mcp: handle called\n")
  (let loop ((msg (read in)))
    (printf "mcp: received message ~s\n" msg)
    (cond
      ((eof-object? msg)
       (write '(goodbye) out)
       (flush-output out)
       (printf "mcp: sent goodbye message\n"))
      (else
       (match msg
         (`(synthesize (,definitions ,inputs ,outputs))
          (write `(synthesizing) out)
          (flush-output out)
          (printf "mcp: sent synthesizing message\n")
          (loop (read in)))
         (`(stop)
          (write `(stopped) out)
          (flush-output out)
          (printf "mcp: sent stopped message\n")
          (loop (read in)))
         (else 'handle
               (format "unknown message type ~s" msg)))))))

;; > (require "mcp.rkt")
;; > (define stop (serve 8080))
;; > (stop)
;; > (define stop (serve 8080))
;; > (stop)
