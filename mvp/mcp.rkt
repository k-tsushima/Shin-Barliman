#lang racket

;; Shin-Barliman Main Control Process (MCP)

;; Uses separate TCP ports for UI and SCP connections.
;;
;; For example, UI might connect on port 8081, while
;; SCP connects on port 8082.

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

(define MAXIMUM-SIMULTANEOUS-UI-CONNECTIONS 1)
(define MAXIMUM-SIMULTANEOUS-SCP-CONNECTIONS 5)

(define mcp-semaphore (make-semaphore 1))

(define *synthesis-task-id-counter* (box 0))
(define *ui-connection-id-counter* (box 0))
(define *scp-connection-id-counter* (box 0))

(define *synthesis-tasks-table-box* (box '()))
(define *ui-connections-table-box* (box '()))
(define *scp-connections-table-box* (box '()))

;;; UI
(define (serve-ui port-no)
  (define listener (tcp-listen port-no MAXIMUM-SIMULTANEOUS-UI-CONNECTIONS #t))
  (define (loop)
    (accept-and-handle-ui listener)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

(define (accept-and-handle-ui listener)
  (define-values (in out) (tcp-accept listener))
  (printf "mcp: accepted ui listener connection\n")  
  (handle-ui in out)
  (close-input-port in)
  (close-output-port out))

(define (handle-ui in out)
  (printf "mcp: handle ui called\n")
  (let loop ((msg (read in)))
    (printf "mcp: received message from ui ~s\n" msg)
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
               (format "unknown message type from ui ~s" msg)))))))


;;; SCP
(define (serve-scp port-no)
  (define listener (tcp-listen port-no MAXIMUM-SIMULTANEOUS-SCP-CONNECTIONS #t))
  (define (loop)
    (accept-and-handle-scp listener)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

(define (accept-and-handle-scp listener)
  (define-values (in out) (tcp-accept listener))
  (define scp-connection-id #f)
  (printf "mcp: accepted scp listener connection\n")
  (call-with-semaphore mcp-semaphore
    (lambda ()
      (set! scp-connection-id (unbox *scp-connection-id-counter*))
      (box-set! *scp-connection-id-counter* (add1 scp-connection-id))))
  (register-scp-connection! scp-connection-id in out)
  (handle-scp scp-connection-id in out)
  (unregister-scp-connection! scp-connection-id)
  (close-input-port in)
  (close-output-port out))

(define (register-scp-connection! scp-connection-id in-port out-port)
  (call-with-semaphore mcp-semaphore
    (lambda ()
      (set-box! *scp-connections-table-box*
                (cons (list scp-connection-id in-port out-port)
                      (unbox *scp-connections-table-box*))))))

(define (handle-scp scp-connection-id in out)
  (printf "mcp: handle scp called for scp-connection-id ~s\n" scp-connection-id)
  (let loop ((msg (read in)))
    (printf "mcp: received message from scp ~s\n" msg)
    (cond
      ((eof-object? msg)
       (write '(goodbye) out)
       (flush-output out)
       (printf "mcp: sent goodbye message\n"))
      (else
       (match msg
         (`(hello)
          (write `(scp-id ,scp-connection-id) out)
          (flush-output out)
          (loop (read in)))
         (`(num-processes ,number-of-synthesis-subprocesses ,scp-id))
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
               (format "unknown message type from scp ~s" msg)))))))

;; > (require "mcp.rkt")
;; > (define stop-ui (serve-ui 8080))
;; > (define stop-scp (serve-scp 8080))
;; > (stop-ui)
;; > (stop-scp)
;; > (define stop-scp (serve-scp 8080))
;; > (stop-scp)
