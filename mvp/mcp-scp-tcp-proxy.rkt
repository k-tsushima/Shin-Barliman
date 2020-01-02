#lang racket/base

;; Proxy started by MCP for TCP communication with one or more SCPs

;; Adapted from https://docs.racket-lang.org/more/

(require
  racket/tcp
  "common.rkt")

(provide  
  (all-from-out racket/tcp)
  (all-defined-out))


(print-as-expression #f)

;; Loading will occur at first use if not explicitly forced like this.
(load-config #t)

(define DEFAULT-TCP-PORT (config-ref 'scp-tcp-port))

(define MAX-CONNECTIONS (config-ref 'max-simultaneous-mcp/scp-connections))

(define PROGRAM-NAME "mcp-scp-tcp-proxy")

(define *scp-id* 0)
(define scp-id-semaphore (make-semaphore 1))

;; Table used to route messages from MCP to the correct SCP.
;; (,scp-id ,input-tcp-port ,output-tcp-port)
(define *scp-connections* '())
(define scp-connections-semaphore (make-semaphore 1))


#| begin infrastructure for ensuring reads and writes from/to stdin/stdout are atomic |#
(define stdin-semaphore (make-semaphore 1))
(define stdout-semaphore (make-semaphore 1))

(define (atomic-read)
  (call-with-semaphore stdin-semaphore (lambda () (read))))

(define (atomic-write/flush val)
  (call-with-semaphore
    stdout-semaphore
    (lambda ()
      (write val)
      (flush-output (current-output-port)))))
#| end infrastructure for ensuring reads and writes from/to stdin/stdout are atomic |#


#| begin logging infrastructure definitions (how to abstract this?) |#
(define ENABLE-LOGGING (config-ref 'enable-proxy-logging))
(define LOG-FILE-NAME (format "~a.log" PROGRAM-NAME))
(define LOG-FILE-OUTPUT-PORT-BOX (box #f))

;; semaphore code, to ensure logging is atomic, is adapted from
;; https://docs.racket-lang.org/guide/concurrency.html?q=semaphore#%28part._.Semaphores%29
(define log-output-semaphore (make-semaphore 1))

(define (logf format-str . args)
  (when ENABLE-LOGGING
    (call-with-semaphore
     log-output-semaphore
     (lambda ()
       (unless (unbox LOG-FILE-OUTPUT-PORT-BOX)
         (define output-port (open-output-file LOG-FILE-NAME
                                               #:mode 'text
                                               #:exists 'replace))
         (set-box! LOG-FILE-OUTPUT-PORT-BOX output-port))
       (apply fprintf (unbox LOG-FILE-OUTPUT-PORT-BOX) format-str args)
       (flush-output (unbox LOG-FILE-OUTPUT-PORT-BOX))))))
#| end logging infrastructure definitions |#


(logf "started ~a\n" PROGRAM-NAME)

(define (serve port-no)
  (logf "serve called\n")
  (define listener (tcp-listen port-no MAX-CONNECTIONS #t))
  (let loop ()
    (accept-and-handle listener)
    (loop)))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (thread
   (lambda ()
     (logf "~a accepted tcp connection\n" PROGRAM-NAME)
     ;; (sleep (random 10)) ; try uncommenting this
     (handle in out)
     (logf "~a closing tcp connection\n" PROGRAM-NAME)
     (close-input-port in)
     (close-output-port out))))

(define (establish-mcp-to-scp-handshake in out)
  (logf "establishing mcp-to-scp handshake...\n")
  (define hello-msg (read in))
  (when (or (eof-object? hello-msg)
            (not (equal? `(hello) hello-msg)))
    (logf "incorrect handshake message!  expected (hello), received ~s\n" hello-msg))
  (define scp-id #f)
  (call-with-semaphore
    scp-id-semaphore    
    (lambda ()
      (set! scp-id *scp-id*)
      (set! *scp-id* (add1 *scp-id*))))
  (logf "assigned scp-id ~s for newly connected SCP\n" scp-id)
  (write `(scp-id ,scp-id) out)
  (flush-output out)
  (logf "mcp-to-scp handshake established\n"))

(define (forward-from-mcp-to-scp out)
  (lambda ()
    (let loop ((msg (atomic-read)))
      (logf "~a received message from mcp ~s\n" PROGRAM-NAME msg)
      (cond
        ((eof-object? msg)
         (logf "read eof from mcp--exiting forward-from-mcp-to-scp\n"))
        (else
         ;; forward message to SCP
         (write msg out)
         (flush-output out)
         (logf "~a forwarded message to scp ~s\n" PROGRAM-NAME msg)
         (loop (atomic-read)))))))

(define (forward-from-scp-to-mcp in)
  (let loop ((msg (read in)))
    (logf "~a received message from scp ~s\n" PROGRAM-NAME msg)
    (cond
      ((eof-object? msg)
       (logf "read eof from scp--exiting forward-from-scp-to-mcp\n"))
      (else
       ;; forward message to MCP
       (atomic-write/flush msg)
       (logf "~a forwarded message to mcp ~s\n" PROGRAM-NAME msg)
       (loop (read in))))))

(define (handle in out)
  (logf "handle called for ~a\n" PROGRAM-NAME)
  (logf "establishing handshake...\n")
  (establish-mcp-to-scp-handshake in out)
  (logf "handshake established\n")
  (logf "starting mcp-to-scp-thread\n")
  (define mcp-to-scp-thread (thread (forward-from-mcp-to-scp out)))
  (logf "mcp-to-scp-thread started\n")
  (logf "calling forward-from-ui-to-mcp\n")
  (forward-from-ui-to-mcp in)
  ;; perhaps should use a custodian for thread cleanup
  (logf "stopping mcp-to-scp-thread\n")
  (kill-thread mcp-to-scp-thread)
  (logf "stopped mcp-to-scp-thread\n"))

(serve DEFAULT-TCP-PORT)
