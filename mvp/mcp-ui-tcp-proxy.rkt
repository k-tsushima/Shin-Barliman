#lang racket/base

;; Proxy started by MCP for TCP communication with UI

;; This proxy uses a single-threaded architecture, and only supports 1
;; UI connection at a time.  We might want to relax this restriction
;; in the future.

;; Adapted from https://docs.racket-lang.org/more/

(require
  racket/tcp
  "common.rkt")

(provide  
  (all-from-out racket/tcp)
  (all-defined-out))


(print-as-expression #f)

;; Loading will occur at first use if not explicitly forced like this.
(load-config #f)

(define DEFAULT-TCP-PORT (config-ref 'ui-tcp-port))

;; MAX-CONNECTIONS is hard-coded, instead of user-configurable, since
;; the MVP currently only supports 1 UI connection at a time
(define MAX-CONNECTIONS 1)

(define PROGRAM-NAME "mcp-ui-tcp-proxy")

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
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (loop))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (logf "~a accepted tcp connection\n" PROGRAM-NAME)
  (handle in out)
  (logf "~a closing tcp connection\n" PROGRAM-NAME)
  (close-input-port in)
  (close-output-port out))

(define (forward-from-mcp-to-ui out)
  (lambda ()
    (let loop ((msg (read)))
      (logf "~a received message from mcp ~s\n" PROGRAM-NAME msg)
      (cond
        ((eof-object? msg)
         (logf "read eof from mcp--exiting forward-from-mcp-to-ui\n"))
        (else
         ;; forward message to UI
         (write msg out)
         (flush-output out)
         (logf "~a forwarded message to ui ~s\n" PROGRAM-NAME msg)
         (loop (read)))))))

(define (forward-from-ui-to-mcp in)
  (let loop ((msg (read in)))
    (logf "~a received message from ui ~s\n" PROGRAM-NAME msg)
    (cond
      ((eof-object? msg)
       (logf "read eof from ui--exiting forward-from-ui-to-mcp\n"))
      (else
       ;; forward message to MCP
       (write msg)
       (flush-output (current-output-port))
       (logf "~a forwarded message to mcp ~s\n" PROGRAM-NAME msg)
       (loop (read in))))))

(define (handle in out)
  (logf "handle called for ~a\n" PROGRAM-NAME)
  (logf "starting mcp-to-ui-thread\n")
  (define mcp-to-ui-thread (thread (forward-from-mcp-to-ui out)))
  (logf "mcp-to-ui-thread started\n")
  (logf "calling forward-from-ui-to-mcp\n")
  (forward-from-ui-to-mcp in)
  ;; perhaps should use a custodian for thread cleanup
  (logf "stopping mcp-to-ui-thread\n")
  (kill-thread mcp-to-ui-thread)
  (logf "stopped mcp-to-ui-thread\n"))

(serve DEFAULT-TCP-PORT)
