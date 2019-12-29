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
(load-config #t)

(define DEFAULT-TCP-PORT (config-ref 'ui-tcp-port))

;; MAX-CONNECTIONS is hard-coded, instead of user-configurable, since
;; the MVP currently only supports 1 UI connection at a time
(define MAX-CONNECTIONS 1)

(define ENABLE-LOGGING (config-ref 'enable-proxy-logging))

(define LOG-FILE-NAME "mcp-ui-tcp-proxy.log")

(define LOG-FILE-OUTPUT-PORT-BOX (box #f))

;; semaphore code, to ensure logging is atomic, is adapted from
;; https://docs.racket-lang.org/guide/concurrency.html?q=semaphore#%28part._.Semaphores%29
(define output-semaphore (make-semaphore 1))

(define (logf format-str . args)
  (when ENABLE-LOGGING
    (call-with-semaphore
     output-semaphore
     (lambda ()
       (unless (unbox LOG-FILE-OUTPUT-PORT-BOX)
         (define output-port (open-output-file LOG-FILE-NAME
                                               #:mode 'text
                                               #:exists 'replace))
         (set-box! LOG-FILE-OUTPUT-PORT-BOX output-port))
       (apply fprintf (unbox LOG-FILE-OUTPUT-PORT-BOX) format-str args)
       (flush-output (unbox LOG-FILE-OUTPUT-PORT-BOX))))))

(logf "started mcp-ui-tcp-proxy\n")

(define (serve port-no)
  (logf "serve called\n")
  (define listener (tcp-listen port-no MAX-CONNECTIONS #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (loop))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (logf "mcp-ui-tcp-proxy accepted tcp connection\n")
  (handle in out)
  (logf "mcp-ui-tcp-proxy closing tcp connection\n")
  (close-input-port in)
  (close-output-port out))

(define (forward-from-mcp-to-ui out)
  (lambda ()
    (let loop ((msg (read)))
      (logf "mcp-ui-tcp-proxy received message from mcp ~s\n" msg)
      (cond
        ((eof-object? msg)
         (logf "read eof from mcp--exiting forward-from-mcp-to-ui\n"))
        (else
         ;; forward message to UI
         (write msg out)
         (flush-output out)
         (logf "mcp-ui-tcp-proxy forwarded message to ui ~s\n" msg)
         (loop (read)))))))

(define (forward-from-ui-to-mcp in)
  (let loop ((msg (read in)))
    (logf "mcp-ui-tcp-proxy received message from ui ~s\n" msg)
    (cond
      ((eof-object? msg)
       (logf "read eof from ui--exiting forward-from-ui-to-mcp\n"))
      (else
       ;; forward message to MCP
       (write msg)
       (flush-output (current-output-port))
       (logf "mcp-ui-tcp-proxy forwarded message to mcp ~s\n" msg)
       (loop (read in))))))

(define (handle in out)
  (logf "handle called for mcp-ui-tcp-proxy\n")
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
