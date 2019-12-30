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

(define ENABLE-LOGGING (config-ref 'enable-proxy-logging))

(define LOG-FILE-NAME "mcp-scp-tcp-proxy.log")

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

(logf "started mcp-scp-tcp-proxy\n")



;; TODO add code here

(serve DEFAULT-TCP-PORT)
