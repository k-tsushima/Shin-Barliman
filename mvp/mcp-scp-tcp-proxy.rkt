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

#| begin logging infrastructure definitions (how to abstract this?) |#
(define ENABLE-LOGGING (config-ref 'enable-proxy-logging))
(define LOG-FILE-NAME (format "~a.log" PROGRAM-NAME))
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
#| end logging infrastructure definitions |#


(logf "started ~a" PROGRAM-NAME)

(define (serve port-no)
  (logf "serve called\n")
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
      (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

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

(define (handle in out)
  (logf "handle called for ~a\n" PROGRAM-NAME)
  ;; TODO
  )


(serve DEFAULT-TCP-PORT)
