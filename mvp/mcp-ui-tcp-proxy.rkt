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

(define (serve port-no)
  (define listener (tcp-listen port-no MAX-CONNECTIONS #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (loop))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (close-input-port in)
  (close-output-port out))

(define (handle in out)
  ;; TODO
  (void)
  ;;
  )

(serve DEFAULT-TCP-PORT)
