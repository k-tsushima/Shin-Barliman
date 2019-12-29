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

;; TODO add code here

(serve DEFAULT-TCP-PORT)
