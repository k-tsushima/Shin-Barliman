#lang racket/base

; Proxy started by MCP for TCP communication with one or more SCPs

(require
  racket/tcp
  "common.rkt")

(provide  
  (all-from-out racket/tcp)
  (all-defined-out))


(print-as-expression #f)

;; Loading will occur at first use if not explicitly forced like this.
(load-config #t)

(define DEFAULT-TCP-IP-ADDRESS (config-ref 'scp-tcp-ip-address))
(define DEFAULT-TCP-PORT (config-ref 'scp-tcp-port))

;; TODO add code here

(connect DEFAULT-TCP-IP-ADDRESS DEFAULT-TCP-PORT)

;; > (require "mcp-scp-tcp-proxy.rkt")
;; > (connect "localhost" 8081)
