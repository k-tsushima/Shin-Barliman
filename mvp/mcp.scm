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

(load "pmatch.scm")
(load "common.scm")

;; Loading will occur at first use if not explicitly forced like this.
(load-config #t)

(define RACKET-BINARY-PATH (config-ref 'racket-binary-path))

(define CHEZ-BINARY-PATH (config-ref 'chez-binary-path))
(define CHEZ-FLAGS "-q")


;; start 'mcp-ui-tcp-proxy.rkt' Racket subprocess for UI TCP proxy

;; start 'mcp-scp-tcp-proxy.rkt' Racket subprocess for SCP TCP proxy

;; start 'synthesis-task-compiler.scm' Scheme subprocess for
;; generating code to be sent to SCP for synthesis

;; event loop: check GUI proxy for messages, then check SCP proxy for
;; messages, updating internal tables and sending messages as
;; necessary
