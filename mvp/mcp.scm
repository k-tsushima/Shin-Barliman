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


;; start Racket subprocesses for GUI TCP proxy and for SCP TCP proxy

;; event loop: check GUI proxy for messages, then check SCP proxy for messages
