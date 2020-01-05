#|

Message types sent and received from UI, MCP, SCP, Synthesis subprocesses

|#

;===================
UI
;===================

;--------------------
Received from MCP
;--------------------
(synthesizing ,synthesis-id)
(synthesis-finished ,synthesis-id ,val ,statistics)
(stopped)

;--------------------
Sent to MCP
;--------------------
(synthesize ,synthesis-id (,definitions ,inputs ,outputs))
(stop)


;===================
MCP
;===================

;--------------------
Received from UI
;--------------------
(synthesize ,synthesis-id (,definitions ,inputs ,outputs))
(stop)

;--------------------
Sent to UI
;--------------------
(synthesizing ,synthesis-id)
(synthesis-finished ,synthesis-id ,val ,statistics)
(stopped)

;--------------------
Received from SCP
;--------------------
(hello) ;; actually handled in mcp-scp-tcp-proxy, which sends the `(scp-id ,scp-id) message in response
(num-processes ,number-of-synthesis-subprocesses ,scp-id)
(synthesis-finished ,scp-id ,synthesis-id ,val ,statistics)
;; error messages sent to MCP (using error port):
(unexpected-eof) ;; ??? do we really need this message for the mvp?
(unknown-message-type ,msg) ;; ??? do we really need this message for the mvp?

;--------------------
Sent to SCP
;--------------------
(scp-id ,scp-id) ;; scp-id is an integer    Message is actually sent by mcp-scp-tcp-proxy
(synthesize ((,definitions ,inputs ,outputs ,synthesis-id) ...)) ;; actually sent by mcp-scp-tcp-proxy
(stop-all-synthesis) ;; Message is actually broadcast by mcp-scp-tcp-proxy
(stop-one-task ,synthesis-id) ;; Currently unimplemented


;===================
SCP
;===================

;--------------------
Received from MCP
;--------------------
(scp-id ,scp-id) ;; scp-id is an integer
(synthesize ((,definitions ,inputs ,outputs ,synthesis-id) ...))
(stop-all-synthesis)
(stop-one-task ,synthesis-id)
;; error messages sent to MCP (using error port):
(unexpected-eof) ;; ??? do we really need this message for the mvp?
(unknown-message-type ,msg) ;; ??? do we really need this message for the mvp?

;--------------------
Sent to MCP
;--------------------
(hello) ;; ??? do we really need this message for the mvp?
(num-processes ,number-of-synthesis-subprocesses ,scp-id)
(synthesis-finished ,scp-id ,synthesis-id ,val ,statistics)
;; error messages sent to MCP (using error port):
(unexpected-eof)
(unknown-message-type ,msg)

;--------------------
Received from Synthesis subprocess
;--------------------
(stopped-synthesis)
(synthesis-finished ,synthesis-id ,val ,statistics)
(status ,stat) ;; stat is either 'synthesizing or 'running
;; error messages sent to SCP (using error port):
(unexpected-eof)
(unknown-message-type ,msg)

;--------------------
Sent to Synthesis subprocess
;--------------------
(stop-synthesis)
(synthesize (,definitions ,inputs ,outputs) ,synthesis-id)
(get-status) ; when will we send this?


;===================
Synthesis subprocess
;===================

;--------------------
Received from SCP
;--------------------
(stop-synthesis)
(synthesize (,definitions ,inputs ,outputs) ,synthesis-id)
(get-status)

;--------------------
Sent to SCP
;--------------------
(stopped-synthesis)
(synthesis-finished ,synthesis-id ,val ,statistics)
(status ,stat) ;; stat is either 'synthesizing or 'running
;; error messages sent to SCP:
(unexpected-eof)
(unknown-message-type ,msg)
