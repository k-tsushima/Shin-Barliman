#|

Message types sent and received from UI, MCP, SCP, Synthesis subprocesses

|#

;===================
UI
;===================

;--------------------
Received from MCP
;--------------------
(synthesizing)
(stopped)
(goodbye)

;--------------------
Sent to MCP
;--------------------
(synthesize (,definitions ,inputs ,outputs))
(stop)


;===================
MCP
;===================

;--------------------
Received from UI
;--------------------
(synthesize (,definitions ,inputs ,outputs))
(stop)

;--------------------
Sent to UI
;--------------------
(synthesizing)
(stopped)
(goodbye)

;--------------------
Received from SCP
;--------------------
(hello)

;--------------------
Sent to SCP
;--------------------
(scp-id ,scp-id) ;; scp-id is an integer
(synthesize ((,definitions ,inputs ,outputs ,synthesis-id) ...))
(stop-all-synthesis)
(stop-one-task ,synthesis-id)
(ping)


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
(unexpected-eof)
(unknown-message-type ,msg)

;--------------------
Sent to MCP
;--------------------
(hello)
(num-processes ,number-of-synthesis-subprocesses ,scp-id)
(synthesis-finished ,scp-id ,synthesis-id ,val ,statistics)
;; error messages sent to MCP (using error port):
(unexpected-eof)
(unknown-message-type ,msg)

;--------------------
Received from Synthesis subprocess
;--------------------
(stopped)
(synthesis-finished ,synthesis-id ,val ,statistics)
;; error messages sent to SCP (using error port):
(unexpected-eof)
(unknown-message-type ,msg)

;--------------------
Sent to Synthesis subprocess
;--------------------
(stop)
(synthesize (,definitions ,inputs ,outputs) ,synthesis-id)


;===================
Synthesis subprocess
;===================

;--------------------
Received from SCP
;--------------------
(ping)
(stop)
(synthesize (,definitions ,inputs ,outputs) ,synthesis-id)

;--------------------
Sent to SCP
;--------------------
(ping)
(stopped)
(synthesis-finished ,synthesis-id ,val ,statistics)
;; error messages sent to SCP:
(unexpected-eof)
(unknown-message-type ,msg)
