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

;--------------------
Received from SCP
;--------------------

;--------------------
Sent to SCP
;--------------------



;===================
SCP
;===================

;--------------------
Received from MCP
;--------------------

;--------------------
Sent to MCP
;--------------------

;--------------------
Received from Synthesis subprocess
;--------------------
(ping)
(stopped)
;; error messages sent to SCP:
(unexpected-eof)
(unknown-message-type ,msg)

;--------------------
Sent to Synthesis subprocess
;--------------------
(ping)
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
;; error messages sent to SCP:
(unexpected-eof)
(unknown-message-type ,msg)
