;; Internal tables kept by MCP and SCP

;===================
MCP
;===================

synthesis task queues (promote tasks from 'pending' to 'running' to 'finished'):

pending-synthesis-tasks
;; pending
(synthesis-task-id (definitions inputs outputs))

running-synthesis-tasks
;; running
(synthesis-task-id scp-id (definitions inputs outputs))

finished-synthesis-tasks
;; finished
(synthesis-task-id scp-id (definitions inputs outputs) results statistics)


ui-connections-table
(ui-id input-tcp-port output-tcp-port)

scp-connections-table
(scp-id num-processors input-tcp-port output-tcp-port)


;===================
SCP
;===================
