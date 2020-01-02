;; Internal tables kept by MCP and SCP

;===================
MCP
;===================

*scp-id* ;; integer counter in mcp-scp-tcp-proxy, protected by scp-id-semaphore

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


#|
;;; Due to the way the Racket proxies work (one thread per
;;; connection), I doubt these tables are actually needed:

ui-connections-table
(ui-id input-tcp-port output-tcp-port)

scp-connections-table
(scp-id num-processors input-tcp-port output-tcp-port)
|#


;===================
SCP
;===================

synthesis-subprocesses table
(synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr ,status) ;; status is 'free or 'working

synthesis-task table ;; the running tasks
(,synthesis-id ,subprocess-id ,definitions ,inputs ,outputs ,status) ;; status choices are...??? -> Currently only 'started

task-queue ;; the next work to do
((,definitions ,inputs ,outputs ,synthesis-id) ...)

stopping-list ;; the process-id of subprocesses which should be stopped
(,process-id ...)
