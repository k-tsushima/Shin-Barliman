;; Internal tables kept by MCP and SCP

;===================
MCP
;===================

*scp-id* ;; integer counter in mcp-scp-tcp-proxy, protected by `scp-id-semaphore`


SCP table:

(,scp-id
 ,num-processors
 ;; list of running synthesis tasks (initially empty), kept in synch
 ;; with `running-synthesis-tasks` table
 (,synthesis-task-id ...))


Synthesis task queues (promote tasks from 'pending' to 'running' to 'finished'):

pending-synthesis-tasks
;; pending
(,synthesis-task-id (,definitions ,inputs ,outputs))

running-synthesis-tasks
;; running
(,synthesis-task-id ,scp-id (,definitions ,inputs ,outputs))

finished-synthesis-tasks
;; finished
(,synthesis-task-id ,scp-id (,definitions ,inputs ,outputs) ,results ,statistics)


;; This table is in mcp-scp-tcp-proxy.rkt
scp-connections
(scp-id input-tcp-port output-tcp-port)



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
