Scenario: MCP starts synthesis tasks, then tries to replace the tasks

*** Seen from the perspective of SCP ***

Start of scenario:

MCP and SCP are already started

SCP has already started 3 synthesis subprocesses

--------------------------------------------

MCP sends initial synthesis message to SCP:

(synthesize ((,definitions-1 ,inputs-1 ,outputs-1 ,synthesis-id-1)
             (,definitions-2 ,inputs-2 ,outputs-2 ,synthesis-id-2)
             (,definitions-3 ,inputs-3 ,outputs-3 ,synthesis-id-3)))

SCP adds these to *task-queue*, and will immediately call the `start-synthesis-with-free-subprocesses` function, resulting in invidual messages being sent to the 3 synthesis subprocesses:

(synthesize (,definitions-1 ,inputs-1 ,outputs-1) ,synthesis-id-1)

and

(synthesize (,definitions-2 ,inputs-2 ,outputs-2) ,synthesis-id-2)

and

(synthesize (,definitions-3 ,inputs-3 ,outputs-3) ,synthesis-id-3)

The synthesis subprocesses start working on their individual synthesis tasks.


Synthesis process 2 finishes its synthesis task, and sends a message to SCP:

(synthesis-finished ,synthesis-id-2 ,val-2 ,statistics-2)


Shortly thereafter, MCP tells SCP to stop synthesis by sending:

(stop-all-synthesis)

followed shortly by another synthesis message:

(synthesize ((,definitions-4 ,inputs-4 ,outputs-4 ,synthesis-id-4)
             (,definitions-5 ,inputs-5 ,outputs-5 ,synthesis-id-5)
             (,definitions-6 ,inputs-6 ,outputs-6 ,synthesis-id-6)))

-----------------------

When SCP receives (stop-all-synthesis) message, it sends (stop-synthesis) messages to all synthesis subprocesses.

SCP also removes all tasks in *task-queue*.

SCP then reads from the subprocesses until it receives a (stopped-synthesis) message from each subprocess.  SCP will discard the (synthesis-finished ,synthesis-id-2 ,val-2 ,statistics-2) message from synthesis process 2, since that mesage comes before the (stopped-synthesis) message.

Once SCP has received a (stopped-synthesis) message from each subprocess, it checks to see if there is another message from MCP.  There is:

(synthesize ((,definitions-4 ,inputs-4 ,outputs-4 ,synthesis-id-4)
             (,definitions-5 ,inputs-5 ,outputs-5 ,synthesis-id-5)
             (,definitions-6 ,inputs-6 ,outputs-6 ,synthesis-id-6)))

Then SCP will save the tasks to *task-queue*, and will immediately call the `start-synthesis-with-free-subprocesses` function, resulting in invidual messages being sent to the 3 synthesis subprocesses:

(synthesize (,definitions-4 ,inputs-4 ,outputs-4) ,synthesis-id-4)

and

(synthesize (,definitions-5 ,inputs-5 ,outputs-5) ,synthesis-id-5)

and

(synthesize (,definitions-6 ,inputs-6 ,outputs-6) ,synthesis-id-6)


SCP will continue checking if there are new messages from MCP.  If not, SCP will check for messages from synthesis subprocesses (both from std-err and std-out).

If a synthesis subprocess finishes, SCP will receive a (synthesis-finished ,synthesis-id ,val ,statistics) message.  SCP will update its internal *synthesis-task-table*, and forward the message to MCP.  SCP then calls the `start-synthesis-with-free-subprocesses` function again, which may then submit a new synthesis task from *task-queue* to the now idle synthesis subprocess.
