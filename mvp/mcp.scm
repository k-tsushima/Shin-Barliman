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

(define MCP-UI-TCP-PROXY-FILE "mcp-ui-tcp-proxy.rkt")
(define MCP-SCP-TCP-PROXY-FILE "mcp-scp-tcp-proxy.rkt")
(define SYNTHESIS-TASK-COMPILER-FILE "synthesis-task-compiler.scm")

(define *ui-out-port-box* (box #f))
(define *ui-in-port-box* (box #f))
(define *ui-err-port-box* (box #f))
(define *ui-pid-box* (box #f))

(define *scp-out-port-box* (box #f))
(define *scp-in-port-box* (box #f))
(define *scp-err-port-box* (box #f))
(define *scp-pid-box* (box #f))

(define *synthesis-task-compiler-out-port-box* (box #f))
(define *synthesis-task-compiler-in-port-box* (box #f))
(define *synthesis-task-compiler-err-port-box* (box #f))
(define *synthesis-task-compiler-pid-box* (box #f))

#|
;; SCP info format:

(,scp-id
 ,num-processors
 ;; list of running synthesis tasks (initially empty), kept in synch
 ;; with `running-synthesis-tasks` table
 (,synthesis-task-id ...))
|#
(define *scp-info* '())


#|
Synthesis task queues (promote tasks from 'pending' to 'running' to 'finished'):
|#
;; (,synthesis-task-id (,definitions ,inputs ,outputs))
(define *pending-synthesis-tasks* '())

;; (,synthesis-task-id ,scp-id (,definitions ,inputs ,outputs))
(define *running-synthesis-tasks* '())

;; (,synthesis-task-id ,scp-id (,definitions ,inputs ,outputs) ,results ,statistics)
(define *finished-synthesis-tasks* '())


(define-syntax add-synthesis-task!
  (syntax-rules ()
    [(_ task table)
     (if (not (member task table))
         (begin
           (set! table (cons task table))
           (printf "added synthesis task ~s to table ~s to produce ~s\n" task 'table table))
         (begin
           (printf
            "*** uh oh!  task ~s already exists in table ~s with entries ~s -- refusing to add duplicate entry!\n"
            task 'table table)))]))

(define-syntax remove-synthesis-task!
  (syntax-rules ()
    [(_ task table)
     (if (member task table)
         (begin
           (set! table (remove task table))
           (printf "removed synthesis task ~s from table ~s to produce ~s\n" task 'table table))
         (begin
           (printf
            "*** uh oh!  task ~s doesn't exist in table ~s with entries ~s -- cannot remove entry!\n"
            task 'table table)))]))

(define-syntax remove-all-synthesis-tasks!
  (syntax-rules ()
    [(_ table)
     (begin
       (set! table '())
       (printf "removed all synthesis tasks from table ~s\n" 'table))]))


(define (start-subprocess! command to-stdin-box from-stdout-box from-stderr-box process-id-box)
  (printf "starting subprocess with command:\n~s\n" command)
  (let-values ([(to-stdin from-stdout from-stderr process-id)
                (open-process-ports command
                                    (buffer-mode block)
                                    (make-transcoder (utf-8-codec)))])
    (printf "started subprocess with process id ~s\n" process-id)
    (set-box! to-stdin-box to-stdin)
    (set-box! from-stdout-box from-stdout)
    (set-box! from-stderr-box from-stderr)
    (set-box! process-id-box process-id)))

(start-subprocess!
  (format "exec ~a ~a" MCP-UI-TCP-PROXY-FILE RACKET-BINARY-PATH)
  *ui-out-port-box*
  *ui-in-port-box*
  *ui-err-port-box*
  *ui-pid-box*)

(start-subprocess!
  (format "exec ~a ~a" MCP-SCP-TCP-PROXY-FILE RACKET-BINARY-PATH)
  *scp-out-port-box*
  *scp-in-port-box*
  *scp-err-port-box*
  *scp-pid-box*)

(start-subprocess!
  (format "exec ~a ~a ~a" CHEZ-BINARY-PATH CHEZ-FLAGS)
  *synthesis-task-compiler-out-port-box*
  *synthesis-task-compiler-in-port-box*
  *synthesis-task-compiler-err-port-box*
  *synthesis-task-compiler-pid-box*)

(define (handle-ui-messages)
  (define ui-in-port (unbox *ui-in-port-box*))
  (define ui-out-port (unbox *ui-out-port-box*))
  (define scp-out-port (unbox *scp-out-port-box*))
  (when (input-port-ready? ui-in-port)
    (let ((msg (read ui-in-port)))
      (cond
        ((eof-object? msg)
         (void))
        (else
         (printf "read message from ui: ~s\n" msg)
         (pmatch msg
           [(stop)
            (printf "writing stop-all-synthesis message\n")
            (write `(stop-all-synthesis) scp-out-port)
            (flush-output-port scp-out-port)
            (printf "wrote stop-all-synthesis message\n")
            (printf "removing all synthesis-task-ids from *scp-info* table\n")
            (set! *scp-info* (map (lambda (info)
                                    (pmatch info
                                      [(,scp-id ,num-processors ,synthesis-task-id*)
                                       `(,scp-id ,num-processors ())]))
                                  *scp-info*))
            (printf "removed all synthesis-task-ids from *scp-info* table\n")

            (remove-all-synthesis-tasks! *pending-synthesis-tasks*)
            (remove-all-synthesis-tasks! *running-synthesis-tasks*)
            
            (printf "writing stopped message to ui\n")
            (write `(stopped) ui-out-port)
            (flush-output-port ui-out-port)
            (printf "wrote stopped message to ui\n")]
           [(synthesize ,synthesis-id (,definitions ,inputs ,outputs))
            ;; TODO
            ;;
            ;; This is where the smarts go!
            ;;
            ;; Here is where the MCP generates multiple templates,
            ;; determines which SCPs to send synthesis tasks to,
            ;; updates the tables of running synthesis tasks, etc.
            ;; Some of these tasks will require calling the
            ;; synthesis-task-compiler, doing load-balancing, etc.  In
            ;; general, handling a single 'synthesize' message from
            ;; the UI might require creating/sending many 'synthesize'
            ;; messages to multiple SCPs.
            ;;
            ;; To begin with, we will do the simplest thing possible,
            ;; by sending a single synthesize message, with the
            ;; orginal definitions, inputs, and outputs, to the first
            ;; SCP in the table.  This will allow us to do simple
            ;; end-to-end testing.
            ;;
            ;; The MCP actually constructs a special 'synthesize'
            ;; message that is handled by the mcp-scp-tcp-proxy, which
            ;; then strips out some of the info when forwarding the
            ;; synthesize message to the SCP.
            (let loop ((scp-info *scp-info*))
              (pmatch scp-info
                (()

                 (add-synthesis-task! `(,synthesis-id (,definitions ,inputs ,outputs)) *pending-synthesis-tasks*)
                 
                 (printf "no SCPs available!  Added task to *pending-synthesis-tasks* table:\n~s\n"
                         *pending-synthesis-tasks*))
                (((,scp-id ,num-processors ,synthesis-task-id*) . ,rest)
                 (printf "scp ~s is using ~s of ~s processors\n"
                         scp-id (length synthesis-task-id*) num-processors)
                 (cond
                   [(> num-processors (length synthesis-task-id*))
                    (printf "found an scp with ~s free processors!\n"
                            (- num-processors (length synthesis-task-id*)))
                    (printf "sending synthesize message for mcp-scp-tcp-proxy to forward to scp\n")
                    (write `(synthesize ,scp-id ,synthesis-id (,definitions ,inputs ,outputs)) scp-out-port)
                    (flush-output-port scp-out-port)
                    (printf "sent synthesize message for mcp-scp-tcp-proxy to forward to scp\n")
                    (printf "sending synthesizing message to ui\n")
                    (write `(synthesizing ,synthesis-id) ui-out-port)
                    (flush-output-port ui-out-port)
                    (printf "sent synthesizing message to ui\n")

                    (set! *scp-info*
                          (cons `(,scp-id ,num-processors ,(cons synthesis-id synthesis-task-id*))
                                (remove `(,scp-id ,num-processors ,synthesis-task-id*) *scp-info*)))

                    (add-synthesis-task! `(,synthesis-id ,scp-id (,definitions ,inputs ,outputs)) *running-synthesis-tasks*)

                    ;; TODO hack to test multiple SCPs! remove!!!
                    (loop rest)
                    ;; TODO end of hack
                    ]
                   [else
                    (printf "no free processors for scp ~s--checking next scp\n" scp-id)
                    (loop rest)]))
                (,else (printf "unexpected *scp-info* table format: ~s\n" *scp-info*))))]
           [,else
            (printf "** unknown message type from ui: ~s\n" msg)]))))))

(define (handle-scp-messages)
  (define scp-in-port (unbox *scp-in-port-box*))
  (define scp-out-port (unbox *scp-out-port-box*))
  (define ui-out-port (unbox *ui-out-port-box*))
  (when (input-port-ready? scp-in-port)
    (let ((msg (read scp-in-port)))
      (cond
        ((eof-object? msg)
         (void))
        (else
         (printf "read message from scp: ~s\n" msg)
         (pmatch msg
           #|
           The `(hello) message is received, and the `(scp-id ,scp-id)
           message is sent, in mcp-scp-tcp-proxy.
           |#
           [(num-processes ,number-of-synthesis-subprocesses ,scp-id)
            ;; Add or update SCP/num-subprocesses info in the *scp-info* table
            (let ((pr (assoc scp-id *scp-info*)))
              (pmatch pr
                [(,scp-id ,old-num-processors ,synthesis-task-id*)
                 (set! *scp-info*
                       (cons `(,scp-id ,number-of-synthesis-subprocesses ,synthesis-task-id*)
                             (remove pr *scp-info*)))]
                [#f
                 (set! *scp-info*
                       (cons `(,scp-id ,number-of-synthesis-subprocesses ())
                             *scp-info*))]))
            (printf "updated *scp-info* table: ~s\n" *scp-info*)

            ;; TODO send pending tasks to the newly-connected SCP, up
            ;; to the number of processes that SCP is running.
            
            ]
           [(synthesis-finished ,scp-id ,synthesis-id ,val ,statistics)
            ;;
            (printf "removing synthesis-id from the list of running tasks for scp-id in *scp-info* table\n")
            (let ((pr (assoc scp-id *scp-info*)))
              (pmatch pr
                [(,scp-id ,num-processors ,synthesis-task-id*)
                 (set! *scp-info*
                       (cons `(,scp-id ,num-processors ,(remove synthesis-id synthesis-task-id*))
                             (remove pr *scp-info*)))
                 (printf "updated *scp-info* table: ~s\n" *scp-info*)]
                [#f (error 'synthesis-finished
                           (format "unexpected #f from (assoc scp-id *scp-info*): ~s ~s"
                                   scp-id *scp-info*))]))
            
            (let ((task (assoc synthesis-id *running-synthesis-tasks*)))
              (pmatch task
                [(,synthesis-id ,scp-id (,definitions ,inputs ,outputs))

                 (remove-synthesis-task! task *running-synthesis-tasks*)
                 
                 (add-synthesis-task!
                   `(,synthesis-id ,scp-id (,definitions ,inputs ,outputs) ,val ,statistics)
                   *finished-synthesis-tasks*)

                 ]
                [#f (error 'synthesis-finished
                           (format "unexpected #f from (assoc synthesis-id *running-synthesis-tasks*): ~s ~s"
                                   synthesis-id *running-synthesis-tasks*))]))
            
            (printf "writing synthesis-finished message to ui\n")
            (write `(synthesis-finished ,synthesis-id ,val ,statistics) ui-out-port)
            (flush-output-port ui-out-port)
            (printf "wrote synthesis-finished message to ui\n")
            ;;
            (printf "send the first pending synthesis task, if there is one, to the newly free processor:\n~s\n"
                    *pending-synthesis-tasks*)
            (pmatch *pending-synthesis-tasks*
              [()
               (printf "no pending synthesis tasks\n")
               (void)]
              [((,synthesis-id (,definitions ,inputs ,outputs)) . ,rest)
               (printf "first pending synthesis task is: ~s\n"
                       `(,synthesis-id (,definitions ,inputs ,outputs)))
               
               (remove-synthesis-task! `(,synthesis-id (,definitions ,inputs ,outputs)) *pending-synthesis-tasks*)
               (add-synthesis-task! `(,synthesis-id ,scp-id (,definitions ,inputs ,outputs)) *running-synthesis-tasks*)

               (let ((msg `(synthesize ,scp-id ,synthesis-id (,definitions ,inputs ,outputs))))
                 (printf "sending message ~s to scp ~s\n" msg scp-id)
                 (write msg scp-out-port)
                 (flush-output-port scp-out-port)
                 (printf "sent message ~s to scp ~s\n" msg scp-id))])]
           [,else
            (printf "** unknown message type from scp: ~s\n" msg)]))))))

(define (handle-synthesis-task-compiler-subprocess-messages)
  (define in-port (unbox *synthesis-task-compiler-in-port-box*))
  (when (input-port-ready? in-port)
    (let ((msg (read in-port)))
      (cond
        ((eof-object? msg)
         (void))
        (else
         (printf "read message from synthesis-task-compiler: ~s\n" msg)
         (pmatch msg
           ;;
           [,else
            (printf "** unknown message type from synthesis-task-compiler: ~s\n" msg)])))))  
  (void))

;; event loop: check GUI proxy for messages, then check SCP proxy for
;; messages, updating internal tables and sending messages as
;; necessary
(let loop ()
  (handle-ui-messages)
  (handle-scp-messages)
  (handle-synthesis-task-compiler-subprocess-messages)
  ;; Sleep for 10 ms (10 million nanoseconds) to avoid using 100% of
  ;; the CPU time checking if a new message has arrived.
  (let ((millisecond (expt 10 6)))
    (sleep (make-time 'time-duration (* 10 millisecond) 0)))
  (loop))
