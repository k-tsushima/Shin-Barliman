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
(load "common-chez.scm")

;; Loading configuration files will occur at first use if not
;; explicitly forced like this.
(load-config #t)

(define RACKET-BINARY-PATH (config-ref 'racket-binary-path))

(define CHEZ-BINARY-PATH (config-ref 'chez-binary-path))
(define CHEZ-FLAGS "-q") ;; '-q' (quiet) supresses the Chez startup
                         ;; banner, which would otherwise appear as a
                         ;; message sent from the subprocess

(define MCP-UI-TCP-PROXY-FILE "mcp-ui-tcp-proxy.rkt")
(define MCP-SCP-TCP-PROXY-FILE "mcp-scp-tcp-proxy.rkt")
(define SYNTHESIS-TASK-COMPILER-FILE "synthesis-task-compiler.scm")

(define *mcp-synthesis-id-counter* 0)

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
 ((,ui-synthesis-task-id ,mcp-synthesis-task-id) ...))
|#
(define *scp-info* '())


#|
Synthesis task queues (promote tasks from 'pending' to 'running' to 'finished'):
|#
;; ((,ui-synthesis-task-id ,mcp-synthesis-task-id) (,definitions ,inputs ,outputs))
(define *pending-synthesis-tasks* '())

;; ((,ui-synthesis-task-id ,mcp-synthesis-task-id) ,scp-id (,definitions ,inputs ,outputs))
(define *running-synthesis-tasks* '())

;; ((,ui-synthesis-task-id ,mcp-synthesis-task-id) ,scp-id (,definitions ,inputs ,outputs) ,results ,statistics)
(define *finished-synthesis-tasks* '())

(define-syntax write/flush
  (syntax-rules ()
    [(_ msg out-port)
     (begin
       (write msg out-port)
       (flush-output-port out-port)
       (printf "wrote msg to ~s:\n\n~s\n\n\n" 'out-port msg))]))

(define (increment-mcp-synthesis-id-counter!)
  (set! *mcp-synthesis-id-counter* (add1 *mcp-synthesis-id-counter*)))

(define print-scp-info-entry
  (lambda (scp-info-entry . args)
    (let ((prefix-str (if (= (length args) 1) (car args) "")))
      (pmatch scp-info-entry
        [(,scp-id ,num-processors ,ui/mcp-synthesis-task-id*)
         (printf "~a(" prefix-str)
         (printf "~s ;; scp-id\n" scp-id)
         (printf "~a ~s ;; num-processors\n\n" prefix-str num-processors)
         (printf "~a ;; ui/mcp-synthesis-task-id*:\n" prefix-str)
         (printf "~a ~s\n" prefix-str ui/mcp-synthesis-task-id*)
         (printf "~a)\n" prefix-str)]
        [else
         (printf "*** unexpected scp-info-entry passed to print-scp-info-entry:\n\n~s\n\n" scp-info-entry)]))))

(define (print-scp-info-table)
  (printf "(\n\n")
  (for-each
    (lambda (e)
      (print-scp-info-entry e "  ")
      (printf "\n"))
    *scp-info*)
  (printf ")\n"))

(define remove-all-synthesis-task-ids-from-scp-table!
  (lambda ()
    (set! *scp-info* (map (lambda (info)
                            (pmatch info
                              [(,scp-id ,num-processors ,ui/mcp-synthesis-task-id*)
                               `(,scp-id ,num-processors ())]))
                          *scp-info*))
    (printf "removed all synthesis-task-ids from *scp-info* table\n\n")
    (printf "updated *scp-info* table:\n\n")
    (print-scp-info-table)
    (printf "\n\n")))

(define remove-ui/mcp-synthesis-id-for-scp-in-scp-table!
  (lambda (ui/mcp-synthesis-id scp-id)
    (let ((scp-info-entry (assoc scp-id *scp-info*)))
      (pmatch scp-info-entry
        [(,scp-id ,num-processors ,ui/mcp-synthesis-task-id*)
         (set! *scp-info*
               (cons `(,scp-id ,num-processors ,(remove ui/mcp-synthesis-id ui/mcp-synthesis-task-id*))
                     (remove scp-info-entry *scp-info*)))
         (printf "removed ui/mcp-synthesis-id ~s from ui/mcp-synthesis-task-id* for scp ~s in *scp-info* table\n\n"
                 ui/mcp-synthesis-id scp-id)
         (printf "updated *scp-info* table:\n\n")
         (print-scp-info-table)
         (printf "\n\n")]
        [#f (printf "*** tried to remove ui/mcp-synthesis-id ~s from scp-id ~s, but no entry for scp-id ~s found in *scp-info* table:\n~s"
                    ui/mcp-synthesis-id scp-id scp-id *scp-info*)]))))

(define add/update-num-processors-for-scp-in-scp-table!
  (lambda (scp-id num-processors)
    (let ((scp-info-entry (assoc scp-id *scp-info*)))
      (pmatch scp-info-entry
        [(,scp-id ,old-num-processors ,ui/mcp-synthesis-task-id*)
         (printf "updating scp info for scp-id ~s, from ~s processors to ~s processors\n\n"
                 scp-id old-num-processors num-processors)
         (set! *scp-info*
               (cons `(,scp-id ,num-processors ,ui/mcp-synthesis-task-id*)
                     (remove scp-info-entry *scp-info*)))]
        [#f
         (printf "adding scp info for new scp-id ~s, with ~s processors\n\n"
                 scp-id num-processors)
         (set! *scp-info*
               (cons `(,scp-id ,num-processors ())
                     *scp-info*))])
      (printf "updated *scp-info* table:\n\n")
      (print-scp-info-table)
      (printf "\n\n"))))

(define (add-ui/mcp-synthesis-id-to-scp-in-scp-table! ui/mcp-synthesis-id scp-id)
  (let ((scp-info-entry (assoc scp-id *scp-info*)))
    (pmatch scp-info-entry
      [(,scp-id ,num-processors ,ui/mcp-synthesis-task-id*)
       (printf "adding ui/mcp-synthesis-id ~s to ui/mcp-synthesis-id* in scp info for scp-id ~s\n\n"
               ui/mcp-synthesis-id scp-id)
       (set! *scp-info*
             (cons `(,scp-id ,num-processors ,(cons ui/mcp-synthesis-id ui/mcp-synthesis-task-id*))
                   (remove scp-info-entry *scp-info*)))
       (printf "updated *scp-info* table:\n\n")
       (print-scp-info-table)
       (printf "\n\n")]
      [#f
       (printf "*** unable to add ui/mcp-synthesis-id ~s to ui/mcp-synthesis-id* in scp info for scp-id ~s, because there was no entry fro scp-id ~s" ui/mcp-synthesis-id scp-id scp-id)])))


(define print-task
  (lambda (task . args)
    (let ((prefix-str (if (= (length args) 1) (car args) "")))
      (pmatch task
        [((,ui-synthesis-task-id ,mcp-synthesis-task-id) (,definitions ,inputs ,outputs))
         (printf "~a(\n" prefix-str)
         (printf "~a (\n" prefix-str)
         (printf "~a  ~s ;; ui-synthesis-task-id\n" prefix-str ui-synthesis-task-id)
         (printf "~a  ~s ;; mcp-synthesis-task-id\n" prefix-str mcp-synthesis-task-id)
         (printf "~a )\n" prefix-str)
         (printf "~a (\n" prefix-str)
         (printf "~a  ;; definitions:\n" prefix-str)
         (printf "~a  ~s\n\n" prefix-str definitions)
         (printf "~a  ;; inputs:\n" prefix-str)
         (printf "~a  ~s\n\n" prefix-str inputs)
         (printf "~a  ;; outputs:\n" prefix-str)
         (printf "~a  ~s\n" prefix-str outputs)
         (printf "~a )\n" prefix-str)
         (printf "~a)\n" prefix-str)]
        [((,ui-synthesis-task-id ,mcp-synthesis-task-id) ,scp-id (,definitions ,inputs ,outputs))
         (printf "~a(\n" prefix-str)
         (printf "~a (\n" prefix-str)
         (printf "~a  ~s ;; ui-synthesis-task-id\n" prefix-str ui-synthesis-task-id)
         (printf "~a  ~s ;; mcp-synthesis-task-id\n" prefix-str mcp-synthesis-task-id)
         (printf "~a )\n" prefix-str)
         (printf "~a ~s ;; scp-id\n\n" prefix-str scp-id)
         (printf "~a (\n" prefix-str)
         (printf "~a  ;; definitions:\n" prefix-str)
         (printf "~a  ~s\n\n" prefix-str definitions)
         (printf "~a  ;; inputs:\n" prefix-str)
         (printf "~a  ~s\n\n" prefix-str inputs)
         (printf "~a  ;; outputs:\n" prefix-str)
         (printf "~a  ~s\n" prefix-str outputs)
         (printf "~a )\n" prefix-str)
         (printf "~a)\n" prefix-str)]
        [((,ui-synthesis-task-id ,mcp-synthesis-task-id) ,scp-id (,definitions ,inputs ,outputs) ,results ,statistics)
         (printf "~a(\n" prefix-str)
         (printf "~a (\n" prefix-str)
         (printf "~a  ~s ;; ui-synthesis-task-id\n" prefix-str ui-synthesis-task-id)
         (printf "~a  ~s ;; mcp-synthesis-task-id\n" prefix-str mcp-synthesis-task-id)
         (printf "~a )\n" prefix-str)
         (printf "~a ~s ;; scp-id\n\n" prefix-str scp-id)
         (printf "~a (\n" prefix-str)
         (printf "~a  ;; definitions:\n" prefix-str)
         (printf "~a  ~s\n\n" prefix-str definitions)
         (printf "~a  ;; inputs:\n" prefix-str)
         (printf "~a  ~s\n\n" prefix-str inputs)
         (printf "~a  ;; outputs:\n" prefix-str)
         (printf "~a  ~s\n" prefix-str outputs)
         (printf "~a )\n\n" prefix-str)
         (printf "~a ;; results\n" prefix-str)
         (printf "~a ~s\n\n" prefix-str results)
         (printf "~a ;; statistics\n" prefix-str)
         (printf "~a ~s\n" prefix-str statistics)
         (printf "~a)\n" prefix-str)]
        [else
         (printf "*** unexpected task format passed to print-task:\n\n~s\n\n" task)]))))

(define print-synthesis-task-table
  (lambda (table)
    (printf "(\n\n")
    (for-each
      (lambda (e)
        (print-task e "  ")
        (printf "\n"))
      table)
    (printf ")\n")))


(define-syntax add-synthesis-task!
  (syntax-rules ()
    [(_ task table)
     (if (not (member task table))
         (begin
           (set! table (cons task table))
           (printf "added to the ~s table the synthesis task:\n\n" 'table)
           (print-task task)
           (printf "\nto produce the updated ~s table:\n\n" 'table)
           (print-synthesis-task-table table)
           (printf "\n\n"))
         (begin
           (printf "*** uh oh!  synthesis task:\n~s\nalready exists in ~s table with entries:\n~s\n"
                   task 'table table)
           (printf "*** refusing to add duplicate entry!\n\n")))]))

(define-syntax remove-synthesis-task!
  (syntax-rules ()
    [(_ task table)
     (if (member task table)
         (begin
           (set! table (remove task table))
           (printf "removed from the ~s table the synthesis task:\n\n" 'table)
           (print-task task)
           (printf "\nto produce the updated ~s table:\n\n" 'table)
           (print-synthesis-task-table table)
           (printf "\n\n"))
         (begin
           (printf
            "*** uh oh!  synthesis task:\n~s\ndoesn't exist in ~s table with entries:\n~s\n"
            task 'table table)
           (printf "*** cannot remove entry!\n\n")))]))

(define-syntax remove-all-synthesis-tasks!
  (syntax-rules ()
    [(_ table)
     (begin
       (set! table '())
       (printf "removed all synthesis tasks from the ~s table\n\n" 'table))]))

(define (find-scp-with-free-processor)
  (let loop ((scp-info *scp-info*))
    (pmatch scp-info
      (() #f)
      (((,scp-id ,num-processors ,ui/mcp-synthesis-task-id*) . ,rest)       
       (cond
         [(> num-processors (length ui/mcp-synthesis-task-id*))
          scp-id]
         [else (loop rest)]))
      (,else (printf "unexpected *scp-info* table format: ~s\n\n" *scp-info*)))))


(define (start-subprocess! command to-stdin-box from-stdout-box from-stderr-box process-id-box)
  (printf "starting subprocess with command:\n~s\n" command)
  (let-values ([(to-stdin from-stdout from-stderr process-id)
                (open-process-ports command
                                    (buffer-mode block)
                                    (make-transcoder (utf-8-codec)))])
    (printf "started subprocess with process id ~s\n\n" process-id)
    (set-box! to-stdin-box to-stdin)
    (set-box! from-stdout-box from-stdout)
    (set-box! from-stderr-box from-stderr)
    (set-box! process-id-box process-id)))

(newline)

(start-subprocess!
  (format "exec ~a ~a" RACKET-BINARY-PATH MCP-UI-TCP-PROXY-FILE)
  *ui-out-port-box*
  *ui-in-port-box*
  *ui-err-port-box*
  *ui-pid-box*)

(start-subprocess!
  (format "exec ~a ~a" RACKET-BINARY-PATH MCP-SCP-TCP-PROXY-FILE)
  *scp-out-port-box*
  *scp-in-port-box*
  *scp-err-port-box*
  *scp-pid-box*)

(start-subprocess!
  (format "exec ~a ~a ~a" CHEZ-BINARY-PATH CHEZ-FLAGS SYNTHESIS-TASK-COMPILER-FILE)
  *synthesis-task-compiler-out-port-box*
  *synthesis-task-compiler-in-port-box*
  *synthesis-task-compiler-err-port-box*
  *synthesis-task-compiler-pid-box*)

(define (handle-ui-messages)
  (define ui-in-port (unbox *ui-in-port-box*))
  (define ui-out-port (unbox *ui-out-port-box*))
  (define scp-out-port (unbox *scp-out-port-box*))
  (define compiler-out-port (unbox *synthesis-task-compiler-out-port-box*))
  (when (input-port-ready? ui-in-port)
    (let ((msg (read ui-in-port)))
      (cond
        ((eof-object? msg)
         (void))
        (else
         (printf "read message from ui:\n\n~s\n\n\n" msg)
         (pmatch msg
           [(stop)
            (write/flush `(stop-all-synthesis) scp-out-port)
            (remove-all-synthesis-task-ids-from-scp-table!)
            (remove-all-synthesis-tasks! *pending-synthesis-tasks*)
            (remove-all-synthesis-tasks! *running-synthesis-tasks*)
            (write/flush `(stopped) ui-out-port)]
           [(synthesize ,ui-synthesis-id (,definitions ,inputs ,outputs))
            (write/flush `(compile ,ui-synthesis-id (,definitions ,inputs ,outputs)) compiler-out-port)]
           [,else
            (printf "** unknown message type from ui: ~s\n\n" msg)]))))))

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
         (printf "read message from scp:\n\n~s\n\n\n" msg)
         (pmatch msg
           #|
           The `(hello) message is received, and the `(scp-id ,scp-id)
           message is sent, in mcp-scp-tcp-proxy.
           |#
           [(num-processes ,num-processors ,scp-id)
            (add/update-num-processors-for-scp-in-scp-table! scp-id num-processors)]
           [(synthesis-finished ,scp-id ,ui/mcp-synthesis-id ,val ,statistics)
            
            (remove-ui/mcp-synthesis-id-for-scp-in-scp-table! ui/mcp-synthesis-id scp-id)

            (let ((running-task (assoc ui/mcp-synthesis-id *running-synthesis-tasks*)))
              (pmatch running-task
                [(,ui/mcp-synthesis-id ,scp-id (,definitions ,inputs ,outputs))
                 (printf "moving running synthesis task:\n\n~s\n\nfrom running to finished...\n\n" running-task)
                 (remove-synthesis-task! running-task *running-synthesis-tasks*)
                 (add-synthesis-task!
                   `(,ui/mcp-synthesis-id ,scp-id (,definitions ,inputs ,outputs) ,val ,statistics)
                   *finished-synthesis-tasks*)]
                [#f (error 'synthesis-finished
                           (format "unexpected #f from (assoc synthesis-id *running-synthesis-tasks*): ~s ~s"
                                   ui/mcp-synthesis-id *running-synthesis-tasks*))]))

            (pmatch ui/mcp-synthesis-id
              [(,ui-synthesis-task-id ,mcp-synthesis-task-id)

               ;; Check if *finished-synthesis-tasks* table already
               ;; contains an entry with the same `result`, the same
               ;; `ui-synthesis-task-id`, and a different
               ;; `mcp-synthesis-task-id`.  If so, we have a duplicate
               ;; result, so do not sent to the UI.

               (let loop ((finished *finished-synthesis-tasks*))
                 (pmatch finished
                   [()
                    ;; the new result is not a duplicate, so send it to the UI
                    (write/flush `(synthesis-finished ,ui-synthesis-task-id ,val ,statistics) ui-out-port)]
                   [(((,old-ui-synthesis-task-id ,old-mcp-synthesis-task-id)
                      ,old--scp-id
                      (,old-definitions ,old-inputs ,old-outputs) ,old-results ,old-statistics)
                     . ,rest)
                    (cond
                      [(and (equal? ui-synthesis-task-id old-ui-synthesis-task-id)
                            (not (equal? mcp-synthesis-task-id old-mcp-synthesis-task-id))
                            (equal? val old-results))
                       ;; the new result is already in the table from
                       ;; a previous synthesis task, so don't send the
                       ;; duplicate result to the UI
                       (printf "ignoring duplicate result for ui-synthesis-task ~s:\n\n~s\n\n"
                               ui-synthesis-task-id
                               val)
                       (void)]
                      [else (loop rest)])]
                   [,else
                    (printf "*** unexpected *finished-synthesis-tasks* format:\n\n~s\n\n" finished)]))]
              [,else (printf "*** unexpected ui/mcp-synthesis-id format:\n\n~s\n\n" ui/mcp-synthesis-id)])
            
            (printf "checking if there is a pending synthesis task for the newly free processor:\n\n~s\n\n"
                    *pending-synthesis-tasks*)
            (pmatch *pending-synthesis-tasks*
              [()
               (printf "no pending synthesis tasks\n\n")
               (void)]
              [(,pending-task . ,rest)
               (pmatch pending-task
                 [(,ui/mcp-synthesis-id (,definitions ,inputs ,outputs))
                  (printf "moving pending synthesis task:\n\n~s\n\nfrom pending to running...\n\n" pending-task)
                  (remove-synthesis-task! pending-task *pending-synthesis-tasks*)
                  (add-synthesis-task!
                    `(,ui/mcp-synthesis-id ,scp-id (,definitions ,inputs ,outputs))
                    *running-synthesis-tasks*)
                  (let ((msg `(synthesize ,scp-id ,ui/mcp-synthesis-id (,definitions ,inputs ,outputs))))
                    (write/flush msg scp-out-port))]
                 [,else
                  (printf "** unknown pending task format: ~s\n\n" pending-task)])])]
           [,else
            (printf "** unknown message type from scp: ~s\n\n" msg)]))))))

(define (handle-synthesis-task-compiler-subprocess-messages)
  (define compiler-in-port (unbox *synthesis-task-compiler-in-port-box*))
  (when (input-port-ready? compiler-in-port)
    (let ((msg (read compiler-in-port)))
      (cond
        ((eof-object? msg)
         (void))
        (else
         (printf "read message from synthesis-task-compiler:\n\n~s\n\n\n" msg)
         (pmatch msg
           [(synthesize* ,ui-synthesis-id ,definitions/inputs/outputs*)
            (for-each
              (lambda (definitions/inputs/outputs)
                (pmatch definitions/inputs/outputs
                  [(,definitions ,inputs ,outputs)
                   (let ((mcp-synthesis-id *mcp-synthesis-id-counter*))
                     (add-synthesis-task!
                       `((,ui-synthesis-id ,mcp-synthesis-id)
                         (,definitions ,inputs ,outputs))
                       *pending-synthesis-tasks*)
                     (increment-mcp-synthesis-id-counter!))]
                  [,else
                   (printf "** unexpected definitions/inputs/outputs format:\n~s\n\n" definitions/inputs/outputs)]))
              definitions/inputs/outputs*)]
           [,else
            (printf "** unknown message type from synthesis-task-compiler:\n~s\n\n" msg)])))))  
  (void))

(define (assign-pending-synthesis-tasks-to-scp)
  (define scp-out-port (unbox *scp-out-port-box*))
  (define ui-out-port (unbox *ui-out-port-box*))
  (let loop ()
    (pmatch *pending-synthesis-tasks*
      [() (void)]
      [(,pending-task . ,rest)
       (pmatch pending-task
         [((,ui-synthesis-task-id ,mcp-synthesis-task-id) (,definitions ,inputs ,outputs))
          (cond
            [(find-scp-with-free-processor) =>
             (lambda (scp-id)
               (printf "assigning pending synthesis task to scp ~s:\n\n~s\n\n" scp-id pending-task)
               
               (write/flush `(synthesize ,scp-id (,ui-synthesis-task-id ,mcp-synthesis-task-id) (,definitions ,inputs ,outputs))
                            scp-out-port)
               (write/flush `(synthesizing ,ui-synthesis-task-id) ui-out-port)

               (add-ui/mcp-synthesis-id-to-scp-in-scp-table! `(,ui-synthesis-task-id ,mcp-synthesis-task-id) scp-id)
                             
               (printf "promoting synthesis task from pending to running...\n\n")
               (remove-synthesis-task! pending-task *pending-synthesis-tasks*)
               (add-synthesis-task!
                `((,ui-synthesis-task-id ,mcp-synthesis-task-id) ,scp-id (,definitions ,inputs ,outputs))
                *running-synthesis-tasks*)
               
               (loop))]
            [else (void)])]
         [,else
          (printf "*** unexpected format for pending-task:\n\n~s\n\n" pending-task)])]
      [,else
       (printf "*** unexpected format for *pending-synthesis-tasks* table:\n\n~s\n\n" *pending-synthesis-tasks*)])))

(let main-mcp-loop ()
  (handle-ui-messages)
  (handle-scp-messages)
  (handle-synthesis-task-compiler-subprocess-messages)
  (assign-pending-synthesis-tasks-to-scp)
  ;; Sleep for 10 ms (10 million nanoseconds) to avoid using 100% of
  ;; the CPU time checking if a new message has arrived.
  (let ((millisecond (expt 10 6)))
    (sleep (make-time 'time-duration (* 10 millisecond) 0)))
  (main-mcp-loop))
