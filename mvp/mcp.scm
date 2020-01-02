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


;; start 'mcp-ui-tcp-proxy.rkt' Racket subprocess for UI TCP proxy
(let ((start-ui-tcp-proxy-command (format "exec ~a mcp-ui-tcp-proxy.rkt" RACKET-BINARY-PATH)))
  (printf "starting ui tcp proxy with command:\n~s\n" start-ui-tcp-proxy-command)
  (let-values ([(to-stdin from-stdout from-stderr process-id)
                (open-process-ports start-ui-tcp-proxy-command
                                    (buffer-mode block)
                                    (make-transcoder (utf-8-codec)))])
    (printf "started ui tcp proxy with process id ~s\n" process-id)
    (set-box! *ui-out-port-box* to-stdin)
    (set-box! *ui-in-port-box* from-stdout)
    (set-box! *ui-err-port-box* from-stderr)
    (set-box! *ui-pid-box* process-id)))

;; start 'mcp-scp-tcp-proxy.rkt' Racket subprocess for SCP TCP proxy
(let ((start-scp-tcp-proxy-command (format "exec ~a mcp-scp-tcp-proxy.rkt" RACKET-BINARY-PATH)))
  (printf "starting scp tcp proxy with command:\n~s\n" start-scp-tcp-proxy-command)
  (let-values ([(to-stdin from-stdout from-stderr process-id)
                (open-process-ports start-scp-tcp-proxy-command
                                    (buffer-mode block)
                                    (make-transcoder (utf-8-codec)))])
    (printf "started scp tcp proxy with process id ~s\n" process-id)
    (set-box! *scp-out-port-box* to-stdin)
    (set-box! *scp-in-port-box* from-stdout)
    (set-box! *scp-err-port-box* from-stderr)
    (set-box! *scp-pid-box* process-id)))

;; start 'synthesis-task-compiler.scm' Chez Scheme subprocess for
;; generating code to be sent to SCP for synthesis
(let ((start-synthesis-task-compiler-subprocess-command
       (format "exec ~a ~a synthesis-task-compiler.scm" CHEZ-BINARY-PATH CHEZ-FLAGS)))
  (printf "starting synthesis task compiler subprocess with command:\n~s\n"
          start-synthesis-task-compiler-subprocess-command)
  (let-values ([(to-stdin from-stdout from-stderr process-id)
                (open-process-ports start-synthesis-task-compiler-subprocess-command
                                    (buffer-mode block)
                                    (make-transcoder (utf-8-codec)))])
    (printf "started synthesis task compiler subprocess with process id ~s\n" process-id)
    (set-box! *synthesis-task-compiler-out-port-box* to-stdin)
    (set-box! *synthesis-task-compiler-in-port-box* from-stdout)
    (set-box! *synthesis-task-compiler-err-port-box* from-stderr)
    (set-box! *synthesis-task-compiler-pid-box* process-id)))

(define (handle-ui-messages)
  (define in-port (unbox *ui-in-port-box*))
  (define out-port (unbox *ui-out-port-box*))
  (when (input-port-ready? in-port)
    (let ((msg (read in-port)))
      (cond
        ((eof-object? msg)
         (void))
        (else
         (printf "read message from ui: ~s\n" msg)
         (pmatch msg
           [(stop)
            ;;
            (write `(stop-all-synthesis) (unbox *scp-out-port-box*))
            (flush-output-port (unbox *scp-out-port-box*))
            ;;
            (write `(stopped) out-port)
            (flush-output-port out-port)
            ;;
            ]
           [(synthesize ,synthesis-id (,definitions ,inputs ,outputs))
            ;;
            (write `(synthesizing) out-port)
            (flush-output-port out-port)
            ;;
            ]
           [,else
            (printf "** unknown message type from ui: ~s\n" msg)])))))
  (void))

(define (handle-scp-messages)
  (define in-port (unbox *scp-in-port-box*))
  (when (input-port-ready? in-port)
    (let ((msg (read in-port)))
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
            (printf "updated *scp-info* table: ~s\n" *scp-info*)]
           [(synthesis-finished ,scp-id ,synthesis-id ,val ,statistics)
            (let ((pr (assoc scp-id *scp-info*)))
              (pmatch pr
                [(,scp-id ,num-processors ,synthesis-task-id*)
                 (set! *scp-info*
                       (cons `(,scp-id ,num-processors ,(remove synthesis-id synthesis-task-id*))
                             (remove pr *scp-info*)))
                 (printf "updated *scp-info* table: ~s\n" *scp-info*)]))
            (let ((pr (assoc synthesis-id *running-synthesis-tasks*)))
              (pmatch pr
                [(,synthesis-id ,scp-id (,definitions ,inputs ,outputs))
                 (set! *finished-synthesis-tasks*
                       (cons `(,synthesis-id ,scp-id (,definitions ,inputs ,outputs) ,val ,statistics)
                             *finished-synthesis-tasks*))
                 (printf "updated *finished-synthesis-tasks* table: ~s\n" *finished-synthesis-tasks*)
                 (set! *running-synthesis-tasks* (remove pr *running-synthesis-tasks*))
                 (printf "updated *running-synthesis-tasks* table: ~s\n" *running-synthesis-tasks*)]))]
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
  (loop))
