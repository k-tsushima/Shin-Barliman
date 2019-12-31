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
  (when (input-port-ready? in-port)
    (let ((msg (read in-port)))
      (printf "read message from ui: ~s\n" msg)))
  (void))

(define (handle-scp-messages)
  (void))

(define (handle-synthesis-task-compiler-subprocess-messages)
  (void))

;; event loop: check GUI proxy for messages, then check SCP proxy for
;; messages, updating internal tables and sending messages as
;; necessary
(let loop ()
  (handle-ui-messages)
  (handle-scp-messages)
  (handle-synthesis-task-compiler-subprocess-messages)
  (loop))
