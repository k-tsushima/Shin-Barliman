;; Scheme subprocess started by MCP for generating code to be sent to SCP for synthesis

(load "pmatch.scm")
(load "common-chez.scm")

;; Loading will occur at first use if not explicitly forced like this.
(load-config #f)

(define PROGRAM-NAME "synthesis-task-compiler")

#| begin logging infrastructure definitions (how to abstract this?) |#
(define ENABLE-LOGGING (config-ref 'enable-synthesis-task-compiler-logging))
(define LOG-FILE-NAME (format "~a.log" PROGRAM-NAME))
(define LOG-FILE-OUTPUT-PORT-BOX (box #f))

(define (logf format-str . args)
  (when ENABLE-LOGGING
    (unless (unbox LOG-FILE-OUTPUT-PORT-BOX)
      (let ((output-port (open-output-file LOG-FILE-NAME 'replace)))
        (set-box! LOG-FILE-OUTPUT-PORT-BOX output-port)))
    (apply fprintf (unbox LOG-FILE-OUTPUT-PORT-BOX) format-str args)
    (flush-output-port (unbox LOG-FILE-OUTPUT-PORT-BOX))))
#| end logging infrastructure definitions |#

(define-syntax write/flush
  (syntax-rules ()
    [(_ msg out-port)
     (begin
       (write msg out-port)
       (flush-output-port out-port)
       (logf "wrote msg to ~s:\n\n~s\n\n\n" 'out-port msg))]))

(logf "started ~a\n" PROGRAM-NAME)

(define (handle-mcp-messages)
  (define mcp-in-port (current-input-port))
  (define mcp-out-port (current-output-port))
  (when (input-port-ready? mcp-in-port)
    (let ((msg (read mcp-in-port)))
      (cond
        ((eof-object? msg)
         (void))
        (else
         (logf "read message from mcp:\n\n~s\n\n\n" msg)
         (pmatch msg
           [(compile ,synthesis-id (,definitions ,inputs ,outputs))
            ;; TODO
            ;;
            ;; This is where the template compilation smarts and
            ;; optimizations will go!
            ;;
            ;; For now, do the boring thing as just echo back the
            ;; synthesis problem to the MCP.
            (let (
                  ;; Most boring version--echo back single, original synthesis "template"
                  (definitions/inputs/outputs* (list `(,definitions ,inputs ,outputs)))

                  ;; Use this version instead to test multiple (identical) synthesis "templates"
                  ;; (definitions/inputs/outputs* (make-list 10 `(,definitions ,inputs ,outputs)))                  
                  )
              (let ((msg `(synthesize* ,synthesis-id ,definitions/inputs/outputs*)))
                (write/flush msg mcp-out-port)
                (logf "wrote message to mcp:\n\n~s\n\n\n" msg)))]
           [,else
            (logf "** unknown message type from mcp: ~s\n\n" msg)]))))))

(let main-compiler-loop ()
  (handle-mcp-messages)  
  ;; Sleep for 10 ms (10 million nanoseconds) to avoid using 100% of
  ;; the CPU time checking if a new message has arrived.
  (let ((millisecond (expt 10 6)))
    (sleep (make-time 'time-duration (* 10 millisecond) 0)))
  (main-compiler-loop))
