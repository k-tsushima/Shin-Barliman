;; Scheme subprocess started by MCP for generating code to be sent to SCP for synthesis

(load "common.scm")

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

(logf "started ~a\n" PROGRAM-NAME)
