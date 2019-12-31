(load "pmatch.scm")
(load "common.scm")
(load "mk-chez.scm")
(load "mk.scm")
(load "interp.scm")
(load "synthesis-template.scm")

;; Loading will occur at first use if not explicitly forced like this.
(load-config #f)

(define PROGRAM-NAME "synthesis")


(define FUEL 100000) ; ticks

(define *engine-box* (box #f))
(define *start-time* (box #f))


#| begin logging infrastructure definitions (how to abstract this?) |#
(define ENABLE-LOGGING (config-ref 'enable-synthesis-subprocess-logging))
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

(logf "writing synthesis-subprocess-ready to stdout\n")

(write '(synthesis-subprocess-ready))
(flush-output-port)

(logf "wrote synthesis-subprocess-ready to stdout\n")

(let loop ((msg (read)))
  (cond
    ((eof-object? msg)
     (write `(unexpected-eof) (current-error-port))
     (flush-output-port (current-error-port))
     (exit))
    (else
     (pmatch msg
       [(stop-synthesis)
        (logf "read message ~s from stdin\n" msg)
        (set-box! *engine-box* #f)
        (set-box! *start-time* #f)
        (write `(stopped-synthesis))
        (flush-output-port)
        (loop (read))]
       [(get-status)
        (logf "read message ~s from stdin\n" msg)
        (write `(status ,(if (unbox *engine-box*) 'synthesizing 'waiting)))
        (flush-output-port)
        (if (input-port-ready? (current-input-port))
            (loop (read))
            (loop `(no-message-to-read)))]
       [(synthesize (,definitions ,inputs ,outputs) ,synthesis-id)
        (logf "read message ~s from stdin\n" msg)
        (let ((expr (fill-in-template definitions inputs outputs)))
          (let ((e (make-engine (lambda ()
                                  (list (eval expr) synthesis-id)))))
            (set-box! *engine-box* e)
            (set-box! *start-time* (current-time))
            (if (input-port-ready? (current-input-port))
                (loop (read))
                (loop `(no-message-to-read)))))]
       [(no-message-to-read)
        ;; don't use 'logf' to log this "dummy" message, since it will
        ;; fill up the log file quickly!
        (let ((e (unbox *engine-box*)))
          (when e
            (pmatch (e FUEL
                       (lambda (remaining-fuel val/synthesis-id)
                         (pmatch val/synthesis-id
                           [(,val ,synthesis-id)
                            `(completed ,remaining-fuel ,val ,synthesis-id)]))
                       (lambda (e)
                         `(expired ,e)))
              [(completed ,remaining-fuel ,val ,synthesis-id)
               (set-box! *engine-box* #f)
               (let ((elapsed-time (time-difference (current-time) (unbox *start-time*))))
                 (let ((elapsed-seconds (time-second elapsed-time))
                       (elapsed-nanoseconds (time-nanosecond elapsed-time)))
                   (let ((statistics `(elapsed-time (seconds ,elapsed-seconds) (nanoseconds ,elapsed-nanoseconds))))
                     (write `(synthesis-finished ,synthesis-id ,val ,statistics))
                     (set-box! *start-time* #f)               
                     (flush-output-port)
                     (loop (read)))))]
              [(expired ,e)
               (set-box! *engine-box* e)])))        
        (if (input-port-ready? (current-input-port))
            (loop (read))
            (loop `(no-message-to-read)))]
       [,msg
        (logf "read unknown message ~s from stdin\n" msg)
        (write `(unknown-message-type ,msg) (current-error-port))
        (flush-output-port (current-error-port))
        (exit)]))))
