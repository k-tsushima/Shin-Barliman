(load "pmatch.scm")
(load "mk-chez.scm")
(load "mk.scm")
(load "interp.scm")
(load "synthesis-template.scm")

(define FUEL 100000) ; ticks

(define *engine-box* (box #f))
(define *start-time* (box #f))

(write '(synthesis-subprocess-ready))
(flush-output-port)

(let loop ((msg (read)))
  (cond
    ((eof-object? msg)
     (write `(unexpected-eof) (current-error-port))
     (flush-output-port (current-error-port))
     (exit))
    (else
     (pmatch msg
       [(stop)
        (write `(stopped))
        (flush-output-port)
        (exit)]
       [(synthesize (,definitions ,inputs ,outputs) ,synthesis-id)
        (let ((expr (fill-in-template definitions inputs outputs)))
          (let ((e (make-engine (lambda ()
                                  (list (eval expr) synthesis-id)))))
            (set-box! *engine-box* e)
            (set-box! *start-time* (current-time))
            (if (input-port-ready? (current-input-port))
                (loop (read))
                (loop `(no-message-to-read)))))]
       [(no-message-to-read)
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
                     (write `(synthesis-finished ,synthesis-id ,val ,statistics)))))
               (set-box! *start-time* #f)
               (write `(synthesis-finished ,val))
               (flush-output-port)
               (loop (read))]
              [(expired ,e)
               (set-box! *engine-box* e)])))        
        (if (input-port-ready? (current-input-port))
            (loop (read))
            (loop `(no-message-to-read)))]
       [,msg
        (write `(unknown-message-type ,msg) (current-error-port))
        (flush-output-port (current-error-port))
        (exit)]))))
