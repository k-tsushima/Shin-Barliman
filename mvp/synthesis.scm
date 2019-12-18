(load "pmatch.scm")
(load "mk-chez.scm")
(load "mk.scm")
(load "interp.scm")
(load "sythesis-template.scm")

(define FUEL 100000) ; ticks

(define *engine-box* (box #f))

(write '(synthesis-subprocess-ready))

(let loop ((msg (read)))  
  (cond
    ((eof-object? msg)
     (write `(unexpected-eof))
     (flush-output-port)
     (exit))
    (else
     (pmatch msg
       [(ping)
        (write `(ping))
        (flush-output-port)
        (if (input-port-ready? (current-input-port))
            (loop (read))
            (loop `(no-message-to-read)))]
       [(stop)
        (write `(stopped))
        (flush-output-port)
        (exit)]
       [(synthesize (,definitions ,inputs ,outputs) ,synthesis-id)
        (let ((expr (fill-in-template definitions inputs outputs)))
          (let ((e (make-engine (lambda ()
                                  (list (eval expr) synthesis-id)))))
            (set-box! *engine-box* e)
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
               (write `(synthesis-finished ,synthesis-id ,val ,statistics))               
               (flush-output-port)
               (loop (read))]
              [(expired ,e)
               (set-box! *engine-box* e)])))        
        (if (input-port-ready? (current-input-port))
            (loop (read))
            (loop `(no-message-to-read)))]
       [,msg
        (write `(unknown-message-type ,msg))
        (flush-output-port)
        (exit)]))))
