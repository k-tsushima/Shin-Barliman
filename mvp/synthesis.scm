(load "pmatch.scm")

(write '(synthesis-subprocess-ready))

(let loop ((msg (read)))
  (cond
    ((eof-object? msg)
     (write `(unexpected-eof)))
    (else
     (pmatch msg
       [(quit)
        (write `(quitting))
        (flush-output-port)]
       [(eval-expr ,expr)        
        (let ((val (eval expr)))
          (write `(value ,expr ,val))
          (flush-output-port)
          (loop (read)))]
       [,else
        (write `(unknown-message ,msg))
        (flush-output-port)
        (loop (read))]))))
