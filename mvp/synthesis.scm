(load "pmatch.scm")
;; load mk & relational interpreter

;; have the standard Barliman synthesis template available

;; have MCP send just the parts to fill in to the standard synthesis
;; template, namely (possibly modified) definitions and input/output examples

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
