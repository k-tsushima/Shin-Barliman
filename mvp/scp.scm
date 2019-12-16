;; Shin-Barliman Sub Controlling Process (SCP)

#|
Description of the Sub Controlling Process.
----------------------------------------

The MCP is responsible for coordinating communication between the
user interface (UI) and the sub-processes responsible for synthesis.

The MCP is also responsible for the policies and strategies used for
efficient synthesis.
|#


(define *program* (box #f))
(define *tests* (box #f))
(define *scm-files* (box #f))

(define number-of-process 3)

(define (starting-subprocess x)
  
  
  )

; call tcp-client-for-subprocess.rkt
; TODO:
; (1) get & keep information from MCP via tcp-client-for-subprocess.
;     (*program*, *tests*, *scm-files*)
(let-values ([(to-stdin from-stdout from-stderr process-id)
	      (open-process-ports "exec /usr/local/bin/racket scp-tcp-proxy.rkt"
		    (buffer-mode block)
		    (make-transcoder (utf-8-codec)))])
  (printf "started process ~s\n" process-id)
  (let loop ()
    (cond
      [(input-port-ready? from-stdout)
       (let ((x (read from-stdout)))
         (printf "read ~s\n" x)
         (cond
           [(eof-object? x)
            (printf "dun with fish!\n")
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr)]
           [else
	    (starting-subprocess x)
            (loop)]))]
      [else (loop)]))
  )

; make subprocess
; (1) divide work to each subprocesses (create scm code? or MCP will send scm code?)
; (2) make subprocess & send information to subprocess
; (3) receive information from subprocess 
; Keeping simple: the number of subprocess will be two


