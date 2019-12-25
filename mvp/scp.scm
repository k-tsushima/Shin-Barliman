; Shin-Barliman Sub Controlling Process (SCP)

(load "pmatch.scm")

#|
Description of the Sub Controlling Process.
----------------------------------------

The MCP is responsible for coordinating communication between the
user interface (UI) and the sub-processes responsible for synthesis.

The MCP is also responsible for the policies and strategies used for
efficient synthesis.
|#


(define RACKET-BINARY-PATH "/usr/local/bin/racket")
;; (define RACKET-BINARY-PATH "/Applications/Racket\\ v7.5/bin/racket")

(define CHEZ-BINARY-PATH "/usr/local/bin/scheme")
(define CHEZ-FLAGS "-q")

(define *program* (box #f))
(define *tests* (box #f))
(define *scm-files* (box #f))

(define *mcp-out-port-box* (box #f))
(define *mcp-in-port-box* (box #f))
(define *mcp-err-port-box* (box #f))
(define *mcp-pid-port-box* (box #f))

(define number-of-synthesis-subprocesses 3)

(define *synthesis-subprocesses-box* (box '()))
; *synthesis-subprocesses-box* is the following
; (list `(synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr free))

(define *scp-id* #f)
(define *synthesis-task-table* '())
; *synthesis-task-table* is the following
; ((,synthesis-id ,subprocess-id ,definitions ,examples ,status) . ,rest)
(define *task-queue* '())
; def-inoutputs-synid

(define (send-number-of-subprocess-to-mcp)
  (let ((out (unbox *mcp-out-port-box*)))
    (write `(num-processes ,number-of-synthesis-subprocesses ,*scp-id*) out)
    (flush-output-port out)))

(define (send-synthesis-finished-to-mcp synthesis-id val statistics)
  (let ((out (unbox *mcp-out-port-box*)))
    (write `(synthesis-finished ,*scp-id* ,synthesis-id ,val ,statistics) out)
    (flush-output-port out)))

(define (check-for-mcp-messages)
  (printf "SCP checking for messages from MCP...\n")

  (when (input-port-ready? (unbox *mcp-err-port-box*))
    (let ((msg (read (unbox *mcp-err-port-box*))))
      (printf "SCP read error message ~s from MCP\n" msg)
      (cond
        ((eof-object? msg)
         (printf "FIXME do nothing ~s\n" msg))
        (else
         (pmatch msg
	   [(unexpected-eof)
	    (printf "SCP receive unexpected EOF from MCP\n")
	    ]
	   [(unknown-message-type ,msg)
	    (printf "SCP receive unknown error message ~s from MCP\n" msg)
	    ]
           [,anything
            (printf "FIXME do nothing ~s\n" msg)]))
        )))
  
  (when (input-port-ready? (unbox *mcp-in-port-box*))
    (let ((msg (read (unbox *mcp-in-port-box*))))
      (printf "SCP read message ~s from MCP\n" msg)
      (cond
        ((eof-object? msg)
         (printf "FIXME do nothing ~s\n" msg))
        (else
         (pmatch msg
	   [(scp-id ,scp-id)
	    ; receiving scp-id, keep it and send number-of-subprocess
	    (set! *scp-id* scp-id)
	    ; Sent to MCP: 
	    (send-number-of-subprocess-to-mcp)
	    ]
	   [(synthesize ,def-inoutputs-synid)
	    (set! *task-queue* (append *task-queue* def-inoutputs-synid))
            ; if there is free subprocesses, start synthesis
	    (start-synthesis-with-free-subprocesses)
	    ]
	   [(stop-all-synthesis)
	    (set! queue '())
	    (stop-all-subprocess)]
	   [(stop-one-task ,synthesis-id)
	    (stop-one-task synthesis-id)]
           [,anything
            (printf "FIXME do nothing ~s\n" msg)]))
        )))
  )

(define (start-synthesis-with-free-subprocesses)
  (let loop ((synthesis-subprocesses (unbox *synthesis-subprocesses-box*)))
    (pmatch synthesis-subprocesses
      [()
       (printf "started synthesis with all free synthesis subprocesses\n")]
      [((synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr free)
        . ,rest)
       (pmatch *task-queue*
	 [() (printf "there is no more job in queue\n")]
	 [((,definitions ,inputs ,outputs ,synthesis-id) . ,rest)
	  (printf "there is job ...\n")
	  (write `(synthesize (,definitions ,inputs ,outputs) ,synthesis-id) to-stdin)
	  (flush-output-port to-stdin)
	  ; update subprocess status to working
	  (update-status 'working process-id)
	  (printf "Process-id ~s started working\n" process-id)
	  (printf "~s\n" (unbox *synthesis-subprocesses-box*))
	  (set! *task-queue* rest)
	  (set! *synthesis-task-table* (cons `(,synthesis-id ,process-id ,definitions ,inputs ,outputs started) *synthesis-task-table*))
	  (start-synthesis-with-free-subprocesses)
	  ])
       ]
      [((synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr working)
        . ,rest)
       (loop rest)
       ])))

(define (opposite status)
  (cond ((equal? status 'working) 'free)
	((equal? status 'free) 'working)
	(else (printf "opposite: status error"))))


(define (update-status-aux status id)
  (let loop ((synthesis-subprocesses (unbox *synthesis-subprocesses-box*)))
    (pmatch synthesis-subprocesses
      [()
       (printf "tried update-status-to ~s, but ~s is not found\n" status id)
       '()]
      [((synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr ,current-status)
        . ,rest)
       (cond
	((equal? process-id id)
	 (cond ((equal? status current-status)
		(printf "tried update-status-to ~s, but ~s is already ~s\n" current-status process-id current-status)
		(cons `(synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr ,current-status) rest))
	       ((equal? status (opposite current-status))
		(printf "update-status-to ~s: updated! id = ~s\n" (opposite current-status) process-id)
		(cons `(synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr ,(opposite current-status)) rest))
	       (else (printf "status error"))))
	(else (cons `(synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr ,(opposite current-status)) (loop rest))))
       ])))

(define (update-status status id)
  (set-box! *synthesis-subprocesses-box* (update-status-aux status id)))



  
(define (check-for-synthesis-subprocess-messages)
  (printf "SCP checking for messages from synthesis subprocesses...\n")
  (let loop ((synthesis-subprocesses (unbox *synthesis-subprocesses-box*)))
    (pmatch synthesis-subprocesses
      [()
       (printf "checked for all synthesis subprocesses messages\n")]
      [((synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr ,status)
        . ,rest)

       (when (input-port-ready? from-stderr)
         (let ((msg (read from-stderr)))
           (printf "SCP read error message ~s from synthesis subprocess ~s\n" msg i)
           (cond
             ((eof-object? msg)
              (printf "FIXME do nothing ~s\n" msg))
             (else
              (pmatch msg
		[(unexpected-eof)
		 (printf "SCP receive unexpected EOF from subprocess\n")]
		[(unknown-message-type ,msg)
		 (printf "SCP receive error message ~s from subprocess\n" msg)]
                [,anything
                 (printf "FIXME do nothing ~s\n" msg)]))
             )))
       
       (when (input-port-ready? from-stdout)
         (let ((msg (read from-stdout)))
           (printf "SCP read message ~s from synthesis subprocess ~s\n" msg i)
           (cond
             ((eof-object? msg)
              (printf "FIXME do nothing ~s\n" msg))
             (else
              (pmatch msg
                [(synthesis-subprocess-ready)
		 (update-status 'free process-id)
	        ; (let ((expr '(* 3 4)))
                ;   (write `(eval-expr ,expr) to-stdin)
		;   (flush-output-port to-stdin))
		 ]
		[(stopped)
		 ; TODO?
		 (void)]
		[(synthesis-finished ,synthesis-id ,val ,statistics)
		 (printf "SCP received synthesis-finished message from ~s" synthesis-id)
		 ; Sent to MCP:
		 (send-synthesis-finished-to-mcp synthesis-id val statistics)
	         ; update the status and start working with the free subprocesses
		 (update-status 'free process-id)
		 (start-synthesis-with-free-subprocesses)
		]
		[(status ,stat)
	         ; TODO
		 (void)]
                [,anything
                 (printf "FIXME do nothing ~s: anything\n" msg)]))
             )))
       (loop rest)])))



; call tcp-client-for-subprocess.rkt
; TODO:
; (1) get & keep information from MCP via tcp-client-for-subprocess.
;     (*program*, *tests*, *scm-files*)


;; start synthesis subprocesses as soon as SCP starts
(printf "starting ~s synthesis subprocesses\n" number-of-synthesis-subprocesses)
(let loop ((i 0))
  (cond
    ((= i number-of-synthesis-subprocesses)
     (printf "started all ~s subprocesses\n" i))
    (else
     (let ((start-synthesis-subprocess-command
            (format "exec ~a ~a synthesis.scm" CHEZ-BINARY-PATH CHEZ-FLAGS)))
       (printf "starting synthesis subprocess with command:\n~s\n" start-synthesis-subprocess-command)
       (let-values ([(to-stdin from-stdout from-stderr process-id)
	             (open-process-ports start-synthesis-subprocess-command
		                         (buffer-mode block)
		                         (make-transcoder (utf-8-codec)))])
         (printf "started synthesis subprocesses ~s with process id ~s\n" i process-id)
         (set-box! *synthesis-subprocesses-box*
                   (append (unbox *synthesis-subprocesses-box*)
                           (list `(synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr free))))))
     (loop (add1 i)))))


;; start TCP proxy so SCP can communicate with MCP
(let ((start-tcp-proxy-command (format "exec ~a scp-tcp-proxy.rkt" RACKET-BINARY-PATH)))
  (printf "starting tcp proxy with command:\n~s\n" start-tcp-proxy-command)
  (let-values ([(to-stdin from-stdout from-stderr process-id)
	        (open-process-ports start-tcp-proxy-command
		                    (buffer-mode block)
		                    (make-transcoder (utf-8-codec)))])
    (printf "started tcp proxy with process id ~s\n" process-id)
    (set-box! *mcp-out-port-box* to-stdin)
    (set-box! *mcp-in-port-box* from-stdout)
    (set-box! *mcp-err-port-box* from-stderr)
    (set-box! *mcp-pid-port-box* process-id)))

; make subprocess
; (1) divide work to each subprocesses (create scm code? or MCP will send scm code?)
; (2) make subprocess & send information to subprocess
; (3) receive information from subprocess 
; Keeping simple: the number of subprocess will be two

(printf "synthesis-subprocesses list:\n~s\n" (unbox *synthesis-subprocesses-box*))

(define (stop-all-subprocess)
  (let loop ((synthesis-subprocesses (unbox *synthesis-subprocesses-box*)))
    (pmatch synthesis-subprocesses
      [() (printf "stopped all synthesis subprocesses\n")]
      [((synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr)
        . ,rest)
       (write `(stop) to-stdin)
       (flush-output-port to-stdin)
       (loop rest)]))
  )

(define (partition func lst)
  (printf "partition: ~s\n" lst)
  (pmatch lst
    [(()) '(() ())]
    [(,a . ,rest)
     (let ((result (partition func rest)))
       (if (func (car a))
	   `(,(cons a (car result)) ,(cdr result))
	   `(,(car result) ,(cons a (cdr result)))))
     ]
    ))

(define (searching-subprocess-out lst id)
  (pmatch lst
    [() (printf "FIXME, there is no subprocess ~s\n" id)]
    [((synthesis-subprocess ,i ,process-id ,to-stdin ,from-stdout ,from-stderr)
      . ,rest)
     (if (equal? id i) ; Is this true?
	   to-stdin
	   (searching-subprocess-out rest id))]
    ))
  

(define (stop-running-one-task id)
  ; find the information in systhesis table and quit that job

  (printf "task-table:~s\n" *synthesis-task-table*)
  (pmatch *synthesis-task-table*
    [() (printf "FIXME, received id is not found in synthesis table\n")]
    [,else 
     (let ((lst (partition (lambda (x) (equal? id (car x))) *synthesis-task-table*)))
       (printf "Partition: ~s\n" lst)
       (pmatch lst
	 [(() . ())
					; the id is not found in the table
	  (printf "FIXME, received id is not found in queue and task table\n")
	  ]
	 [((,synthesis-id ,subprocess-id ,definitions ,examples ,status) . ,rest)
       ; the id is found in the table
	  (set! *synthesis-task-table* rest)
	  (let ((out (searching-subprocess-out (unbox *synthesis-subprocesses-box*) id)))
	    (write `(stop) out)
	    (flush-output-port out)
	    )  
       ; TODO: start another work?
       
       ]))]))

(define (stop-one-task id)
  ; in the case, that task is in the queue
  (let ((lst (partition (lambda (x) (equal? id (car x))) *task-queue*)))
    (pmatch lst
      [(() . ,rest)
       ; the id is not found in the queue
       (stop-running-one-task id)
       ]
      [(,a . ,rest)
       ; the id is found in the queue
       (set! *task-queue* rest)])))

#!eof

;; process messages
(let loop ()
  (check-for-mcp-messages)
 ; (send-number-of-subprocess-to-mcp)
  (check-for-synthesis-subprocess-messages)
  (loop))
