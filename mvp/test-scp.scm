(set! *task-queue* (let ((definitions '((define append
                         (lambda (l s)
                           (if (null? l)
                               ,A
                               (cons ,B ,C))))))
        (inputs '((append '() '())
                  (append '(,g1) '(,g2))
                  (append '(,g3) '(,g4))
                  (append '(,g5 ,g6) '(,g7 ,g8))))
        (outputs '(()
                   (,g1 ,g2)
                   (,g3 ,g4)
                   (,g5 ,g6 ,g7 ,g8)))
        (synthesis-id 1))
		     `((,definitions ,inputs ,outputs ,synthesis-id) . ())))

(set! *task-queue* (let ((definitions '((define append
                         (lambda (k s)
                           (if (null? k)
                               s
                               (cons (car k) (append (cdr k) s)))))))
        (inputs '((append '() '())
                  (append '(,g1) '(,g2))
                  (append '(,g3) '(,g4))
                  (append '(,g5 ,g6) '(,g7 ,g8))))
        (outputs '(()
                   (,g1 ,g2)
                   (,g3 ,g4)
                   (,g5 ,g6 ,g7 ,g8)))
        (synthesis-id 1))
	`((,definitions ,inputs ,outputs ,synthesis-id) . ,*task-queue*)))

(start-synthesis-with-free-subprocesses)

#!eof

; checked working with "tmp-mcp-client.rkt"

(send-number-of-subprocess-to-mcp) ; message sent to MCP.
(stop-all-subprocess)  ; stopped (= removed) all subprocesses (looking ./top)
(stop-one-task 59857) ; the number should be one of working number. stopped (= removed) the subprocess (looking ./top)
(start-synthesis-with-free-subprocesses) ; synthesis started & after using (check-for-synthesis-subprocess-messages) two times, the results sent to MCP.

; todo
(synthesis-subprocess-ready) ; what should we do for this?
loop part


