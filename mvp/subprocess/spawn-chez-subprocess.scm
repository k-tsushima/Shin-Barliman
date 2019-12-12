;; open-process-ports
;; https://cisco.github.io/ChezScheme/csug9.5/foreign.html#./foreign:s5

;; process port
;; https://cisco.github.io/ChezScheme/csug9.5/io.html#./io:s145

;; transcoder
;; https://scheme.com/tspl4/io.html#./io:s19

(open-process-ports "exec /usr/local/bin/scheme -q"
                    (buffer-mode block)
                    (make-transcoder (utf-8-codec)))

;; ========
;; The Plan
;; ========
;;
;; I suspect it is cleaner to start up N subprocesses at the
;; beginning, use engines within each subprocess to respond to
;; commands, and send/poll/replace/remove synthesis tasks for those
;; subprocesses (instead of starting/killing new subprocesses after
;; every keystroke).
;;
;; Remember to 'flush-output-port' after writing a command to 'to-stdin'.
;; And use 'input-port-ready?'.
;; And use engines.
;;
;; Develop a protocol for communication with the subprocess.
;; Be able to send an 'exit' message to the subprocess.
;; Subprocess should use the engine to periodically stop and check for
;; messages from the parent process, including requests for status.
;;
;; Think about security.


;; Seems that the process exits, but becomes a zombie process, with
;; an entry still in the process table.  Should I call 'wait' on the
;; PID?
;;
;; Hmm--the behavior seems non-deterministic.  When I evaluate this
;; expression multiple times, only some of the processes end up as zombies.
;; Others seem to be removed from the process table (or at least aren't
;; visible using 'ps -f')
(let-values ([(to-stdin from-stdout from-stderr process-id)
              (open-process-ports "exec /usr/local/bin/scheme -q omega.scm"
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
            (loop)]))]
      [else (loop)])))
;;
;; =>
;;
;; started process 79174
;; read 7
;; read #!eof
;; dun with fish!



;; Starts a new subprocess
;; If the Scheme program being run is Omega, and multiple subprocesses are created, this will staurate all the cores.
;; If the Chez Scheme process that spawned these subprocesses exits or is killed, the subprocesses will terminate.
(open-process-ports "exec /usr/local/bin/scheme -q omega.scm" (buffer-mode block) (make-transcoder (utf-8-codec)))
