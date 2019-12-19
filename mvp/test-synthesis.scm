(let-values ([(to-stdin from-stdout from-stderr process-id)
                (open-process-ports "/usr/local/bin/scheme -q synthesis.scm"
                                    (buffer-mode block)
                                    (make-transcoder (utf-8-codec)))])
    (printf "read msg: ~s\n" (read from-stdout))
    ;;(write '(stop) to-stdin)
    (write (list 'synthesize
                 (list '((define append
                           (lambda (l s)
                             (if (null? l)
                                 ,A
                                 (cons ,B ,C)))))
                       '((append '() '())
                         (append '(,g1) '(,g2))
                         (append '(,g3) '(,g4))
                         (append '(,g5 ,g6) '(,g7 ,g8)))
                       '(()
                         (,g1 ,g2)
                         (,g3 ,g4)
                         (,g5 ,g6 ,g7 ,g8)))
                 1)
           to-stdin)
    (flush-output-port to-stdin)
    (printf "read msg: ~s\n" (read from-stdout))
    (let loop ((x (read from-stderr)))
      (cond
        ((eof-object? x) (void))
        (else (printf "~s" x)
              (loop (read from-stderr))))))
