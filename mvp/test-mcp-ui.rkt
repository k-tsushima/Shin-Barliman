#lang racket

;; Simulates a UI interaction with the MCP, in order to test the MCP.

(provide
  (all-from-out racket/tcp)
  (all-defined-out))

(require
  racket/tcp
  "common.rkt")

(print-as-expression #f)

;; Loading will occur at first use if not explicitly forced like this.
(load-config #t)

(define DEFAULT-TCP-IP-ADDRESS (config-ref 'ui-tcp-ip-address))
(define DEFAULT-TCP-PORT (config-ref 'ui-tcp-port))

(define (simulate-ui address port)
  (define-values (in out) (tcp-connect address port))
  ;;
  (define definitions '((define append
                          (lambda (l s)
                            (if (null? l)
                                ,A
                                (cons ,B ,C))))))
  (define inputs '((append '() '())
                   (append '(,g1) '(,g2))
                   (append '(,g3) '(,g4))
                   (append '(,g5 ,g6) '(,g7 ,g8))))
  (define outputs '(()
                    (,g1 ,g2)
                    (,g3 ,g4)
                    (,g5 ,g6 ,g7 ,g8)))
  (define synthesize-msg `(synthesize (,definitions ,inputs ,outputs)))
  (printf "fake ui writing synthesize message ~s\n" synthesize-msg)
  (write synthesize-msg out)
  (flush-output out)
  (printf "fake ui wrote synthesize message ~s\n" synthesize-msg)
  ;;
  (define msg1 (read in))
  (printf "fake ui received message ~s\n" msg1)
  ;;
  (unless (equal? '(synthesizing) msg1)
    (printf "*** fake ui received unexpected message ~s--expected (synthesizing)\n" msg1))
  ;;
  (define msg2 (read in))
  (printf "fake ui received message ~s\n" msg2)

  (match msg2
    [`(synthesis-finished ,scp-id ,synthesis-id ,val ,statistics)
     (printf "synthesis finished!\n")]
    [else (printf "*** fake ui received unexpected message ~s--expected (synthesis-finished ...)\n" msg2)])
  
  ;; cleanup
  (close-input-port in)
  (close-output-port out)
  )

(simulate-ui DEFAULT-TCP-IP-ADDRESS DEFAULT-TCP-PORT)
