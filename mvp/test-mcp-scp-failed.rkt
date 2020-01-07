#lang racket

;; Simulates an SCP interaction with the MCP, in order to test the MCP.
;;
;; `synthesis-finished-msg` that is eventually sent to the MCP contains no answers
;; (`vals` is the empty list), representing failure.

(provide
  (all-from-out racket/tcp)
  (all-defined-out))

(require
  racket/tcp
  "common.rkt")

(print-as-expression #f)

;; Loading will occur at first use if not explicitly forced like this.
(load-config #t)

(define DEFAULT-TCP-IP-ADDRESS (config-ref 'scp-tcp-ip-address))
(define DEFAULT-TCP-PORT (config-ref 'scp-tcp-port))

(define (simulate-scp address port)
  (printf "fake scp connecting to mcp at ~s:~s...\n" address port)
  (define-values (in out) (tcp-connect address port))
  (printf "fake scp connected to mcp at ~s:~s\n" address port)
  ;;
  (define hello-msg `(hello))
  (printf "fake scp writing hello message ~s\n" hello-msg)
  (write hello-msg out)
  (flush-output out)
  (printf "fake scp wrote hello message ~s\n" hello-msg)
  ;;
  (define msg1 (read in))
  (printf "fake scp received message ~s\n" msg1)
  ;;

  (define scp-id #f)
  
  (match msg1
    [`(scp-id ,my-scp-id)
     (set! scp-id my-scp-id)
     (printf "received scp-id ~s\n" scp-id)]
    [else (printf "*** fake scp received unexpected message ~s--expected (scp-id ,scp-id)\n" msg1)])

  (define number-of-synthesis-subprocesses 1)
  (define num-processes-msg `(num-processes ,number-of-synthesis-subprocesses ,scp-id))
  (printf "fake scp writing num-processes message ~s\n" num-processes-msg)
  (write num-processes-msg out)
  (flush-output out)
  (printf "fake scp wrote num-processes message ~s\n" num-processes-msg)
    
  (define msg2 (read in))
  (printf "fake scp received message ~s\n" msg2)

  (define definitions #f)
  (define inputs #f)
  (define outputs #f)
  (define synthesis-id #f)
  
  (match msg2
    [`(synthesize ((,my-definitions ,my-inputs ,my-outputs ,my-synthesis-id)))
     (set! definitions my-definitions)
     (set! inputs my-inputs)
     (set! outputs my-outputs)
     (set! synthesis-id my-synthesis-id)
     (printf "fake scp received synthesize message with synthesis-id ~s\n" synthesis-id)]
    [else (printf "*** fake scp received unexpected message ~s--expected synthesize msg\n" msg2)])
  
  (define val '()) ;; failure!
  (define statistics '(elapsed-time (seconds 0) (nanoseconds 127104000)))
  (define synthesis-finished-msg `(synthesis-finished ,scp-id ,synthesis-id ,val ,statistics))
  (printf "fake scp writing synthesis-finished message ~s\n" synthesis-finished-msg)
  (write synthesis-finished-msg out)
  (flush-output out)
  (printf "fake scp wrote synthesis-finished message ~s\n" synthesis-finished-msg)
  
  ;; cleanup
  (close-input-port in)
  (close-output-port out)
  )

(simulate-scp DEFAULT-TCP-IP-ADDRESS DEFAULT-TCP-PORT)
