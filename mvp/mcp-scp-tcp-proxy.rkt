#lang racket/base

;; Proxy started by MCP for TCP communication with one or more SCPs

;; Adapted from https://docs.racket-lang.org/more/

(require
 racket/match
  racket/tcp
  "common.rkt")

(provide  
  (all-from-out racket/tcp)
  (all-defined-out))


(print-as-expression #f)

;; Loading will occur at first use if not explicitly forced like this.
(load-config #t)

(define DEFAULT-TCP-PORT (config-ref 'scp-tcp-port))

(define MAX-CONNECTIONS (config-ref 'max-simultaneous-mcp/scp-connections))

(define PROGRAM-NAME "mcp-scp-tcp-proxy")

(define *scp-id* 0)
(define scp-id-semaphore (make-semaphore 1))


#| begin infrastructure for ensuring writes to stdout are atomic

   Since there is only one thread reading from the MCP, we only need
   to worry about writes, not reads.
|#
(define stdout-semaphore (make-semaphore 1))

(define (atomic-write/flush val)
  (call-with-semaphore
   stdout-semaphore
   (lambda ()
     (write val)
     (flush-output (current-output-port)))))
#| end infrastructure for ensuring reads and writes from/to stdin/stdout are atomic |#


;; Table used to route messages from MCP to the correct SCP.
;;
;; (,scp-id ,input-tcp-port ,output-tcp-port)
;;
(define *scp-connections* '())
(define scp-connections-semaphore (make-semaphore 1))

#| begin logging infrastructure definitions (how to abstract this?) |#
(define ENABLE-LOGGING (config-ref 'enable-proxy-logging))
(define LOG-FILE-NAME (format "~a.log" PROGRAM-NAME))
(define LOG-FILE-OUTPUT-PORT-BOX (box #f))

;; semaphore code, to ensure logging is atomic, is adapted from
;; https://docs.racket-lang.org/guide/concurrency.html?q=semaphore#%28part._.Semaphores%29
(define log-output-semaphore (make-semaphore 1))

(define (logf format-str . args)
  (when ENABLE-LOGGING
    (call-with-semaphore
     log-output-semaphore
     (lambda ()
       (unless (unbox LOG-FILE-OUTPUT-PORT-BOX)
         (define output-port (open-output-file LOG-FILE-NAME
                                               #:mode 'text
                                               #:exists 'replace))
         (set-box! LOG-FILE-OUTPUT-PORT-BOX output-port))
       (apply fprintf (unbox LOG-FILE-OUTPUT-PORT-BOX) format-str args)
       (flush-output (unbox LOG-FILE-OUTPUT-PORT-BOX))))))
#| end logging infrastructure definitions |#


(logf "started ~a\n" PROGRAM-NAME)

(define handle-mcp-messages
  (lambda ()
    (let loop ((msg (read)))
      (logf "~a received message from mcp ~s\n" PROGRAM-NAME msg)
      (cond
        ((eof-object? msg)
         (logf "read eof from mcp--exiting handle-mcp-messages\n"))
        (else
         (match msg
           [`(stop-all-synthesis)
            (logf "broadcasting stop-all-synthesis message to each active SCP\n")
            (call-with-semaphore
             scp-id-semaphore
             (lambda ()
               (for-each
                 (lambda (e)
                   (match e
                     [`(,scp-id ,input-tcp-port ,output-tcp-port)
                      (unless (port-closed? output-tcp-port)
                        (write `(stop-all-synthesis) output-tcp-port)
                        (flush-output output-tcp-port)
                        (logf "sent stop-all-synthesis message to scp-id ~s\n" scp-id))]))
                 *scp-connections*)))
            (logf "broadcasted stop-all-synthesis message to each active SCP\n")
            (loop (read))]
           [`(synthesize ,scp-id ,synthesis-id (,definitions ,inputs ,outputs))
            (logf "sending synthesize message to SCP ~s\n" scp-id)
            (call-with-semaphore
             scp-connections-semaphore
             (lambda ()
               (cond
                 ((assoc scp-id *scp-connections*) =>
                  (lambda (e)
                    (match e
                      [`(,scp-id ,input-tcp-port ,output-tcp-port)
                       (let ((msg `(synthesize ((,definitions ,inputs ,outputs ,synthesis-id)))))
                         (if (port-closed? output-tcp-port)
                             (begin
                               (logf "output port for SCP ~s is closed--can't send synthesize message ~s!\n" scp-id msg))
                             (begin
                               (write msg output-tcp-port)
                               (flush-output output-tcp-port)
                               (logf "sent synthesize message ~s to scp-id ~s\n" msg scp-id))))])))
                 (else
                  (logf "SCP with scp-id ~s isn't in *scp-connections* table ~s\n" scp-id *scp-connections*)))))
            (loop (read))]
           [else
            (logf "unexpected message from mcp--ignoring message!\n")
            (loop (read))]))))))

(logf "starting handle-mcp-messages-thread\n")

;; Start separate thread to handle messages from MCP
(define handle-mcp-messages-thread
  (thread handle-mcp-messages))

(logf "started handle-mcp-messages-thread\n")

(define (serve port-no)
  (logf "serve called\n")
  (define listener (tcp-listen port-no MAX-CONNECTIONS #t))
  (let loop ()
    (accept-and-handle listener)
    (loop)))

(define (accept-and-handle listener)
  (define-values (scp-in scp-out) (tcp-accept listener))
  (thread
   (lambda ()
     (logf "~a accepted tcp connection\n" PROGRAM-NAME)
     ;; (sleep (random 10)) ; try uncommenting this
     (handle scp-in scp-out)
     (logf "~a closing tcp connection\n" PROGRAM-NAME)
     (close-input-port scp-in)
     (close-output-port scp-out))))

(define (establish-mcp-to-scp-handshake scp-in scp-out)
  (logf "establishing mcp-to-scp handshake...\n")
  (define hello-msg (read scp-in))
  (when (or (eof-object? hello-msg)
            (not (equal? `(hello) hello-msg)))
    (logf "incorrect handshake message!  expected (hello), received ~s\n" hello-msg))
  (define scp-id #f)
  (call-with-semaphore
    scp-id-semaphore
    (lambda ()
      (set! scp-id *scp-id*)
      (set! *scp-id* (add1 *scp-id*))))
  (logf "assigned scp-id ~s for newly connected SCP\n" scp-id)
  (call-with-semaphore
    scp-connections-semaphore
    (lambda ()
      (set! *scp-connections*
            (cons `(,scp-id ,scp-in ,scp-out)
                  *scp-connections*))
      (logf "Updated *scp-connections* table: ~s" *scp-connections*)))
  (write `(scp-id ,scp-id) scp-out)
  (flush-output scp-out)
  (logf "mcp-to-scp handshake established\n"))

(define (forward-from-scp-to-mcp scp-in)
  (let loop ((msg (read scp-in)))
    (logf "~a received message from scp ~s\n" PROGRAM-NAME msg)
    (cond
      ((eof-object? msg)
       (logf "read eof from scp--exiting forward-from-scp-to-mcp\n"))
      (else
       ;; forward message to MCP
       (atomic-write/flush msg)
       (logf "~a forwarded message to mcp ~s\n" PROGRAM-NAME msg)
       (loop (read scp-in))))))

(define (handle scp-in scp-out)
  (logf "handle called for ~a\n" PROGRAM-NAME)
  (logf "establishing handshake...\n")
  (establish-mcp-to-scp-handshake scp-in scp-out)
  (logf "handshake established\n")
  (logf "calling forward-from-scp-to-mcp\n")
  (forward-from-scp-to-mcp scp-in))

(serve DEFAULT-TCP-PORT)
