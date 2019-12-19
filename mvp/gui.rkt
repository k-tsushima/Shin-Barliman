#lang racket

;;; Racket port of the "Classic" Barliman GUI.

;;; Code adapted from the mediKanren Racket GUI
;;; (https://github.com/webyrd/mediKanren).

#|
TODO

* Be able to recover when a connection to the server hangs (for example, if the server isn't running), and to be able to explicitly stop the connection process.

* Support any number of input/output tests.

* Support multiple synthesized results.

* Finish i18n support:
+ Update placeholder "Disconnect" messages for other languages.
+ Consider changing application title in the window's title bar when the language is changed.
+ Persistent preference mechanism to allow setting a default language.
+ Consider moving i18n strings to a separate "strings" file.

* Add font size preferences, etc., to a "Preferences" menu item.

* Update the available menus and menu items to better match the choices of a regular application.

* Clean up, simplify, generalize code. Aim for "pearl quality."
|#

(require
  racket/gui/base
  racket/tcp
  framework
  ;racket/engine
  ;(except-in racket/match ==)
  )

(provide
  launch-gui)

(define MAJOR-VERSION-NUMBER 0)
(define MINOR-VERSION-NUMBER 1)

;; 新 = しん = shin = "new"
;; https://jisho.org/search/%E6%96%B0
(define GUI-VERSION-STRING
  (format "新-Barliman ~a.~a" MAJOR-VERSION-NUMBER MINOR-VERSION-NUMBER))

(printf "Starting ~a\n..." GUI-VERSION-STRING)

;;; Initial window size
(define HORIZ-SIZE 1200)
(define VERT-SIZE 900)

(define *verbose* #t)

(define MAX-UNDO-DEPTH 1000)

(define DEFAULT-PROGRAM-TEXT "(define ,A\n  (lambda ,B\n    ,C))")

(define INITIAL-STATUS-MESSAGE-STRING (make-string 70 #\ ))
(define INITIAL-PLACEHOLDER-LABEL-STRING (make-string 40 #\ ))

(define INVALID-EXPRESSION-VALUE 'invalid-expression)

(define DEFINITIONS 'definitions)
(define SYNTHESIZED-RESULT 'synthesized-result)
(define EXPRESSION 'expression)
(define VALUE 'value)

(define CONNECTED 'connected)
(define DISCONNECTED 'disconnected)

(define NOT-SYNTHESIZING 'not-synthesizing)
(define SYNTHESIZING 'synthesizing)


(define I18N-STRINGS
  '(("English" .
     ("Language"
      "Server"
      "Port"
      "Connect"
      "Disconnect"
      "Synthesize"
      "Stop"
      "Definitions"
      "Synthesized Result"
      "Test"
      "Illegal expression!"
      "Too many expressions!"
      "Not connected"
      "\nConnecting to ~a..."
      "\nConnected to ~a"
      "\nUnable to connect to ~a"
      "\nDisconnecting from ~a..."
      "\nDisconnected from ~a"))
    ("日本語" .
     ("言語"
      "サーバー"
      "ポート番号"
      "接続"
      "FIXME 日本語 Disconnect"
      "FIXME 日本語 Synthesize"
      "FIXME 日本語 Stop"
      "定義"
      "プログラム合成結果"
      "テスト"
      "FIXME 日本語 Illegal expression!"
      "FIXME 日本語 Too many expressions!"
      "FIXME 日本語 Not connected"
      "\nFIXME 日本語 Connecting to ~a..."
      "\nFIXME 日本語 Connected to ~a"
      "\nFIXME 日本語 Unable to connect to ~a"
      "\nFIXME 日本語 Disconnecting from ~a..."
      "\nFIXME 日本語 Disconnected from ~a"))
    ("中文" .
     ("语言"
      "服务器"
      "端口"
      "链接"
      "FIXME 中文 Disconnect"
      "FIXME 中文 Synthesize"
      "FIXME 中文 Stop"
      "定义"
      "合成结果"
      "测试"
      "FIXME 中文 Illegal expression!"
      "FIXME 中文 Too many expressions!"
      "FIXME 中文 Not connected"
      "\nFIXME 中文 Connecting to ~a..."
      "\nFIXME 中文 Connected to ~a"
      "\nFIXME 中文 Unable to connect to ~a"
      "\nFIXME 中文 Disconnecting from ~a..."
      "\nFIXME 中文 Disconnected from ~a"))))

(define *editable-code-items-box* (box #f))
(define *synthesize-button-box* (box #f))

(define *receive-mcp-messages-thread-box* (box #f))

(define *test-messages-box* (box #f))

(define *current-focus-box* (box #f))
(define *tab-focus-order-box* (box '()))

(define *input-port-from-server-box* (box #f))
(define *output-port-to-server-box* (box #f))

(define *connection-state-box* (box DISCONNECTED))

(define *synthesis-state-box* (box NOT-SYNTHESIZING))


(define *GUI-language-box* (box (caar I18N-STRINGS)))

(define *connect-str-box* (box #f))
(define *disconnect-str-box* (box #f))

(define *synthesize-str-box* (box #f))
(define *stop-synthesis-str-box* (box #f))

(define *illegal-expression-str-box* (box #f))
(define *too-many-expressions-str-box* (box #f))

(define *not-connected-str-box* (box #f))
(define *connecting-to-str-box* (box #f))
(define *connected-to-str-box* (box #f))
(define *unable-to-connect-str-box* (box #f))
(define *disconnecting-str-box* (box #f))
(define *disconnected-str-box* (box #f))


(define (read-exprs-from-string str name)
  (with-handlers ([exn:fail? (lambda (exn)
                               (printf "exn for ~s: ~s\n" name exn)
                               INVALID-EXPRESSION-VALUE)])
    (let ((ip (open-input-string str)))
      (let loop ([x (read ip)])
        (cond
          [(eof-object? x) '()]
          [else (cons x (loop (read ip)))])))))

;; Current expression(s) for each user-editable editor canvas.
;;
;; Valid expressions are contained in a list.
;;
;; A list may contain multiple valid expressions.  This certainly
;; makes sense for definitions.  Multiple expressions for other
;; canvases may not make sense, and should perhaps be treated as an
;; error.
;;
;; Invalid expression(s) are represented by the non-list value
;; INVALID-EXPRESSION-VALUE.
(define *definitions-exprs-box*
  (box (read-exprs-from-string DEFAULT-PROGRAM-TEXT 'default-program-text)))
;;
(define *test-1-expression-exprs-box* (box '()))
(define *test-1-value-exprs-box* (box '()))
;;
(define *test-2-expression-exprs-box* (box '()))
(define *test-2-value-exprs-box* (box '()))
;;
(define *test-3-expression-exprs-box* (box '()))
(define *test-3-value-exprs-box* (box '()))
;;
(define *test-4-expression-exprs-box* (box '()))
(define *test-4-value-exprs-box* (box '()))
;;
(define *test-5-expression-exprs-box* (box '()))
(define *test-5-value-exprs-box* (box '()))
;;
(define *test-6-expression-exprs-box* (box '()))
(define *test-6-value-exprs-box* (box '()))

;; List of *box*/canvas-type/[optional-canvas-number] for the
;; user-editable canvas boxes containing exprs.
(define *user-editable-canvases-boxes*
  (list
    (list *definitions-exprs-box* DEFINITIONS)
    (list *test-1-expression-exprs-box* (list EXPRESSION 1))
    (list *test-1-value-exprs-box* (list VALUE 1))
    (list *test-2-expression-exprs-box* (list EXPRESSION 2))
    (list *test-2-value-exprs-box* (list VALUE 2))
    (list *test-3-expression-exprs-box* (list EXPRESSION 3))
    (list *test-3-value-exprs-box* (list VALUE 3))
    (list *test-4-expression-exprs-box* (list EXPRESSION 4))
    (list *test-4-value-exprs-box* (list VALUE 4))
    (list *test-5-expression-exprs-box* (list EXPRESSION 5))
    (list *test-5-value-exprs-box* (list VALUE 5))
    (list *test-6-expression-exprs-box* (list EXPRESSION 6))
    (list *test-6-value-exprs-box* (list VALUE 6))
    ))

(define (print-all-user-editable-canvases-boxes-values)
  (let loop ([b* *user-editable-canvases-boxes*])
    (match b*
      ['() (void)]
      [`((,b ,type) . ,rest)
       (printf "~s: ~s\n" type (unbox b))
       (loop rest)])))

(define (all-user-editable-canvases-boxes-values)
  (let loop ([b* *user-editable-canvases-boxes*])
    (match b*
      ['() '()]
      [`((,b ,type) . ,rest)
       (cons
        (list type (unbox b))
        (loop rest))])))

(define (all-input-boxes-values)
  (let loop ([b* *user-editable-canvases-boxes*])
    (match b*
      ['() '()]
      [`((,b (expression ,n)) . ,rest)
       (let ((v (unbox b)))
         (cond
           ((null? v) (loop rest))
           (else (cons (car v) (loop rest)))))]
      [`((,b ,type) . ,rest)
       (loop rest)])))

(define (all-output-boxes-values)
  (let loop ([b* *user-editable-canvases-boxes*])
    (match b*
      ['() '()]
      [`((,b (value ,n)) . ,rest)
       (let ((v (unbox b)))
         (cond
           ((null? v) (loop rest))
           (else (cons (car v) (loop rest)))))]
      [`((,b ,type) . ,rest)
       (loop rest)])))

(define (send-synthesize-message)  
  (when (all-user-canvas-boxes-have-legal-exprs?)
    (define definitions
      (unbox *definitions-exprs-box*))
    (define inputs
      (all-input-boxes-values))
    (define outputs
      (all-output-boxes-values))
    (define msg
      `(synthesize (,definitions ,inputs ,outputs)))
    (send-message msg)))

(define (send-stop-synthesis-message)
  (define msg
    `(stop))
  (send-message msg))

(define (send-message msg)
  (define in (unbox *input-port-from-server-box*))
  (define out (unbox *output-port-to-server-box*))
  (when (and in out)
    (printf "sending message ~s\n" msg)
    (write msg out)
    (flush-output out)))

(define wait-on-mcp-synthesis-results
  (lambda ()
    (printf "wait-on-mcp-synthesis-results starting up...\n")
    (define in (unbox *input-port-from-server-box*))
    (define out (unbox *output-port-to-server-box*))
    (when (and in out)
      (printf "wait-on-mcp-synthesis-results waiting for message...\n")
      (let loop ((msg (read in)))
        (printf "wait-on-mcp-synthesis-results received message ~s\n" msg)
        (cond
          ((eof-object? msg)
           (void))
          (else
           (match msg
             (`(goodbye)
              (printf "wait-on-mcp-synthesis-results received goodbye from mcp!  Dun with fish\n"))
             (`(keep-going)
              (printf "wait-on-mcp-synthesis-results xoreceived keep-going from mcp!  Onward...\n")
              (loop (read in)))
             (else (error 'wait-on-mcp-synthesis-results
                          (format "unknown message type: ~s" msg))))))))))

(define smart-top-level-window%
 (class frame%
   (super-new)
   (define (on-subwindow-focus receiver on?)
     (if on?
         (set-box! *current-focus-box* receiver)
         (set-box! *current-focus-box* #f))
     (void))
   (define (on-traverse-char event)
     (let ((key-code (send event get-key-code)))
       (if (eqv? #\tab key-code)
           (let ((current-focus (unbox *current-focus-box*)))             
             (if current-focus
                 (let* ((tfo (unbox *tab-focus-order-box*))
                        (shift-down? (send event get-shift-down))
                        (tfo (if shift-down? (reverse tfo) tfo))
                        (o* (member current-focus tfo)))
                   (if o*
                       (send (cadr o*) focus)
                       #f))   
                 #f))
           #f)))
   (override on-traverse-char)
   (override on-subwindow-focus)))

(define (get-user-canvas-box type)
  (let loop ([b* *user-editable-canvases-boxes*])
    (match b*
      ['() (error 'get-user-canvas-box
                  (format "box not found: ~s" type))]
      [`((,b ,t) . ,rest)
       (if (equal? t type)
           b
           (loop rest))])))

(define (update-user-canvas-box! type new-expr)
  (let ((b (get-user-canvas-box type)))
    (set-box! b new-expr)))

(define (all-user-canvas-boxes-have-legal-exprs?)
  (let loop ([b* *user-editable-canvases-boxes*])    
    (match b*
      ['() #t]
      [`((,b ,t) . ,rest)
       (and (not (user-canvas-box-error b t))
            (loop rest))])))

(define (user-canvas-box-error exprs-box type)
  (let ((expr* (unbox exprs-box)))
    (cond
      [(equal? INVALID-EXPRESSION-VALUE expr*)
       (unbox *illegal-expression-str-box*)]
      [(list? expr*)
       (match type
         [`(,t ,m)
          (unless (or (equal? t EXPRESSION)
                      (equal? t VALUE))
            (error 'user-canvas-box-has-legal-exprs?
                   (format "unknown type: ~s\n" t)))
          ;; for test expression and value canvases,
          ;; make sure the list of expressions is either
          ;; empty or of length 1 (disallow multiple
          ;; expressions)
          (if (> (length expr*) 1)
              (unbox *too-many-expressions-str-box*)
              #f)]
         [else #f])]
      [else (error 'user-canvas-box-error
                   (format "unexpected expr*: ~s" expr*))])))

(define (make-smart-text% type canvas status-message . args)
  (let ((exprs-box (if (= (length args) 1)
                       (car args)
                       #f)))
    (let ((name (match type
                  [`(,t ,n) (format "~s ~s" t n)]
                  [else (format "~s" type)])))
      (class racket:text%
        (super-new)
        (define (after-insert start len)
          (printf "Hello from ~s\n" name))
        (define (after-edit-sequence)
          (printf "after-edit-sequence called for ~s\n" name)
          (define str (send this get-text))
          (printf "text for ~s: ~s\n" name str)
          (define exprs-in-list (read-exprs-from-string str name))
          (printf "~s exprs-in-list: ~s\n" name exprs-in-list)
          
          ;; Ignore any canvas that isn't enabled/user editable
          ;; ('synthesized-result')
          (when (send canvas is-enabled?)
            (set-box! exprs-box exprs-in-list)            
            (when (list? exprs-in-list)
              (if (= (length exprs-in-list) 1)
                  (printf "~s single raw expr: ~s\n" name (car exprs-in-list))
                  (begin
                    (printf "~s multiple raw exprs:\n" name)
                    (for-each
                      (lambda (expr)
                        (printf "~s\n" expr))
                      exprs-in-list))))

            (let ((e (user-canvas-box-error exprs-box type)))
              (if e
                  (send status-message set-label e)
                  (send status-message set-label INITIAL-STATUS-MESSAGE-STRING)))
            
            (printf "======================================\n")
            (print-all-user-editable-canvases-boxes-values)            
            (printf "======================================\n")                       
            (if (all-user-canvas-boxes-have-legal-exprs?)
                (begin
                  (printf "all user canvas boxes have legal exprs!\n")
                  (when (and (equal? (unbox *connection-state-box*) CONNECTED)
                             (equal? (unbox *synthesis-state-box*) NOT-SYNTHESIZING))
                    (send (unbox *synthesize-button-box*) enable #t)))
                (begin
                  (printf "at least one canvas box contains an illegal expr!\n")
                  (when (and (equal? (unbox *connection-state-box*) CONNECTED)
                             (equal? (unbox *synthesis-state-box*) NOT-SYNTHESIZING))
                    (send (unbox *synthesize-button-box*) enable #f))))
            (newline))
          (void))
        (augment after-insert)
        (augment after-edit-sequence)))))


(define smart-text%
 (class racket:text%
   (super-new)
   (define (after-insert start len)
     (printf "Hello\n"))
   (augment after-insert)))


(define (launch-main-window)
  (let ((top-window (new smart-top-level-window%
                         (label GUI-VERSION-STRING)
                         (width HORIZ-SIZE)
                         (height VERT-SIZE))))

    (define outermost-hor-draggable-panel
      (new panel:horizontal-dragable%
           (parent top-window)
           (alignment '(left center))
           (stretchable-height #t)))

    (define left-vert-draggable-panel
      (new panel:vertical-dragable%
           (parent outermost-hor-draggable-panel)
           (alignment '(left center))))

    (define right-panel
      (new vertical-pane%
           (parent outermost-hor-draggable-panel)
           (alignment '(left top))
           (stretchable-height #f)))

    (define left-top-panel
      (new vertical-pane%
           (parent left-vert-draggable-panel)
           (alignment '(left center))))

    (define left-bottom-panel
      (new vertical-pane%
           (parent left-vert-draggable-panel)
           (alignment '(left center))))

    (define server-info-panel
      (new horizontal-pane%
           (parent left-top-panel)
           (alignment '(center center))
           (stretchable-height #f)))

    (define update-GUI-text-for-language
      (lambda () (error 'update-GUI-text-for-language "uninitialized")))
    
    (define gui-language-choice
      (new choice%
           (label "Language")
           (parent server-info-panel)
           (choices (map car I18N-STRINGS))
           (callback (lambda (self event)
                       (define lang (send self get-string-selection))
                       (printf "User selected language ~s\n" lang)
                       (set-box! *GUI-language-box* lang)
                       (update-GUI-text-for-language)))))
    
    (define server-info-hor-draggable-panel
      (new panel:horizontal-dragable%
           (parent server-info-panel)
           (alignment '(left center))
           (stretchable-height #f)))
    
    (define server-ip-address-field
      (new text-field%
           (parent server-info-hor-draggable-panel)
           (label "Server")
           (init-value "localhost")))

    (define server-port-field
      (new text-field%
           (parent server-info-hor-draggable-panel)
           (label "Port")
           (init-value "8080")))

    (define server-connect-button
      (new button%
           (parent server-info-panel)
           (label "Connect")
           (callback (lambda (self event)

                       (define address-str
                         (send server-ip-address-field
                               get-value))
                       (define port-str
                         (send server-port-field
                               get-value))
                       (define full-address-str
                         (string-append address-str ":" port-str))

                       ;; TODO -- this may not be a legal port number,
                       ;; or a number at all!
                       (define port (string->number port-str))
                         
                       (cond
                         ((equal? (unbox *connection-state-box*) DISCONNECTED)

                          ;; TODO Implement timeout for connecting
                          ;; (for example, server may not be running)
                            
                          (send server-messages-text insert
                                (format (unbox *connecting-to-str-box*)
                                        full-address-str))
                          (printf "Connecting to ~a...\n"
                                  full-address-str)

                          (define-values (in out)
                            (tcp-connect address-str port))

                          (if (and in out)
                              (begin
                                ;; connection succeeded...
                                (set-box! *input-port-from-server-box* in)
                                (set-box! *output-port-to-server-box* out)
                                
                                (set-box! *connection-state-box* CONNECTED)
                                  
                                (send self set-label (unbox *disconnect-str-box*))
                                  
                                (send server-ip-address-field enable #f)
                                (send server-port-field enable #f)

                                (send synthesize-button enable #t)
                                
                                (send server-messages-text insert
                                      (format (unbox *connected-to-str-box*)
                                              full-address-str))
                                (printf "Connected to ~a\n"
                                        full-address-str)                                                                  
                                )
                              (begin

                                (send server-messages-text insert
                                      (format (unbox *unable-to-connect-str-box*)
                                              full-address-str))
                                (printf "Unable to connect to ~a\n"
                                        full-address-str)                                  
                                ))
                          )

                         ((equal? (unbox *connection-state-box*) CONNECTED)

                          (send server-messages-text insert
                                (format (unbox *disconnecting-str-box*)
                                        full-address-str))
                          (printf "Disconnecting from ~a...\n"
                                  full-address-str)

                          (define in (unbox *input-port-from-server-box*))
                          (define out (unbox *output-port-to-server-box*))
                           
                          (when in
                            (close-input-port in)
                            (set-box! *input-port-from-server-box* #f))
                          (when out
                            (close-output-port out)
                            (set-box! *output-port-to-server-box* #f))

                          ;; disconnection succeeded
                          ;;
                          (set-box! *connection-state-box* DISCONNECTED)
                          ;;
                          (send self set-label (unbox *connect-str-box*))
                          ;;
                          (send server-ip-address-field enable #t)
                          (send server-port-field enable #t)
                          
                          (send synthesize-button enable #f)
                          
                          (send server-messages-text insert
                                (format (unbox *disconnected-str-box*)
                                        full-address-str))
                          (printf "Disconnected from ~a\n"
                                  full-address-str)
                            
                          )

                         (else
                          (error 'server-connect-button
                                 (format "unexpected state ~s" (unbox *connection-state-box*)))))
                       ))))

    (define synthesize-button
      (new button%
           (parent server-info-panel)
           (label "Synthesize")
           (enabled #f)
           (callback (lambda (self event)
                       (printf "clicked on 'Synthesize' button\n")

                       (define old-synthesize-state (unbox *synthesis-state-box*))
                       (define new-synthesize-state
                         (cond
                           ((equal? SYNTHESIZING old-synthesize-state) NOT-SYNTHESIZING)
                           ((equal? NOT-SYNTHESIZING old-synthesize-state) SYNTHESIZING)
                           (else (error 'synthesize-button "unknown synthesize state ~s" old-synthesize-state))))

                       (printf "old-synthesize-state ~s\n" old-synthesize-state)
                       (printf "new-synthesize-state ~s\n" new-synthesize-state)
                       
                       (set-box! *synthesis-state-box* new-synthesize-state)

                       (if (equal? SYNTHESIZING new-synthesize-state)
                           (send synthesize-button set-label (unbox *stop-synthesis-str-box*))
                           (send synthesize-button set-label (unbox *synthesize-str-box*)))

                       (cond
                         ((equal? SYNTHESIZING new-synthesize-state)
                          
                          ;; disable editing for definitions and all input/output examples
                          (for-each (lambda (obj) (send obj enable #f)) (unbox *editable-code-items-box*))
                          
                          ;; send synthesis message to MCP
                          (if (all-user-canvas-boxes-have-legal-exprs?)
                              (send-synthesize-message)
                              (error 'synthesize-button
                                     "tried to send synthesis message with illegal exprs"))
                          
                          ;; start thread with loop waiting for MCP synthesis results/displaying synthesis results
                          (set-box! *receive-mcp-messages-thread-box* (thread wait-on-mcp-synthesis-results)))
                         ((equal? NOT-SYNTHESIZING new-synthesize-state)
                          (send-stop-synthesis-message)

                          ;; kill loop waiting thread
                          (when (unbox *receive-mcp-messages-thread-box*)
                            (kill-thread (unbox *receive-mcp-messages-thread-box*))
                            (set-box! *receive-mcp-messages-thread-box* #f))
                          
                          ;; enable editing for definitions and all input/output examples
                          (for-each (lambda (obj) (send obj enable #t)) (unbox *editable-code-items-box*)))
                         )
                       ))))
    (set-box! *synthesize-button-box* synthesize-button)

    (define server-messages-editor-canvas
      (new editor-canvas%
           (parent left-top-panel)
           (enabled #f)
           (line-count 2)
           (stretchable-height #f)
           (label "Server Messages")))
    (define server-messages-text (new text%))
    (send server-messages-editor-canvas
          set-editor server-messages-text)
    
    (define definitions-messages-panel
      (new horizontal-pane%
           (parent left-top-panel)
           (alignment '(center center))
           (stretchable-height #f)))

    (define definitions-messages-panel-left
      (new horizontal-pane%
           (parent definitions-messages-panel)
           (alignment '(left center))
           (stretchable-height #f)))

    (define definitions-messages-panel-right
      (new horizontal-pane%
           (parent definitions-messages-panel)
           (alignment '(right center))
           (stretchable-height #f)))
    
    (define definitions-message
      (new message%
           (parent definitions-messages-panel-left)
           (label INITIAL-PLACEHOLDER-LABEL-STRING)))

    (define definitions-status-message
      (new message%
           (parent definitions-messages-panel-right)
           (label INITIAL-STATUS-MESSAGE-STRING)
           (auto-resize #f)))

    (define definitions-editor-canvas
      (new editor-canvas%
           (parent left-top-panel)
           (label INITIAL-PLACEHOLDER-LABEL-STRING)
           (style '(hide-hscroll hide-vscroll))))
    (define definitions-text
      (new (make-smart-text%
            DEFINITIONS
            definitions-editor-canvas
            definitions-status-message
            *definitions-exprs-box*)))
    (send definitions-text insert DEFAULT-PROGRAM-TEXT)
    (send definitions-editor-canvas set-editor definitions-text)
    (send definitions-text set-max-undo-history MAX-UNDO-DEPTH)


    (define synthesized-result-messages-panel
      (new horizontal-pane%
           (parent left-top-panel)
           (alignment '(center center))
           (stretchable-height #f)))

    (define synthesized-result-messages-panel-left
      (new horizontal-pane%
           (parent synthesized-result-messages-panel)
           (alignment '(left center))
           (stretchable-height #f)))

    (define synthesized-result-messages-panel-right
      (new horizontal-pane%
           (parent synthesized-result-messages-panel)
           (alignment '(right center))
           (stretchable-height #f)))


    

    (define synthesized-result-message
      (new message%
           (parent synthesized-result-messages-panel-left)
           (label INITIAL-PLACEHOLDER-LABEL-STRING)))

    (define synthesized-result-status-message
      (new message%
           (parent synthesized-result-messages-panel-right)
           (label INITIAL-STATUS-MESSAGE-STRING)))

    
    (define synthesized-result-editor-canvas
      (new editor-canvas%
           (parent left-bottom-panel)
           (label INITIAL-PLACEHOLDER-LABEL-STRING)
           (style '(hide-hscroll hide-vscroll))
           (enabled #f)))
    (define synthesized-result-text
      (new (make-smart-text%
            SYNTHESIZED-RESULT
            synthesized-result-editor-canvas
            synthesized-result-status-message)))
    (send synthesized-result-text insert "")
    (send synthesized-result-editor-canvas set-editor synthesized-result-text)


    
    (define (make-test-message/status/status/expression/value
             n
             parent-panel
             expression-exprs-box
             value-exprs-box)

      (define (make-test-editor-canvas
               type-name
               n
               parent-panel
               status-message
               exprs-box)
        (define test-editor-canvas
          (new editor-canvas%
               (parent parent-panel)
               (style '(hide-hscroll hide-vscroll))))
        (define test-text
          (new (make-smart-text%
                (list type-name n)
                test-editor-canvas
                status-message
                exprs-box)))
        (send test-editor-canvas set-editor test-text)
        (send test-text set-max-undo-history MAX-UNDO-DEPTH)

        test-editor-canvas)

      (define expression-messages-panel
        (new horizontal-pane%
             (parent parent-panel)
             (alignment '(center center))
             (stretchable-height #f)))

      (define expression-messages-panel-left
        (new horizontal-pane%
             (parent expression-messages-panel)
             (alignment '(left center))
             (stretchable-height #f)))

      (define expression-messages-panel-right
        (new horizontal-pane%
             (parent expression-messages-panel)
             (alignment '(right center))
             (stretchable-height #f)))

      
      (define test-expression-message
        (new message%
             (parent expression-messages-panel-left)
             (label (format "~a ~a      " INITIAL-PLACEHOLDER-LABEL-STRING n))))

      (define test-expression-status-message
        (new message%
             (parent expression-messages-panel-right)
             (label INITIAL-STATUS-MESSAGE-STRING)))

      (define test-expression-editor-canvas
        (make-test-editor-canvas
         EXPRESSION
         n
         parent-panel
         test-expression-status-message
         expression-exprs-box))

      (define value-messages-panel
        (new horizontal-pane%
             (parent parent-panel)
             (alignment '(center center))
             (stretchable-height #f)))

      (define value-messages-panel-left
        (new horizontal-pane%
             (parent value-messages-panel)
             (alignment '(left center))
             (stretchable-height #f)))

      (define value-messages-panel-right
        (new horizontal-pane%
             (parent value-messages-panel)
             (alignment '(right center))
             (stretchable-height #f)))
      
      (define test-value-status-message
        (new message%
             (parent value-messages-panel-right)
             (label INITIAL-STATUS-MESSAGE-STRING)))
      
      (define test-value-editor-canvas
        (make-test-editor-canvas
         VALUE
         n
         parent-panel
         test-value-status-message
         value-exprs-box))
      
      (values test-expression-message
              test-expression-status-message
              test-value-status-message
              test-expression-editor-canvas
              test-value-editor-canvas))

    (let*-values ([(test-1-message
                    test-1-expression-status-message
                    test-1-value-status-message
                    test-expression-1-editor-canvas
                    test-value-1-editor-canvas)
                   (make-test-message/status/status/expression/value
                    1
                    right-panel
                    *test-1-expression-exprs-box*
                    *test-1-value-exprs-box*)]
                  [(test-2-message
                    test-2-expression-status-message
                    test-2-value-status-message
                    test-expression-2-editor-canvas
                    test-value-2-editor-canvas)
                   (make-test-message/status/status/expression/value
                    2
                    right-panel
                    *test-2-expression-exprs-box*
                    *test-2-value-exprs-box*)]
                  [(test-3-message
                    test-3-expression-status-message
                    test-3-value-status-message
                    test-expression-3-editor-canvas
                    test-value-3-editor-canvas)
                   (make-test-message/status/status/expression/value
                    3
                    right-panel
                    *test-3-expression-exprs-box*
                    *test-3-value-exprs-box*)]
                  [(test-4-message
                    test-4-expression-status-message
                    test-4-value-status-message
                    test-expression-4-editor-canvas
                    test-value-4-editor-canvas)
                   (make-test-message/status/status/expression/value
                    4
                    right-panel
                    *test-4-expression-exprs-box*
                    *test-4-value-exprs-box*)]
                  [(test-5-message
                    test-5-expression-status-message
                    test-5-value-status-message
                    test-expression-5-editor-canvas
                    test-value-5-editor-canvas)
                   (make-test-message/status/status/expression/value
                    5
                    right-panel
                    *test-5-expression-exprs-box*
                    *test-5-value-exprs-box*)]
                  [(test-6-message
                    test-6-expression-status-message
                    test-6-value-status-message
                    test-expression-6-editor-canvas
                    test-value-6-editor-canvas)
                   (make-test-message/status/status/expression/value
                    6
                    right-panel
                    *test-6-expression-exprs-box*
                    *test-6-value-exprs-box*)])

      (define test-messages
        (list
         test-1-message
         test-2-message
         test-3-message
         test-4-message
         test-5-message
         test-6-message))

      (set-box! *test-messages-box* test-messages)

      (define editable-code-items
        (list
         definitions-editor-canvas
         ;;
         test-expression-1-editor-canvas
         test-value-1-editor-canvas
         ;;
         test-expression-2-editor-canvas
         test-value-2-editor-canvas
         ;;
         test-expression-3-editor-canvas
         test-value-3-editor-canvas
         ;;
         test-expression-4-editor-canvas
         test-value-4-editor-canvas
         ;;
         test-expression-5-editor-canvas
         test-value-5-editor-canvas
         ;;
         test-expression-6-editor-canvas
         test-value-6-editor-canvas
         ))

      (set-box! *editable-code-items-box* editable-code-items)
      
      (define tabbable-items editable-code-items)

      (define wrappable-tabbable-items
        (append
         ;; wrap around (reverse)
         (list (car (reverse tabbable-items)))
         tabbable-items
         ;; wrap around (forward)
         (list (car tabbable-items))))

      (set! update-GUI-text-for-language
        (lambda ()
          
          (define lang (unbox *GUI-language-box*))
          
          (define current-lang-strings (assoc lang I18N-STRINGS))
          
          (match current-lang-strings
            (`(,_
               ,language-str
               ,server-str
               ,port-str
               ,connect-str
               ,disconnect-str
               ,synthesize-str
               ,stop-synthesis-str
               ,definitions-str
               ,synthesized-result-str
               ,test-str
               ,illegal-expression-str
               ,too-many-expressions-str
               ,not-connected-str
               ,connecting-to-str
               ,connected-to-str
               ,unable-to-connect-str
               ,disconnecting-str
               ,disconnected-str)
             
             (send gui-language-choice set-label language-str)
             (send server-ip-address-field set-label server-str)
             (send server-port-field set-label port-str)
             
             (set-box! *connect-str-box* connect-str)
             (set-box! *disconnect-str-box* disconnect-str)
             
             (if (equal? (unbox *connection-state-box*) DISCONNECTED)
                 (send server-connect-button set-label (unbox *connect-str-box*))
                 (send server-connect-button set-label (unbox *disconnect-str-box*)))


             (set-box! *synthesize-str-box* synthesize-str)
             (set-box! *stop-synthesis-str-box* stop-synthesis-str)
             
             (if (equal? (unbox *synthesis-state-box*) SYNTHESIZING)
                 (send synthesize-button set-label (unbox *stop-synthesis-str-box*))
                 (send synthesize-button set-label (unbox *synthesize-str-box*)))
             
             
             (set-box! *illegal-expression-str-box* illegal-expression-str)
             (set-box! *too-many-expressions-str-box* too-many-expressions-str)

             (set-box! *not-connected-str-box* not-connected-str)
             (set-box! *connecting-to-str-box* connecting-to-str)
             (set-box! *connected-to-str-box* connected-to-str)
             (set-box! *unable-to-connect-str-box* unable-to-connect-str)
             (set-box! *disconnecting-str-box* disconnecting-str)
             (set-box! *disconnected-str-box* disconnected-str)             
             
             (send definitions-message set-label definitions-str)
             (send synthesized-result-message set-label synthesized-result-str)
             
             (define test-messages (unbox *test-messages-box*))
             (when test-messages
               (let loop ((test-messages test-messages)
                          (n 1))
                 (cond
                   ((null? test-messages) (void))
                   (else (let ((test-message (car test-messages)))
                           (send test-message set-label
                                 (format "~a ~a" test-str n)))
                         (loop (cdr test-messages)
                               (add1 n))))))
             )
            (else (error 'update-GUI-text-for-language
                         (format "current-lang-strings is ~s for language ~s" current-lang-strings lang))))))
      
      (set-box! *tab-focus-order-box* wrappable-tabbable-items)

      (update-GUI-text-for-language)
      
      ;; trigger reflowing of object sizes
      (send top-window reflow-container)
    
      (send top-window show #t)
      (send definitions-editor-canvas focus)
      
      )))


(define (launch-gui)
  (launch-main-window))

(displayln
  "Launching GUI")

(launch-gui)
