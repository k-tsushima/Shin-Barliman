#lang racket

;;; Racket port of the "Classic" Barliman GUI.

;;; Code adapted from the mediKanren Racket GUI
;;; (https://github.com/webyrd/mediKanren).

(require
  racket/gui/base
  racket/tcp
  framework
  ;racket/engine
  ;(except-in racket/match ==)
  )

(provide
  launch-gui)

;; 新 = しん = shin = "new"
;; https://jisho.org/search/%E6%96%B0
(define CLASSIC-GUI-VERSION-STRING "新-Barliman Classic 4.3")

(displayln "Starting Shin-Barliman Classic...")
(displayln CLASSIC-GUI-VERSION-STRING)

;;; Initial window size
(define HORIZ-SIZE 1200)
(define VERT-SIZE 900)

(define *verbose* #t)

(define MAX-UNDO-DEPTH 1000)

(define DEFAULT-PROGRAM-TEXT "(define ,A\n  (lambda ,B\n    ,C))")

(define INITIAL-STATUS-MESSAGE-STRING (make-string 50 #\ ))

(define INVALID-EXPRESSION-VALUE 'invalid-expression)

(define DEFINITIONS 'definitions)
(define BEST-GUESS 'best-guess)
(define EXPRESSION 'expression)
(define VALUE 'value)

(define CONNECTED 'connected)
(define DISCONNECTED 'disconnected)

(define LANG_ENGLISH "English")
(define LANG_JAPANESE "日本語")

(define *current-focus-box* (box #f))
(define *tab-focus-order-box* (box '()))

(define *input-port-from-server* (box #f))
(define *output-port-to-server* (box #f))

(define (read-expr*-from-string str name)
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
(define *definitions-expr*-box*
  (box (read-expr*-from-string DEFAULT-PROGRAM-TEXT 'default-program-text)))
;;
(define *test-1-expression-expr*-box* (box '()))
(define *test-1-value-expr*-box* (box '()))
;;
(define *test-2-expression-expr*-box* (box '()))
(define *test-2-value-expr*-box* (box '()))
;;
(define *test-3-expression-expr*-box* (box '()))
(define *test-3-value-expr*-box* (box '()))
;;
(define *test-4-expression-expr*-box* (box '()))
(define *test-4-value-expr*-box* (box '()))
;;
(define *test-5-expression-expr*-box* (box '()))
(define *test-5-value-expr*-box* (box '()))
;;
(define *test-6-expression-expr*-box* (box '()))
(define *test-6-value-expr*-box* (box '()))

;; List of *box*/canvas-type/[optional-canvas-number] for the
;; user-editable canvas boxes containing exprs.
(define *user-editable-canvases-boxes*
  (list
    (list *definitions-expr*-box* DEFINITIONS)
    (list *test-1-expression-expr*-box* (list EXPRESSION 1))
    (list *test-1-value-expr*-box* (list VALUE 1))
    (list *test-2-expression-expr*-box* (list EXPRESSION 2))
    (list *test-2-value-expr*-box* (list VALUE 2))
    (list *test-3-expression-expr*-box* (list EXPRESSION 3))
    (list *test-3-value-expr*-box* (list VALUE 3))
    (list *test-4-expression-expr*-box* (list EXPRESSION 4))
    (list *test-4-value-expr*-box* (list VALUE 4))
    (list *test-5-expression-expr*-box* (list EXPRESSION 5))
    (list *test-5-value-expr*-box* (list VALUE 5))
    (list *test-6-expression-expr*-box* (list EXPRESSION 6))
    (list *test-6-value-expr*-box* (list VALUE 6))
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

(define (send-synthesize-message)

  (define in (unbox *input-port-from-server*))
  (define out (unbox *output-port-to-server*))
  
  (when (and in
             out
             (all-user-canvas-boxes-have-legal-exprs?))
    (define vals
      (all-user-editable-canvases-boxes-values))
    (define synthesize-msg
      `(synthesize-kudasai
        (from gui)
        (vals ,vals)))

    (printf "sending message ~s\n"
            synthesize-msg)

    (write synthesize-msg out)
    (flush-output out)
    )
  )

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

(define (user-canvas-box-error expr*-box type)
  (let ((expr* (unbox expr*-box)))
    (cond
      [(equal? INVALID-EXPRESSION-VALUE expr*)
       "Illegal expression!"]
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
              "Too many expressions!"
              #f)]
         [else #f])]
      [else (error 'user-canvas-box-error
                   (format "unexpected expr*: ~s" expr*))])))

(define (make-smart-text% type canvas status-message . args)
  (let ((expr*-box (if (= (length args) 1)
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
          (define expr*-in-list (read-expr*-from-string str name))
          (printf "~s expr*-in-list: ~s\n" name expr*-in-list)
          
          ;; Ignore any canvas that isn't enabled/user editable
          ;; ('best-guess')
          (when (send canvas is-enabled?)
            (set-box! expr*-box expr*-in-list)            
            (when (list? expr*-in-list)
              (if (= (length expr*-in-list) 1)
                  (printf "~s single raw expr: ~s\n" name (car expr*-in-list))
                  (begin
                    (printf "~s multiple raw exprs:\n" name)
                    (for-each
                      (lambda (expr)
                        (printf "~s\n" expr))
                      expr*-in-list))))

            (let ((e (user-canvas-box-error expr*-box type)))
              (if e
                  (send status-message set-label e)
                  (send status-message set-label INITIAL-STATUS-MESSAGE-STRING)))
            
            (printf "======================================\n")
            (print-all-user-editable-canvases-boxes-values)
            (send-synthesize-message)
            (printf "======================================\n")
            (if (all-user-canvas-boxes-have-legal-exprs?)
                (printf "all user canvas boxes have legal exprs!\n")
                (printf "at least one canvas box contains an illegal expr!\n"))
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
                         (label CLASSIC-GUI-VERSION-STRING)
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

    (define gui-language-choice
      (new choice%
           (label "Language")
           (parent server-info-panel)
           (choices (list LANG_ENGLISH LANG_JAPANESE))
           (callback (lambda (self event)
                       
                       (define lang (send self get-string-selection))
                       
                       (printf "User selected language ~s\n" lang)

                       (cond
                         ((equal? lang LANG_ENGLISH)
                          (printf "Eigo\n"))
                         ((equal? lang LANG_JAPANESE)
                          (printf "Nihongo\n"))
                         (else (error 'gui-language-choice
                                      (format "unknown language ~s" lang))))
                       
                       ))))

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
      (let ((state DISCONNECTED))
        (new button%
             (parent server-info-panel)
             (label " Connect ")
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
                           ((equal? state DISCONNECTED)

                            ;; TODO Implement timeout for connecting
                            ;; (for example, server may not be running)
                            
                            (send server-messages-text insert
                                  (format "\nConnecting to ~a..."
                                          full-address-str))
                            (printf "Connecting to ~a...\n"
                                    full-address-str)

                            (define-values (in out)
                              (tcp-connect address-str port))

                            (if (and in out)
                                (begin
                                  ;; connection succeeded...
                                  (set-box! *input-port-from-server* in)
                                  (set-box! *output-port-to-server* out)
                                  
                                  (set! state CONNECTED)
                                  
                                  (send self set-label "Disconnect")
                                  
                                  (send server-ip-address-field enable #f)
                                  (send server-port-field enable #f)
                                  
                                  (send server-messages-text insert
                                        (format "\nConnected to ~a"
                                                full-address-str))
                                  (printf "Connected to ~a\n"
                                          full-address-str)
                                  
                                  ;; send message with definitions and
                                  ;; examples to server
                                  (send-synthesize-message)
                                  
                                  )
                                (begin

                                  (send server-messages-text insert
                                        (format "\nUnable to connect to ~a"
                                                full-address-str))
                                  (printf "Unable to connect to ~a\n"
                                          full-address-str)
                                  
                                  ))
                            )

                           ((equal? state CONNECTED)

                            (send server-messages-text insert
                                  (format "\nDisconnecting from ~a..."
                                          full-address-str))
                            (printf "Disconnecting from ~a...\n"
                                    full-address-str)

                            (define in (unbox *input-port-from-server*))
                            (define out (unbox *output-port-to-server*))
                           
                            (when in
                              (close-input-port in)
                              (set-box! *input-port-from-server* #f))
                            (when out
                              (close-output-port out)
                              (set-box! *output-port-to-server* #f))

                            ;; disconnection succeeded
                            ;;
                            (set! state DISCONNECTED)
                            ;;
                            (send self set-label " Connect ")
                            ;;
                            (send server-ip-address-field enable #t)
                            (send server-port-field enable #t)
                            
                            (send server-messages-text insert
                                  (format "\nDisconnected from ~a"
                                          full-address-str))
                            (printf "Disconnected from ~a\n"
                                    full-address-str)
                            
                            )

                           (else
                            (error 'server-connect-button
                                   (format "unexpected state ~s" state))))
                         
                                                       
                         )))))          

    (define server-messages-editor-canvas
      (new editor-canvas%
           (parent left-top-panel)
           (enabled #f)
           (line-count 2)
           (stretchable-height #f)
           (label "Server Messages")))
    (define server-messages-text (new text%))
    (send server-messages-text insert "Not connected")
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
           (label "Definitions")))

    (define definitions-status-message
      (new message%
           (parent definitions-messages-panel-right)
           (label INITIAL-STATUS-MESSAGE-STRING)
           (auto-resize #f)))

    
    (define definitions-editor-canvas
      (new editor-canvas%
           (parent left-top-panel)
           (label "Definitions")
           (style '(hide-hscroll hide-vscroll))))
    (define definitions-text
      (new (make-smart-text%
            DEFINITIONS
            definitions-editor-canvas
            definitions-status-message
            *definitions-expr*-box*)))
    (send definitions-text insert DEFAULT-PROGRAM-TEXT)
    (send definitions-editor-canvas set-editor definitions-text)
    (send definitions-text set-max-undo-history MAX-UNDO-DEPTH)


    (define best-guess-messages-panel
      (new horizontal-pane%
           (parent left-top-panel)
           (alignment '(center center))
           (stretchable-height #f)))

    (define best-guess-messages-panel-left
      (new horizontal-pane%
           (parent best-guess-messages-panel)
           (alignment '(left center))
           (stretchable-height #f)))

    (define best-guess-messages-panel-right
      (new horizontal-pane%
           (parent best-guess-messages-panel)
           (alignment '(right center))
           (stretchable-height #f)))


    

    (define best-guess-message
      (new message%
           (parent best-guess-messages-panel-left)
           (label "Best Guess")))

    (define best-guess-status-message
      (new message%
           (parent best-guess-messages-panel-right)
           (label INITIAL-STATUS-MESSAGE-STRING)))

    
    (define best-guess-editor-canvas
      (new editor-canvas%
           (parent left-bottom-panel)
           (label "Best Guess")
           (style '(hide-hscroll hide-vscroll))
           (enabled #f)))
    (define best-guess-text
      (new (make-smart-text%
            BEST-GUESS
            best-guess-editor-canvas
            best-guess-status-message)))
    (send best-guess-text insert "")
    (send best-guess-editor-canvas set-editor best-guess-text)


    
    (define (make-test-message/status/status/expression/value
             n
             parent-panel
             expression-expr*-box
             value-expr*-box)

      (define (make-test-editor-canvas
               type-name
               n
               parent-panel
               status-message
               expr*-box)
        (define test-editor-canvas
          (new editor-canvas%
               (parent parent-panel)
               (style '(hide-hscroll hide-vscroll))))
        (define test-text
          (new (make-smart-text%
                (list type-name n)
                test-editor-canvas
                status-message
                expr*-box)))
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
             (label (format "Test ~s" n))))

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
         expression-expr*-box))

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
         value-expr*-box))
      
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
                    *test-1-expression-expr*-box*
                    *test-1-value-expr*-box*)]
                  [(test-2-message
                    test-2-expression-status-message
                    test-2-value-status-message
                    test-expression-2-editor-canvas
                    test-value-2-editor-canvas)
                   (make-test-message/status/status/expression/value
                    2
                    right-panel
                    *test-2-expression-expr*-box*
                    *test-2-value-expr*-box*)]
                  [(test-3-message
                    test-3-expression-status-message
                    test-3-value-status-message
                    test-expression-3-editor-canvas
                    test-value-3-editor-canvas)
                   (make-test-message/status/status/expression/value
                    3
                    right-panel
                    *test-3-expression-expr*-box*
                    *test-3-value-expr*-box*)]
                  [(test-4-message
                    test-4-expression-status-message
                    test-4-value-status-message
                    test-expression-4-editor-canvas
                    test-value-4-editor-canvas)
                   (make-test-message/status/status/expression/value
                    4
                    right-panel
                    *test-4-expression-expr*-box*
                    *test-4-value-expr*-box*)]
                  [(test-5-message
                    test-5-expression-status-message
                    test-5-value-status-message
                    test-expression-5-editor-canvas
                    test-value-5-editor-canvas)
                   (make-test-message/status/status/expression/value
                    5
                    right-panel
                    *test-5-expression-expr*-box*
                    *test-5-value-expr*-box*)]
                  [(test-6-message
                    test-6-expression-status-message
                    test-6-value-status-message
                    test-expression-6-editor-canvas
                    test-value-6-editor-canvas)
                   (make-test-message/status/status/expression/value
                    6
                    right-panel
                    *test-6-expression-expr*-box*
                    *test-6-value-expr*-box*)])

    
      (define tabbable-items
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

      (define wrappable-tabbable-items
        (append
         ;; wrap around (reverse)
         (list (car (reverse tabbable-items)))
         tabbable-items
         ;; wrap around (forward)
         (list (car tabbable-items))))
    
      (set-box! *tab-focus-order-box* wrappable-tabbable-items)

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
