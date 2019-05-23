#lang racket

;;; Racket port of the "Classic" Barliman GUI.

;;; Code adapted from the mediKanren Racket GUI
;;; (https://github.com/webyrd/mediKanren).

(require
  racket/gui/base
  framework
  ;racket/engine
  ;(except-in racket/match ==)
  )

(provide
  launch-gui)

(define CLASSIC_GUI_VERSION_STRING "Shin-Barliman Classic 4.3")

(displayln "Starting Shin-Barliman Classic...")
(displayln CLASSIC_GUI_VERSION_STRING)

;;; Initial window size
(define HORIZ-SIZE 800)
(define VERT-SIZE 400)

(define *verbose* #t)

(define input-response-latency 50)

(define MAX-CHAR-WIDTH 150)

(define TEXT-FIELD-FONT-SIZE 16)
(define TEXT-FIELD-FONT (make-font #:size TEXT-FIELD-FONT-SIZE))

(define DEFAULT-PROGRAM-TEXT "(define ,A\n  (lambda ,B\n    ,C))")

(define *current-focus-box* (box #f))
(define *tab-focus-order-box* (box '()))

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

(define smart-text%
 (class racket:text%
   (super-new)
   (define (after-insert start len)
     (printf "Hello\n"))
   (define (after-edit-sequence)
     (printf "after-edit-sequence called\n")
     (define str (send this get-text))
     (printf "text: ~s\n" str)
     (define expr (with-handlers ([exn:fail? (lambda (exn)
                                               (printf "exn: ~s\n" exn)
                                               "invalid expression!")])
                    (read (open-input-string str))))
     (printf "expr: ~s\n" expr)
     (printf "syntaxexpr: ~s\n"
             (with-handlers ([exn:fail? (lambda (exn)
                                          (printf "exn: ~s\n" exn)
                                          "invalid syntax expression!")])
               (read-syntax #f str)))
     (void))
   (augment after-insert)
   (augment after-edit-sequence)))


(define (launch-main-window)
  (let ((top-window (new smart-top-level-window%
                         (label CLASSIC_GUI_VERSION_STRING)
                         (width HORIZ-SIZE)
                         (height VERT-SIZE))))

    (define outermost-hor-draggable-panel (new panel:horizontal-dragable%
                                               (parent top-window)
                                               (alignment '(left center))
                                               (stretchable-height #t)))

    (define left-vert-draggable-panel (new panel:vertical-dragable%
                                           (parent outermost-hor-draggable-panel)
                                           (alignment '(left center))))

    (define right-panel (new vertical-pane%
                             (parent outermost-hor-draggable-panel)
                             (alignment '(left top))
                             (stretchable-height #f)))

    (define left-top-panel (new vertical-pane%
                                (parent left-vert-draggable-panel)
                                (alignment '(left center))))

    (define left-bottom-panel (new vertical-pane%
                                   (parent left-vert-draggable-panel)
                                   (alignment '(left center))))
    
    (define definitions-message (new message%
                                     (parent left-top-panel)
                                     (label "Definitions")))
    
    (define definitions-editor-canvas (new editor-canvas%
                                           (parent left-top-panel)
                                           (label "Definitions")))
    (define definitions-text (new smart-text%))
    (send definitions-text insert DEFAULT-PROGRAM-TEXT)
    (send definitions-editor-canvas set-editor definitions-text)
    (send definitions-text set-max-undo-history 1000)




    (define best-guess-message (new message%
                                    (parent left-bottom-panel)
                                    (label "Best Guess")))
    
    (define best-guess-editor-canvas (new editor-canvas%
                                          (parent left-bottom-panel)
                                          (label "Best Guess")
					  (enabled #f)))
    (define best-guess-text (new smart-text%))
    (send best-guess-text insert "")
    (send best-guess-editor-canvas set-editor best-guess-text)

    (define test-1-message (new message%
                                (parent right-panel)
                                (label "Test 1")))

    (define test-expression-1-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")
                                         (font TEXT-FIELD-FONT)
					 (callback (lambda (self event)
                                                     (printf "expression 1!\n")))))

    (define test-value-1-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")
                                    (font TEXT-FIELD-FONT)
                                    (callback (lambda (self event)
                                                (printf "value 1!\n")))))

    (define test-2-message (new message%
                                (parent right-panel)
                                (label "Test 2")))
    
    (define test-expression-2-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")
                                         (font TEXT-FIELD-FONT)
                                         (callback (lambda (self event)
                                                     (printf "expression 2!\n")))))

    (define test-value-2-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")
                                    (font TEXT-FIELD-FONT)
                                    (callback (lambda (self event)
                                                (printf "value 2!\n")))))

    (define test-3-message (new message%
                                (parent right-panel)
                                (label "Test 3")))
    
    (define test-expression-3-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")
                                         (font TEXT-FIELD-FONT)
                                         (callback (lambda (self event)
                                                     (printf "expression 3!\n")))))

    (define test-value-3-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")
                                    (font TEXT-FIELD-FONT)
                                    (callback (lambda (self event)
                                                (printf "value 3!\n")))))

    (define test-4-message (new message%
                                (parent right-panel)
                                (label "Test 4")))
    
    (define test-expression-4-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")
                                         (font TEXT-FIELD-FONT)
                                         (callback (lambda (self event)
                                                     (printf "expression 4!\n")))))

    (define test-value-4-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")
                                    (font TEXT-FIELD-FONT)
                                    (callback (lambda (self event)
                                                (printf "value 4!\n")))))

    (define test-5-message (new message%
                                (parent right-panel)
                                (label "Test 5")))
    
    (define test-expression-5-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")
                                         (font TEXT-FIELD-FONT)
                                         (callback (lambda (self event)
                                                     (printf "expression 5!\n")))))

    (define test-value-5-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")
                                    (font TEXT-FIELD-FONT)
                                    (callback (lambda (self event)
                                                (printf "value 5!\n")))))

    (define test-6-message (new message%
                                (parent right-panel)
                                (label "Test 6")))

    (define test-expression-6-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")
                                         (font TEXT-FIELD-FONT)
                                         (callback (lambda (self event)
                                                     (printf "expression 6!\n")))))

    (define test-value-6-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")
                                    (font TEXT-FIELD-FONT)
                                    (callback (lambda (self event)
                                                (printf "value 6!\n")))))
        

    (set-box! *tab-focus-order-box*
              (list
               ;; wrap around (reverse)
               test-value-6-field
               ;; start
               definitions-editor-canvas
               ;;
               test-expression-1-field
               test-value-1-field
               test-expression-2-field
               test-value-2-field
               test-expression-3-field
               test-value-3-field
               test-expression-4-field
               test-value-4-field
               test-expression-5-field
               test-value-5-field
               test-expression-6-field
               test-value-6-field
               ;; wrap around
               definitions-editor-canvas
               ))

    ;; trigger reflowing of object sizes
    (send top-window reflow-container)
    
    (send top-window show #t)
    (send definitions-editor-canvas focus)
    ))


(define (launch-gui)
  (launch-main-window))

(displayln
  "Launching GUI")

(launch-gui)
