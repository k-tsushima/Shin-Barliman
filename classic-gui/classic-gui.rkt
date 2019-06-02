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

;; 新 = しん = shin = "new"
;; https://jisho.org/search/%E6%96%B0
(define CLASSIC-GUI-VERSION-STRING "新-Barliman Classic 4.3")

(displayln "Starting Shin-Barliman Classic...")
(displayln CLASSIC-GUI-VERSION-STRING)

;;; Initial window size
(define HORIZ-SIZE 1200)
(define VERT-SIZE 800)

(define *verbose* #t)

(define MAX-UNDO-DEPTH 1000)

(define DEFAULT-PROGRAM-TEXT "(define ,A\n  (lambda ,B\n    ,C))")

(define INVALID-EXPRESSION-VALUE 'invalid-expression)

(define DEFINITIONS 'definitions)
(define BEST-GUESS 'best-guess)
(define EXPRESSION 'expression)
(define VALUE 'value)

(define *current-focus-box* (box #f))
(define *tab-focus-order-box* (box '()))

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
    (list *test-1-expression-expr*-box* EXPRESSION 1)
    (list *test-1-value-expr*-box* VALUE 1)
    (list *test-2-expression-expr*-box* EXPRESSION 2)
    (list *test-2-value-expr*-box* VALUE 2)
    (list *test-3-expression-expr*-box* EXPRESSION 3)
    (list *test-3-value-expr*-box* VALUE 3)
    (list *test-4-expression-expr*-box* EXPRESSION 4)
    (list *test-4-value-expr*-box* VALUE 4)
    (list *test-5-expression-expr*-box* EXPRESSION 5)
    (list *test-5-value-expr*-box* VALUE 5)
    (list *test-6-expression-expr*-box* EXPRESSION 6)
    (list *test-6-value-expr*-box* VALUE 6)
    ))

(define (print-all-user-editable-canvases-boxes-values)
  (let loop ([b* *user-editable-canvases-boxes*])
    (match b*
      ['() (void)]
      [`((,b ,type ,n) . ,rest)
       (printf "~s ~s: ~s\n" type n (unbox b))
       (loop rest)]
      [`((,b ,type) . ,rest)
       (printf "~s: ~s\n" type (unbox b))
       (loop rest)])))

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

(define (make-smart-text% canvas type . args)
  (let ((n (if (= (length args) 1)
               (car args)
               #f)))
    (let ((name (if n
                    (format "~s ~s" type n)
                    (format "~s" type))))
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
          ;;
          (define (get-user-canvas-box type n)
             (let loop ([b* *user-editable-canvases-boxes*])
               (match b*
                 ['() (error 'get-user-canvas-box
                             (format "box not found: ~s ~s" type n))]
                 [`((,b ,t ,m) . ,rest)
                  (if (and (equal? t type) (= n m))
                      b
                      (loop rest))]
                 [`((,b ,t) . ,rest)
                  (if (equal? t type)
                      b
                      (loop rest))])))
          (define (update-user-canvas-box! type n new-expr)
            (let ((b (get-user-canvas-box type n)))
              (set-box! b new-expr)))
          (when (send canvas is-enabled?)
            (update-user-canvas-box! type n expr*-in-list))
          ;;
          (when (list? expr*-in-list)
            (if (= (length expr*-in-list) 1)
                (printf "~s single raw expr: ~s\n" name (car expr*-in-list))
                (begin
                  (printf "~s multiple raw exprs:\n" name)
                  (for-each
                    (lambda (expr)
                      (printf "~s\n" expr))
                    expr*-in-list))))
          (printf "======================================\n")
          (print-all-user-editable-canvases-boxes-values)
          (printf "======================================\n\n")
          (void))
        (augment after-insert)
        (augment after-edit-sequence)))))



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
    
    (define definitions-message
      (new message%
           (parent left-top-panel)
           (label "Definitions")))
    
    (define definitions-editor-canvas
      (new editor-canvas%
           (parent left-top-panel)
           (label "Definitions")))
    (define definitions-text
      (new (make-smart-text% definitions-editor-canvas DEFINITIONS)))
    (send definitions-text insert DEFAULT-PROGRAM-TEXT)
    (send definitions-editor-canvas set-editor definitions-text)
    (send definitions-text set-max-undo-history MAX-UNDO-DEPTH)




    (define best-guess-message
      (new message%
           (parent left-bottom-panel)
           (label "Best Guess")))
    
    (define best-guess-editor-canvas
      (new editor-canvas%
           (parent left-bottom-panel)
           (label "Best Guess")
           (enabled #f)))
    (define best-guess-text
      (new (make-smart-text% best-guess-editor-canvas BEST-GUESS)))
    (send best-guess-text insert "")
    (send best-guess-editor-canvas set-editor best-guess-text)


    
    (define (make-test-message/expression/value n parent-panel)

      (define (make-test-editor-canvas type n parent-panel)
        (define test-editor-canvas
          (new editor-canvas%
               (parent parent-panel)))
        (define test-text
          (new (make-smart-text% test-editor-canvas type n)))
        (send test-editor-canvas set-editor test-text)
        (send test-text set-max-undo-history MAX-UNDO-DEPTH)

        test-editor-canvas)
      
      (define test-message
        (new message%
             (parent parent-panel)
             (label (format "Test ~s" n))))

      (define test-expression-editor-canvas
        (make-test-editor-canvas EXPRESSION n parent-panel))
      (define test-value-editor-canvas
        (make-test-editor-canvas VALUE n parent-panel))
      
      (list test-message
            test-expression-editor-canvas
            test-value-editor-canvas))


    (define test-1-message/expression/value
      (make-test-message/expression/value 1 right-panel))
    (define test-1-message
      (car test-1-message/expression/value))
    (define test-expression-1-editor-canvas
      (cadr test-1-message/expression/value))
    (define test-value-1-editor-canvas
      (caddr test-1-message/expression/value))

    (define test-2-message/expression/value
      (make-test-message/expression/value 2 right-panel))
    (define test-2-message
      (car test-2-message/expression/value))
    (define test-expression-2-editor-canvas
      (cadr test-2-message/expression/value))
    (define test-value-2-editor-canvas
      (caddr test-2-message/expression/value))

    (define test-3-message/expression/value
      (make-test-message/expression/value 3 right-panel))
    (define test-3-message
      (car test-3-message/expression/value))
    (define test-expression-3-editor-canvas
      (cadr test-3-message/expression/value))
    (define test-value-3-editor-canvas
      (caddr test-3-message/expression/value))

    (define test-4-message/expression/value
      (make-test-message/expression/value 4 right-panel))
    (define test-4-message
      (car test-4-message/expression/value))
    (define test-expression-4-editor-canvas
      (cadr test-4-message/expression/value))
    (define test-value-4-editor-canvas
      (caddr test-4-message/expression/value))

    (define test-5-message/expression/value
      (make-test-message/expression/value 5 right-panel))
    (define test-5-message
      (car test-5-message/expression/value))
    (define test-expression-5-editor-canvas
      (cadr test-5-message/expression/value))
    (define test-value-5-editor-canvas
      (caddr test-5-message/expression/value))

    (define test-6-message/expression/value
      (make-test-message/expression/value 6 right-panel))
    (define test-6-message
      (car test-6-message/expression/value))
    (define test-expression-6-editor-canvas
      (cadr test-6-message/expression/value))
    (define test-value-6-editor-canvas
      (caddr test-6-message/expression/value))

    
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
    ))


(define (launch-gui)
  (launch-main-window))

(displayln
  "Launching GUI")

(launch-gui)
