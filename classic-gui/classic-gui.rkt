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

(define smart-text%
 (class racket:text%
   (super-new)
   (define (after-insert start len)
     (printf "Hello\n"))
   (augment after-insert)))


(define (launch-main-window)  
  (let ((frame (new frame%
                    (label CLASSIC_GUI_VERSION_STRING)
                    (width HORIZ-SIZE)
                    (height VERT-SIZE))))

    (define outermost-hor-draggable-panel (new panel:horizontal-dragable%
                                               (parent frame)
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
                                           (label "Definitions")
					   ))
    (define definitions-text (new smart-text%))
    (send definitions-text insert "")
    (send definitions-editor-canvas set-editor definitions-text)




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
					 (callback (lambda (self event)
                                                         (printf "Hello\n")))))

    (define test-value-1-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")))

    (define test-2-message (new message%
                                (parent right-panel)
                                (label "Test 2")))
    
    (define test-expression-2-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")))

    (define test-value-2-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")))

    (define test-3-message (new message%
                                (parent right-panel)
                                (label "Test 3")))
    
    (define test-expression-3-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")))

    (define test-value-3-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")))

    (define test-4-message (new message%
                                (parent right-panel)
                                (label "Test 4")))
    
    (define test-expression-4-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")))

    (define test-value-4-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")))

    (define test-5-message (new message%
                                (parent right-panel)
                                (label "Test 5")))
    
    (define test-expression-5-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")))

    (define test-value-5-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")))

    (define test-6-message (new message%
                                (parent right-panel)
                                (label "Test 6")))

    (define test-expression-6-field (new text-field%
                                         (label "")
                                         (parent right-panel)
                                         (init-value "")))

    (define test-value-6-field (new text-field%
                                    (label "")
                                    (parent right-panel)
                                    (init-value "")))
        
    
    ;; trigger reflowing of object sizes
    (send frame reflow-container)        
    
    (send frame show #t)))


#|
(define (launch-main-window)
  (let ((frame (new frame%
                    (label CLASSIC_GUI_VERSION_STRING)
                    (width HORIZ-SIZE)
                    (height VERT-SIZE))))

    (define outer-vert-draggable-panel (new panel:vertical-dragable%
                                            (parent frame)
                                            (alignment '(left center))))
    
    (define upper-pane (new panel:vertical-dragable%
                            (parent outer-vert-draggable-panel)
                            (alignment '(left center))))

    (define lower-pane (new panel:vertical-dragable%
                            (parent outer-vert-draggable-panel)
                            (alignment '(left center))))        

    (define concept-1-overall-pane (new vertical-pane%
                                        (parent upper-pane)
                                        (alignment '(left center))))
    
    (define concept-1-search/isa-panel (new panel:horizontal-dragable%
                                            (parent concept-1-overall-pane)
                                            (alignment '(left center))
                                            (stretchable-height #f)))
    (define concept-1-list-boxes-panel (new panel:horizontal-dragable%
                                            (parent concept-1-overall-pane)
                                            (alignment '(left center))))
    (define concept-1-list-box (concept-list concept-1-overall-pane
                                             concept-1-search/isa-panel
                                             concept-1-list-boxes-panel
                                             "Concept 1"
                                             *concept-1-name-string*
                                             *concept-1-isa-flag*
                                             *concept-1-choices*
                                             (lambda () predicate-1-list-box)
                                             *predicate-1-choices*
                                             'out-edge
                                             *last-concept-1-column-clicked-for-sorting*
                                             *concept-1-column-sort-order*
                                             *concept-1-choices*
                                             convert-concept-1/2-to-column-sorting-format
                                             (make-send-concepts-to-concept-1/2-list-box (lambda () concept-1-list-box))))
    (define predicate-1-list-box (new list-box%
                                      (label "Predicate 1")
                                      (choices (unbox *predicate-1-choices*))
                                      (columns '("Name"))
                                      (parent concept-1-list-boxes-panel)
                                      (style '(extended))
                                      (callback go-callback)))
    (define edge-description (new message%
                                  (parent concept-1-overall-pane)
                                  (label "Concept 1 -> Predicate 1 -> [X] -> Predicate 2 -> Concept 2")))

    (define concept-2-overall-pane (new vertical-pane%
                                        (parent upper-pane)
                                        (alignment '(left center))))
    
    (define concept-2-search/isa-panel (new panel:horizontal-dragable%
                                            (parent concept-2-overall-pane)
                                            (alignment '(left center))
                                            (stretchable-height #f)))
    (define concept-2-list-boxes-panel (new panel:horizontal-dragable%
                                            (parent concept-2-overall-pane)
                                            (alignment '(left center))))
    (define predicate-2-list-box (new list-box%
                                      (label "Predicate 2")
                                      (choices (unbox *predicate-2-choices*))
                                      (columns '("Name"))
                                      (parent concept-2-list-boxes-panel)
                                      (style '(extended))
                                      (callback go-callback)))
    (define concept-2-list-box (concept-list concept-2-overall-pane
                                             concept-2-search/isa-panel
                                             concept-2-list-boxes-panel
                                             "Concept 2"
                                             *concept-2-name-string*
                                             *concept-2-isa-flag*
                                             *concept-2-choices*
                                             (lambda () predicate-2-list-box)
                                             *predicate-2-choices*
                                             'in-edge
                                             *last-concept-2-column-clicked-for-sorting*
                                             *concept-2-column-sort-order*
                                             *concept-2-choices*
                                             convert-concept-1/2-to-column-sorting-format
                                             (make-send-concepts-to-concept-1/2-list-box (lambda () concept-2-list-box))))

    (define running-status-description (new message%
                                            (parent concept-2-overall-pane)
                                            (label "                                                                ")))

    (define properties/pubmed-panel (new panel:horizontal-dragable%
                                         (parent lower-pane)
                                         (alignment '(left center))
                                         (stretchable-height #t)))

    (define subject-properties-list-box (new smart-column-width-list-box%
                                             (label "Subject")
                                             (choices '())
                                             (columns '("Property" "Value"))
                                             (parent properties/pubmed-panel)
                                             (style '(column-headers reorderable-headers extended))
                                             (callback (lambda (self event)
                                                         (void)))))
    
    (define edge-properties-list-box (new smart-column-width-list-box%
                                     (label "Edge")
                                     (choices '())
                                     (columns '("Property" "Value"))
                                     (parent properties/pubmed-panel)
                                     (style '(column-headers reorderable-headers extended))
                                     (callback (lambda (self event)
                                                 (void)))))

    (define object-properties-list-box (new smart-column-width-list-box%
                                            (label "Object")
                                            (choices '())
                                            (columns '("Property" "Value"))
                                            (parent properties/pubmed-panel)
                                            (style '(column-headers reorderable-headers extended))
                                            (callback (lambda (self event)
                                                        (void)))))    
    
    ;; trigger reflowing of object sizes
    (send frame reflow-container)        
    
    (send frame show #t)))
|#



(define (launch-gui)
  (launch-main-window))

(displayln
  "Launching GUI")

(launch-gui)
