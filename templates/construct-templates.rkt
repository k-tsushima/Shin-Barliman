#lang racket

(require
 )

(provide
  (all-defined-out))


;;; Construct quasiquoted template for functions that recur over
;;; lists, with place holders for logic variables.

;;; TODO
;;;
;;; be able to unify template with user-provided function skeleton;
;;; this probably means either making the arguments to a template
;;; unbound logic variables, or using nominal unification (or de
;;; Bruijn indices, HOAS, etc.)
;;;
;;; user in Shin-Barliman should be able to indicate whether the
;;; function is deeply-recursive (yes/no/don't know), etc., and the
;;; same for variadic vs. fixed-arity function; in the Shin-Barliman
;;; GUI, may want to show a selection list of generated templates that
;;; Shin-Barliman will consider during synthesis, with the user able
;;; to select/deselect/prioritize the templates under consideration.
;;;
;;; explore how precise we should be in generating type/arity
;;; information from example programs; also, user should be able to
;;; specify all or some of the type/arity information, which we should
;;; unify with the information inferred from examples.  Perhaps a
;;; typed version of Scheme (ML-like Scheme) would work best here.
;;;
;;; try restricting the allowed grammar for synthesis for each hole
;;; in a generated template, based on context; may want to use types,
;;; machine learning, heuristics, symbolic reasoning, etc., to
;;; determine allowable grammars;  also need to update the synthesis
;;; to efficiently handle restricted grammars.
;;;
;;; our "pretty argument name" generation isn't perfect, since we
;;; aren't preventing possible collision with user-introduced names;
;;; may want to actually use gensym (with the pretty names), or
;;; generate guarenteed unique names, or do an alpha-renaming step
;;; first, or whatever
;;;
;;; handle multiple list args
;;;
;;; refactor generate-pretty-arg-names to avoid duplicate code, etc.
;;;
;;; more generally: identify and inplement the various types of
;;; patterns/templates we want to handle (accumulator passing style,
;;; CPS?, recursion on natural numbers (up/down), recursion on
;;; multiple lists (including cdr'ing on two lists simultaneously, one
;;; list getting bigger while another gets smaller, etc.))
;;;
;;; add parameter for specifying which arguments are being recurred
;;; on, whether up/down for numbers, etc.
;;;
;;; template for deep/tree recursion (when the car of a list is a list)
;;;
;;; create templates using 'match' as well as 'cond'/'null?'/'car'/'cdr'


;;; NOTES
;;;
;;; inputs as to the type of template/which arguments to recur
;;; on/whether to use deep recursion/whether to count up/down for
;;; numbers, etc., could come from at least three different sources
;;; (or any combination of these sources):
;;;
;;; 1. from the user
;;;
;;; 2. from machine learning/pattern recognition
;;;
;;; 3. guessing/running in parallel

(define get-args-that-are-lists
  (lambda (inputs arg-names)
    (match `(,inputs ,arg-names)
      ['(() ()) '()]
      [`((,in-a . ,in-d) (,arg-a . ,arg-d))
       (if (list? in-a)
           (cons arg-a (get-args-that-are-lists in-d arg-d))
           (get-args-that-are-lists in-d arg-d))])))


;;; simple and easy to explain!
(define simple-generate-pretty-arg-names
  (lambda (args)
    (let loop ((args args)
               (count 1))
      (cond
        ((null? args) '())
        (else (cons (string->symbol (format "a~s" count))
                    (loop (cdr args) (+ 1 count))))))))

;;; produces smarter variables names, but is complex!
(define smart-generate-pretty-arg-names
  (lambda (args)
    (let ((list-count (length (filter list? args)))
          (num-count (length (filter number? args)))
          (sym-count (length (filter symbol? args))))
      (let ((other-count (- (length args) list-count num-count sym-count)))
        (let ((pretty-arg-name/counts (lambda (arg counts)
                                        (match counts
                                          [`(,lc ,nc ,sc ,oc)
                                           (cond
                                             ((list? arg)
                                              (let ((counts `(,(add1 lc) ,nc ,sc ,oc)))
                                                (if (> list-count 1)
                                                    (cons (format "l~s" lc) counts)
                                                    (cons (format "l") counts))))
                                             ((number? arg)
                                              (let ((counts `(,lc ,(add1 nc) ,sc ,oc)))
                                                (if (> num-count 1)
                                                    (cons (format "n~s" nc) counts)
                                                    (cons (format "n") counts))))
                                             ((symbol? arg)
                                              (let ((counts `(,lc ,nc ,(add1 sc) ,oc)))
                                                (if (> sym-count 1)
                                                    (cons (format "x~s" sc) counts)
                                                    (cons (format "x") counts))))
                                             (else
                                              (let ((counts `(,lc ,nc ,sc ,(add1 oc))))
                                                (if (> other-count 1)
                                                    (cons (format "a~s" oc) counts)
                                                    (cons (format "a") counts)))))]))))
          (let loop ((args args)
                     (counts '(1 1 1 1)))
            (cond
              ((null? args) '())
              (else
               (let ((name/counts (pretty-arg-name/counts (car args) counts)))
                 (let ((name (car name/counts))
                       (counts (cdr name/counts)))
                   (cons (string->symbol name)
                         (loop (cdr args) counts))))))))))))


; 知りたい情報、なんばんめでリストにマッチングするか
; ios = pairs of input and output
(define const-pattern
  (lambda (fname ios)
    (match ios
      [`((,inputs . ,output) . ,_)
       (let ((arg-names (smart-generate-pretty-arg-names inputs)))
         (let ((list-args (get-args-that-are-lists inputs arg-names)))
           (match list-args
             ['()
              `(define ,fname
                 (lambda ,arg-names
                   ,',A))]
             [`(,la)
              `(define ,fname
                 (lambda ,arg-names
                   (if (null? ,la)
                       ,',B                       
                       (,',C (car ,la) (,fname . ,(map (lambda (input arg-name)
                                                         (if (list? input)
                                                             `(cdr ,arg-name)
                                                             arg-name))
                                                       inputs arg-names))))))]
             [else (error 'const-pattern (format "more than one list argument in ~s" inputs))])))])))
