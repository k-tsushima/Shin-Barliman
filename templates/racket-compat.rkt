#lang racket

(require
  )

(provide
  )

;; Make Racket act like Scheme when printing a value: the value of
;; (quote foo) will print as foo rather than as 'foo
;;
;; https://docs.racket-lang.org/reference/Writing.html#%28def._%28%28quote._~23~25kernel%29._print-as-expression%29%29
;; https://docs.racket-lang.org/drracket/output-syntax.html
(print-as-expression #f)
