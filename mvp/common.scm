;; Helper functions common to different parts of the Shin-Barliman MVP codebase.

;; Includes code adapted from https://github.com/webyrd/mediKanren/blob/master/biolink/common.rkt

(define (filter-not pred ls) (filter (lambda (x) (not (pred x))) ls))

(define (read/file path)  (with-input-from-file  path (lambda () (read))))
(define (read/string str) (with-input-from-string str (lambda () (read))))

(define box:config (box #f))
(define (config)
  (define cfg (unbox box:config))
  (cond (cfg cfg)
        (else (load-config #t #f)
              (unbox box:config))))
(define (config-ref key)
  (define kv (assoc key (config)))
  (unless kv (error "missing configuration key:" key))
  (cdr kv))
(define (load-config verbose? . rest)
  (define path:config (if (and (= (length rest) 1) (string? (car rest)))
                          (car rest)
                          #f))
  (define path:config.user     (or path:config "config.scm"))
  (define path:config.defaults "config.defaults.scm")
  (when verbose? (printf "loading configuration defaults: ~a\n"
                         path:config.defaults))
  (when verbose? (printf "loading configuration overrides: ~a\n"
                         path:config.user))
  (let ()
    (define config.user     (if (file-exists? path:config.user)
                                (read/file path:config.user)
                                '()))
    (define config.defaults (read/file path:config.defaults))
    (unless (and (list? config.user) (andmap pair? config.user))
      (error "invalid configuration overrides:" config.user))
    (let ()
      (define user-keys (map car config.user))
      (define (user-defined? kv) (member (car kv) user-keys))
      (set-box! box:config
                (append config.user (filter-not user-defined? config.defaults))))))
