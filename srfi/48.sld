;; Portable (srfi 48) -- Intermediate Format Strings.
;;
;; (format [destination] format-string arg ...)
;;   destination:
;;     omitted / a string in its place -> the format-string is the first argument and
;;                                         the result is RETURNED as a string
;;     #f   -> result returned as a string
;;     #t   -> written to (current-output-port); returns unspecified
;;     port -> written to that port; returns unspecified
;;
;;   Directives (case-insensitive letter; [w] / [w,d] = optional numeric parameters):
;;     ~a        display the next argument
;;     ~s        write the next argument
;;     ~[w]d     next argument (a number) in decimal, right-justified in w columns
;;     ~[w]b ~[w]o ~[w]x   ditto in binary / octal / hexadecimal
;;     ~c        the next argument (a character)
;;     ~w        write the next argument (shared/circular-safe: write-shared)
;;     ~y        the next argument, "pretty printed" -- here a plain `write` (see note)
;;     ~[w[,d]]f next argument (a number or string) as fixed-point: d digits after the
;;               decimal point, right-justified in w columns
;;     ~?  ~k    indirection: the next argument is a format-string and the one after a
;;               LIST of its arguments; both are consumed and formatted in place
;;     ~~        a literal tilde
;;     ~%        a newline
;;     ~_        a space
;;     ~&        a freshline (a newline unless the output is already at line start)
;;     ~h        a human-readable summary of these directives
;;
;; Pure R7RS -- the SAME file is loaded identically by pyScheme and cppScheme2 (no
;; native part).  It relies only on string ports / write-string / char comparisons
;; from (scheme base) and write/write-shared from (scheme write).
;;
;; Intentional deviations from the SRFI 48 sample implementation, all minor:
;;   * ~y is a plain `write`, not a true pretty-printer (there is no portable R7RS
;;     pretty-printer to build on; the output is valid, just not reflowed).
;;   * Output is buffered into a string and emitted once at the end, so ~& measures
;;     freshness against the format's own output rather than the destination port's
;;     prior column.

(define-library (srfi 48)
  (export format)
  (import (scheme base) (scheme write) (scheme char))   ; (scheme char) for char-downcase
  (begin

    ;; ---- small helpers ------------------------------------------------------------
    (define (pad-left s width pad-char)
      (let ((n (string-length s)))
        (if (or (not width) (>= n width))
            s
            (string-append (make-string (- width n) pad-char) s))))

    (define (ascii-digit? c) (and (char<=? #\0 c) (char<=? c #\9)))

    ;; read a run of ASCII digits from index k; return (value . next-index), value #f
    ;; when there were no digits.
    (define (read-digits fmt k len)
      (let loop ((k k) (acc #f))
        (if (and (< k len) (ascii-digit? (string-ref fmt k)))
            (loop (+ k 1)
                  (+ (* (or acc 0) 10)
                     (- (char->integer (string-ref fmt k)) (char->integer #\0))))
            (cons acc k))))

    ;; parse `~` parameters starting at k: [width][,digits].  Return a 3-list
    ;; (width digits directive-index); width/digits are integers or #f.
    (define (parse-params fmt k len)
      (let* ((w (read-digits fmt k len))
             (width (car w)) (k1 (cdr w)))
        (if (and (< k1 len) (char=? (string-ref fmt k1) #\,))
            (let* ((dd (read-digits fmt (+ k1 1) len))
                   (digits (or (car dd) 0)) (k2 (cdr dd)))
              (list width digits k2))
            (list width #f k1))))

    ;; integer N in RADIX, right-justified to MIN-WIDTH columns with spaces (or as-is).
    (define (radix-string n radix min-width)
      (pad-left (number->string n radix) min-width #\space))

    ;; fixed-point rendering of X with exactly DIGITS places after the point, then
    ;; right-justified to WIDTH.  A non-rational X (inf/nan) or a string falls back to
    ;; its plain rendering; DIGITS #f means "no rounding, just display".
    (define (fixed-string x width digits)
      (let ((body
             (cond
               ((string? x) x)
               ((not (and (number? x) (real? x) (rational? x))) (number->string x))
               ((not digits) (number->string x))
               (else
                (let* ((neg (negative? x))
                       (ax (abs (inexact x)))
                       (scale (expt 10 digits))
                       (scaled (exact (round (* ax scale))))
                       (ip (quotient scaled scale))
                       (fp (remainder scaled scale)))
                  (string-append
                   (if neg "-" "")
                   (number->string ip)
                   (if (> digits 0)
                       (string-append "." (pad-left (number->string fp) digits #\0))
                       "")))))))
        (pad-left body width #\space)))

    (define help-text
      (string-append
       "SRFI 48 format directives:\n"
       "  ~a display   ~s write   ~c char   ~w write-shared   ~y (write)\n"
       "  ~[w]d ~[w]b ~[w]o ~[w]x  number in dec/bin/oct/hex, w-wide\n"
       "  ~[w[,d]]f  fixed-point: d places, w-wide\n"
       "  ~? ~k indirection (fmt + arg-list)   ~~ tilde  ~% newline\n"
       "  ~_ space   ~& freshline   ~h this help\n"))

    ;; ---- the worker: render FMT with ARGS into the string port P -----------------
    ;; Returns the unconsumed args (so a future extension could chain); ~? recurses.
    (define (render p fmt args)
      (define len (string-length fmt))
      (define (need args)
        (if (null? args) (error "format: too few arguments for the format string" fmt)
            (car args)))
      (let loop ((i 0) (args args))
        (if (>= i len)
            args
            (let ((c (string-ref fmt i)))
              (if (not (char=? c #\~))
                  (begin (write-char c p) (loop (+ i 1) args))
                  (if (>= (+ i 1) len)
                      (error "format: incomplete escape sequence at end of string" fmt)
                      (let* ((params (parse-params fmt (+ i 1) len))
                             (width  (car params))
                             (digits (cadr params))
                             (j      (car (cddr params)))   ; caddr lives in (scheme cxr)
                             (dir    (char-downcase (string-ref fmt j)))
                             (next   (+ j 1)))
                        (cond
                          ((char=? dir #\a) (display (need args) p) (loop next (cdr args)))
                          ((char=? dir #\s) (write   (need args) p) (loop next (cdr args)))
                          ((char=? dir #\d)
                           (write-string (radix-string (need args) 10 width) p)
                           (loop next (cdr args)))
                          ((char=? dir #\b)
                           (write-string (radix-string (need args) 2 width) p)
                           (loop next (cdr args)))
                          ((char=? dir #\o)
                           (write-string (radix-string (need args) 8 width) p)
                           (loop next (cdr args)))
                          ((char=? dir #\x)
                           (write-string (radix-string (need args) 16 width) p)
                           (loop next (cdr args)))
                          ((char=? dir #\c) (write-char (need args) p) (loop next (cdr args)))
                          ((char=? dir #\w) (write-shared (need args) p) (loop next (cdr args)))
                          ((char=? dir #\y) (write (need args) p) (loop next (cdr args)))
                          ((char=? dir #\f)
                           (write-string (fixed-string (need args) width digits) p)
                           (loop next (cdr args)))
                          ((or (char=? dir #\?) (char=? dir #\k))
                           (let ((subfmt (need args))
                                 (subargs (if (null? (cdr args))
                                              (error "format: ~? needs a format string and an argument list" fmt)
                                              (cadr args))))
                             (render p subfmt subargs)
                             (loop next (cddr args))))
                          ((char=? dir #\~) (write-char #\~ p) (loop next args))
                          ((char=? dir #\%) (newline p) (loop next args))
                          ((char=? dir #\_) (write-char #\space p) (loop next args))
                          ((char=? dir #\&)
                           (let ((cur (get-output-string p)))
                             (when (and (> (string-length cur) 0)
                                        (not (char=? (string-ref cur (- (string-length cur) 1))
                                                     #\newline)))
                               (newline p)))
                           (loop next args))
                          ((char=? dir #\h) (write-string help-text p) (loop next args))
                          (else
                           (error "format: unrecognized directive"
                                  (string #\~ (string-ref fmt j))))))))))))

    ;; ---- entry point --------------------------------------------------------------
    (define (run dest fmt args)
      (let ((p (open-output-string)))
        (render p fmt args)
        (let ((result (get-output-string p)))
          (if dest
              (begin (write-string result dest) (if #f #f))   ; port: write, return unspecified
              result))))                                       ; #f: return the string

    (define (format dest-or-fmt . rest)
      (cond
        ((string? dest-or-fmt)       (run #f dest-or-fmt rest))
        ((eq? dest-or-fmt #f)        (run #f (car rest) (cdr rest)))
        ((eq? dest-or-fmt #t)        (run (current-output-port) (car rest) (cdr rest)))
        ((output-port? dest-or-fmt)  (run dest-or-fmt (car rest) (cdr rest)))
        (else (error "format: destination must be #f, #t, a port, or a format string"
                     dest-or-fmt))))))
