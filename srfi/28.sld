;; Portable (srfi 28) -- Basic Format Strings.
;;
;; (format format-string obj ...) -> string
;;   Builds a string from FORMAT-STRING, replacing escape sequences:
;;     ~a   the next argument as if by `display`
;;     ~s   the next argument as if by `write`
;;     ~%   a newline
;;     ~~   a literal tilde
;;   Any other character (including after ~) that is not one of the above escapes
;;   is an error; everything else is copied verbatim.
;;
;; Pure R7RS -- the SAME file is loaded identically by pyScheme and cppScheme2 (no
;; native part), so behaviour is parity-by-construction.  SRFI 48 (intermediate
;; format) is a strict superset of these directives but uses a different argument
;; convention (an optional destination), so it is a separate library, (srfi 48).
;;
;; Reference: Scott G. Miller, SRFI 28.  This is the SRFI's own sample implementation,
;; transcribed unchanged but for the error messages.

(define-library (srfi 28)
  (export format)
  (import (scheme base) (scheme write))
  (begin

    (define (format format-string . objects)
      (let ((out (open-output-string)))
        (let loop ((chars (string->list format-string))
                   (objects objects))
          (cond
            ((null? chars)
             (get-output-string out))
            ((char=? (car chars) #\~)
             (if (null? (cdr chars))
                 (error "format: incomplete escape sequence at end of string"
                        format-string)
                 (case (cadr chars)
                   ((#\a)
                    (when (null? objects)
                      (error "format: not enough arguments for ~a" format-string))
                    (display (car objects) out)
                    (loop (cddr chars) (cdr objects)))
                   ((#\s)
                    (when (null? objects)
                      (error "format: not enough arguments for ~s" format-string))
                    (write (car objects) out)
                    (loop (cddr chars) (cdr objects)))
                   ((#\%)
                    (newline out)
                    (loop (cddr chars) objects))
                   ((#\~)
                    (write-char #\~ out)
                    (loop (cddr chars) objects))
                   (else
                    (error "format: unrecognized escape sequence"
                           (string #\~ (cadr chars)))))))
            (else
             (write-char (car chars) out)
             (loop (cdr chars) objects))))))))
