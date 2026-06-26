;; Portable (srfi 13) -- String Library (compatibility shim).
;;
;; SRFI 152 is the modern reduced successor of SRFI 13 and deliberately KEEPS SRFI
;; 13's argument orders (including the historical inconsistency: string-index/-count/
;; -skip take the string first, but string-filter/-delete take the criterion first).
;; So this shim simply re-exports the SRFI-152 procedures that share an SRFI-13 name
;; and signature, and adds the handful of SRFI-13-only procedures (string-reverse,
;; string-titlecase, string-tokenize, xsubstring) on top.
;;
;; SCOPE: like (srfi 152), criteria are PREDICATES ONLY -- the char / SRFI-14 char-set
;; forms of a criterion are not supported (pass a one-argument char predicate instead).
;; NOT provided: string-hash / string-hash-ci and the rarely-used KMP search helpers.
;; Pure Scheme, no native part -> byte-identical on pyScheme and cppScheme2.

(define-library (srfi 13)
  (import (scheme base) (scheme char)
          (only (srfi 152)
                string-null? string-every string-any string-tabulate
                string-unfold string-unfold-right reverse-list->string
                string-take string-drop string-take-right string-drop-right
                string-pad string-pad-right
                string-trim string-trim-right string-trim-both
                string-replace string-prefix-length string-suffix-length
                string-prefix? string-suffix?
                string-index string-index-right string-skip string-skip-right
                string-contains string-concatenate string-concatenate-reverse
                string-join string-fold string-fold-right string-count
                string-filter string-remove string-replicate))
  (export
    ;; plain R7RS string procedures that SRFI 13 also specifies
    string? string-length string-ref substring string-copy string-append
    string string->list list->string string-map string-for-each
    string-set! string-fill! string-copy! make-string
    string=? string<? string>? string<=? string>=?
    string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
    ;; re-exported from (srfi 152) (same name + signature in SRFI 13)
    string-null? string-every string-any string-tabulate
    string-unfold string-unfold-right reverse-list->string
    string-take string-drop string-take-right string-drop-right
    string-pad string-pad-right string-trim string-trim-right string-trim-both
    string-replace string-prefix-length string-suffix-length
    string-prefix? string-suffix?
    string-index string-index-right string-skip string-skip-right
    string-contains string-concatenate string-concatenate-reverse
    string-join string-fold string-fold-right string-count string-filter
    ;; SRFI-13-only procedures
    string-delete string-reverse string-reverse!
    string-titlecase string-titlecase! string-tokenize xsubstring)
  (begin

    ;; SRFI 13 calls SRFI 152's string-remove "string-delete" (same arg order).
    (define string-delete string-remove)
    ;; xsubstring is SRFI 152's string-replicate with TO optional (defaults so the
    ;; result length equals the source substring's length).
    (define (xsubstring s from . opt)
      (let* ((start (if (>= (length opt) 2) (list-ref opt 1) 0))
             (end   (if (>= (length opt) 3) (list-ref opt 2) (string-length s)))
             (to    (if (>= (length opt) 1) (list-ref opt 0) (+ from (- end start)))))
        (string-replicate s from to start end)))

    ;; ---- SRFI-13-only string procedures -----------------------------------
    (define (%bnd s opt)        ; (start . end) over s from an optional tail
      (cons (if (pair? opt) (car opt) 0)
            (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) (string-length s))))

    ;; (string-reverse s [start end]) -- fresh reversed copy of s[start,end).
    (define (string-reverse s . opt)
      (let* ((b (%bnd s opt)) (start (car b)) (end (cdr b)))
        (list->string (reverse (string->list s start end)))))

    ;; (string-reverse! s [start end]) -- reverse s[start,end) in place.
    (define (string-reverse! s . opt)
      (let* ((b (%bnd s opt)) (start (car b)) (end (cdr b)))
        (let loop ((i start) (j (- end 1)))
          (when (< i j)
            (let ((t (string-ref s i)))
              (string-set! s i (string-ref s j))
              (string-set! s j t))
            (loop (+ i 1) (- j 1))))))

    ;; (string-titlecase s [start end]) -- fresh string: upcase the first cased char
    ;; of each run of cased chars, downcase the rest.  char-alphabetic? marks "cased".
    (define (string-titlecase s . opt)
      (let* ((b (%bnd s opt)) (start (car b)) (end (cdr b))
             (out (open-output-string)))
        (let loop ((i start) (prev-cased #f))
          (if (>= i end) (get-output-string out)
              (let* ((c (string-ref s i)) (cased (char-alphabetic? c)))
                (write-char (cond ((not cased) c)
                                  (prev-cased (char-downcase c))
                                  (else (char-upcase c)))
                            out)
                (loop (+ i 1) cased))))))

    ;; (string-titlecase! s [start end]) -- in-place titlecasing of s[start,end).
    (define (string-titlecase! s . opt)
      (let* ((b (%bnd s opt)) (start (car b)) (end (cdr b)))
        (let loop ((i start) (prev-cased #f))
          (when (< i end)
            (let* ((c (string-ref s i)) (cased (char-alphabetic? c)))
              (string-set! s i (cond ((not cased) c)
                                     (prev-cased (char-downcase c))
                                     (else (char-upcase c))))
              (loop (+ i 1) cased))))))

    ;; (string-tokenize s [pred start end]) -- list of maximal runs of chars
    ;; satisfying PRED (default: non-whitespace).
    (define (string-tokenize s . opt)
      (let* ((pred  (if (pair? opt) (car opt) (lambda (c) (not (char-whitespace? c)))))
             (rest  (if (pair? opt) (cdr opt) '()))
             (start (if (pair? rest) (car rest) 0))
             (end   (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) (string-length s))))
        (let loop ((i start) (acc '()))
          (cond
            ((>= i end) (reverse acc))
            ((pred (string-ref s i))
             (let scan ((j (+ i 1)))
               (if (and (< j end) (pred (string-ref s j)))
                   (scan (+ j 1))
                   (loop j (cons (substring s i j) acc)))))
            (else (loop (+ i 1) acc))))))))
