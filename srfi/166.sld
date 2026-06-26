;; Portable (srfi 166) -- Monadic Formatting (the `show` combinator library).
;;
;; A practical CORE of SRFI 166, written in pure R7RS so the SAME file loads
;; identically on pyScheme and cppScheme2 (parity by construction, no native part).
;; SRFI 166 is the hygienic successor of SRFI 159; this implements the widely-used
;; combinators and state machinery.
;;
;; IMPLEMENTED: show; displayed written written-simply written-shared; numeric
;;   numeric/comma; nl fl nothing space-to tab-to escaped maybe-escaped; each
;;   each-in-list; joined joined/prefix joined/suffix joined/last joined/dot
;;   joined/range; padded padded/right padded/both; trimmed trimmed/right
;;   trimmed/both; fitted fitted/right fitted/both; upcased downcased; the state
;;   machinery with with! fn forked call-with-output and the state variables
;;   radix precision sign-rule comma-rule comma-sep decimal-sep pad-char ellipsis
;;   col row port.
;;
;; DELIBERATELY DEFERRED (the rarely-needed extensions; add when wanted): the
;;   columnar / tabular / wrapped / justified / line-numbers family, numeric/si,
;;   numeric/fitted, trimmed/lazy, Unicode-width state variables (string-width,
;;   substring/width, ambiguous-is-wide?), and the pretty-printing combinators.
;;   Column widths here are measured as character counts (string-length).
;;
;; NOTES on the API actually supported:
;;   * `fn` uses the explicit binding form only: (fn ((id state-var) ...) body...);
;;     the (fn (id ...) ...) abbreviation is not supported -- write (id id).
;;   * `with`/`with!` take ((state-var value) ...) where state-var is one of the
;;     exported state-variable identifiers.

(define-library (srfi 166)
  (export show displayed written written-simply written-shared
          numeric numeric/comma
          nl fl nothing space-to tab-to escaped maybe-escaped
          each each-in-list
          joined joined/prefix joined/suffix joined/last joined/dot joined/range
          padded padded/right padded/both
          trimmed trimmed/right trimmed/both
          fitted fitted/right fitted/both
          upcased downcased
          with with! fn forked call-with-output
          radix precision sign-rule comma-rule comma-sep decimal-sep
          pad-char ellipsis col row port)
  (import (scheme base) (scheme write) (scheme char))
  (begin

    ;; ---- state variables (unique keys) ------------------------------------
    ;; col/row/port read from the state's position fields; the rest live in an
    ;; association list inside the state and have the defaults in %defaults.
    (define radix 'srfi166:radix)
    (define precision 'srfi166:precision)
    (define sign-rule 'srfi166:sign-rule)
    (define comma-rule 'srfi166:comma-rule)
    (define comma-sep 'srfi166:comma-sep)
    (define decimal-sep 'srfi166:decimal-sep)
    (define pad-char 'srfi166:pad-char)
    (define ellipsis 'srfi166:ellipsis)
    (define col 'srfi166:col)
    (define row 'srfi166:row)
    (define port 'srfi166:port)

    (define %defaults
      (list (cons radix 10) (cons precision #f) (cons sign-rule #f)
            (cons comma-rule #f) (cons comma-sep #\,) (cons decimal-sep #\.)
            (cons pad-char #\space) (cons ellipsis "")))

    ;; ---- state -------------------------------------------------------------
    ;; Immutable; threaded through formatters.  PORT and the position (COL/ROW)
    ;; carry forward; the var alist is what `with` saves and restores.
    (define-record-type <show-state>
      (%make-state st-port st-col st-row st-vars)
      show-state?
      (st-port %sp) (st-col %sc) (st-row %sr) (st-vars %sv))

    (define (%st-ref st key)
      (cond ((eq? key col) (%sc st))
            ((eq? key row) (%sr st))
            ((eq? key port) (%sp st))
            (else (let ((p (assq key (%sv st))))
                    (if p (cdr p)
                        (let ((d (assq key %defaults)))
                          (if d (cdr d) #f)))))))

    ;; Emit STR to the port, advancing the tracked column/row.  Returns new state.
    (define (%emit str st)
      (write-string str (%sp st))
      (let ((n (string-length str)))
        (let scan ((i 0) (c (%sc st)) (r (%sr st)))
          (if (= i n)
              (%make-state (%sp st) c r (%sv st))
              (if (char=? (string-ref str i) #\newline)
                  (scan (+ i 1) 0 (+ r 1))
                  (scan (+ i 1) (+ c 1) r))))))

    ;; Run a formatter: a procedure (state->state); a string/char outputs itself;
    ;; a number is formatted via numeric (respecting state); anything else, written.
    (define (%run fmt st)
      (cond ((procedure? fmt) (fmt st))
            ((string? fmt) (%emit fmt st))
            ((char? fmt) (%emit (string fmt) st))
            ((number? fmt) ((numeric fmt) st))
            (else (%emit (%write->string fmt) st))))

    (define (%display->string obj)
      (let ((p (open-output-string))) (display obj p) (get-output-string p)))
    (define (%write->string obj)
      (let ((p (open-output-string))) (write obj p) (get-output-string p)))

    ;; ---- entry point -------------------------------------------------------
    (define (show dest . fmts)
      (cond
        ((eq? dest #f)
         (let ((sp (open-output-string)))
           (%run (each-in-list fmts) (%make-state sp 0 0 '()))
           (get-output-string sp)))
        (else
         (let ((p (if (eq? dest #t) (current-output-port) dest)))
           (%run (each-in-list fmts) (%make-state p 0 0 '()))
           (if #f #f)))))

    ;; ---- sequencing --------------------------------------------------------
    (define (each-in-list fmts)
      (lambda (st)
        (let loop ((fs fmts) (st st))
          (if (null? fs) st (loop (cdr fs) (%run (car fs) st))))))
    (define (each . fmts) (each-in-list fmts))

    ;; ---- state machinery ---------------------------------------------------
    (define (%with bindings fmt)
      (lambda (st)
        (let* ((child (%make-state (%sp st) (%sc st) (%sr st)
                                   (append bindings (%sv st))))
               (res (%run fmt child)))
          ;; restore vars, keep the port + advanced position
          (%make-state (%sp res) (%sc res) (%sr res) (%sv st)))))

    (define (%with! bindings fmt)
      (lambda (st)
        (%run fmt (%make-state (%sp st) (%sc st) (%sr st)
                               (append bindings (%sv st))))))

    (define-syntax with
      (syntax-rules ()
        ((_ ((var val) ...) fmt ...)
         (%with (list (cons var val) ...) (each fmt ...)))))

    (define-syntax with!
      (syntax-rules ()
        ((_ ((var val) ...) fmt ...)
         (%with! (list (cons var val) ...) (each fmt ...)))))

    (define-syntax fn
      (syntax-rules ()
        ((_ ((id var) ...) body ...)
         (lambda (st)
           (let ((id (%st-ref st var)) ...)
             (%run (begin body ...) st))))))

    ;; Run A on a discardable copy of the state (its output still happens), then
    ;; B on the original state -- as though A had not run.
    (define (forked a b)
      (lambda (st)
        (%run a (%make-state (%sp st) (%sc st) (%sr st) (%sv st)))
        (%run b st)))

    ;; Capture FMT's output into a string, then run (MAPPER string) on the original.
    (define (call-with-output fmt mapper)
      (lambda (st)
        (let ((sp (open-output-string)))
          (%run fmt (%make-state sp 0 0 (%sv st)))
          (%run (mapper (get-output-string sp)) st))))

    ;; ---- base formatters ---------------------------------------------------
    (define (displayed obj)
      (cond ((procedure? obj) obj)
            ((string? obj) (lambda (st) (%emit obj st)))
            ((char? obj) (lambda (st) (%emit (string obj) st)))
            ((number? obj) (numeric obj))
            (else (lambda (st) (%emit (%display->string obj) st)))))

    (define (written obj)
      (if (number? obj) (numeric obj)
          (lambda (st) (%emit (%write->string obj) st))))

    (define (written-simply obj)
      (lambda (st)
        (let ((p (open-output-string)))
          (write-simple obj p) (%emit (get-output-string p) st))))

    (define (written-shared obj)
      (lambda (st)
        (let ((p (open-output-string)))
          (write-shared obj p) (%emit (get-output-string p) st))))

    (define nl (lambda (st) (%emit "\n" st)))
    (define (fl st) (if (= 0 (%sc st)) st (%emit "\n" st)))
    (define nothing (lambda (st) st))

    (define (space-to k)
      (fn ((c col) (pc pad-char))
        (if (>= c k) nothing (make-string (- k c) pc))))

    (define (tab-to . o)
      (let ((w (if (pair? o) (car o) 8)))
        (fn ((c col) (pc pad-char))
          (if (= 0 (modulo c w)) nothing
              (make-string (- (* w (+ 1 (quotient c w))) c) pc)))))

    (define (escaped s . o)
      (let ((q   (if (>= (length o) 1) (list-ref o 0) #\"))
            (esc (if (>= (length o) 2) (list-ref o 1) #\\))
            (rn  (if (>= (length o) 3) (list-ref o 2) (lambda (c) #f))))
        (lambda (st)
          (let ((out (open-output-string)))
            (string-for-each
             (lambda (c)
               (let ((r (rn c)))
                 (cond (r (write-char (or esc q) out) (write-char r out))
                       ((char=? c q)
                        (if esc (begin (write-char esc out) (write-char q out))
                            (begin (write-char q out) (write-char q out))))
                       ((and esc (char=? c esc))
                        (write-char esc out) (write-char esc out))
                       (else (write-char c out)))))
             s)
            (%emit (get-output-string out) st)))))

    (define (maybe-escaped s pred . o)
      (let ((q   (if (>= (length o) 1) (list-ref o 0) #\"))
            (esc (if (>= (length o) 2) (list-ref o 1) #\\)))
        (lambda (st)
          (let ((needs
                 (let loop ((i 0))
                   (and (< i (string-length s))
                        (let ((c (string-ref s i)))
                          (or (char=? c q) (and esc (char=? c esc)) (pred c)
                              (loop (+ i 1))))))))
            (if needs
                (%run (each (string q) (apply escaped s o) (string q)) st)
                (%emit s st))))))

    ;; ---- numbers -----------------------------------------------------------
    (define %digits "0123456789abcdefghijklmnopqrstuvwxyz")

    (define (%int->radix n r)        ; n: exact nonneg integer
      (if (or (= r 2) (= r 8) (= r 10) (= r 16))
          (number->string n r)
          (if (= n 0) "0"
              (let loop ((n n) (acc '()))
                (if (= n 0) (list->string acc)
                    (loop (quotient n r)
                          (cons (string-ref %digits (remainder n r)) acc)))))))

    (define (%fixed-point mag p ds)  ; mag: exact nonneg rational, p>=0
      (if (<= p 0)
          (number->string (round mag))
          (let* ((scaled (round (* mag (expt 10 p))))
                 (s (number->string scaled))
                 (s (if (< (string-length s) (+ p 1))
                        (string-append (make-string (- (+ p 1) (string-length s)) #\0) s)
                        s))
                 (n (string-length s)))
            (string-append (substring s 0 (- n p)) (string ds) (substring s (- n p) n)))))

    (define (%index-of s ch)
      (let loop ((i 0))
        (cond ((>= i (string-length s)) #f)
              ((char=? (string-ref s i) ch) i)
              (else (loop (+ i 1))))))

    (define (%group-fixed digits k cs)
      (let ((n (string-length digits)))
        (if (<= n k) digits
            (string-append (%group-fixed (substring digits 0 (- n k)) k cs)
                           (string cs) (substring digits (- n k) n)))))

    (define (%join-groups groups cs)
      (if (null? groups) ""
          (let loop ((g (cdr groups)) (s (car groups)))
            (if (null? g) s (loop (cdr g) (string-append s (string cs) (car g)))))))

    (define (%group-list digits sizes cs)
      (let loop ((s digits) (szs sizes) (acc '()))
        (let ((n (string-length s)))
          (if (= n 0)
              (%join-groups acc cs)
              (let* ((k (min (car szs) n))
                     (grp (substring s (- n k) n)))
                (loop (substring s 0 (- n k))
                      (if (pair? (cdr szs)) (cdr szs) szs)
                      (cons grp acc)))))))

    (define (%group-commas digits cr cs)
      (cond ((not cr) digits)
            ((and (integer? cr) (> cr 0)) (%group-fixed digits cr cs))
            ((pair? cr) (%group-list digits cr cs))
            (else digits)))

    (define (%apply-sign body neg sr)
      (cond ((pair? sr) (if neg (string-append (car sr) body (cdr sr)) body))
            (neg (string-append "-" body))
            ((eq? sr #t) (string-append "+" body))
            (else body)))

    (define (%num->string num r p sr cr cs ds)
      (let* ((neg (and (real? num) (negative? num)))
             (mag (if (real? num) (abs num) num))
             (body0
              (cond ((and p (= r 10))
                     (%fixed-point (if (exact? mag) mag (exact mag)) p ds))
                    ((and (integer? mag) (exact? mag)) (%int->radix mag r))
                    (else (let ((s (number->string mag)))
                            (if (char=? ds #\.) s
                                (list->string
                                 (map (lambda (c) (if (char=? c #\.) ds c))
                                      (string->list s))))))))
             (dpos (%index-of body0 ds))
             (ipart (if dpos (substring body0 0 dpos) body0))
             (rest (if dpos (substring body0 dpos (string-length body0)) ""))
             (body (string-append (%group-commas ipart cr cs) rest)))
        (%apply-sign body neg sr)))

    (define (numeric num . opt)
      (lambda (st)
        (let ((r  (if (>= (length opt) 1) (list-ref opt 0) (%st-ref st radix)))
              (p  (if (>= (length opt) 2) (list-ref opt 1) (%st-ref st precision)))
              (sr (if (>= (length opt) 3) (list-ref opt 2) (%st-ref st sign-rule)))
              (cr (if (>= (length opt) 4) (list-ref opt 3) (%st-ref st comma-rule)))
              (cs (if (>= (length opt) 5) (list-ref opt 4) (%st-ref st comma-sep)))
              (ds (if (>= (length opt) 6) (list-ref opt 5) (%st-ref st decimal-sep))))
          (%emit (%num->string num r p sr cr cs ds) st))))

    (define (numeric/comma num . opt)
      (let ((cr (if (>= (length opt) 1) (list-ref opt 0) 3))
            (r  (and (>= (length opt) 2) (list-ref opt 1)))
            (p  (and (>= (length opt) 3) (list-ref opt 2)))
            (sr (and (>= (length opt) 4) (list-ref opt 3))))
        (lambda (st)
          (%emit (%num->string num
                               (or r (%st-ref st radix))
                               (or p (%st-ref st precision))
                               (or sr (%st-ref st sign-rule))
                               (or cr 3)
                               (%st-ref st comma-sep)
                               (%st-ref st decimal-sep))
                 st))))

    ;; ---- joins -------------------------------------------------------------
    (define (joined mapper lst . o)
      (let ((sep (if (pair? o) (car o) "")))
        (lambda (st)
          (let loop ((xs lst) (first #t) (st st))
            (if (null? xs) st
                (loop (cdr xs) #f
                      (%run (mapper (car xs)) (if first st (%run sep st)))))))))

    (define (joined/prefix mapper lst . o)
      (let ((sep (if (pair? o) (car o) "")))
        (lambda (st)
          (let loop ((xs lst) (st st))
            (if (null? xs) st
                (loop (cdr xs) (%run (mapper (car xs)) (%run sep st))))))))

    (define (joined/suffix mapper lst . o)
      (let ((sep (if (pair? o) (car o) "")))
        (lambda (st)
          (let loop ((xs lst) (st st))
            (if (null? xs) st
                (loop (cdr xs) (%run sep (%run (mapper (car xs)) st))))))))

    (define (joined/last mapper last-mapper lst . o)
      (let ((sep (if (pair? o) (car o) "")))
        (lambda (st)
          (let loop ((xs lst) (first #t) (st st))
            (cond ((null? xs) st)
                  ((null? (cdr xs))
                   (%run (last-mapper (car xs)) (if first st (%run sep st))))
                  (else
                   (loop (cdr xs) #f
                         (%run (mapper (car xs)) (if first st (%run sep st))))))))))

    (define (joined/dot mapper dot-mapper lst . o)
      (let ((sep (if (pair? o) (car o) "")))
        (lambda (st)
          (let loop ((xs lst) (first #t) (st st))
            (cond ((null? xs) st)
                  ((pair? xs)
                   (loop (cdr xs) #f
                         (%run (mapper (car xs)) (if first st (%run sep st)))))
                  (else
                   (%run (dot-mapper xs) (%run sep st))))))))

    (define (joined/range mapper start . o)
      (let ((end (if (pair? o) (car o) #f))
            (sep (if (and (pair? o) (pair? (cdr o))) (cadr o) "")))
        (lambda (st)
          (let loop ((i start) (first #t) (st st))
            (if (and end (>= i end)) st
                (loop (+ i 1) #f
                      (%run (mapper i) (if first st (%run sep st)))))))))

    ;; ---- padding -----------------------------------------------------------
    (define (padded width . fmts)
      (call-with-output (each-in-list fmts)
        (lambda (s)
          (fn ((pc pad-char))
            (each (make-string (max 0 (- width (string-length s))) pc) s)))))

    (define (padded/right width . fmts)
      (call-with-output (each-in-list fmts)
        (lambda (s)
          (fn ((pc pad-char))
            (each s (make-string (max 0 (- width (string-length s))) pc))))))

    (define (padded/both width . fmts)
      (call-with-output (each-in-list fmts)
        (lambda (s)
          (fn ((pc pad-char))
            (let* ((pad (max 0 (- width (string-length s))))
                   (l (quotient pad 2)) (r (- pad l)))
              (each (make-string l pc) s (make-string r pc)))))))

    ;; ---- trimming ----------------------------------------------------------
    (define (trimmed width . fmts)
      (call-with-output (each-in-list fmts)
        (lambda (s)
          (fn ((e ellipsis))
            (let ((n (string-length s)))
              (if (<= n width) s
                  (let* ((ew (string-length e)) (keep (max 0 (- width ew))))
                    (string-append e (substring s (- n keep) n)))))))))

    (define (trimmed/right width . fmts)
      (call-with-output (each-in-list fmts)
        (lambda (s)
          (fn ((e ellipsis))
            (let ((n (string-length s)))
              (if (<= n width) s
                  (let* ((ew (string-length e)) (keep (max 0 (- width ew))))
                    (string-append (substring s 0 keep) e))))))))

    (define (trimmed/both width . fmts)
      (call-with-output (each-in-list fmts)
        (lambda (s)
          (fn ((e ellipsis))
            (let ((n (string-length s)))
              (if (<= n width) s
                  (let* ((ew (string-length e))
                         (avail (max 0 (- width (* 2 ew))))
                         (start (quotient (- n avail) 2)))
                    (string-append e (substring s start (+ start avail)) e))))))))

    ;; ---- fitting (trim then pad to exactly WIDTH) --------------------------
    (define (fitted width . fmts)
      (padded/right width (apply trimmed/right width fmts)))
    (define (fitted/right width . fmts)
      (padded width (apply trimmed width fmts)))
    (define (fitted/both width . fmts)
      (padded/both width (apply trimmed/both width fmts)))

    ;; ---- case --------------------------------------------------------------
    (define (upcased . fmts)
      (call-with-output (each-in-list fmts) (lambda (s) (string-upcase s))))
    (define (downcased . fmts)
      (call-with-output (each-in-list fmts) (lambda (s) (string-downcase s))))))
