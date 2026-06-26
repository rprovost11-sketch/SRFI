;; Portable (srfi 152) -- String Library (reduced).  COMPLETE implementation of the
;; SRFI 152 procedure set, written in pure R7RS so the SAME file loads identically on
;; pyScheme and cppScheme2 (parity by construction, no native part).
;;
;; DELIBERATE SCOPE DECISION:
;;   * CRITERIA ARE PREDICATES ONLY.  SRFI 152 also accepts a char or a SRFI-14
;;     char-set wherever a "criterion" is expected; here a criterion is a one-argument
;;     predicate on a char (the trim/search procedures default to char-whitespace?).
;;     This keeps the library free of any SRFI 14 dependency.  To match a specific
;;     char, pass a predicate, e.g. (string-index s (lambda (c) (char=? c #\,))).
;;
;; Procedures that are plain R7RS with SRFI-152-compatible signatures (string?,
;; make-string, string, the comparison predicates, string-copy, string->list,
;; read-string, write-string, the mutators, ...) are RE-EXPORTED from (scheme base)
;; and (scheme char) rather than redefined.  Everything SRFI-152-specific is defined
;; here.  All index ranges follow SRFI 152: optional START is inclusive, END exclusive.

(define-library (srfi 152)
  (export
    ;; predicates
    string? string-null? string-every string-any
    ;; constructors
    make-string string string-tabulate string-unfold string-unfold-right
    ;; conversion
    string->vector string->list vector->string list->string reverse-list->string
    ;; selection
    string-length string-ref substring string-copy
    string-take string-drop string-take-right string-drop-right
    string-pad string-pad-right
    string-trim string-trim-right string-trim-both
    ;; replacement
    string-replace
    ;; comparison
    string=? string<? string>? string<=? string>=?
    string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
    ;; prefixes & suffixes
    string-prefix-length string-suffix-length string-prefix? string-suffix?
    ;; searching
    string-index string-index-right string-skip string-skip-right
    string-contains string-contains-right
    string-take-while string-take-while-right
    string-drop-while string-drop-while-right
    string-span string-break
    ;; concatenation
    string-append string-concatenate string-concatenate-reverse string-join
    ;; fold, map & friends
    string-fold string-fold-right string-map string-for-each
    string-count string-filter string-remove
    ;; replication & splitting
    string-replicate string-segment string-split
    ;; input/output
    read-string write-string
    ;; mutation
    string-set! string-fill! string-copy!)
  (import (scheme base) (scheme char))
  (begin

    ;; ---- small helpers -----------------------------------------------------
    ;; Resolve an optional (start end) tail against a string of length N.
    (define (%start opt n) (if (pair? opt) (car opt) 0))
    (define (%end opt n) (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) n))
    ;; Append a "stringable" THING (char or string) to output port OUT.  Used by
    ;; the unfold procedures, whose mapper/base/make-final may yield either.
    (define (%emit thing out)
      (cond ((char? thing) (write-char thing out))
            ((string? thing) (write-string thing out))
            (else (error "srfi 152: expected a char or string" thing))))

    ;; ---- predicates --------------------------------------------------------
    (define (string-null? s) (= 0 (string-length s)))

    (define (string-any pred s . opt)
      (let ((start (%start opt (string-length s)))
            (end (%end opt (string-length s))))
        (let loop ((i start))
          (if (>= i end) #f
              (let ((r (pred (string-ref s i)))) (if r r (loop (+ i 1))))))))

    (define (string-every pred s . opt)
      (let ((start (%start opt (string-length s)))
            (end (%end opt (string-length s))))
        (let loop ((i start) (last #t))
          (if (>= i end) last
              (let ((r (pred (string-ref s i)))) (if r (loop (+ i 1) r) #f))))))

    ;; ---- constructors ------------------------------------------------------
    (define (string-tabulate proc len)
      (let ((out (open-output-string)))
        (let loop ((i 0))
          (if (>= i len) (get-output-string out)
              (begin (write-char (proc i) out) (loop (+ i 1)))))))

    ;; (string-unfold stop? mapper successor seed [base make-final])
    ;; base ++ (mapper seed) ++ (mapper (successor seed)) ++ ... until (stop? seed),
    ;; then ++ (make-final seed).  mapper/base/make-final yield a char or string.
    (define (string-unfold stop? mapper successor seed . opt)
      (let ((base (if (pair? opt) (car opt) ""))
            (make-final (if (and (pair? opt) (pair? (cdr opt))) (cadr opt)
                            (lambda (x) "")))
            (out (open-output-string)))
        (%emit base out)
        (let loop ((seed seed))
          (if (stop? seed)
              (begin (%emit (make-final seed) out) (get-output-string out))
              (begin (%emit (mapper seed) out) (loop (successor seed)))))))

    ;; Right variant: the body is assembled right-to-left, so the result reads
    ;; (make-final seed) ++ ...(mapper (successor seed)) ++ (mapper seed)... ++ base.
    ;; Collect the mapper pieces (newest at the front) then emit final, pieces, base.
    (define (string-unfold-right stop? mapper successor seed . opt)
      (let ((base (if (pair? opt) (car opt) ""))
            (make-final (if (and (pair? opt) (pair? (cdr opt))) (cadr opt)
                            (lambda (x) ""))))
        (let collect ((seed seed) (pieces '()))
          (if (stop? seed)
              (let ((out (open-output-string)))
                (%emit (make-final seed) out)
                (for-each (lambda (p) (%emit p out)) pieces)
                (%emit base out)
                (get-output-string out))
              (collect (successor seed) (cons (mapper seed) pieces))))))

    ;; ---- conversion --------------------------------------------------------
    (define (reverse-list->string chars) (list->string (reverse chars)))

    ;; ---- selection ---------------------------------------------------------
    (define (string-take s n) (substring s 0 n))
    (define (string-drop s n) (substring s n (string-length s)))
    (define (string-take-right s n) (substring s (- (string-length s) n) (string-length s)))
    (define (string-drop-right s n) (substring s 0 (- (string-length s) n)))

    ;; (string-pad s len [char start end]) -- right-justify within LEN: pad on the
    ;; left with CHAR (default space) or, if too long, keep the rightmost LEN chars.
    (define (string-pad s len . opt)
      (let* ((ch    (if (pair? opt) (car opt) #\space))
             (rest  (if (pair? opt) (cdr opt) '()))
             (start (%start rest (string-length s)))
             (end   (%end rest (string-length s)))
             (sub   (substring s start end))
             (n     (string-length sub)))
        (cond ((= n len) sub)
              ((> n len) (substring sub (- n len) n))
              (else (string-append (make-string (- len n) ch) sub)))))

    (define (string-pad-right s len . opt)
      (let* ((ch    (if (pair? opt) (car opt) #\space))
             (rest  (if (pair? opt) (cdr opt) '()))
             (start (%start rest (string-length s)))
             (end   (%end rest (string-length s)))
             (sub   (substring s start end))
             (n     (string-length sub)))
        (cond ((= n len) sub)
              ((> n len) (substring sub 0 len))
              (else (string-append sub (make-string (- len n) ch))))))

    ;; trim: left / right / both.  Optional criterion predicate (default whitespace)
    ;; then optional start/end.
    (define (string-trim s . opt)
      (let* ((pred  (if (pair? opt) (car opt) char-whitespace?))
             (rest  (if (pair? opt) (cdr opt) '()))
             (start (%start rest (string-length s)))
             (end   (%end rest (string-length s))))
        (let loop ((i start))
          (cond ((>= i end) "")
                ((pred (string-ref s i)) (loop (+ i 1)))
                (else (substring s i end))))))

    (define (string-trim-right s . opt)
      (let* ((pred  (if (pair? opt) (car opt) char-whitespace?))
             (rest  (if (pair? opt) (cdr opt) '()))
             (start (%start rest (string-length s)))
             (end   (%end rest (string-length s))))
        (let loop ((i end))
          (cond ((<= i start) "")
                ((pred (string-ref s (- i 1))) (loop (- i 1)))
                (else (substring s start i))))))

    (define (string-trim-both s . opt)
      (let* ((pred  (if (pair? opt) (car opt) char-whitespace?))
             (rest  (if (pair? opt) (cdr opt) '()))
             (start (%start rest (string-length s)))
             (end   (%end rest (string-length s))))
        ;; trim right within [start,end), then left within the result range
        (let rloop ((hi end))
          (cond ((<= hi start) "")
                ((pred (string-ref s (- hi 1))) (rloop (- hi 1)))
                (else
                 (let lloop ((lo start))
                   (cond ((>= lo hi) "")
                         ((pred (string-ref s lo)) (lloop (+ lo 1)))
                         (else (substring s lo hi)))))))))

    ;; ---- replacement -------------------------------------------------------
    ;; (string-replace s1 s2 start1 end1 [start2 end2]) -- s1 with [start1,end1)
    ;; replaced by s2[start2,end2) (default whole of s2).
    (define (string-replace s1 s2 start1 end1 . opt)
      (let ((start2 (%start opt (string-length s2)))
            (end2 (%end opt (string-length s2))))
        (string-append (substring s1 0 start1)
                       (substring s2 start2 end2)
                       (substring s1 end1 (string-length s1)))))

    ;; ---- prefixes & suffixes ----------------------------------------------
    ;; Optional tail is (start1 end1 start2 end2) over s1 then s2.
    (define (%pfx-ranges s1 s2 opt)
      (let ((l1 (string-length s1)) (l2 (string-length s2)))
        (list (if (>= (length opt) 1) (list-ref opt 0) 0)
              (if (>= (length opt) 2) (list-ref opt 1) l1)
              (if (>= (length opt) 3) (list-ref opt 2) 0)
              (if (>= (length opt) 4) (list-ref opt 3) l2))))

    (define (string-prefix-length s1 s2 . opt)
      (let* ((r (%pfx-ranges s1 s2 opt))
             (a (car r)) (ae (cadr r)) (b (list-ref r 2)) (be (list-ref r 3)))
        (let loop ((i a) (j b) (n 0))
          (if (or (>= i ae) (>= j be)
                  (not (char=? (string-ref s1 i) (string-ref s2 j))))
              n
              (loop (+ i 1) (+ j 1) (+ n 1))))))

    (define (string-suffix-length s1 s2 . opt)
      (let* ((r (%pfx-ranges s1 s2 opt))
             (a (car r)) (ae (cadr r)) (b (list-ref r 2)) (be (list-ref r 3)))
        (let loop ((i (- ae 1)) (j (- be 1)) (n 0))
          (if (or (< i a) (< j b)
                  (not (char=? (string-ref s1 i) (string-ref s2 j))))
              n
              (loop (- i 1) (- j 1) (+ n 1))))))

    ;; (string-prefix? s1 s2 ...) -- is s1 a prefix of s2?  SRFI-152 order.
    (define (string-prefix? s1 s2 . opt)
      (let* ((r (%pfx-ranges s1 s2 opt))
             (a (car r)) (ae (cadr r)) (b (list-ref r 2)) (be (list-ref r 3))
             (la (- ae a)))
        (and (<= la (- be b))
             (= la (string-prefix-length s1 s2 a ae b be)))))

    (define (string-suffix? s1 s2 . opt)
      (let* ((r (%pfx-ranges s1 s2 opt))
             (a (car r)) (ae (cadr r)) (b (list-ref r 2)) (be (list-ref r 3))
             (la (- ae a)))
        (and (<= la (- be b))
             (= la (string-suffix-length s1 s2 a ae b be)))))

    ;; ---- searching ---------------------------------------------------------
    (define (string-index s pred . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let loop ((i start))
          (cond ((>= i end) #f)
                ((pred (string-ref s i)) i)
                (else (loop (+ i 1)))))))

    (define (string-index-right s pred . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let loop ((i (- end 1)))
          (cond ((< i start) #f)
                ((pred (string-ref s i)) i)
                (else (loop (- i 1)))))))

    (define (string-skip s pred . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let loop ((i start))
          (cond ((>= i end) #f)
                ((pred (string-ref s i)) (loop (+ i 1)))
                (else i)))))

    (define (string-skip-right s pred . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let loop ((i (- end 1)))
          (cond ((< i start) #f)
                ((pred (string-ref s i)) (loop (- i 1)))
                (else i)))))

    ;; first index >= START in s1 at which s2[sb,se) occurs, searching s1[a,ae); #f.
    (define (%find s1 s2 a ae b be)
      (let ((lsub (- be b)))
        (if (= lsub 0)
            a
            (let loop ((i a))
              (cond ((> (+ i lsub) ae) #f)
                    ((let inner ((k 0))
                       (cond ((= k lsub) #t)
                             ((char=? (string-ref s1 (+ i k)) (string-ref s2 (+ b k)))
                              (inner (+ k 1)))
                             (else #f)))
                     i)
                    (else (loop (+ i 1))))))))

    (define (string-contains s1 s2 . opt)
      (let* ((r (%pfx-ranges s1 s2 opt))
             (a (car r)) (ae (cadr r)) (b (list-ref r 2)) (be (list-ref r 3)))
        (%find s1 s2 a ae b be)))

    (define (string-contains-right s1 s2 . opt)
      (let* ((r (%pfx-ranges s1 s2 opt))
             (a (car r)) (ae (cadr r)) (b (list-ref r 2)) (be (list-ref r 3))
             (lsub (- be b)))
        (if (= lsub 0)
            ae
            (let loop ((i (- ae lsub)) (found #f))
              (cond ((< i a) found)
                    ((let inner ((k 0))
                       (cond ((= k lsub) #t)
                             ((char=? (string-ref s1 (+ i k)) (string-ref s2 (+ b k)))
                              (inner (+ k 1)))
                             (else #f)))
                     i)
                    (else (loop (- i 1) found)))))))

    (define (string-take-while s pred . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let ((i (let loop ((i start))
                   (cond ((>= i end) end)
                         ((pred (string-ref s i)) (loop (+ i 1)))
                         (else i)))))
          (substring s start i))))

    (define (string-take-while-right s pred . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let ((i (let loop ((i (- end 1)))
                   (cond ((< i start) start)
                         ((pred (string-ref s i)) (loop (- i 1)))
                         (else (+ i 1))))))
          (substring s i end))))

    (define (string-drop-while s pred . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let ((i (let loop ((i start))
                   (cond ((>= i end) end)
                         ((pred (string-ref s i)) (loop (+ i 1)))
                         (else i)))))
          (substring s i end))))

    (define (string-drop-while-right s pred . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let ((i (let loop ((i (- end 1)))
                   (cond ((< i start) start)
                         ((pred (string-ref s i)) (loop (- i 1)))
                         (else (+ i 1))))))
          (substring s start i))))

    ;; (string-span s pred ...) -> two values: the take-while prefix and the rest.
    (define (string-span s pred . opt)
      (let* ((start (%start opt (string-length s))) (end (%end opt (string-length s)))
             (i (let loop ((i start))
                  (cond ((>= i end) end)
                        ((pred (string-ref s i)) (loop (+ i 1)))
                        (else i)))))
        (values (substring s start i) (substring s i end))))

    ;; (string-break s pred ...) -> prefix of chars NOT satisfying pred, and the rest.
    (define (string-break s pred . opt)
      (let* ((start (%start opt (string-length s))) (end (%end opt (string-length s)))
             (i (let loop ((i start))
                  (cond ((>= i end) end)
                        ((pred (string-ref s i)) i)
                        (else (loop (+ i 1)))))))
        (values (substring s start i) (substring s i end))))

    ;; ---- concatenation -----------------------------------------------------
    (define (string-concatenate string-list)
      (apply string-append string-list))

    ;; (string-concatenate-reverse string-list [final-string end])
    (define (string-concatenate-reverse string-list . opt)
      (let* ((final (if (pair? opt) (car opt) ""))
             (final (if (and (pair? opt) (pair? (cdr opt)))
                        (substring final 0 (cadr opt))
                        final)))
        (string-append (apply string-append (reverse string-list)) final)))

    ;; (string-join strings [delim grammar]); grammar in
    ;; {infix(default) strict-infix prefix suffix}.
    (define (string-join strings . opt)
      (let ((delim   (if (pair? opt) (car opt) " "))
            (grammar (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) 'infix)))
        (cond
          ((null? strings)
           (if (eq? grammar 'strict-infix)
               (error "string-join: empty list with strict-infix grammar")
               ""))
          (else
           (let ((body (let loop ((rest (cdr strings)) (acc (car strings)))
                         (if (null? rest) acc
                             (loop (cdr rest) (string-append acc delim (car rest)))))))
             (case grammar
               ((suffix) (string-append body delim))
               ((prefix) (string-append delim body))
               (else body)))))))

    ;; ---- fold, map & friends ----------------------------------------------
    (define (string-fold kons knil s . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let loop ((i start) (acc knil))
          (if (>= i end) acc
              (loop (+ i 1) (kons (string-ref s i) acc))))))

    (define (string-fold-right kons knil s . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let loop ((i (- end 1)) (acc knil))
          (if (< i start) acc
              (loop (- i 1) (kons (string-ref s i) acc))))))

    (define (string-count s pred . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s))))
        (let loop ((i start) (c 0))
          (cond ((>= i end) c)
                ((pred (string-ref s i)) (loop (+ i 1) (+ c 1)))
                (else (loop (+ i 1) c))))))

    (define (string-filter pred s . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s)))
            (out (open-output-string)))
        (let loop ((i start))
          (cond ((>= i end) (get-output-string out))
                (else (when (pred (string-ref s i)) (write-char (string-ref s i) out))
                      (loop (+ i 1)))))))

    (define (string-remove pred s . opt)
      (let ((start (%start opt (string-length s))) (end (%end opt (string-length s)))
            (out (open-output-string)))
        (let loop ((i start))
          (cond ((>= i end) (get-output-string out))
                (else (unless (pred (string-ref s i)) (write-char (string-ref s i) out))
                      (loop (+ i 1)))))))

    ;; ---- replication & splitting ------------------------------------------
    ;; (string-replicate s from to [start end]) -- substring of the conceptual
    ;; bi-infinite replication of s[start,end), indexed so 0..len-1 = the substring.
    (define (string-replicate s from to . opt)
      (let* ((start (%start opt (string-length s))) (end (%end opt (string-length s)))
             (len (- end start)) (out (open-output-string)))
        (when (and (> to from) (= len 0))
          (error "string-replicate: empty range cannot be replicated" s))
        (let loop ((i from))
          (if (>= i to) (get-output-string out)
              (begin (write-char (string-ref s (+ start (modulo i len))) out)
                     (loop (+ i 1)))))))

    ;; (string-segment s k) -- list of consecutive length-K chunks (last may be short).
    (define (string-segment s k)
      (when (<= k 0) (error "string-segment: chunk length must be positive" k))
      (let ((n (string-length s)))
        (let loop ((i 0) (acc '()))
          (if (>= i n) (reverse acc)
              (let ((j (min n (+ i k))))
                (loop j (cons (substring s i j) acc)))))))

    ;; (string-split s delim [grammar limit start end]) -- split s[start,end) on the
    ;; STRING delimiter DELIM.  GRAMMAR in {infix(default) strict-infix prefix suffix};
    ;; LIMIT caps the number of splits.  An empty delimiter splits into single chars.
    (define (string-split s delim . opt)
      (let* ((grammar (if (>= (length opt) 1) (list-ref opt 0) 'infix))
             (limit   (if (and (>= (length opt) 2) (list-ref opt 1)) (list-ref opt 1) #f))
             (start   (if (>= (length opt) 3) (list-ref opt 2) 0))
             (end     (if (>= (length opt) 4) (list-ref opt 3) (string-length s)))
             (sub     (substring s start end))
             (n       (string-length sub))
             (ld      (string-length delim)))
        (let ((pieces
               (if (= ld 0)
                   (map string (string->list sub))
                   (let loop ((p 0) (acc '()) (count 0))
                     (let ((idx (and (or (not limit) (< count limit))
                                     (%find sub delim p n 0 ld))))
                       (if (not idx)
                           (reverse (cons (substring sub p n) acc))
                           (loop (+ idx ld)
                                 (cons (substring sub p idx) acc)
                                 (+ count 1))))))))
          (case grammar
            ((suffix)
             (let ((r (reverse pieces)))
               (if (and (pair? r) (string-null? (car r))) (reverse (cdr r)) pieces)))
            ((prefix)
             (if (and (pair? pieces) (string-null? (car pieces))) (cdr pieces) pieces))
            (else pieces)))))))
