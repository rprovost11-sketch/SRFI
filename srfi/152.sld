;; Portable (srfi 152) -- String Library (reduced).  PARTIAL implementation: a
;; practical CORE of the most-used procedures, enough for text/parsing work (e.g. a
;; .log-file parser) and broadly useful, written in pure R7RS so the SAME file loads
;; identically on pyScheme and cppScheme2 (parity by construction, no native part).
;;
;; DELIBERATE SCOPE DECISIONS:
;;   * CRITERIA ARE PREDICATES ONLY.  SRFI 152 also accepts a char or a SRFI-14
;;     char-set as a "criterion"; here a criterion is a one-argument predicate on a
;;     char (the trim procedures default to char-whitespace?).  This keeps the library
;;     free of any SRFI 14 dependency.  To match a specific char, pass a predicate,
;;     e.g. (string-index s (lambda (c) (char=? c #\,))).
;;   * Not all ~40 SRFI 152 procedures are here yet -- this is a growable core (cf.
;;     the partial (srfi 64)).  Add procedures as needed.
;;
;; Implemented: string-null? string-prefix? string-suffix? string-contains
;;   string-take string-drop string-take-right string-drop-right
;;   string-trim string-trim-right string-trim-both
;;   string-index string-index-right string-count string-any string-every
;;   string-split string-join
;; Argument order follows SRFI 152: (string-prefix? prefix s), (string-contains s sub)
;; returns the index of the first match or #f.

(define-library (srfi 152)
  (export string-null? string-prefix? string-suffix? string-contains
          string-take string-drop string-take-right string-drop-right
          string-trim string-trim-right string-trim-both
          string-index string-index-right string-count string-any string-every
          string-split string-join)
  (import (scheme base) (scheme char))
  (begin

    (define (string-null? s) (= 0 (string-length s)))

    (define (string-prefix? p s)
      (let ((lp (string-length p)) (ls (string-length s)))
        (and (<= lp ls)
             (let loop ((i 0))
               (cond ((= i lp) #t)
                     ((char=? (string-ref p i) (string-ref s i)) (loop (+ i 1)))
                     (else #f))))))

    (define (string-suffix? p s)
      (let* ((lp (string-length p)) (ls (string-length s)) (off (- ls lp)))
        (and (<= lp ls)
             (let loop ((i 0))
               (cond ((= i lp) #t)
                     ((char=? (string-ref p i) (string-ref s (+ off i))) (loop (+ i 1)))
                     (else #f))))))

    ;; first index in S (>= START) at which SUB occurs, or #f.
    (define (%find s sub start)
      (let ((ls (string-length s)) (lsub (string-length sub)))
        (if (= lsub 0)
            start
            (let loop ((i start))
              (cond ((> (+ i lsub) ls) #f)
                    ((let inner ((j 0))
                       (cond ((= j lsub) #t)
                             ((char=? (string-ref s (+ i j)) (string-ref sub j)) (inner (+ j 1)))
                             (else #f)))
                     i)
                    (else (loop (+ i 1))))))))

    (define (string-contains s sub) (%find s sub 0))

    (define (string-take s n) (substring s 0 n))
    (define (string-drop s n) (substring s n (string-length s)))
    (define (string-take-right s n) (substring s (- (string-length s) n) (string-length s)))
    (define (string-drop-right s n) (substring s 0 (- (string-length s) n)))

    ;; trim: left / right / both.  Optional criterion predicate (default whitespace).
    (define (string-trim s . opt)
      (let ((pred (if (pair? opt) (car opt) char-whitespace?))
            (n (string-length s)))
        (let loop ((i 0))
          (cond ((>= i n) "")
                ((pred (string-ref s i)) (loop (+ i 1)))
                (else (substring s i n))))))

    (define (string-trim-right s . opt)
      (let ((pred (if (pair? opt) (car opt) char-whitespace?)))
        (let loop ((i (string-length s)))
          (cond ((<= i 0) "")
                ((pred (string-ref s (- i 1))) (loop (- i 1)))
                (else (substring s 0 i))))))

    (define (string-trim-both s . opt)
      (let ((pred (if (pair? opt) (car opt) char-whitespace?)))
        (string-trim-right (string-trim s pred) pred)))

    ;; index of first/last char satisfying PRED in [start,end); #f if none.
    (define (%bounds s opt) ; -> (start . end)
      (let ((n (string-length s)))
        (cons (if (pair? opt) (car opt) 0)
              (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) n))))

    (define (string-index s pred . opt)
      (let* ((b (%bounds s opt)) (start (car b)) (end (cdr b)))
        (let loop ((i start))
          (cond ((>= i end) #f)
                ((pred (string-ref s i)) i)
                (else (loop (+ i 1)))))))

    (define (string-index-right s pred . opt)
      (let* ((b (%bounds s opt)) (start (car b)) (end (cdr b)))
        (let loop ((i (- end 1)))
          (cond ((< i start) #f)
                ((pred (string-ref s i)) i)
                (else (loop (- i 1)))))))

    (define (string-count s pred . opt)
      (let* ((b (%bounds s opt)) (start (car b)) (end (cdr b)))
        (let loop ((i start) (c 0))
          (cond ((>= i end) c)
                ((pred (string-ref s i)) (loop (+ i 1) (+ c 1)))
                (else (loop (+ i 1) c))))))

    ;; string-any: first non-#f (pred char); string-every: last (pred char) if all true.
    (define (string-any pred s)
      (let ((n (string-length s)))
        (let loop ((i 0))
          (if (>= i n) #f
              (let ((r (pred (string-ref s i)))) (if r r (loop (+ i 1))))))))

    (define (string-every pred s)
      (let ((n (string-length s)))
        (let loop ((i 0) (last #t))
          (if (>= i n) last
              (let ((r (pred (string-ref s i)))) (if r (loop (+ i 1) r) #f))))))

    ;; split S on the STRING delimiter DELIM.  GRAMMAR in {infix (default), suffix,
    ;; prefix}; LIMIT caps the number of splits.  An empty delimiter splits into chars.
    (define (string-split s delim . opt)
      (let ((grammar (if (pair? opt) (car opt) 'infix))
            (limit   (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) #f))
            (ld      (string-length delim))
            (n       (string-length s)))
        (let ((pieces
               (if (= ld 0)
                   (map string (string->list s))
                   (let loop ((start 0) (acc '()) (count 0))
                     (let ((idx (and (or (not limit) (< count limit))
                                     (%find s delim start))))
                       (if (not idx)
                           (reverse (cons (substring s start n) acc))
                           (loop (+ idx ld)
                                 (cons (substring s start idx) acc)
                                 (+ count 1))))))))
          (case grammar
            ((suffix)
             (let ((r (reverse pieces)))
               (if (and (pair? r) (string-null? (car r))) (reverse (cdr r)) pieces)))
            ((prefix)
             (if (and (pair? pieces) (string-null? (car pieces))) (cdr pieces) pieces))
            (else pieces)))))

    ;; join STRINGS with DELIM (default " "); GRAMMAR in {infix(default), suffix, prefix}.
    (define (string-join strings . opt)
      (let ((delim   (if (pair? opt) (car opt) " "))
            (grammar (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) 'infix)))
        (if (null? strings)
            (if (eq? grammar 'suffix) "" "")
            (let ((body (let loop ((rest (cdr strings)) (acc (car strings)))
                          (if (null? rest) acc
                              (loop (cdr rest) (string-append acc delim (car rest)))))))
              (case grammar
                ((suffix) (string-append body delim))
                ((prefix) (string-append delim body))
                (else body))))))))
