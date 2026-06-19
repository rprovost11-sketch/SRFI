;; Portable (srfi 64) -- a partial SRFI-64 test harness (plus chibi (chibi test)'s
;; `(test expected expr)` form), enough to run chibi's r7rs-tests.scm and our own
;; metamorphic/property suites on pyscheme / cppscheme2.  Written in portable R7RS
;; so the SAME file is loaded identically by both ports (parity by construction).
;;
;; Reporting: each FAIL prints one "FAIL ..." line; skipped/expected outcomes print
;; "SKIP ..." / "XFAIL ..." / "XPASS ..." lines.  The outermost test-end prints
;;   === P passed, F failed ===
;; (this exact substring is relied on by the chibi survey and run-tests.sh's
;; `0 failed` grep, so it is kept byte-stable) followed by a second
;;   (X expected-fail, Y unexpected-pass, Z skipped)
;; line ONLY when any of those categories is nonzero -- so files that use none of
;; the new features produce byte-identical output to the older harness.
;;
;; This is a deliberately small subset (not the full SRFI-64 runner object model).
;; Deviations from the spec, all intentional and cheap:
;;   * No runner object: the count accessors are variadic and ignore any argument,
;;     so both `(test-runner-fail-count)` and `(test-runner-fail-count runner)` work.
;;   * Specifier procedures (for test-skip/test-expect-fail) receive the test NAME,
;;     not a runner.  Integer specifiers match the next N tests; string/symbol
;;     specifiers match by test name.
;;   * skip and expect-fail specifiers are checked once per test (their internal
;;     counters advance on every test, including skipped ones).
(define-library (srfi 64)
  (export test test-equal test-eqv test-assert test-error test-values
          test-begin test-end test-group
          test-skip test-expect-fail
          test-runner-current
          test-runner-pass-count test-runner-fail-count
          test-runner-xpass-count test-runner-xfail-count
          test-runner-skip-count)
  (import (scheme base) (scheme write) (scheme complex))
  (begin
    (define %pass 0)
    (define %fail 0)
    (define %xpass 0)   ; expected to fail, but passed (unexpected pass)
    (define %xfail 0)   ; expected to fail, and did (counts as OK)
    (define %skip 0)
    (define %depth 0)
    (define %error-marker (list '<error>))

    ;; Active specifier procedures (each: name -> boolean), most-recent first.
    (define %skip-specs '())
    (define %fail-specs '())

    ;; Catch exceptions without `guard` (which currently expands to an internal
    ;; %guard-eval not visible inside a library env): call/cc + with-exception-
    ;; handler, both real primitives exported by (scheme base).
    (define (%try thunk on-error)
      (call-with-current-continuation
        (lambda (k)
          (with-exception-handler
            (lambda (e) (k (on-error e)))
            thunk))))

    ;; ---- count accessors (variadic: ignore any runner argument) -------------
    (define (test-runner-current . _) 'srfi64-default-runner)
    (define (test-runner-pass-count  . _) %pass)
    (define (test-runner-fail-count  . _) %fail)
    (define (test-runner-xpass-count . _) %xpass)
    (define (test-runner-xfail-count . _) %xfail)
    (define (test-runner-skip-count  . _) %skip)

    ;; ---- float-tolerant equality (matches (chibi test)) ---------------------
    (define (%nan? x) (and (number? x) (real? x) (not (= x x))))
    (define (%inexact-real? x) (and (real? x) (inexact? x)))
    (define (%approx=? a b)
      (let ((eps 1e-6))   ; relative; loose enough to ignore last-ULP noise
        (<= (abs (- a b)) (* eps (max 1.0 (abs a) (abs b))))))
    ;; equal?, but NaN=NaN and inexact reals compared approximately.
    (define (%same? expected actual)
      (cond
        ((eqv? expected actual) #t)   ; also makes +inf.0 = +inf.0 etc.
        ((and (%nan? expected) (%nan? actual)) #t)
        ((and (%inexact-real? expected) (%inexact-real? actual))
         (%approx=? expected actual))
        ((and (number? expected) (number? actual)
              (inexact? expected) (inexact? actual)
              (or (not (real? expected)) (not (real? actual))))
         (and (%approx=? (real-part expected) (real-part actual))
              (%approx=? (imag-part expected) (imag-part actual))))
        (else (equal? expected actual))))

    ;; ---- specifiers ---------------------------------------------------------
    ;; Turn a count / name / predicate into a stateful (name -> boolean) proc.
    (define (%as-specifier spec)
      (cond
        ((procedure? spec) spec)                  ; user predicate on the name
        ((and (integer? spec) (exact? spec))      ; match the next `spec` tests
         (let ((count 0))
           (lambda (name) (set! count (+ count 1)) (<= count spec))))
        ((string? spec)
         (lambda (name)
           (cond ((string? name) (string=? name spec))
                 ((symbol? name) (string=? (symbol->string name) spec))
                 (else #f))))
        ((symbol? spec)
         (lambda (name) (and (symbol? name) (eq? name spec))))
        (else (lambda (name) #f))))

    ;; Call EVERY spec once (advancing its counter), return #t if any matched.
    (define (%match-specs specs name)
      (let loop ((s specs) (hit #f))
        (if (null? s)
            hit
            (loop (cdr s) (or ((car s) name) hit)))))

    (define (test-skip spec)
      (set! %skip-specs (cons (%as-specifier spec) %skip-specs)))
    (define (test-expect-fail spec)
      (set! %fail-specs (cons (%as-specifier spec) %fail-specs)))

    ;; ---- the core: classify one test ----------------------------------------
    ;; `produce` is a thunk, evaluated only if the test isn't skipped, returning
    ;; (list ok? expected actual).
    (define (%test name produce)
      (let ((skip?  (%match-specs %skip-specs name))
            (xfail? (%match-specs %fail-specs name)))
        (cond
          (skip?
           (set! %skip (+ %skip 1))
           (display "SKIP ") (write name) (newline))
          (else
           (let* ((r (produce))
                  (ok? (car r))
                  (expected (car (cdr r)))
                  (actual (car (cdr (cdr r)))))
             (cond
               (xfail?
                (cond
                  (ok? (set! %xpass (+ %xpass 1))
                       (display "XPASS ") (write name)
                       (display " (expected fail, but passed)") (newline))
                  (else (set! %xfail (+ %xfail 1))
                        (display "XFAIL ") (write name) (newline))))
               (ok? (set! %pass (+ %pass 1)))
               (else
                (set! %fail (+ %fail 1))
                (display "FAIL ") (write name)
                (display " expected ") (write expected)
                (display " got ") (write actual) (newline))))))))

    ;; ---- producers for each test form ---------------------------------------
    (define (%produce-equal expected thunk)
      (lambda ()
        (let ((actual (%try thunk (lambda (e) %error-marker))))
          (if (eq? actual %error-marker)
              (list #f expected '<raised-error>)
              (list (%same? expected actual) expected actual)))))

    (define (%produce-assert thunk)
      (lambda ()
        (let ((v (%try thunk (lambda (e) %error-marker))))
          (if (and (not (eq? v %error-marker)) v)
              (list #t 'true v)
              (list #f 'true v)))))

    (define (%produce-error thunk)
      (lambda ()
        (let ((raised (%try (lambda () (thunk) #f) (lambda (e) #t))))
          (list raised '<error-expected> (if raised '<error> 'no-error)))))

    ;; ---- the test forms -----------------------------------------------------
    (define-syntax test
      (syntax-rules ()
        ((_ expected expr) (%test 'expr (%produce-equal expected (lambda () expr))))
        ((_ name expected expr) (%test name (%produce-equal expected (lambda () expr))))))

    (define-syntax test-equal
      (syntax-rules ()
        ((_ expected expr) (test expected expr))
        ((_ name expected expr) (test name expected expr))))

    (define-syntax test-eqv
      (syntax-rules ()
        ((_ expected expr) (test expected expr))))

    (define-syntax test-assert
      (syntax-rules ()
        ((_ expr) (%test 'expr (%produce-assert (lambda () expr))))
        ((_ name expr) (%test name (%produce-assert (lambda () expr))))))

    ;; (test-error expr) / (test-error pred expr) / (test-error name pred expr)
    ;; -- we only check that an error is raised, ignoring any predicate.
    (define-syntax test-error
      (syntax-rules ()
        ((_ expr) (%test 'expr (%produce-error (lambda () expr))))
        ((_ a expr) (%test 'expr (%produce-error (lambda () expr))))
        ((_ a b expr) (%test 'expr (%produce-error (lambda () expr))))))

    (define-syntax test-values
      (syntax-rules ()
        ((_ expected expr)
         (%test 'expr
                (%produce-equal (call-with-values (lambda () expected) list)
                                (lambda () (call-with-values (lambda () expr) list)))))))

    ;; ---- grouping / summary -------------------------------------------------
    (define-syntax test-begin
      (syntax-rules ()
        ((_) (set! %depth (+ %depth 1)))
        ((_ name) (set! %depth (+ %depth 1)))))

    (define (%test-end)
      (set! %depth (- %depth 1))
      (if (<= %depth 0)
          (begin
            (display "=== ") (display %pass) (display " passed, ")
            (display %fail) (display " failed ===") (newline)
            (if (or (> %xfail 0) (> %xpass 0) (> %skip 0))
                (begin
                  (display "    (")
                  (display %xfail) (display " expected-fail, ")
                  (display %xpass) (display " unexpected-pass, ")
                  (display %skip) (display " skipped)") (newline))))))
    (define-syntax test-end
      (syntax-rules ()
        ((_) (%test-end))
        ((_ name) (%test-end))))

    (define-syntax test-group
      (syntax-rules ()
        ((_ name body ...) (begin (test-begin name) body ... (test-end name)))))
    ))
