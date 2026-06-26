;; (scheme string) -- R7RS-large (Red Edition) String library.
;;
;; The Red Edition adopts SRFI 152 verbatim as (scheme string), so this is a thin
;; alias that re-exports the whole (srfi 152) surface under the R7RS-large name.  Load
;; it the same way as the SRFIs: with -L pointing at this repo root, so (scheme string)
;; resolves to scheme/string.sld and (srfi 152) to srfi/152.sld.  Pure Scheme, no
;; native part -> byte-identical on pyScheme and cppScheme2.

(define-library (scheme string)
  (import (srfi 152))
  (export
    string? string-null? string-every string-any
    make-string string string-tabulate string-unfold string-unfold-right
    string->vector string->list vector->string list->string reverse-list->string
    string-length string-ref substring string-copy
    string-take string-drop string-take-right string-drop-right
    string-pad string-pad-right
    string-trim string-trim-right string-trim-both
    string-replace
    string=? string<? string>? string<=? string>=?
    string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
    string-prefix-length string-suffix-length string-prefix? string-suffix?
    string-index string-index-right string-skip string-skip-right
    string-contains string-contains-right
    string-take-while string-take-while-right
    string-drop-while string-drop-while-right
    string-span string-break
    string-append string-concatenate string-concatenate-reverse string-join
    string-fold string-fold-right string-map string-for-each
    string-count string-filter string-remove
    string-replicate string-segment string-split
    read-string write-string
    string-set! string-fill! string-copy!))
