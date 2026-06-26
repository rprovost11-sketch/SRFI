# SRFI libraries for pyScheme & cppScheme2

Shared SRFI implementations for the two mirror-port R7RS interpreters
([pyScheme] and [cppScheme2]). The design goal is **maximum sharing with
per-port native parts where unavoidable**.

## Layout & the shared-`.scm` + per-port-native model

Each SRFI lives under `srfi/` and may be made of up to three colocated files:

```
srfi/
  64.sld          ; portable R7RS Scheme  -- SHARED, both ports read it
  <n>.sld         ; portable part of SRFI <n>           (shared)
  <n>.py          ; pyScheme native part of SRFI <n>    (pyScheme only)
  <n>.dll         ; cppScheme2 native part of SRFI <n>  (cppScheme2 only)
```

Both interpreters resolve `(import (srfi <n>))` to `srfi/<n>.*` under a
library-path entry, and **each loads the shared `.sld` plus its own native
file, ignoring the other port's**:

- **pyScheme** loads `srfi/<n>.py` (if present) then `srfi/<n>.sld`. The `.py`
  module exposes a `register(env)` entry that installs native primitives.
- **cppScheme2** loads `srfi/<n>.dll` (if present) then `srfi/<n>.sld`. The
  `.dll` is a plugin exporting a register entry that installs native primitives
  (requires a cppScheme2 build with native-library import).

So the *portable* logic is written once in the `.sld` (parity by
construction); only the irreducibly-native primitives differ per port, and they
sit right next to the shared code. A pure-Scheme SRFI needs only a `.sld` and
works on both ports immediately.

## Using it

Add this repo's root to the interpreter library path:

    cppscheme2 -L /path/to/SRFI  program.scm
    python -m pyscheme -L /path/to/SRFI  program.scm
    # or: SCHEME_LIBRARY_PATH=/path/to/SRFI

`(import (srfi <n>))` then resolves to `srfi/<n>.*`. Keep the `srfi/`
subdirectory name lowercase so resolution works on case-sensitive filesystems.

## Contents

All of the following are pure R7RS (`.sld` only, no native part), so a single
file serves both ports byte-identically.

- **`srfi/64.sld`** — a minimal, portable SRFI-64 test harness (plus
  `(chibi test)`'s `(test expected expr)` form), enough to run chibi's
  `r7rs-tests.scm` on both ports. This is what the scheme-tests chibi survey
  imports via `-L`.
- **`srfi/28.sld`** — Basic Format Strings (`~a ~s ~% ~~`).
- **`srfi/48.sld`** — Intermediate Format Strings (a superset of 28: `~c ~w ~y`,
  radix/width directives, fixed-point `~f`, indirection `~? ~k`, destinations).
- **`srfi/166.sld`** — Monadic Formatting (the `show` combinator library), a
  practical core: `show`, the `displayed`/`written`/`numeric` formatters, the
  `joined*`/`padded*`/`trimmed*`/`fitted*` combinators, `upcased`/`downcased`,
  and the `with`/`with!`/`fn`/`forked`/`call-with-output` state machinery with
  state variables (`radix`, `precision`, `pad-char`, `ellipsis`, `col`, ...).
  The columnar/wrapped/Unicode-width extensions are deferred (see the file head).
- **`srfi/19.sld`** — Time/Date data types and procedures.
- **`srfi/152.sld`** — String Library (reduced), the COMPLETE procedure set.
  Criteria are predicates only (no SRFI-14 char-sets); pass a one-argument char
  predicate where a criterion is expected.
- **`srfi/13.sld`** — String Library (the older SRFI 13) as a compatibility shim
  over `(srfi 152)`: re-exports the shared procedures and adds the SRFI-13-only
  ones (`string-reverse`, `string-titlecase`, `string-tokenize`, `xsubstring`,
  ...). Predicates-only criteria, like 152.
- **`scheme/string.sld`** — the R7RS-large (Red Edition) `(scheme string)`
  library, a thin alias re-exporting all of `(srfi 152)` under the standard
  name. (Note: this one lives under `scheme/`, not `srfi/`, so `(import (scheme
  string))` resolves to `scheme/string.sld` under the same `-L` root.)

[pyScheme]: ../3PyScheme
[cppScheme2]: ../4CPPScheme2
