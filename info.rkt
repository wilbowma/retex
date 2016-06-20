#lang info
(define collection 'multi)
(define deps '("base" ("redex-lib" #:version "1.11") "typeset-rewriter" "scribble-lib"
               "redex-pict-lib" "pict" "scribble-latex-utils"))
(define build-deps '())
(define pkg-desc "abstractions for LaTeX-y looking Redex.")
(define version "0.1")
(define pkg-authors '(wilbowma))
