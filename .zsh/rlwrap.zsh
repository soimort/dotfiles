#!/usr/bin/env zsh

function {
    for x in $*; do
        alias $x="rlwrap $x"
    done
} \
         tclsh \
         gosh guile kawa qexo scheme \
         racket \
         acl2 ccl ccl64 cmucl ecl lisp sbcl \
         mosml sml \
         ocaml \
         maxima prover9
