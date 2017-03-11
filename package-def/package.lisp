#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

...wanted to declare package entries in each module, but SBC spit out
errors when I made a second defpackage file with some new exports
in it.  So, I'be put exports here in sections based on the module
they belong to.

|#

  (defpackage #:tm
    (:use common-lisp)
    (:use local-time)
    (:export 

;;;--------------------------------------------------------------------------------
;;; src-0
;;;     

    ;; fundamental
    ;;
      #:defsynonym
      #:∅
      #:¬

      #:≠
      #:≥
      #:≤
      #:∧
      #:∨

      #:string≠
      #:string≤
      #:string≥

      #:λ
      #:nl
      #:defparam

    ;;list-L
    ;;  also defines reader macros for {} and []
    ;;
      #:o
      #:q
      #:unwrap
      #:L

    ;;functions
    ;;
      #:boolify
      #:do-nothing
      #:echo
      #:be
      #:notλ
      #:box
      #:unbox

;;;--------------------------------------------------------------------------------
;;; src-test
;;;     
    ;; test framework
    ;;
      #:test-hook
      #:test-remove
      #:test-all
      #:*log-default-file-name*
      #:print-to-log

;;;--------------------------------------------------------------------------------
;;; src-1
;;;     
    ;;tm-def
    ;;
      #:tape-machine ; class

    ;;tm-mk
    ;;
      #:init
      #:mk
      #:mount

    ;;tm-primitives
    ;;
      #:mk-shallow-copy
      #:r
      #:esr
      #:w
      #:esw
      #:cue-leftmost
      #:s
      #:a

    ;;tm-derived
    ;;
      #:cue-rightmost
      #:as
      #:a&h◨
      #:as&h◨

    ;;tm-quantifiers
    ;;
      #:⟳
      #:⟳-loop
      #:⟳-return
      #:⟳-work-step

      #:∃
      #:¬∀
      #:¬∃
      #:∀

      #:∃*
      #:¬∀*
      #:¬∃*
      #:∀*

    ;;nd-tm-def
    ;;
      #:nd-tape-machine ; class

    ;;nd-tm-primitives
    ;;
      #:init-entangled
      #:heads-on-same-cell

    ;;nd-tm-derived
    ;;
      #:mk-entangled
      #:recycle-entangled
      #:r◧
      #:w◧
      #:s≠
      #:on-leftmost
      #:on-rightmost
      #:a◨

    ;;nd-tm-quantifiers
    ;;
      #:s-together
      #:esnr
      #:esnw

    ;;nd-tm-quantified
    ;;
      #:eas*
      #:an

    ;;solo-tm-def
    ;;
      #:solo-tape-machine ; class
     
    ;;solo-tm-primitives
    ;;
      #:a◧
      #:d
      #:d◧

    ;;solo-tm-primitives
    ;;
      #:d*
      #:d◧*
      #:dn

    ;;ea-tm-def
    ;;
      #:ea-tape-machine
      
    ;;ea-tm-primitives
    ;;
    ;; adds no new generic functions

    ;;ea-tm-derived
    ;;
    ;; adds no new generic functions

    ;; location
    ;;
      #:on+1
      #:on+n
      #:on-rightmost-1
      #:on-rightmost+n ; n will typically be negative
      #:address
      #:distance+1
      #:distance+n
      #:distance

      #:location-cmp
      #:location≥
      #:location>
      #:location<
      #:location≤
      #:location=
      #:location≠

    ;; number of allocated cells, i.e. length
    ;;
      #:singleton
      #:doubleton
      #:tripleton
      #:length-cmp

      #:length≥
      #:length>
      #:length<
      #:length≤
      #:length=
      #:length≠

      #:length-of


    ;; dataflow
    ;;
      #:synch
      
;;;--------------------------------------------------------------------------------
;;; src-2     
;;

    ;; arrays
      #:tm-array
      #:tm-array-adj
      #:tm-aggregate

    ;; worker
    ;;  
      #:def-worker

    ;; worker-utilities
    ;;
      #:binner  

    ;; list-lang
    ;;
      #:Δ

      ))

