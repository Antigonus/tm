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
    ;; parking
    ;;
      #:parked
      #:not-parked
      #:has-tape
      #:park

    ;;tm-mk
    ;;
      #:tape-machine ; class
      #:init
      #:mk
      #:mount

    ;;tm-primitives
    ;;
      #:r
      #:w
      #:heads-on-same-cell
      #:so
      #:s
      #:a
      #:d

    ;;tm-derived - more than just a primitive interface
    ;;
      #:cue-to
      #:dup

      #:ws
      #:r-index
      #:w-index

      #:cue-rightmost
      #:cue-leftmost

      #:on-leftmost
      #:on-rightmost

      #:s≠ 

      #:a◧
      #:as
      #:a&h◨
      #:a&h◨s
      #:-a
      #:-a-s

      #:d◧

      #:m  

    ;; quantifiers
    ;;
      #:⟳
      #:⟳-work-step

      #:∃
      #:¬∀
      #:¬∃
      #:∀

      #:∃*
      #:¬∀*
      #:¬∃*
      #:∀*

      #:s-together
      
      #:w*
      #:s*
      #:-s*
      #:a*
      #:as*
      #:d*

      #:sn
      #:an
      #:asn
      #:dn

    ;;tm-subspace
    ;;
      #:si
      #:ai
      #:ais
      #:di

    ;;tm-convert
    ;;
      #:unmount
      #:to-list
      #:to-array-adj
      #:to-array

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

    ;; data-structures
    ;;
      #:stack-enqueue
      #:stack-dequeue

      #:queue-enqueue
      #:queue-dequeue

      #:buffer
      #:enqueue
      #:dequeue
      #:empty

      #:stack
      #:queue

    ;; fundamental machines
    ;;
      #:tm-void
      #:tm-singular-affine
      #:tm-singular-projective

      #:tm-interval

      #:tm-mk-void
      #:tm-mk-singular-affine
      #:mk-singular-projective
      #:tm-mk-interval

    ;; tm-line
    ;;
      #:tm-line

    ;; tm-list-primitives
    ;;
      #:tm-list
      #:tm-mk-list

    ;; buffer
    ;;
      #:stack-list
      #:mk-stack-list
      #:queue-list
      #:mk-queue-list

    ;; tree
    ;;  
      #:tm-tree

      #:tm-depth ;class
      #:tm-depth-list ;class
      #:s-depth-ru
      #:s-depth ; not yet written

      #:tm-breadth ;class
      #:tm-breadth-list ;class
      #:s-breadth

      #:tm-transform

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

