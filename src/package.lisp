#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

;;--------------------------------------------------------------------------------
;;
  (defpackage #:tm
    (:use common-lisp)
    (:use local-time)
    (:export 

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

    ;;functions
    ;;
      #:boolify
      #:do-nothing
      #:echo
      #:be
      #:notλ

    ;;mk-tm
    ;;
      #:mk-tm

    ;;tm-primitives
    ;;
      #:tape-machine ; class
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
      #:ah◨
      #:ah◨s
      #:-a
      #:-a-s

      #:d◧

      #:m  

    ;;tm-si
    ;;
      #:si
      #:ai
      #:ais
      #:di

    ;; quantifiers
    ;;
      #:⟳

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
      #:a*
      #:as*
      #:d*

      #:sn
      #:an
      #:asn
      #:dn


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

    ;; useful machines
    ;;
      #:mk-tm-void
      #:mk-tm-singular-affine
      #:mk-singular-projective

      #:tm-void
      #:tm-singular-affine
      #:tm-singular-projective

    ;; number of allocated cells, i.e. length
    ;;
      #:singleton
      #:doubleton
      #:tripleton

      #:length≥
      #:length>
      #:length<
      #:length≤
      #:length=
      #:length≠

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

    ;; tm-list-primitives
    ;;
      #:mk-tm-list
      #:tm-list

    ;; tm-list-buffer
    ;;
      #:stack-list
      #:mk-stack-list
      #:queue-list
      #:mk-queue-list

    ;; list-0
    ;;
      #:bifurcate

    ;; tree-0
    ;;
      #:bifurcate-tree

    ;;string
    ;;
      #:newline
      #:add-quotes
      #:add-escaped-quotes
      #:add-squotes
      #:add-escaped-squotes
      #:drop-ends
      #:string-∅
      #:is-prefix
      #:drop

            
    ;;list-L
    ;;
      #:o
      #:q
      #:unwrap
      #:L

    ;; test framework
    ;;
      #:test-hook
      #:test-remove
      #:test-all
      #:*log-default-file-name*
      #:print-to-log

      ))

