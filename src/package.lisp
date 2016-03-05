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

    ;; test
    ;;
      #:test-hook
      #:test-remove
      #:test-all
      #:*log-default-file-name*
      #:print-to-log

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

    ;;tm-primary
    ;;
      #:tape-machine ; class
      #:r
      #:w
      #:cue-leftmost
      #:cue-to
      #:tms-on-same-cell
      #:so
      #:a
      #:-a◧-s
      #:d
      #:◧d
      #:m  

    ;;tm-secondary
    ;;
      #:dup
      #:ws
      #:r-index
      #:w-index

      #:cue-rightmost

      #:on-leftmost
      #:on-rightmost

      #:s
      #:s≠

      #:as
      #:a◨
      #:a◨s

      #:gs

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
      #:on-rightnost+n ; no will typically be negative
      #:address
      #:distance-1
      #:distance-n
      #:location-cmp

      #:location≥
      #:location>
      #:location<
      #:location≤
      #:location=
      #:location≠

    ;; useful machines
    ;;
      #:tm-void
      #:tm-singular-affine
      #:tm-singular-projective

      #:mk-tm-void
      #:mk-tm-singular-affine
      #:mk-singular-projective

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
      #:embedded-stack-dequeue
      #:embedded-empty

      #:queue-enqueue
      #:queue-dequeue
      #:embedded-queue-dequeue

      #:buffer
      #:enqueue
      #:dequeue
      #:empty

      #:stack
      #:queue

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

      ))

