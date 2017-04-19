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
      #:def-type
      #:def-function-class
      #:defun-typed
      #:nl

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
      #:cant-happen
      #:alloc-fail
      #:not-implemented
      #:operation-on-abandoned
      #:use-of-empty
      #:access-through-parked-head
      #:box
      #:unbox
      #:remove-key-pair
      #:remove-key-pairs

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
;;; src-1ist
;;;     
    ;;tm-type
    ;;
      #:tape-machine ; type
      #:init
      #:mk

    ;;tm-decls
    ;;
      #:r
      #:esr
      #:w
      #:esw
      #:ec◧r
      #:ec◧sr
      #:ec◧w
      #:ec◧sw
      #:c◧
      #:s
      #:a
      #:on-leftmost
      #:on-rightmost
      #:tape-length-is-one
      #:tape-length-is-two

    ;;tm-generic
    ;;
      #:c◨
      #:as
      #:a&h◨
      #:as&h◨

    ;;quantifiers
    ;;
      #:⟳
      #:always-true
      #:always-false

      #:∃
      #:c◧∃
      #:∀
      #:c◧∀
      #:∃*
      #:c◧∃*
      #:∀*
      #:c◧∀*

    ;;tm-quantified
    ;;
      #:w*
      #:s*
      #:-s*
      #:a*
      #:as*
      #:sn
      #:asn

    ;;nd-tm-type
    ;;
      #:nd-tape-machine ; type
      #:entangle

    ;;nd-tm-decls
    ;;
      #:entangled ; a predicate
      #:with-entangled
      #:s≠
      #:heads-on-same-cell
      #:a◨

    ;;nd-tm-generic
    ;;
      #:tm-print

    ;;nd-tm-quantified
    ;;
      #:esnr
      #:esnw
      #:eas*
      #:an

    ;;solo-tm-type
    ;;
      #:solo-tape-machine ; type

    ;;solo-tm-decls
    ;;
      #:a◧
      #:d
      #:d◧

    ;;solo-tm-quantified
    ;;
      #:d*
      #:d◧*
      #:dn

    ;;haz-tm-type
    ;;
      #:haz-tape-machine ; type
      
    ;;haz-tm-decls
    ;;
      #:d.
      
    ;;bi-tm-decls
    ;;
      #:-s
      #:-a
      #:-d

    ;;list implementations
    ;;
      #:list-tm
      #:list-nd-tm
      #:list-solo-tm
      #:list-haz-tm

      #:bilist-tm
      #:bilist-nd-tm
      #:bilist-solo-tm
      #:bilist-haz-tm

    ;;generators
    ;;
      #:recursive ; type
      #:increment-to
      #:decrement-to
      #:mk-interval
      #:mk-Natural

    ;;transforms
    ;;
      #:idenity-tr ; type
      #:ensemble-tr  ; type

    ;;second level
    ;;
      #:status-tm ; type
      #:park
      #:abandon
      
      #:ea-tm ; type
      #:ts1-tm ; type

      ))

