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
      #:accessed-empty
      #:access-through-parked-head
      #:box
      #:unbox
      #:remove-key-pair
      #:remove-key-pairs

    ;; do
    ;;
      #:⟳  ; repeats a block of code


;;;--------------------------------------------------------------------------------
;;; src-test
;;;     
    ;; test framework
    ;;
      #:test-hook
      #:test-remove
      #:test-all
      #:test-all-and-self
      #:*log-default-file-name*
      #:print-to-log

;;;--------------------------------------------------------------------------------
;;; src-init
;;;     
      #:init
      #:mk
      #:clone

;;;--------------------------------------------------------------------------------
;;; src-cell
;;;     

    ;; type
    ;;
      #:cell
      #:cell-virtual
      #:cell-substrate
      #:cell-alternatives
      #:context

    ;; multiple world support
    ;;
      #:wr<cell> ; uses context to return a cell among the alternatives
      #:wa<cell> ; add another world alternative to cell

    ;; queries
    ;;
      #:=<cell>
      #:r<cell>
      #:w<cell>

    ;; traversal
    ;;
      #:neighbor

    ;; topology manipulation
    ;;
      #:a<cell>
      #:a
      #:d<cell>
      #:d.<cell>
      #:d+<cell>

;;;--------------------------------------------------------------------------------
;;; src-tape
;;;     
      #:bound-left
      #:bound-right
      #:bound ;*

      #:◧r ; read from bound-left
      #:◧w

      #:◨r ; read from bound-right
      #:◨w

      #:epa<tape> ;* prepend a cell
      #:epa       ; append an instance
      #:epd<tape>  ;* delete the bound-left (causes neighbor to beomce the bound)
      #:epd+<tape>  ; delete the entire tape
      #:◧d.<tape>  ; swap contents with bound-left neighbor, delete neighbor

#|
;;;--------------------------------------------------------------------------------
;;; src-tape-machine
;;;     


    ;;quantifiers
    ;;
      #:∃
      #:-s*∃
      #:∀
      #:-s*∀
      #:∃*
      #:-s*∃*
      #:∀*
      #:-s*∀*

    ;;tm-quantified
    ;;
      #:w*
      #:s*
      #:-s*
      #:a*
      #:as*
      #:sn
      #:asn
      #:equiv

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
      #:◨a

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
      #:epa
      #:d
      #:epd

    ;;solo-tm-quantified
    ;;
      #:d*
      #:dn
      #:filter

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


    ;;
    ;; copying
      #:c ; a shallow copy
      #:c-fit ; procrustrean version


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


;;;--------------------------------------------------------------------------------
;;; src-generators
;;;     
    ;;generators
    ;;
      #:recursive ; type
      #:increment-to
      #:decrement-to
      #:mk-interval
      #:mk-Natural

;;;--------------------------------------------------------------------------------
;;; src-tr
;;;     
    ;;transforms
    ;;
      #:idenity-tr ; type
      #:ensemble-tr  ; type

;;;--------------------------------------------------------------------------------
;;; src-second-level
;;;     
    ;;second level
    ;;
      #:status-tm ; type
      #:abandoned
      #:empty
      #:parked
      #:active
      #:status-abandoned ; type
      #:status-active ; type
      #:status-empty ; type
      #:status-parked ; type

      #:p
      #:abandon
      #:p∃
      #:p∀
      #:p∃*
      #:p∀*

      #:ea-tm ; type
      #:ts1-tm ; type
|#
      ))

