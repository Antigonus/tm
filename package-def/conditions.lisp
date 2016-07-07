#|

Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; fundamental
;;
  ;; Sometimes the program logic assures that a continuation can not be reached.  In such
  ;; a case this condition should be raised.  Reasons for reaching such conditions include
  ;; design mistakes, and program bugs either in the code, or due to unconsidered
  ;; interactions between threads.
  (define-condition impossible-to-get-here (error)
    ((text :initarg :text :reader text)))

  ;; This warning is issued if one performs a tape machine action which is computationally
  ;; difficult, such as stepping left on a single linked list tape.
  (define-condition computationally-difficult (warning)
    ((text :initarg :text :reader text)))

  ;; The underlying tape implementation precludes this action.  Before we had a single
  ;; linked list implementation that did not keep a leftmost pointer, hence many left
  ;; going actions were not just computationally difficult, they were impossible.
  (define-condition computationally-impossible (error)
    ((text :initarg :text :reader text)))

  ;; request to step into subspace, but no subsace was present
  (define-condition mount-failed (error)
    ((text :initarg :text :reader text)))



;;--------------------------------------------------------------------------------
;; list-L
;;
  ;; the tm:o function may only be used inside of L (or [])
  (define-condition use-of-o (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; mk
;;
  (define-condition bad-init-value (error)
    ((text :initarg :text :reader text)))

  (define-condition unrecognized-instance-type (error)
    ((text :initarg :text :reader text)))

  (define-condition unrecognized-sequence-type (error)
    ((text :initarg :text :reader text)))

  ;; sometimes the type of the tm matters
  (define-condition wrong-tm-type (error)
    ((text :initarg :text :reader text)))
  
  (define-condition mount-fail (error)
    ((text :initarg :text :reader text)))

  (define-condition missing-tm-type (error)
    ((text :initarg :text :reader text)))


;;--------------------------------------------------------------------------------
;; tm-primitives
;;

  ;; tape machine is read only, but someone tried to write to it..
  (define-condition operation-on-abandoned (error)
    ((text :initarg :text :reader text)))

  (define-condition attempt-to-copy-solo-machine (error)
    ((text :initarg :text :reader text)))

  (define-condition destructive-op-on-nd-machine (error)
    ((text :initarg :text :reader text)))
  

  ;; tape machine is read only, but someone tried to write to it..
  (define-condition tm-read-only (error)
    ((text :initarg :text :reader text)))

  ;; tape machine is write only, but someone tried to write to it..
  ;; (for example for the front end of a pipe)
  (define-condition tm-write-only (error)
    ((text :initarg :text :reader text)))

  ;; can't read or write to this machine
  (define-condition parked-head-use (error)
    ((text :initarg :text :reader text)))

  (define-condition step-from-rightmost (error)
    ((text :initarg :text :reader text)))

  ;; used for #'a and #'d on spill when allocation fails
  (define-condition alloc-fail (error)
    ((text :initarg :text :reader text)))

  (define-condition spill-not-supported (error)
    ((text :initarg :text :reader text)))

  ;; alloc and dealloc on arrays, as examples
  (define-condition not-supported (error)
    ((text :initarg :text :reader text)))

  (define-condition dealloc-on-void (error)
    ((text :initarg :text :reader text)))

  (define-condition access-void (error)
    ((text :initarg :text :reader text)))

  (define-condition dealloc-on-rightmost (error)
    ((text :initarg :text :reader text)))

  (define-condition dealloc-entangled (error)
    ((text :initarg :text :reader text)))

  (define-condition malformed-entanglements (error)
    ((text :initarg :text :reader text)))
 

;;--------------------------------------------------------------------------------
;; tm-derived
;;
  ;; used with r-index and derivatives
  ;; the head can not be off the tape
  (define-condition read-beyond-rightmost (error)
    ((text :initarg :text :reader text)))

  ;; this is the default behavior for attempting to step into a non-sequence
  (define-condition cant-si (error)
    ((text :initarg :text :reader text)))

  ;; nothing has been allocated at this location
  (define-condition no-such-location (error)
    ((text :initarg :text :reader text)))


;;--------------------------------------------------------------------------------
;; dataflow
;;
  (define-condition not-ready (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; buffers
;;
  (define-condition dequeue-from-empty (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; worker
;;
  ;; nothing has been allocated at this location
  (define-condition worker-must-have-src-or-dst (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; worker-utilities
;;
  (define-condition binner-no-such-bin (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; tm-aggregate
;;
  ;; nothing has been allocated at this location
  (define-condition object-not-tape-machine (error)
    ((text :initarg :text :reader text)))



;;--------------------------------------------------------------------------------
;; access lang
;;
  (define-condition Δ-malformed-access-program (error)
    ((text :initarg :text :reader text)))

  (define-condition Δ-unrecognized-command (error)
    ((text :initarg :text :reader text)))

  (define-condition Δ-required-arg-missing (error)
    ((text :initarg :text :reader text)))
  
