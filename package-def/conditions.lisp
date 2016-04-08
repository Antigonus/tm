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
  (define-condition tm-impossible-to-get-here (error)
    ((text :initarg :text :reader text)))

  ;; This warning is issued if one performs a tape machine action which is computationally
  ;; difficult, such as stepping left on a single linked list tape.
  (define-condition tm-computationally-difficult (warning)
    ((text :initarg :text :reader text)))

  ;; The underlying tape implementation precludes this action.  Before we had a single
  ;; linked list implementation that did not keep a leftmost pointer, hence many left
  ;; going actions were not just computationally difficult, they were impossible.
  (define-condition tm-computationally-impossible (error)
    ((text :initarg :text :reader text)))

  ;; request to step into subspace, but no subsace was present
  (define-condition tm-mount-failed (error)
    ((text :initarg :text :reader text)))



;;--------------------------------------------------------------------------------
;; list-L
;;
  ;; the tm:o function may only be used inside of L (or [])
  (define-condition use-of-o (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; tm-mk
;;
  (define-condition tm-mk-bad-init-type (error)
    ((text :initarg :text :reader text)))

  (define-condition tm-init-unrecognized-instance-type (error)
    ((text :initarg :text :reader text)))

  (define-condition mount-unrecognized-sequence-type (error)
    ((text :initarg :text :reader text)))

  ;; sometimes the type of the tm matters
  (define-condition wront-tm-type (error)
    ((text :initarg :text :reader text)))
  

;;--------------------------------------------------------------------------------
;; tm-primitives
;;

  ;; tape machine is read only, but someone tried to write to it..
  (define-condition tm-read-only (error)
    ((text :initarg :text :reader text)))

  ;; tape machine is write only, but someone tried to write to it..
  ;; (for example for the front end of a pipe)
  (define-condition tm-write-only (error)
    ((text :initarg :text :reader text)))

  ;; used for #'a and #'d on spill when allocation fails
  (define-condition tm-alloc-fail (error)
    ((text :initarg :text :reader text)))

  ;; When the head is on rightmost, there are not cells further right that can be
  ;; deallocated.
  (define-condition tm-deallocation-request-at-rightmost (error)
    ((text :initarg :text :reader text)))


;;--------------------------------------------------------------------------------
;; tm-derived
;;
  ;; used with r-index and derivatives
  ;; the head can not be off the tape
  (define-condition tm-read-beyond-rightmost (error)
    ((text :initarg :text :reader text)))

  ;; this is the default behavior for attempting to step into a non-sequence
  (define-condition tm-cant-si (error)
    ((text :initarg :text :reader text)))

  ;; nothing has been allocated at this location
  (define-condition tm-no-such-location (error)
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
  
