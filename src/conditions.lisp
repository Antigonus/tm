#|

Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; conditions
;;

  ;; the tm:o function may only be used inside of L (or [])
  (define-condition use-of-o (error)
    ((text :initarg :text :reader text)))

  ;; used for #'a and #'d on spill when allocation fails
  (define-condition tm-alloc-fail (error)
    ((text :initarg :text :reader text)))

  (define-condition tm-wrong-cell-type (error)
    ((text :initarg :text :reader text)))

  (define-condition tm-mk-bad-init-type (error)
    ((text :initarg :text :reader text)))

  ;; our tape machines can't be initialized to be nil
  ;; no operands provided will initialize to a meta cell that identifies the tape type.
  (define-condition tm-mk-∅-init-type (error)
    ((text :initarg :text :reader text)))

  ;; oops sorry, haven't written it yet marker (chances are you won't even get to see this ..)
  (define-condition tm-method-unimplemented (error)
    ((text :initarg :text :reader text)))

  ;; tape machine is read only, but someone tried to write to it..
  (define-condition tm-read-only (error)
    ((text :initarg :text :reader text)))

  ;; tape machine is write only, but someone tried to write to it..
  ;; (for example for the front end of a pipe)
  (define-condition tm-write-only (error)
    ((text :initarg :text :reader text)))


  ;; used with r-index and derivatives
  ;; the head can not be off the tape
  (define-condition tm-read-beyond-rightmost (error)
    ((text :initarg :text :reader text)))

  ;; This may be used with forms that take a count.  When possible forms should deal with
  ;; negative counts, for example a step by a negative acount is a left going step. This
  ;; should only be issued in cases where negatives just don't make sense in the given
  ;; context.
  (define-condition tm-negative-count (error)
    ((text :initarg :text :reader text)))

  ;; this is the default behavior for attempting to step into a non-sequence
  (define-condition tm-cant-si (error)
    ((text :initarg :text :reader text)))

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

  ;; When the head is on rightmost, there are not cells further right that can be
  ;; deallocated.
  (define-condition tm-deallocation-request-at-rightmost (error)
    ((text :initarg :text :reader text)))

  ;; When mapping between trees against a provided traversal order, there is a history of
  ;; traversal points, or of subtrees to visit, kept by the source, and a history of
  ;; attachment points kept by the destination.  if these are not in synch, then either
  ;; the programmer has screwed with the history buffers are tree structures between
  ;; steps, or there is a program bug.  I imagine the programmer does not want to ever see
  ;; this error.
  (define-condition tm-src-dst-history-misalignment (error)
    ((text :initarg :text :reader text)))

  ;; nothing has been allocated at this location
  (define-condition tm-no-such-location (error)
    ((text :initarg :text :reader text)))

  (define-condition binner-no-such-bin (error)
    ((text :initarg :text :reader text)))


  ;; nothing has been allocated at this location
  (define-condition worker-must-have-src-or-dst (error)
    ((text :initarg :text :reader text)))


;;--------------------------------------------------------------------------------
;; data structures
;;
  (define-condition dequeue-from-empty (error)
    ((text :initarg :text :reader text)))

