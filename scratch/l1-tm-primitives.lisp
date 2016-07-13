#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A machine is defined by giving definitions to these primitives.  Entanglement is
possible with non-destructive machine, but not with solo machines, hence recycle-entangled-with
and mk-entangled-with are defined with non-destructive machines.  Destructive operations
are possible on solo machines, so they are defined there.


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defun r
    (
      tm
      &optional
      (cont-ok #'echo)
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "Given a tape machine, returns the object from the cell under the tape head."
    (r-0 tm (state tm) cont-ok cont-parked)
    )
  (defgeneric r-0 (tm state cont-ok cont-parked))
  (defmethod r-0 ((tm tape-machine) (state void) cont-ok cont-parked)
    (declare (ignore tm cont-ok))
    (funcall cont-parked)
    )
  (defmethod r-0 ((tm tape-machine) (state parked) cont-ok cont-parked)
    (declare (ignore tm cont-ok))
    (funcall cont-parked)
    )
  (defmethod r-0 ((tm tape-machine) (state abandoned) cont-ok cont-parked)
    (declare (ignore tm cont-ok cont-parked))
    (error 'operation-on-abandoned)
    )

  (defun w 
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "Writes object into the cell under the tape head."
    (w-0 tm (state tm) object cont-ok cont-parked)
    )
  (defgeneric w-0 (tm state object cont-ok cont-parked))
  (defmethod w-0 ((tm tape-machine) (state void) object cont-ok cont-parked)
    (declare (ignore tm state object cont-ok))
    (funcall cont-parked)
    )
  (defmethod w-0 ((tm tape-machine) (state parked) object cont-ok cont-parked)
    (declare (ignore tm state object cont-ok))
    (funcall cont-parked)
    )
  (defmethod w-0 ((tm tape-machine) (state abandoned) object cont-ok cont-parked)
    (declare (ignore tm object cont-ok cont-parked))
    (error 'operation-on-abandoned)
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defun cue-leftmost
    (
      tm
      &optional
      (cont-ok (be t))
      (cont-void (λ()(error 'access-void)))
      )
    "Cue tm's head to the leftmost cell."
    (cue-leftmost-0 tm (state tm) cont-ok cont-void)
    )
  (defgeneric cue-leftmost-0 (tm state cont-ok cont-void))
  (defmethod cue-leftmost-0 ((tm tape-machine) (state void) cont-ok cont-void)
    (declare (ignore tm state cont-ok))
    (funcall cont-void)
    )
  (defmethod cue-leftmost-0 ((tm tape-machine) (state abandoned) cont-ok cont-void)
    (declare (ignore tm cont-ok cont-void))
    (error 'operation-on-abandoned)
    )

;;--------------------------------------------------------------------------------
;; head location
;;
  (defun heads-on-same-cell 
    (
      tm0 
      tm1
      &optional 
      (cont-true (be t))
      (cont-false (be ∅))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "tm0 and tm1 heads are on the same cell"
    (heads-on-same-cell-0 tm0 (state tm0) tm1 (state tm1) cont-true cont-false cont-parked)
    )
  (defgeneric heads-on-same-cell-0 (tm0 state0 tm1 state1 cont-true cont-false cont-parked))

  (defmethod heads-on-same-cell-0
    (
      (tm0 tape-machine)
      (state0 abandoned)
      (tm1 tape-machine)
      state1 
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore tm0 state1 cont-true cont-false cont-parked))
    (error 'operation-on-abandoned)
    )
  (defmethod heads-on-same-cell-0
    (
      (tm0 tape-machine)
      state0
      (tm1 tape-machine)
      (state1 abandoned)
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore tm0 state0 cont-true cont-false cont-parked))
    (error 'operation-on-abandoned)
    )

  (defmethod heads-on-same-cell-0
    (
      (tm0 tape-machine)
      (state0 void)
      (tm1 tape-machine)
      (state1 void) 
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore tm0 cont-true cont-false))
    (funcall cont-parked)
    )
  (defmethod heads-on-same-cell-0
    (
      (tm0 tape-machine)
      (state0 void)
      (tm1 tape-machine)
      (state1 parked)
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore tm0 cont-true cont-false))
    (funcall cont-parked)
    )
  (defmethod heads-on-same-cell-0 
    (
      (tm0 tape-machine)
      (state0 void) 
      (tm1 tape-machine)
      (state1 active)
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore tm0 cont-true cont-parked))
    (funcall cont-false)
    )

  (defmethod heads-on-same-cell-0
    (
      (tm0 tape-machine)
      (state0 parked)
      (tm1 tape-machine)
      (state1 void) 
      cont-true
      cont-false 
      cont-parked
      )
    (declare (ignore tm0 cont-true cont-false))
    (funcall cont-parked)
    )
  (defmethod heads-on-same-cell-0
    (
      (tm0 tape-machine)
      (state0 parked) 
      (tm1 tape-machine)
      (state1 parked) 
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore tm0 cont-true cont-false))
    (funcall cont-parked)
    )
  (defmethod heads-on-same-cell-0 
    (
      (tm0 tape-machine)
      (state0 parked)
      (tm1 tape-machine)
      (state1 active)
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore tm0 cont-true cont-parked))
    (funcall cont-false)
    )

  (defmethod heads-on-same-cell-0
    (
      (tm0 tape-machine)
      (state0 active)
      (tm1 tape-machine)
      (state1 void) 
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore tm0 cont-true cont-parked))
    (funcall cont-false)
    )
  (defmethod heads-on-same-cell-0
    (
      (tm0 tape-machine)
      (state0 active)
      (tm1 tape-machine)
      (state1 parked) 
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore tm0 cont-true cont-parked))
    (funcall cont-false)
    )

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defun s
    (
      tm
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    "If the head is on a cell, and there is a right neighbor, puts the head on the
     right neighbor and cont-ok.  If there is no right neighbor, then cont-rightmost.
     "
    (s-0 tm (state tm) cont-ok cont-rightmost)
    )
  (defgeneric s-0 (tm state cont-ok cont-rightmost))
  (defmethod s-0 ((tm tape-machine) (state void) cont-ok cont-rightmost)
    (declare (ignore tm state cont-ok))
    (funcall cont-rightmost) ; derived from limiting behavior when in parked state
    )
  (defmethod s-0 ((tm tape-machine) (state parked) cont-ok cont-rightmost)
    (declare (ignore cont-rightmost)); parked machine has at least 1 cell
    (cue-leftmost-0 tm state cont-ok #'cant-happen) 
    )
  (defmethod s-0 ((tm tape-machine) (state abandoned) cont-ok cont-rightmost)
    (declare (ignore tm cont-ok cont-rightmost)); parked machine has at least 1 cell
    (error 'operation-on-abandoned)
    )

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun a
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "If no cells are available, cont-no-alloc.  Otherwise, allocate a new cell and place
     it to the right of the cell the head is currently on.  The newly allocated cell will
     be initialized with the given object.
     "
    (a-0 tm (state tm) object cont-ok cont-no-alloc)
    )

  (defgeneric a-0 (tm state object cont-ok cont-no-alloc))

  (defmethod a-0 ((tm tape-machine) (state abandoned) object cont-ok cont-no-alloc)
    (declare (ignore tm object cont-ok cont-not-supported cont-no-alloc))
    (error 'operation-on-abandoned)
    )

  ;; void and parked state cases purposely left undefined for a-0, as nd-tm-primitives
  ;; does not support a◧
