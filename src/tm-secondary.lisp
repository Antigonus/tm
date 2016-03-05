#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:le)


;;--------------------------------------------------------------------------------
;; tape-machine duplication
;;
;;   a dangerous operation to combine with deallocation. If one machine deletes
;;   a cell that another machine's head is on, things probably go haywire afterward.
;;   adding listeners and checks is one of the todo items, but those will come
;;   at a higher price if we do them.
;;
;;   originally I thought cue-to could be generic, but we cue-to a machine with
;;   more slots than head and tape, and expect to go back again, hence we have to
;;   with within the those slots and this cue-to to not have an entropy problem.
;;
  (defun cue-to
    "Unless tm-cued type is (typep (type-of tm-orig)), tm-cued is changed to the type of tm-orig.
     tm-cued is either already on the same tape as tm-orig, or it mounts it.
     tm-cued is cued to the same cell that tm-orig's head is on.
     Returns tm-cued.
     "
    (
      (tm-cued tape-machine) ; tm-cued original contents get clobbered
      (tm-orig tape-machine) ; tm-orign remains undisturbed
      )
    (unless (typep tm-cued (type-of tm-orig)) (change-class tm-cued (type-of tm-orig)))
    (setf (tape tm-cued) (tape tm-orig))
    (setf (HA tm-cued) (HA tm-orig))
    tm-cued
    )

  (defun dup ((tm-orig tape-machine))
    "Returns a tm with head on the same cell."
    (let(
          (tm-dup (make-instance (type-of tm-orig)))
          )
      (cue-to tm-dup tm-orig)
      ))

;;--------------------------------------------------------------------------------
;; accessing data
;;

  (defgeneric ws (tm object &optional index cont-ok cont-rightmost)
    (:documentation "Writes object into the cell under the tape head, and steps tm.")
    )

  (defmethod ws
    (
      (tm tape-machine)
      object
      &optional 
      (index 1)
      (cont-ok (be t)) 
      (cont-rightmost (be ∅))
      )
    (w tm object)
    (sn tm index cont-ok cont-rightmost)
    )

  (defgeneric r-index (tm &optional index cont-ok cont-index-beyond-rightmost)
    (:documentation 
      "Read as though tm stepped index number of times before the read.
       index defaults to 1."
      ))

  ;; cont-rightmost throws an error, because anything we might return could alias 
  ;; against echo.  Chances are when using this method one will want to define
  ;; the continatuations. More specific versions might perform better.
  (defmethod r-index
    (
      (tm tape-machine)
      &optional 
      (index 1)
      (cont-ok #'echo) 
      (cont-index-beyond-rightmost
        (λ() (error 'tm-read-beyond-rightmost :text "attempt to read beyond the rightmost allocated cell of the tape") ∅)
        )
      )
    (let(
          (tm1 (dup tm))
          )
      (sn tm index
        (λ()(funcall cont-ok (r tm1)))
        (funcall cont-index-beyond-rightmost)
        )))

  (defgeneric w-index (tm object &optional index cont-ok cont-index-beyond-rightmost))

  (defmethod w-index
    (
      (tm tape-machine)
      object
      &optional 
      (index 1)
      (cont-ok (be t)) 
      (cont-index-beyond-rightmost (be ∅))
      )
    (let(
          (tm1 (dup tm))
          )
      (sn tm index
        (λ() (w tm1 object) (funcall cont-ok))
        (funcall cont-index-beyond-rightmost)
        )))

;;--------------------------------------------------------------------------------
;; cueing
;;

  (defgeneric cue-rightmost (tm)
    (:documentation "Cue tm's head to the rightmost cell.")
    )
  (defmethod cue-rightmost ((tm tape-machine)) (s* tm))


;;--------------------------------------------------------------------------------
;; These are primitives for the generic implementation of location.  They must have
;; specializations.
;;  
  (defgeneric on-leftmost (tm &optional cont-true cont-false)
    (:documentation
      "tm head is on the leftmost cell."
      ))

  (defmethod on-leftmost
    (
      tm 
      &optional 
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (let(
          (tm1 (dup tm))
          )
      (cue-leftmost tm1)
      (tms-on-same-cell tm1 tm cont-true cont-false)
      ))

  (defgeneric on-rightmost (tm &optional cont-true cont-false)
    (:documentation
      "tm head is on the rightmost cell."
      ))

  (defmethod on-rightmost
    (
      tm 
      &optional 
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (let(
          (tm1 (dup tm))
          )
      (s tm1 cont-false cont-true)
      ))

;;--------------------------------------------------------------------------------
;; stepping a single tape machine
;;
;;
  (defgeneric s≠ (tm0 tm1 &optional cont-ok cont-rightmost cont-bound)
    (:documentation "step tm0 unless it is equal to tm1")
    )

  (defmethod s≠ 
    (
      (tm0 tape-machine) 
      (tm1 tape-machine)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-bound (be ∅))
      )
    (cond
      ((tms-on-same-cell tm0 tm1) (funcall cont-bound))
      (t
        (s tm0 cont-ok cont-rightmost)
      )))

;;--------------------------------------------------------------------------------
;; allocate new cells  (add cells)
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; by a three way fill function.  The three being skip, object, tm-fill.  
;;

  (defgeneric as (tm object &optional cont-ok cont-no-alloc)
    (:documentation 
      "Like #'a, but tm is stepped to the new cell"
      ))

  (defmethod as
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (a tm object (λ()(s tm)(funcall cont-ok)) cont-no-alloc)
    )

  (defgeneric -a◧ (tm object &optional cont-ok cont-no-alloc)
    (:documentation 
      " With a contract that tm is at leftmost, inserts a cell to the left of HA.
        Like -a◧-s, but no step is taken.
      "
      ))

  (defmethod -a◧
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (let(
          (tm1 (dup tm))
          )
      (-a◧-s tm1 object (λ()(s tm)(funcall cont-ok)) cont-no-alloc)
      ))

  (defgeneric a◨ (tm object &optional cont-ok cont-no-alloc)
    (:documentation 
      "#'a with a contract that tm is at rightmost.
       Some implementatons will be able to specialize this and make it more efficient.
      "))

  (defmethod a◨
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (a tm object (λ()(s tm)(funcall cont-ok)) cont-no-alloc)
    )

  (defgeneric a◨s (tm object &optional cont-ok cont-no-alloc)
    (:documentation 
      "#'as with a contract that tm is at rightmost.
       Some implementatons will be able to specialize this and make it more efficient.
      "))

  (defmethod a◨s
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (as tm object cont-ok cont-no-alloc)
    )

;;--------------------------------------------------------------------------------
;; gather
;;
  (defgeneric gs (tm cell-reference &optional cont-ok cont-rightmost cont-no-alloc)
    (:documentation "Similar to #'g, but steps tm after the operation.")
    )

  (defmethod gs 
    (
      (tm tape-machine)
      cell-reference
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (g tm cell-reference 
      (λ()
        (s tm cont-ok cont-rightmost)
        )
      cont-no-alloc
      ))

     
;;--------------------------------------------------------------------------------
;; deallocate cells (delete cells)  
;;  deallocated cells can be moved to spill, so this can also be reallocation
;;


     
