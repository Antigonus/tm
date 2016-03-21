#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A subspace is a tape held as an object within a cell.  A subspace
may be entered using #'si from the cell.  Once a subspace has been entered, leftmost
and rightmost, etc. apply to the subspace (not the original tape).

|#

(in-package #:tm)

  (defun si 
    (
      tm
      &optional
      (cont-ok (be t))
      (cont-tm-mk-fail (be ∅))
      )
    "If either object is a tm, or #'tm-mk succeeds on the object, steps in.
     Otherwise cont-tm-mk-fail.
     "
    (let(
          (object (r tm))
          )
      (if
        (typep object 'tape-machine)
        (progn
          (cue-to tm object)
          (funcall cont-ok)
          )
        (tm-mk (type-of object) object
          (λ(new-tm) 
            (cue-to tm new-tm)
            (funcall cont-ok)
            )
          cont-tm-mk-fail
          ))))

  ;; when a sublist is empty, it should be represented with ∅, then ai
  ;; will exit with cont-mk-fail on the first insertion, where the programmer
  ;; can then create teh appropriate singleton sublist initialized with object.
  (defun ai
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-tm-mk-fail (be ∅)) ; OUH could not be interpretted as a tape
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    "Head is on a given cell.  That cell has an object.  The object should be either
     tm-mk-able or be ∅.  If it is ∅, we exit with cont-mk-fail where the programmer
     can then build the singleton sublist of the desired type.  If it is tm-mk-able
     then a new cell is prepended and initialized to object.
     "
    (let(
          (sublist (r tm))
          )
      (if
       (typep sublist 'tape-machine)
        (progn
          (cue-leftmost sublist)
          (-a sublist object cont-ok cont-no-alloc)
          )
        (tm-mk (type-of object) object
          (λ()
            (-a sublist object cont-ok cont-no-alloc)
            )
          cont-tm-mk-fail
          ))))

  (defun ais
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-tm-mk-fail (be ∅)) ; OUH could not be interpretted as a tape
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    "like ai, but the tape-machine is stepped into the new cell"
    (let(
          (sublist (r tm))
          )
      (if
       (typep sublist 'tape-machine)
        (progn
          (cue-leftmost sublist)
          (-a-s sublist object cont-ok cont-no-alloc)
          )
        (tm-mk (type-of object) object
          (λ()
            (-a-s sublist object cont-ok cont-no-alloc)
            )
          cont-tm-mk-fail
          ))))

  (defgeneric di (tm &optional spill cont-ok cont-rightmost cont-tm-mk-fail)
    (:documentation 
      "(r tm) is an object.  This object should be a tape machine, or an object that can
       be passed to tm-mk to get a tape machine.  This function deallocates the leftmost
       cell from that machine's tape. The deallocated cell is #'a onto spill. Should the
       user attempt to deallocate the last cell of the tape machine, then this routine
       exits via cont-rightmost, where the user can (w tm) replace the object with
       whatever empty marker is used, typically ∅.
       "
      ))

  (defmethod di 
    (
      (tm tape-machine)
      &optional
      spill
      cont-ok
      cont-rightmost
      cont-tm-mk-fail
      )
    (let(
          (object (r tm))
          )
      (if
        (typep object 'tape-machine)
        (d object spill cont-ok cont-rightmost)
        (tm-mk (type-of object) object
          (λ(new-tm) 
            (d new-tm spill cont-ok cont-rightmost)
            )
          cont-tm-mk-fail
          ))))
    
