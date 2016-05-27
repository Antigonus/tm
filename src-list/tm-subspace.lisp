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
      (cont-mount-failed (be ∅))
      )
    "If either: object is a tm, or #'mk succeeds on the object, steps in.
     Otherwise cont-mount-failed.
     "
    (r tm
      (λ(subspace)
        (if 
          (typep subspace 'tape-machine)
          (progn
            (cue-to tm subspace)
            (funcall cont-ok)
            )
          (mount subspace
            (λ(new-tm) ; should mark new-tm readonly, except we don't support that yet
              (cue-to tm new-tm)
              (funcall cont-ok)
              )
            cont-mount-failed
            )))
        cont-mount-failed
        ))

  (defun ai
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-mount-fail (be ∅)) ; object isn't a tape machine
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Allocates a new cell to the left of leftmost in the subspace.
     "
    (r tm
      (λ(subspace)
        (if 
          (typep subspace 'tape-machine)
          (a◧ subspace object cont-ok cont-no-alloc)
          cont-mount-fail
          ))
      cont-mount-fail
      ))

  (defun aisi
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-mount-fail (be ∅)) 
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "#'ai then step into the subspace"
    (ai tm object
      (λ()(cue-to tm (r tm))(funcall cont-ok))
      cont-mount-fail
      cont-no-alloc
      ))

  (defun di (
              tm 
              &optional 
              spill 
              (cont-ok #'echo)
              (cont-mount-fail (λ()(error 'mount-fail)))
              (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
              (cont-not-supported (λ()(error 'not-supported)))
              (cont-collision (λ()(error 'dealloc-entangled)))
              (cont-no-alloc (λ()(error 'alloc-fail)))
              )
    "Object is subspace. Deletes the leftmost cell of the subspace."
    (r tm
      (λ(subspace)
        (if 
          (typep subspace 'tape-machine)
          (d◧ subspace spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc)
          cont-mount-fail
          ))
      cont-mount-fail
      ))

    
