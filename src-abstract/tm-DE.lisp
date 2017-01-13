#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  :diffs { diff0  diff1 ...}  

  diff0 is the start value

  Does not support allocation or deallocation.
  When written the head appears to be parked.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type tm-DE (tape-machine)())

  (defun-typed init 
    (
      (tm tm-DE)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (destructuring-bind
      (&key diffs &allow-other-keys) init-list
      (setf (entanglements tm) (make-entanglements tm))
      (cond
        ((¬ diffs)
          (setf (state tm) active)
          (setf (head tm) 0)
          (setf (tape tm) {1 0})
          (setf (parameters tm) {0 1})
          (funcall cont-ok)
          )
        ((consp diffs)
          (setf (state tm) active)
          (setf (head tm) (car diffs))
          (setf (tape tm) (reverse diffs))
          (setf (parameters tm) diffs)
          (funcall cont-ok)
          )
        (t
          (funcall cont-fail)
          ))))

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defun-typed r-0
    (
      (tm tm-DE)
      state
      cont-ok
      cont-parked
      )
    (declare (ignore state cont-parked))
    (funcall cont-ok (head tm))
    )

  (defun-typed w-0
    (
      (tm tm-DE)
      state
      instance
      cont-ok
      cont-parked
      )
    (declare (ignore state instance cont-ok))
    (funcall cont-parked)
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defun-typed cue-leftmost-0
    (
      (tm tm-DE)
      state
      cont-ok
      cont-void
      )
    (declare (ignore state cont-void))
    (setf (head tm) (car (parameters tm)))
    (setf (tape tm) (reverse (parameters tm)))
    (funcall cont-ok)
    )

  (defun-typed heads-on-same-cell-0
    (
      (tm0 tm-DE)
      state0
      tm1
      state1
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore state0 tm1 state1 cont-true cont-parked))
    (funcall cont-false)
    )
  (defun-typed heads-on-same-cell-0
    (
      tm0
      state0
      (tm1 tm-DE)
      state1
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore tm0 state0 state1 cont-true cont-parked))
    (funcall cont-false)
    )
  (defun-typed heads-on-same-cell-0
    (
      (tm0 tm-DE)
      state0
      (tm1 tm-DE)
      state1
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore state0 state1 cont-parked))
    (if
      (∧
        (= (head tm0) (head tm1))
        (equal (tape tm0) (tape tm1))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defun-typed s-0
    (
      (tm tm-DE)
      state
      cont-ok
      cont-rightmost
      )
    (let( 
          (diffs0 (mount (tape tm)))
          (diffs1 (mk 'tm-list))
          (sum 0)
          )
      (⟳(λ(cont-loop cont-return)
          (setf sum (+ sum (r diffs0)))
          (as diffs1 sum)
          (s diffs0 cont-loop cont-return)
          ))
      (setf (head tm) sum)
      (setf (tape tm) diffs1)
      ))

