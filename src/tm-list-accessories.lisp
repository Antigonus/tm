#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tape is implemented with a singly linked list.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; accessing data
;;

;;--------------------------------------------------------------------------------
;; absolute head placement
;;

;;--------------------------------------------------------------------------------
;;  head location predicates
;;

  (defmethod on-rightmost
    (
      (tm0 tm-list)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (if
      (cdr (HA tm0))
      (funcall cont-false)
      (funcall cont-true)
      ))

  (defmethod on-leftmost
    (
      (tm0 tm-list)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (if
      (eq (cdr (tape tm0)) (cdr (HA tm0)))
      (funcall cont-true)
      (funcall cont-false)
      ))

;;--------------------------------------------------------------------------------
;; head stepping
;;


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defmethod as
    (
      (tm tm-list)
      object 
      &optional
      (cont-ok (be t))
      cont-no-alloc
      )
    (declare (ignore cont-no-alloc)) ;; should do something with this ..
    (let(
          (new-cell (cons object (cdr (HA tm))))
          )
      (rplacd (HA tm) new-cell)
      (setf (HA tm) (cdr (HA tm)))
      (funcall cont-ok)
      ))

  (defun test-as-0 ()
    (let*(
          (tm0 (mk-tm-list (list 7 9 11)))
          (tm1 (mk-tm-list tm0))
          )
      (as tm0 8)
      (s tm0)
      (as tm0 10)
      (and
        (= (r tm0) 10)
        (equal (HA tm1) '(7 8 9 10 11)) ; head is not for public use
        )))

;;--------------------------------------------------------------------------------
;; deallocating cells
;;

  (defun test-d◧-0 ()
    (let*(
           (a (list 1 2 3))
           (tm1 (mk-tm-list a))
           )
      (d◧ tm1)
      (equal
        (tape tm1)
        '(2 3)
        )))
  (test-hook test-d◧-0)

  (defun test-d◧-1 ()
    (let*(
           (tm1 (mk-tm-list))
           )
      (d◧ tm1 'd (be ∅) (be t))
      ))
  (test-hook test-d◧-1)



        
