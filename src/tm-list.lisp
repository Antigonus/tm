#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch

  Tape is implemented with a singly linked list.

|#

(in-package #:le)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-list (tape-machine)())

  (defun init-tm-list-0
    (
      instance
      &optional 
      init
      (cont-ok #'echo) 
      (cont-fail 
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized list tape type"))
        ))

    (cond
      ((¬ init) ; user ∅ or default, goes to a meta list first cell
        (let(
              (first-cell (cons 'list ∅))
              )
          (setf (tape instance) first-cell)
          (setf (HA instance) first-cell)
          (funcall cont-ok instance)
          ))

      ((typep init 'tm-list)
        (setf (tape instance) (tape init))
        (setf (HA instance) (HA init))
        (funcall cont-ok instance)
        )

      ((eq (type-of init) 'cons)
        (setf (tape instance) init)
        (setf (HA instance) init)
        (funcall cont-ok instance)
        )

      (t
        (funcall cont-fail)
        )))

  ;; This is used internally, it is forward reference friendly.
  ;; For the externally visible version, see tape-machine-mk.lisp
  ;; init supports tm init vals, but mk-tm-list does not
  ;; use cue-to to get a duplicate of another tm
  (defun mk-tm-list-0
    (
      &optional 
      init
      (cont-ok #'echo) 
      (cont-fail 
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized list tape type"))
        ))
    (let(
          (instance (make-instance 'tm-list))
          )
      (if 
        (eq (type-of init) 'tm-list) 
        (init-tm-list-0 instance ∅ cont-ok cont-fail)
        (init-tm-list-0 instance init cont-ok cont-fail)
        )))

   (defmethod to-list ((tm tm-list)) (tape tm))

;;--------------------------------------------------------------------------------
;;  tape-machine properties
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

  (defmethod tms-on-same-cell 
    (
      (tm0 tm-list) 
      (tm1 tm-list) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (if
      ;; compares pointers, can't compare objects
      ;; our boundary value calculus causes this test to be complete (without end cases)
      (eq (cdr (HA tm0)) (cdr (HA tm1))) 
      (funcall cont-true)
      (funcall cont-false)
      ))

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r ((tm tm-list)) (car (HA tm)))
  (defmethod w ((tm tm-list) object) (setf (car (HA tm)) object) t)

  (defun test-mk-tm-list-0-0 ()
    (let*(
          (tm0 (mk-tm-list-0))
          (tm1 (mk-tm-list-0 (list 7 2 -3)))
          (tm2 (mk-tm tm1))
          )
      (and
        (eq (r tm0) 'list)
        (eql (r tm1) 7)
        (eq (car (tape tm2)) 'list)
        (on-rightmost tm2)
        )))
  (test-hook test-mk-tm-list-0-0)


;;--------------------------------------------------------------------------------
;; cueing
;;
  (defmethod cue-leftmost  ((tm tm-list)) 
    (setf (HA tm) (tape tm))
    )
  
  (defmethod cue-rightmost ((tm tm-list)) (¬∀ tm (be t)) t)

  (defun test-cue-0 ()
    (let(
          (x (mk-tm-list-0 '(a b c)))
          (y (mk-tm-list-0))
          )
      (and
        (eq (r x) 'a)
        (eq (r y) 'list)
        (s x)
        (cue-to y x) 
        (eq (r x) 'b)
        (eq (r y) 'b)
        (tms-on-same-cell x y)
        (cue-rightmost x)
        (eq (r x) 'c)
        )))
  (test-hook test-cue-0)

;;--------------------------------------------------------------------------------
;; stepping
;;
  (defmacro s-work-list (tm cont-ok cont-rightmost)
    `(if
       (and
         (cdr (HA ,tm))
         (setf (HA ,tm) (cdr (HA ,tm)))
         )
       (funcall ,cont-ok)
       (funcall ,cont-rightmost)
       ))

  (defmethod s
    (
      (tm tm-list)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (s-work-list tm cont-ok cont-rightmost)
    )

  ;; this does not get specialized by the tree methods, thus makes
  ;; list stepping available to those implementations. A list step
  ;; over a list object is step over a subtree.
  (defmethod so
    (
      (tm tm-list)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (s-work-list tm cont-ok cont-rightmost)
    )

  (defun test-s-0 ()
    (let*(
           (y '(1 2 (3 4) 5))
           (ytm (mk-tm-list-0 y))
          )
      (and
        (s ytm)
        (s ytm)
        (equal '(3 4) (r ytm))
        (s ytm)
        (not (s ytm))
        )))
  (test-hook test-s-0) 

  (defmethod s≠ 
    (
      (tm0 tm-list) 
      (tm1 tm-list)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-bound (be ∅))
      )
    (cond
      ((tms-on-same-cell tm0 tm1) (funcall cont-bound))
      ((¬ (cdr (HA tm0))) (funcall cont-rightmost))
      (t
        (setf (HA tm0) (cdr (HA tm0)))
        (funcall cont-ok)
        )
      ))
    
  ; step left one -- you wish ;-)
  ;;  


;;--------------------------------------------------------------------------------
;; allocate new cells
;;
  (defmethod a ((tm tm-list) object)
    (let(
          (new-cell (cons object (cdr (HA tm))))
          )
      (rplacd (HA tm) new-cell)
      (setf (HA tm) new-cell) ; moves head right, to the new cell
      t
      ))

  (defun test-a-0 ()
    (let*(
          (tm0 (mk-tm-list-0 (list 7 9 11)))
          (tm1 (mk-tm-list-0 tm0))
          )
      (a tm0 8)
      (s tm0)
      (a tm0 10)
      (and
        (= (r tm0) 10)
        (equal (HA tm1) '(7 8 9 10 11)) ; head is not for public use
        )))

  (defmethod a. ((tm tm-list) object)
    (let(
          (new-cell (cons object (cdr (HA tm))))
          )
      (rplacd (HA tm) new-cell)
      t
      ))

  ;; programmer promises that tm is at rightmost
  (defmethod a◨ ((tm tm-list) object)
    (let(
          (new-cell (cons object ∅))
          )
      (rplacd (HA tm) new-cell)
      (setf (HA tm) new-cell) ; moves head right, to the new cell
      t
      ))

  (defmethod a◨. ((tm tm-list) object)
    (let(
          (new-cell (cons object ∅))
          )
      (rplacd (HA tm) new-cell)
      t
      ))

  (defun test-a◨-10 ()
    (let*(
           (a (list 1 2 3))
           (tm1 (mk-tm-list-0 a))
           )
      (cue-rightmost tm1)
      (a◨ tm1 '4)
      (equal
        (to-list tm1)
        '(1 2 3 4)
        )))
  (test-hook test-a◨-10)

  ;; programmer promises that tm is at leftmost
  ;; tape r/w head moves left to be on the new cell
  (defmethod -a◧ ((tm tm-list) object)
    (let(
          (new-cell (cons object ∅))
          )
      (rplacd new-cell (HA tm))
      (setf (HA tm) new-cell) 
      t
      ))

;;--------------------------------------------------------------------------------
;; gather
;;
  ;; prepends cell to spill, steps spill making the end of spill the new attachment point
  (defmethod g 
    (
      (spill tm-list)
      (cell cons)
      )
    (rplacd cell (cdr (HA spill)))
    (rplacd (HA spill) cell)
    (setf (HA spill) cell)
    )

  ;; prepends cell to spill, no step
  (defmethod g.
    (
      (spill tm-list)
      (cell cons)
      )
    (rplacd cell (cdr (HA spill)))
    (rplacd (HA spill) cell)
    )

;;--------------------------------------------------------------------------------
;; deallocating cells
;;
  ;; deallocates the cell just to the right of the head
  (defmethod d 
    (
      (tm tm-list)
      &optional 
      (spill 'd)
      (cont-ok (be t))
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      )
    (let(
          (cell-1 (cdr (HA tm))) ; cell-1 is the cell to be deallocated
          )
      (if
        cell-1
        (progn
          (rplacd (HA tm) (cdr cell-1)) ; re-route around cell-1
          (cond
            ((eq spill 'd) (funcall cont-ok))
            ((eq spill 'r) (funcall cont-ok (car cell-1)))
            ((typep spill 'tape-machine)
              (g spill cell-1)
              (funcall cont-ok)
              )
            (t
              (error
                'tm-deallocate-bad-spill-command
                :text "allowed spill commands are: 'd 'r and (typep tape-machine)"
              ))))
        ;;else there is no cell-1 to cut, no cell-2 to route to
        (funcall cont-rightmost)
        )))

  (defun test-d-0 ()
    (let*(
           (a (list 1 2 3))
           (tm1 (mk-tm-list-0 a))
           )
      (d tm1)
      (equal
        (to-list tm1)
        '(1 3)
        )))
  (test-hook test-d-0)

  ;; deallocates the leftmost cell, if the head is on leftmost, moves it to the
  ;; new leftmost
  (defmethod ◧d 
    (
      (tm tm-list)
      &optional 
      (spill 'd)
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      )

    (unless ; can't delete lm if it is the only cell
      (cdr (tape tm))
      (return-from ◧d (funcall cont-rightmost))
      )

    (when ; if head is on lm, step it
      (eq (cdr (tape tm)) (cdr (HA tm)))
      (setf (HA tm) (cdr (HA tm)))
      )

    
    (let(
          (de-lm (tape tm)) ; what we store as 'tape is in fact lm
          )
      (setf (tape tm) (cdr (tape tm))) ; this deallocates de-lm

       ;; now that de-lm has been deallocated from the tape, what to do with it?
      (cond
        ((eq spill 'd) (funcall cont-ok))
        ((eq spill 'r) (funcall cont-ok de-lm))
        ((typep spill 'tape-machine)
          (g spill de-lm)
          (funcall cont-ok)
          )
        (t
          (error
            'tm-deallocate-bad-spill-command
            :text "allowed spill commands are: 'd 'r and (typep tape-machine)"
            )))
      ))

  (defun test-◧d-0 ()
    (let*(
           (a (list 1 2 3))
           (tm1 (mk-tm-list-0 a))
           )
      (◧d tm1)
      (equal
        (to-list tm1)
        '(2 3)
        )))
  (test-hook test-◧d-0)

  (defun test-◧d-1 ()
    (let*(
           (tm1 (mk-tm-list-0))
           )
      (◧d tm1 'd (be ∅) (be t))
      ))
  (test-hook test-◧d-1)


  ;; the - behaviors need to be better explored
  ;; spilling with --d is problematic, as it calls for stepping left after the spill
  ;; though we could implement -d?
  ;; seems like we get four versions of these, dallocate from left, spill to the right
  ;;   etc.
  ;; not clear without thinking about this which, if any - versions are tractable for
  ;; for the list implementations
  #|
    (defmethod --d.  ; deallocate left, spill left
      (
        (tm tm-list)
        &optional 
        (spill ∅)
        (cont-ok (be t))
        (cont-rightmost (be ∅))
       )
     ...
     )
  |#

        
