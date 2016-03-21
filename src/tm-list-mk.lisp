#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines from other objects.
  Make other objects from list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making tm-list machines from other objects
;;
  (defclass tm-list (tape-machine)())

  (defun init-tm-list
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
  (defun mk-tm-list
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
        (init-tm-list instance ∅ cont-ok cont-fail)
        (init-tm-list instance init cont-ok cont-fail)
        )))

   (mk-tm-hook 'tm-list #'mk-tm-list)
   (mk-tm-hook 'cons #'mk-tm-list)
   (mk-tm-hook 'list #'mk-tm-list)


;;--------------------------------------------------------------------------------
;; making other objects from tm-list machines
;;
  (defmethod to-list ((tm tm-list))(tape tm))
  
    
