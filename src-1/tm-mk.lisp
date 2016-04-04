#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  A new tape machine may be made by calling tm-mk.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; the base type
;;
  (defclass tape-machine ()
    (
      (HA 
        :initarg :HA 
        :accessor HA
        )
      (tape
        :initarg :tape
        :accessor tape
        )
      ))

;;--------------------------------------------------------------------------------
;; make a tape machine of the specified type and init value
;;

  ;; all tape machines should provide an implementation for this:
  ;; (then tm-mk will work)
  ;;
    (defgeneric tm-init (instance init-list))

    ;; if no specialization on instance is found, then:
    (defmethod tm-init (instance init-list)
      (declare (ignore instance init-list))
      (error 'tm-mk-unrecognized-instance-type)
      )

  (defun tm-mk (tm-type &rest init-list)
    (tm-init (make-instance tm-type) init-list)
    )

;;--------------------------------------------------------------------------------
;;  given a sequence return a tape machine over that sequence
;;
;;
   (defgeneric mount (sequence &optional cont-ok cont-fail))

   (defmethod mount
     (
       sequence 
       &optional 
       cont-ok 
       (cont-fail (error 'mount-unrecognized-sequence-type))
       )
     (declare (ignore sequence cont-ok))
     (funcall cont-fail)
     )

