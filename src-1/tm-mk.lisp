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
    (defgeneric tm-init (instance &optional init-value cont-ok cont-fail))

    ;; if no specialization on instance is found, then:
    (defmethod tm-init (instance &optional init-value cont-ok cont-fail)
      (declare (ignore instance init-value cont-ok))
      (funcall cont-fail)
      )

  (defun tm-mk 
    (
      tm-type ; this must be valid, if not, make-instance will fail (no continuation)
      &optional 
      init-value 
      (cont-ok #'echo)
      (cont-fail (λ()(error 'tm-mk-init-failed)))
      )
    (let(
          (i (make-instance tm-type))
          )
       (tm-init i init-value cont-ok cont-fail)
       ))

;;--------------------------------------------------------------------------------
;;  given a sequence return a tape machine over that sequence
;;
;;  differs from tm-mk in that tm-mk may take non-sequence init values, and may
;;  convert the init value into other types of sequences.
;;
   (defgeneric mount (sequence &optional cont-ok cont-fail))

   (defmethod mount (sequence &optional cont-ok cont-fail)
     (declare (ignore sequence cont-ok))
     (funcall cont-fail)
     )


