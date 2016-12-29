#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  note (adjustable-array-p a) for checking if an array is adjustable
  this may be needed as (type-of an-array) just returns (vector T size)
  both are also (typep a 'array).

  (array dimension d 0) returns allocation length for 0 dimension of array d, while
  (length d) returns the fill pointer for vector d, as does (fill-pointer d).

.. the new tm-mk approach makes the tm-init initialized from sequence code look a bit
funny .. also probably should remove the initialization of tm-array-adj so as not to
duplicate computation when tape is set explicitly.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-array-adj (tape-machine)())

;;--------------------------------------------------------------------------------
;;
  (defmethod tm-init ((instance tm-array-adj) init-list)
    (if (¬ init-list) 

      (let(
            (an-array (make-array 1 :fill-pointer 1 :adjustable t))
            )
        (setf (aref an-array 0) 'array)
        (setf (tape instance) an-array)
        (setf (head instance) 0)
        instance
        )

      (let(
            (init (car init-list))
            )
        (cond
          ((∧ 
             (¬ (cdr init-list)) 
             (arrayp init)
             (adjustable-array-p init)
             )
            (setf (tape instance) init)
            (setf (head instance) 0)
            instance
            )

          ((∧ 
             (¬ (cdr init-list)) 
             (arrayp init)
             (¬ (adjustable-array-p init))
             )
            (setf (tape instance)
              (make-array 
                (length init)
                :fill-pointer (length init)
                :adjustable t
                :initial-contents init
                ))
            (setf (head instance) 0)
            instance
            )

          (t 
            (error 'tm-mk-bad-init-type)
            )))))


  ;; need to share this between adjustable and fixed arrays,
  ;; as both will come through (sequence 'array)
  ;;
    (defmethod mount
      (
        (sequence array) 
        &optional
        (cont-ok #'echo)
        (cont-fail 
          (λ() (error 'tm-mk-init-failed :text "unrecognized adjustable array tape type"))
          ))
      (declare (ignore cont-fail))
      (funcall cont-ok 
        (if 
          (adjustable-array-p sequence) 
          (make-instance 'tm-array-adj :tape sequence :head 0)
          (make-instance 'tm-array :tape sequence :head 0)
          )))

  
