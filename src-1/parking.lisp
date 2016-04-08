#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; head parking - moving the head into and out of the address space
;;
  (defun is-parked (tm)
    "true if tape machine head is parked"
    (eq (HA tm) 'parked)
    )

  (defun has-tape (tm)
    "true if the tape machine tape is not ∅"
    (boolify (tape tm))
    )

  (defgeneric park (tm)
    (:documentation
      "Parks the head."
      ))

  (defmethod park ((tm tape-machine))
    (setf (HA tm) 'parked)
    )
