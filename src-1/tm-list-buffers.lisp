#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a more specific stack interface
;;
  (defclass stack-list (stack tm-list)())

  (defmethod tm-init
    (
      (instance stack-list)
      &optional
      init
      (cont-ok #'echo)
      (cont-fail
        (λ() (error 'tm-mk-init-failed :text "unrecognized stack list tape type"))
        ))

    (change-class instance 'tm-list)
    (tm-init 
      instance
      init
      (λ(instance) (funcall cont-ok (change-class instance 'stack-list)))
      cont-fail
      ))

;;--------------------------------------------------------------------------------
;; a more specific queue interface
;;
  (defclass queue-list (queue tm-list)())

  (defmethod tm-init
    (
      (instance queue-list)
      &optional
      init
      (cont-ok #'echo)
      (cont-fail
        (λ() (error 'tm-mk-init-failed :text "unrecognized queue list tape type"))
        ))

    (change-class instance 'tm-list)
    (tm-init
      instance
      init 
      (λ(instance) (funcall cont-ok (change-class instance 'queue-list)))
      cont-fail
      ))

