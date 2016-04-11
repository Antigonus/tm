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

  (defmethod init 
    (
      (instance stack-list)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (change-class instance 'tm-list)
    (init instance init-list 
      (λ()
        (change-class instance 'stack-list)
        (funcall cont-ok)
        )
      (funcall cont-fail)
      ))

;;--------------------------------------------------------------------------------
;; a more specific queue interface
;;
  (defclass queue-list (queue tm-list)())

  (defmethod init 
    (
      (instance queue-list)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (change-class instance 'tm-list)
    (init instance init-list 
      (λ()
        (change-class instance 'stack-list)
        (funcall cont-ok)
        )
      (funcall cont-fail)
      ))

