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

  (defun mk-stack-list
    (
      &optional
      init
      (cont-ok #'echo)
      (cont-fail
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized stack list tape type"))
        ))

    (tm-mk-list init 
      (λ(instance) (funcall cont-ok (change-class instance 'stack-list)))
      cont-fail
      ))

  (tm-mk-hook 'stack-list #'mk-stack-list)

;;--------------------------------------------------------------------------------
;; a more specific queue interface
;;
  (defclass queue-list (queue tm-list)())

  (defun mk-queue-list
    (
      &optional
      init
      (cont-ok #'echo)
      (cont-fail
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized queue list tape type"))
        ))

    (tm-mk-list init 
      (λ(instance) (funcall cont-ok (change-class instance 'queue-list)))
      cont-fail
      ))

  (tm-mk-hook 'queue-list #'mk-queue-list)

