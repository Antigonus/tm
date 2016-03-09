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

  (defun init-stack-list
    (
      instance
      &optional
      init
      (cont-ok #'echo)
      (cont-fail
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized stack list tape type"))
        ))
    (init-tm-list instance init cont-ok cont-fail)
    )

  (defun mk-stack-list ()
    (let(
          (instance (make-instance 'stack-list))
          )
      (init-stack-list instance)
    ))

  (mk-tm-hook 'stack-list #'mk-stack-list)

;;--------------------------------------------------------------------------------
;; a more specific queue interface
;;

  (defclass queue-list (queue tm-list)())

  (defun init-queue-list
    (
      instance
      &optional
      init
      (cont-ok #'echo)
      (cont-fail
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized queue list tape type"))
        ))
    (init-tm-list instance init cont-ok cont-fail)
    )

  (defun mk-queue-list ()
    (let(
          (instance (make-instance 'queue-list))
          )
      (init-queue-list instance)
    ))

  (mk-tm-hook 'queue-list #'mk-queue-list)

