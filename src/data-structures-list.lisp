#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch

|#

(in-package #:le)

;;--------------------------------------------------------------------------------
;; a more specific stack interface
;;
  (defclass stack-list (stack tm-list)())

  (defun init-stack-list-0
    (
      instance
      &optional
      init
      (cont-ok #'echo)
      (cont-fail
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized stack list tape type"))
        ))
    (init-tm-list-0 instance init cont-ok cont-fail)
    )

  (defun mk-stack-list-0 ()
    (let(
          (instance (make-instance 'stack-list))
          )
      (init-stack-list-0 instance)
    ))

  (defun test-stack-0 ()
    (let(
          (a-stack (mk-stack-list-0))
          )
      (enqueue a-stack 1)
      (enqueue a-stack 2)
      (enqueue a-stack 3)
      (and
        (= (dequeue a-stack) 3)
        (not (empty a-stack))
        (= (dequeue a-stack) 2)
        (= (dequeue a-stack) 1)
        (dequeue a-stack (be ∅) (be t))
        (empty a-stack)
        )))
  (test-hook test-stack-0)

  (defun test-stack-1 ()
    (let*(
           (a (mk-stack-list-0))
           (a1  (progn (enqueue a 1) (dequeue a)))
           (a2  (progn (enqueue a 2) (dequeue a)))
          )
      (and
        (= a1 1)
        (= a2 2)
        )))
  (test-hook test-stack-1)

;;--------------------------------------------------------------------------------
;; a more specific queue interface
;;

  (defclass queue-list (queue tm-list)())

  (defun init-queue-list-0
    (
      instance
      &optional
      init
      (cont-ok #'echo)
      (cont-fail
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized queue list tape type"))
        ))
    (init-tm-list-0 instance init cont-ok cont-fail)
    )

  (defun mk-queue-list-0 ()
    (let(
          (instance (make-instance 'queue-list))
          )
      (init-queue-list-0 instance)
    ))

  (defun test-queue-0 ()
    (let(
          (a-queue (mk-queue-list-0))
          )
      (enqueue a-queue 1)
      (enqueue a-queue 2)
      (enqueue a-queue 3)
      (and
        (= (dequeue a-queue) 1)
        (not (empty a-queue))
        (= (dequeue a-queue) 2)
        (= (dequeue a-queue) 3)
        (dequeue a-queue (be ∅) (be t))
        (empty a-queue)
        )))
  (test-hook test-queue-0)

  (defun test-queue-1 ()
    (let*(
           (a (mk-queue-list-0))
           (a1  (progn (enqueue a 1) (dequeue a)))
           (a2  (progn (enqueue a 2) (dequeue a)))
          )
      (and
        (= a1 1)
        (= a2 2)
        )))
  (test-hook test-queue-1)
