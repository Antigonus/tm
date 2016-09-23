#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Uses list as a base implementation.

|#

(in-package #:tm)

(defun test-stack-0 ()
  (let*(
        (a-stack (make-instance 'stack))
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
         (a (make-instance 'stack))
         (a1  (progn (enqueue a 1) (dequeue a)))
         (a2  (progn (enqueue a 2) (dequeue a)))
         )
    (and
      (= a1 1)
      (= a2 2)
      )))
(test-hook test-stack-1)

(defun test-queue-0 ()
  (let(
        (a-queue (make-instance 'queue))
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
         (a (make-instance 'queue))
         (a1  (progn (enqueue a 1) (dequeue a)))
         (a2  (progn (enqueue a 2) (dequeue a)))
         )
    (and
      (= a1 1)
      (= a2 2)
      )))
(test-hook test-queue-1)
