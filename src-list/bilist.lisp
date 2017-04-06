#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defstruct binode
  instance
  right-neighbor
  left-neighbor
  )

(defun make-bilist-1 (a-list left-neighbor)
  (cond
    (a-list
      (let(
            (node (make-binode))
            (instance (car a-list))
            (rest-list (cdr a-list))
            )
        (setf (binode-right-neighbor left-neighbor) node)
        (setf (binode-left-neighbor node) left-neighbor)
        (setf (binode-instance node) instance)
        (make-bilist-1 rest-list node)
        ))
    (t
      (setf (binode-right-neighbor left-neighbor) ∅)
      )))

(defun make-bilist (a-list)
  (cond
    (a-list
      (let(
            (node (make-binode))
            (instance (car a-list))
            (rest-list (cdr a-list))
            )
        (setf (binode-left-neighbor node) ∅)
        (setf (binode-instance node) instance)
        (make-bilist-1 rest-list node)
        node
        ))
    (t
      ∅
      )))

(defun print-bilist-1 (bl)
  (cond
    (bl
      (princ " ")
      (princ (binode-instance bl))
      (print-bilist-1 (binode-right-neighbor bl))
      )
    (t (nl))
    ))


(defun print-bilist (bl)
  (cond
    (bl
      (princ (binode-instance bl))
      (print-bilist-1 (binode-right-neighbor bl))
      )
    (t (nl))
    ))
      

