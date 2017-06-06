#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; first level and a status machine
;;
  (defun-typed c ((src tape-machine) (dst empty)  &optional ➜)
    (destructuring-bind
      (&key
        (➜dst-full (be ∅))
        &allow-other-keys
        )
      ➜
      [➜dst-full]
      ))

  (defun-typed c ((src tape-machine) (dst parked)  &optional ➜)
    (s dst {:➜ok #'do-nothing :➜rightmost #'cant-happen})
    (c src dst ➜)
    )

;;--------------------------------------------------------------------------------
;; abandoned
;;
  (defun-typed c ((src abandoned) (dst tape-machine)  &optional ➜)
    (declare (ignore src dst ➜))
    (operation-on-abandoned)
    )

  (defun-typed c ((src tape-machine) (dst abandoned)  &optional ➜)
    (declare (ignore src dst ➜))
    (operation-on-abandoned)
    )

;;--------------------------------------------------------------------------------
;; empty
;;
  (defun-typed c ((src empty) (dst empty)  &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      [➜ok]
      ))
  (defun-typed c ((src empty) (dst tape-machine)  &optional ➜)
    (destructuring-bind
      (&key
        (➜src-depleted (be ∅)) ;; but still room on dst
        &allow-other-keys
        )
      ➜
      [➜src-depleted]
      ))

  (defun-typed c-fit ((src empty) (dst empty))
    (declare (ignore src dst))
    t
    )

  (defun-typed c-fit ((src empty) (dst status-tape-machine))
    (declare (ignore src))
    (p dst)
    (d* dst)
    )

;;--------------------------------------------------------------------------------
;; parked
;;
  (defun-typed c ((src parked) (dst empty)  &optional ➜)
    (destructuring-bind
      (&key
        (➜dst-full (be ∅))  ;; but still instances uncopied from src
        &allow-other-keys
        )
      ➜
      (-s* src)
      [➜dst-full]
      ))

  (defun-typed c ((src parked) (dst parked)  &optional ➜)
    (-s* src)
    (-s* dst)
    (c src dst ➜)
    )

  (defun-typed c ((src parked) (dst tape-machine) &optional ➜)
    (-s* src)
    (c src dst ➜)
    )

;;--------------------------------------------------------------------------------
;; active
;;
  (defun-typed c ((src active) (dst empty)  &optional ➜)
    (destructuring-bind
      (&key
        (➜dst-full (be ∅))  ;; but still instances uncopied from src
        &allow-other-keys
        )
      ➜
      [➜dst-full]
      ))

  (defun-typed c ((src active) (dst parked)  &optional ➜)
    (-s* dst)
    (c src dst ➜)
    )

