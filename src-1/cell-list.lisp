#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Implementation of cell intended for use in a list.

CLOS version of the cons cell.

For supporting subspace, the contents is turned into a tape-array of instances rather than
an individual instance. 

For Multiple universe, an extra layer of tape-array is added.  Contents becomes a tape-array
of instances, where each instance is s tape-array for the contents.

Neighbor is a tape array where the instances are links to neighbors.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type definition
;;

  ;; contents is used directly
  (def-type cell-list (cell)
    (
      (contents 
        :init-form (make-tape-array)
        )
      (neighbor
        :init-form (make-tape-array)
        )
      ))

  ;; contents have been forked into multiple universes
  ;; (links to neighbors are forked individually)
  (def-type cell-list-multi (cell)())

  (defun-typed init ((cell cell-list) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        &allow-other-keys
        )
      ➜
      (a◨<tape-array> (contents cell) instance)
      [➜ok cell]
      ))

;;--------------------------------------------------------------------------------
;; read
;;
  ;; sid == subspace id
  (defmacro r<cell> (cell accessor ➜)
    `(destructuring-bind
       (&key
         (➜ok #'echo)
         (sid 0)
         &allow-other-keys
         )
       ➜
       [➜ok (r<tape-array> (apply ,accessor ,cell) sid)]
       ))
  (defun-typed r<contents> ((cell cell-list) &optional ➜)
    (r<cell> cell #'contents ➜)
    )
  (defun-typed r<neighbor> ((cell cell-list) &optional ➜)
    (r<cell> cell #'neighbor ➜)
    )

  (defmacro r<cell>-multi (cell accessor ➜)
    `(destructuring-bind
       (&key
         (➜ok #'echo)
         (➜no-universe (λ()(error 'no-universe)))
         (sid 0)
         (uid 0) ; if uid is nonzero, then 'parents' must be supplied, enforced by contract with programmer
         parents 
         &allow-other-keys
         )
       ➜
       (⟳ (λ(➜again)
            (setf contents (r<tape-array> (apply ,accessor ,cell) uid))
            (if
              contents
              [➜ok (r<tape-array> contents sid)]
              (if
                (= uid 0)
                [➜no-universe]
                (progn
                  (setf uid (get-hash uid parents))
                  [➜again]
                  )))))
       ))

  (defun-typed r<contents> ((cell cell-list-multi) &optional ➜)
    (r<cell>-multi cell #'contents ➜)
    )
  (defun-typed r<neighbor> ((cell cell-list-multi) &optional ➜)
    (r<cell>-multi cell #'neighbor ➜)
    )

;;--------------------------------------------------------------------------------
;; write
;;
  (defmacro w<cell> (cell instance accessor ➜)
    `(destructuring-bind
       (&key
         (➜ok (be t))
         (sid 0)
         &allow-other-keys
         )
       ➜
       (w<tape-array> (,accessor ,cell) instance sid)
       ))
  (defun-typed w<contents> ((cell cell-list) instance &optional ➜)
    (w<cell> cell instance 'contents ➜)
    )
  (defun-typed w<neighbor> ((cell cell-list) instance &optional ➜)
    (w<cell> cell instnace 'neighbor ➜)
    )

  (defmacro wa<cell>-multi (cell instance accessor operator ➜)
    `(destructuring-bind
       (&key
         (➜ok #'echo)
         (sid 0)
         (uid 0)
         parents
         &allow-other-keys
         )
       ➜
       (expand<tape-array> (,accessor ,cell) uid)
       (,operator
         (r<tape-array> (,accessor ,cell) uid)
         instance
         sid
         )
       [➜ok]
       ))
  (defun-typed w<contents> ((cell cell-list-multi) instance &optional ➜)
    (wa<cell> cell instance 'contents 'w<tape-array> ➜)
    )
  (defun-typed w<neighbor> ((cell cell-list-multi) instance &optional ➜)
    (w<cell> cell instance 'neighbor 'w<tape-array> ➜)
    )
  (defun-typed a<contents> ((cell cell-list-multi) instance &optional ➜)
    (wa<cell> cell instance 'contents 'a◨<tape-array> ➜)
    )
  (defun-typed a<neighbor> ((cell cell-list-multi) instance &optional ➜)
    (w<cell> cell instance 'neighbor 'a◨<tape-array> ➜)
    )
