#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Implementation of cell intended for use with a bidirectional list.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type definition
;;
  (def-type bilink ()
    (
      (right-neighbor
        :initarg right-neighbor
        :accessor right-neighbor
        )
      (left-neighbor
        :initarg left-neighbor
        :accessor left-neighbor
        )
      ))

  (def-type cell-bilist (bilink cell)
    (
      (contents :initarg contents :accessor contents)
      ))

  (def-type bilist-leftmost-interior (cell-bilist leftmost-interior)())
  (def-type bilist-rightmost-interior (cell-bilist rightmost-interior)())

  (def-type bilist-interior (bilist-leftmost-interior bilist-rightmost-interior interior)())
  (def-type bilist-leftmost (bilist-leftmost-interior leftmost)())
  (def-type bilist-rightmost (bilist-rightmost-interior rightmost)())
  (def-type bilist-solitary (bilist-leftmost bilist-rightmost solitary)())

  (defun-typed to-cell ((cell cell-bilist))(change-class cell 'cell-bilist))
  (defun-typed to-interior ((cell cell-bilist))(change-class cell 'bilist-interior))
  (defun-typed to-leftmost ((cell cell-bilist))(change-class cell 'bilist-leftmost))
  (defun-typed to-rightmost ((cell cell-bilist))(change-class cell'bilist-rightmost))
  (defun-typed to-solitary ((cell cell-bilist))(change-class cell 'bilist-solitary))

;;--------------------------------------------------------------------------------
;; queries
;;
  (defun-typed r<cell> ((cell cell-bilist)) (contents cell))
  (defun-typed w<cell> ((cell cell-bilist) instance) (setf (contents cell) instance))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  (defun-typed a<cell> ((c0 bilist-solitary) (new-cell cell-bilist))
    (let(
          (c1 (right-neighbor c0)) ; this will be the list header, a bilink type
          )
      (place-inbetween c0 c1 new-cell)
      (to-leftmost c0)
      (to-rightmost new-cell)
      (values)
      ))
  (defun-typed a<cell> ((c0 bilist-rightmost) (new-cell cell-bilist))
    (let(
          (c1 (right-neighbor c0)) ; this will be the list header, a bilink type
          )
      (place-inbetween c0 c1 new-cell)
      (to-interior c0)
      (to-rightmost new-cell)
      (values)
      ))


  (defun-typed -a<cell> ((c1 bilist-solitary) (new-cell cell-bilist))
    (let(
          (c0 (left-neighbor c1)) ; this will be tape, the list header
          )
      (place-inbetween c0 c1 new-cell)
      (to-rightmost c1)
      (to-leftmost new-cell)
      (values)
      ))
  (defun-typed -a<cell> ((c1 bilist-leftmost) (new-cell cell-bilist))
    (let(
          (c0 (left-neighbor c1)) ; this will be tape, the list header
          )
      (place-inbetween c0 c1 new-cell)
      (to-interior c1)
      (to-leftmost new-cell)
      (values)
      ))

  ;; Deletes the right neighbor cell.
  ;; This function is unable to make the tape empty.
  ;; This is here instead of in cell.lisp because c2 might be the tape header,
  ;; and other types might not have a link type tape header.
  ;;
    (defun-typed d<cell> ((cell bilist-leftmost) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let*(
               (c0 cell)
               (c1 (right-neighbor c0)) ; this might be rightmost
               (c2 (right-neighbor c1)) ; this might be the tape header
               )
          (extract c0 c1 c2)
          (when 
            (typep c1 'rightmost)
            (to-solitary c0)
            )
          (to-cell c1)
          [➜ok c1]
          )))
    (defun-typed d<cell> ((cell bilist-interior) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let*(
               (c0 cell)
               (c1 (right-neighbor c0)) ; this might be rightmost
               (c2 (right-neighbor c1)) ; this might be the tape header
               )
          (extract c0 c1 c2)
          (when 
            (typep c1 'rightmost)
            (to-rightmost c0)
            )
          (to-cell c1)
          [➜ok c1]
          )))

  ;; deletes the left neighbor cell
  ;; this function is unable to make the tape empty
  (defun-typed -d<cell> ((cell bilist-rightmost) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 cell)
             (c1 (left-neighbor c2)) ; this might be leftmost
             (c0 (left-neighbor c1)) ; this might be the tape header
             )
        (extract c0 c1 c2)
        (when 
          (typep c1 'leftmost)
          (to-solitary c2)
          )
        (to-cell c1)
        [➜ok c1]
        )))
  (defun-typed -d<cell> ((cell bilist-interior) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 cell)
             (c1 (left-neighbor c2)) ; this might be leftmost
             (c0 (left-neighbor c1)) ; this might be the tape header
             )
        (extract c0 c1 c2)
        (when 
          (typep c1 'leftmost)
          (to-leftmost c2)
          )
        (to-cell c1)
        [➜ok c1]
        )))

