#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Implementation of cell intended for use in a list.

CLOS version of the cons cell.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type definition
;;
  (def-type link ()
    (
      (right-neighbor
        :initarg :right-neighbor
        :accessor right-neighbor
        )
      ))
  ;; though a list cell only has one link, the header for a list tape keeps track of both
  ;; tape boundaries
  (def-type bilink (link)
    (
      (left-neighbor
        :initarg :left-neighbor
        :accessor left-neighbor
        )
      ))

  (defparameter *direction-right* 0)
  (defparameter *direction-left*  1)

  (def-type cell-list (link cell real)
    (
      (contents :initarg :contents :accessor contents)
      ))

  ;; these are only used as typed function argument specifiers
  ;;
    (def-type list-leftmost-interior (cell-list leftmost-interior)())
    (def-type list-rightmost-interior (cell-list rightmost-interior)())

  ;; all cells appearing on a tape have exactly one of these subtypes
  ;;
    (def-type list-interior  (list-leftmost-interior list-rightmost-interior interior)())
    (def-type list-leftmost  (list-leftmost-interior leftmost)())
    (def-type list-rightmost (list-rightmost-interior rightmost)())
    (def-type list-solitary  (list-leftmost list-rightmost solitary)())
    
  (defun-typed to-cell      ((cell cell-list))(change-class cell 'cell-list))
  (defun-typed to-interior  ((cell cell-list))(change-class cell 'list-interior))
  (defun-typed to-leftmost  ((cell cell-list))(change-class cell 'list-leftmost))
  (defun-typed to-rightmost ((cell cell-list))(change-class cell 'list-rightmost))
  (defun-typed to-solitary  ((cell cell-list))(change-class cell 'list-solitary))


  (defun-typed init ((cell cell-list) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        status right-neighbor
        )
      ➜
      (w<cell> cell instance)

      (cond
        (right-neighbor     (connect cell right-neighbor))
        ((¬ right-neighbor) (cap-right cell))
        )
      (when status
        (case status
          (interior  (to-interior  cell))
          (leftmost  (to-leftmost  cell))
          (rightmost (to-rightmost cell))
          (solitary  (to-solitary  cell))
          (otherwise (return-from init [➜fail]))
          ))
      [➜ok cell]
      ))

;;--------------------------------------------------------------------------------
;; cell functions
;;
  ;; we don't provide a cell copy, so one can usually just use eq
  (defun-typed =<cell> ((cell-0 cell-list) (cell-1 cell-list) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (if
        (∧
          (eq (right-neighbor cell-0) (right-neighbor cell-1))
          (eq (cargo cell-0) (cargo cell-1))
          )
        [➜t]
        [➜∅]
        )))

  (defun-typed r ((cell cell-list)) (contents cell))
  (defun-typed w ((cell cell-list) instance) (setf (contents cell) instance))

;;--------------------------------------------------------------------------------
;; cell neighbor functions
;;
  ;; by contract distance is positive when neighbor-1 is called
  ;; differential is a function
  (defun neighbor-1 (cell differential n &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ(cell n)(declare (ignore cell n)(error 'step-from-rightmost))))
        &allow-other-keys
        )
      ➜
      (⟳(λ(➜again)
          (cond
            ((typep cell 'rightmost) [➜rightmost cell n])
            ((= n 0) [➜ok cell])
            (t
              (setf cell [differential cell])
              (decf n)
              [➜again]
              ))))))

  (defun-typed neighbor ((cell cell-list) &optional ➜)
    (destructuring-bind
      (&key
        (➜bad-direction (λ()(error 'bad-direction)))
        (distance 1)
        &allow-other-keys
        )
      ➜
      (cond
        ((< n 0) [➜bad-direction])
        (t 
          (neighbor-1 cell #'right-neighbor distance ➜)
          ))))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  (def-funciton-class connect (c0 c1))
  (defun-typed connect ((c0 cell-list) (c1 cell-list))
    (setf (right-neighbor-link c0) c1)
    )

  (defun-typed a<cell> ((c0 cell-list-solitary) (new-cell cell-list))
    (connect c0 new-cell)
    (to-leftmost c0)
    (to-rightmost new-cell)
    t
    ))
  (defun-typed a<cell> ((c0 cell-list-rightmost) (new-cell cell-list))
    (connect c0 new-cell)
    (to-interior c0)
    (to-rightmost new-cell)
    t
    ))
  (defun-typed a<cell> ((c0 leftmost-interior) (new-cell cell))
    (connect c0 new-cell)
    (to-interior new-cell)
    t
    ))

  ;; Deletes the right neighbor cell.
  ;; This function is unable to make the tape empty.
  ;; This is here instead of in cell.lisp because c2 might be the tape header,
  ;; and other types might not have a link type tape header.
  ;;

--->

    (defun-typed d<cell> ((cell list-leftmost) &optional ➜)
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
    (defun-typed d<cell> ((cell list-interior) &optional ➜)
      (destructuring-bind
        (&keyp
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
  (defun-typed -d<cell> ((cell list-rightmost) &optional ➜)
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
  (defun-typed -d<cell> ((cell list-interior) &optional ➜)
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

    (defun-typed d+<cell> ((cell list-leftmost-interior) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let(
              (rn (right-neighbor cell))
              )
          (disconnect cell rn)
          [➜ok rn]
          )))
