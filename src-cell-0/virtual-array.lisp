#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

The cells in an array structure have an abstracted topology rather than one embedded into
the cells.  The cells in the array do not have neighbor pointers.  Rather we infer a
neighbor relationship through a function that operates on addresses.

We can fit an array into the tape machine model by emulateing cells, and then use using
the state of the cell emulations in place of a real cell.  When a state for an emulated X
is used in place of an actual X, we say that the emulated X is a 'virtual' X.  Hence
what we will have 'virtual cells'.

Of course we must embed the emulator somewhere in our code, and this is straight forward
with CLOS due to the function dispatch.  We only need to give our virtual cells a
specialized type, then dispatch will send call through our emulatore. With this gasket in
place, a virtual cell can be used as a cell on the cell interface.

A virtual cell does not live on a tape or in another container, as do real cells.  The
real tape, still holds an array of real cells, but those real cells do not have neighbor
ponters. Rather when we call a function such as #'bound-left on a array tape, we get back a
virtual cell.  We can then call #'r #'w, #'neighbor, etc. on the virtual cell, and the
functionality will be correct.

In this context a virtual cell is the same thing as an iterator in other languages.
In Lisp a trivial example of a virtual cell is a reference to a cell.

Emulations can be incomplete.  For example, our basic array virtual cell does not support
topological changes.  The library user can not, for example, delete a virtual array cell.
This reflects the physical fact that array cells can not be deleted.  However, it is
conceivable that features can be added to the emulator to support this.  Perhaps by
keeping a parallel mark array or some such.  For now we do not do this.  The user's
of an array must be cognizant of this.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type definition
;;
  (def-type cell-array (cell virtual)
    (
      (base
        :initarg :base
        :accessor base
        )
      (index
        :initarg :index
        :accessor index
        )
      (maxdex
        :initarg :maxdex
        :accessor maxdex
        )))

  (def-type array-bound-left-interior (cell-array bound-left-interior)())
  (def-type array-bound-right-interior (cell-array bound-right-interior)())

  (def-type array-interior  (array-bound-left-interior array-bound-right-interior interior)())
  (def-type array-bound-left  (array-bound-left-interior bound-left)())
  (def-type array-bound-right (array-bound-right-interior bound-right)())
  (def-type array-solitary  (array-bound-left array-bound-right solitary)())
    
  (defun-typed to-cell      ((cell cell-array))(change-class cell 'cell-array))
  (defun-typed to-interior  ((cell cell-array))(change-class cell 'array-interior))
  (defun-typed to-bound-left  ((cell cell-array))(change-class cell 'array-bound-left))
  (defun-typed to-bound-right ((cell cell-array))(change-class cell 'array-bound-right))
  (defun-typed to-solitary  ((cell cell-array))(change-class cell 'array-solitary))


  (defun-typed init ((cell cell-array) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        status
        )
      ➜
      (w cell instance)
      (when status
        (case status
          (interior  (to-interior  cell))
          (bound-left  (to-bound-left  cell))
          (bound-right (to-bound-right cell))
          (solitary  (to-solitary  cell))
          (otherwise (return-from init [➜fail]))
          ))
      [➜ok cell]
      ))

;;--------------------------------------------------------------------------------
;; queries
;;
  (defun-typed r<cell> ((cell cell-array)) (contents cell))
  (defun-typed w<cell> ((cell cell-array) instance) (setf (contents cell) instance))

  ;; the more specific righmost case is handled on the interface
  (defun-typed esr<cell> ((cell cell) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (r<cell> (right-neighbor cell))]
      ))
  (defun-typed esw<cell> ((cell cell) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (w<cell> (right-neighbor cell) instance)]
      ))

  (def-function-class right-neighbor (cell &optional ➜))
  (defun-typed right-neighbor ((cell bound-right) &optional ➜)
    (destructuring-bind
      (&key
        (➜bound-right (λ(cell n)(declare (ignore cell n))(error 'step-from-bound-right)))
        &allow-other-keys
        )
      ➜
      [➜bound-right]
      ))
  (defun-typed right-neighbor ((cell array-bound-left-interior) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (right-neighbor-link cell)]
      ))

  (def-function-class left-neighbor (cell &optional ➜))
  (defun-typed left-neighbor ((cell bound-left) &optional ➜)
    (destructuring-bind
      (&key
        (➜bound-left (λ(cell n)(declare (ignore cell n))(error 'step-from-bound-left)))
        &allow-other-keys
        )
      ➜
      [➜bound-left]
      ))
  (defun-typed left-neighbor ((cell array-bound-right-interior) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (left-neighbor-link cell)]
      ))

  ;; then nth neighbor to the right
  ;; For arrays, this just increments the array index, which is why this is here
  ;; instead of being part of the tape machine.
  ;;
    (defun right-neighbor-n (cell n &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜bound-right (λ(cell n)(declare (ignore cell n))(error 'step-from-bound-right)))
          &allow-other-keys
          )
        ➜
      (cond
        ((< n 0) (left-neighbor-n cell (- n) ➜))
        (t
          (loop 
            (cond
              ((= n 0) (return [➜ok cell]))
              ((typep cell 'bound-right)(return [➜bound-right cell n]))
              (t
                (decf n)
                (setf cell (right-neighbor cell))
                )))))))

    (defun left-neighbor-n (cell n &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜bound-left (λ(cell n)(declare (ignore cell n))(error 'step-from-bound-left)))
          &allow-other-keys
          )
        ➜
      (cond
        ((< n 0) (right-neighbor-n cell (- n) ➜))
        (t
          (loop 
            (cond
              ((= n 0) (return [➜ok cell]))
              ((typep cell 'bound-left)(return [➜bound-left cell n]))
              (t
                (decf n)
                (setf cell (left-neighbor cell))
                )))))))

  (defun-typed neighbor((cell cell-array) &optional ➜)
    (destructuring-bind
      (&key
        (direction direction-right)
        (distance 0)
        (➜unknown-direction (λ()(error 'unknown-direction)))
        &allow-other-keys
        )
      ➜
      (cond
        ((= direction direction-right) (right-neighbor-n cell distance ➜))
        ((= direction direction-left) (left-neighbor-n cell distance ➜))
        (t [➜unknown-direction])
        )))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;

  (defun-typed a<cell> ((c0 array-solitary) (new-cell cell-array))
    (let(
          (c1 (right-neighbor-link c0)) ; this will be the list header, a bilink type
          )
      (insert-between c0 c1 new-cell)
      (to-bound-left c0)
      (to-bound-right new-cell)
      (values)
      ))
  (defun-typed a<cell> ((c0 array-bound-right) (new-cell cell-array))
    (let(
          (c1 (right-neighbor-link c0)) ; this will be the list header, a bilink type
          )
      (insert-between c0 c1 new-cell)
      (to-interior c0)
      (to-bound-right new-cell)
      (values)
      ))
  (defun-typed a<cell> ((c0 bound-left-interior) (new-cell cell))
    (let(
          (c1 (right-neighbor c0)) ; c0 is not bound-right, but c1 might be
          )
      (insert-between c0 c1 new-cell)
      (to-interior new-cell)
      (values)
      ))


  (defun-typed -a<cell> ((c1 array-solitary) (new-cell cell-array))
    (let(
          (c0 (left-neighbor-link c1)) ; this will be tape, the list header
          )
      (insert-between c0 c1 new-cell)
      (to-bound-right c1)
      (to-bound-left new-cell)
      (values)
      ))
  (defun-typed -a<cell> ((c1 array-bound-left) (new-cell cell-array))
    (let(
          (c0 (left-neighbor-link c1)) ; this will be tape, the list header
          )
      (insert-between c0 c1 new-cell)
      (to-interior c1)
      (to-bound-left new-cell)
      (values)
      ))
  ;;The solitary and bound-left cases must be handled by specialized implementation
  ;;functions.  We can't do it here because we don't know the relationship between
  ;;bound-right and the tape header for the implementation.
  (defun-typed -a<cell> ((c1 array-bound-right-interior) (new-cell cell))
    (let(
          (c0 (left-neighbor c1)) ; c1 is not bound-left, but c0 might be
          )
      (insert-between c0 c1 new-cell)
      (to-interior new-cell)
      (values)
      ))


  ;; Deletes the right neighbor cell.
  ;; This function is unable to make the tape empty.
  ;; This is here instead of in cell.lisp because c2 might be the tape header,
  ;; and other types might not have a link type tape header.
  ;;
    (defun-typed d<cell> ((cell array-bound-left) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let*(
               (c0 cell)
               (c1 (right-neighbor-link c0)) ; this might be bound-right
               (c2 (right-neighbor-link c1)) ; this might be the tape header
               )
          (extract c0 c1 c2)
          (when 
            (typep c1 'bound-right)
            (to-solitary c0)
            )
          (to-cell c1)
          [➜ok c1]
          )))
    (defun-typed d<cell> ((cell array-interior) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let*(
               (c0 cell)
               (c1 (right-neighbor-link c0)) ; this might be bound-right
               (c2 (right-neighbor-link c1)) ; this might be the tape header
               )
          (extract c0 c1 c2)
          (when 
            (typep c1 'bound-right)
            (to-bound-right c0)
            )
          (to-cell c1)
          [➜ok c1]
          )))

  ;; deletes the left neighbor cell
  ;; this function is unable to make the tape empty
  (defun-typed -d<cell> ((cell array-bound-right) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 cell)
             (c1 (left-neighbor-link c2)) ; this might be bound-left
             (c0 (left-neighbor-link c1)) ; this might be the tape header
             )
        (extract c0 c1 c2)
        (when 
          (typep c1 'bound-left)
          (to-solitary c2)
          )
        (to-cell c1)
        [➜ok c1]
        )))
  (defun-typed -d<cell> ((cell array-interior) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 cell)
             (c1 (left-neighbor-link c2)) ; this might be bound-left
             (c0 (left-neighbor-link c1)) ; this might be the tape header
             )
        (extract c0 c1 c2)
        (when 
          (typep c1 'bound-left)
          (to-bound-left c2)
          )
        (to-cell c1)
        [➜ok c1]
        )))

    (defun-typed d+<cell> ((cell array-bound-left-interior) &optional ➜)
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
