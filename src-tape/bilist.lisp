#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Conceptually this is cleaner, but it doesn't bind to Lisp's linked list.  Rather it
creates its own list form.  Perhaps this is what lists would have looked like had CLOS
been part of the original language.

Inheritance structure.

               bilink
                ^  ^
                |   \ 
                |    bilist-with-contents
                |
             list-tape
               ^  ^
               |   |
 list-tape-empty   list-tape-active

The list header will be a list-tape type. The header will hold links to rightmost and
leftmost.

Due to list-tape being inherited from link, the list-tape header can be passed to
functions that manipulate links. This simplifies end case processing.

Because list-tape-empty and list-tape-active are inherited from list-tape without the
addiition of slots, we can change-type between them without penalty.  The status of
the list is carried in its type.

Would rather have used static structs, but didn't want to fight Lisp over being able to
change the type of a list-tape-empty to a list-tape-active when list tape is a structure.
Though they are of identical format #'coerce refused to do it. Also, this is a bit 
of a research project and I prefer to keep the language syntax paradigm consistent.


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
  (def-type cell-bilist-leftmost (cell-bilist cell-leftmost)())
  (def-type cell-bilist-rightmost (cell-bilist cell-rightmost)())
  (def-type cell-bilist-end (cell-bilist-rightmost cell-bilist-leftmost cell-end)())

  (defun-typed to-cell ((cell cell-bilist))(change-class cell 'cell-bilist))
  (defun-typed to-end ((cell cell-bilist))(change-class cell 'cell-bilist-end))
  (defun-typed to-leftmost ((cell cell-bilist))(change-class cell 'cell-bilist-leftmost))
  (defun-typed to-rightmost ((cell cell-bilist))(change-class cell 'cell-bilist-rightmost))


  (def-type tape-bilist (bilink tape) ())
  (def-type tape-bilist-abandoned (tape-bilist tape-abandoned))
  (def-type tape-bilist-empty (tape-bilist tape-empty))
  (def-type tape-bilist-active (tape-bilist tape-active))

  (defun-typed to-abandoned ((tape tape-bilist)) (change-class tape 'tape-bilist-abandoned))
  (defun-typed to-empty  ((tape tape-bilist)) (change-class tape 'tape-bilist-empty))
  (defun-typed to-active ((tape tape-bilist)) (change-class tape 'tape-bilist-active))

;;--------------------------------------------------------------------------------
;; initializer
;;
  ;; shallow copies from another tape
  (defun-typed init ((tape-1 tape-bilist) (tape-0 tape) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (setf (right-neighbor tape-1) tape-1)
      (setf (left-neighbor tape-1) tape-1)
      (to-empty tape-1)
      (shallow-copy-topo tape-1 tape-0)
      [➜ok tape-1]
      ))

;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun-typed =<cell> ((cell-0 cell-bilist) (cell-1 cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (if
        (eq cell-0 cell-1)
        [➜t]
        [➜∅]
        )))

  (defun-typed r<cell> ((cell cell-bilist)) (contents cell))
  (defun-typed w<cell> ((cell cell-bilist) instance) (setf (contents cell) instance))

  (defun-typed leftmost ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (right-neighbor tape)]
      ))

  ;;tape.list handles the cell-rightmost case
  (defun-typed right-neighbor ((cell cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok right-neighbor cell]
      ))

  (defun-typed rightmost ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (left-neighbor tape)]
      ))

  ;; tape.lisp handles the cell-leftmost case
  (defun-typed left-neighbor ((cell cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (left-neighbor cell)
      ))
    
;;--------------------------------------------------------------------------------
;; topology manipulation
;;
;; The cell types can be changed when performing topology manipulation.
;; #'mk returns a basic cell type, 'cell.  All routines that return cells will
;; also return a cell of 'cell type.
;;
  (defun-typed epa<tape> ((tape1 tape-bilist-active) (tape0 tape-bilist-active))
    (let(
          (leftmost-tape0  (right-neighbor tape0))
          (rightmost-tape0 (left-neighbor tape0))
          (leftmost-tape1  (right-neighbor tape1))
          (rightmost-tape1 (left-neighbor tape1))
          )

      (setf (right-neighbor rightmost-tape0) (leftmost-tape1))
      (setf (left-neighbor leftmost-tape1) (rightmost-tape0))
      (to-cell rightmost-tape0)
      (to-cell leftmost-tape1)
      (to-abandoned tape0)
      ))

  ;; for internal use  
  (defun insert<cell> (c0 c1 new-cell)
    (setf (left-neighbor new-cell) c0)
    (setf (right-neighbor new-cell) c1)
    (setf (left-neighbor c1) new-cell)
    (setf (right-neighbor c0) new-cell)
    )

  ;; accepts a tape-bilist and a cell, makes the cell the new leftmost
  ;; will be a problem if (cons-bilist tape) is shared
  (defun-typed epa<cell> ((tape tape-bilist-active) (cell cell-bilist))
    (let(
          (c0 tape) ; note the tape header is inherited from bilink, it looks like a tape node
          (c1 (right-neighbor tape))
          (new-cell cell)
          )
      (to-cell c1) ; old leftmost is no longer leftmost
      (to-leftmost new-cell) ; the new cell becomes leftmost
      (insert<cell> c0 c1 new-cell)
      ))

  ;; accepts a tape-bilist and an instance, makes a new leftmost initialized with the
  ;; instance will be a problem if (cons-bilist tape) is shared
  (defun-typed epa<instance> ((tape tape-bilist-empty) instance)
    (let(
          (c0 tape)
          (c1 tape)
          (new-cell (make-instance 'cell-bilist :contents instance))
          )
      (to-active tape)
      (to-end new-cell)
      (insert<cell> tape tape new-cell)
      ))
  (defun-typed epa<instance> ((tape tape-bilist-active) instance)
    (let(
          (c0 tape)
          (c1 (right-neighbor tape))
          (new-cell (make-instance 'cell-bilist :contents instance))
          )
      (if 
        (typep c1 'cell-end)
        (to-rightmost c1)
        (to-cell c1)
        )
      (to-leftmost new-cell)
      (insert<cell> c0 c1 new-cell)
      ))

  ;; the new-cell is type 'cell,and not 'cell-rightmost or 'cell-leftmost etc.
  (defun-typed a<cell> ((c0 cell-bilist) (new-cell cell-bilist))
    (insert<cell> c0 (right-neighbor c0) new-cell)
    )

  (defun-typed a<instance> ((c0 cell-bilist) instance)
    (let(
          (new-cell (make-instance 'cell-bilist :contents instance))
          )
      (insert<cell> c0 (right-neighbor c0) new-cell)
      ))

  (defun-typed -a<cell> ((c0 cell-bilist) (new-cell cell-bilist))
    (insert<cell> (left-neighbor c0) c0 new-cell)
    )

  (defun-typed -a<instance> ((c0 cell-bilist) instance)
    (let(
          (new-cell (make-instance 'cell-bilist :contents instance))
          )
      (insert<cell> (left-neighbor c0) c0 new-cell)
      ))

  ;; for internal use
  ;; removes c1
  (defun remove<cell> (c0 c1 c2)
    (setf (left-neighbor c2) c0)
    (setf (right-neighbor c0) c2)
    (setf (left-neighbor c1) c1) ; so that c1 can not prevent neighbors from being gc'ed
    (setf (right-neighbor c1) c1)
    (to-cell c1)
    )

  ;; removes the leftmost cell and returns it
  ;; an active tape always has a leftmost to be deleted
  ;; will be a problem if the tape is intangled, as partners will not have correct leftmost afterward
  (defun-typed epd<tape> ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c0 tape)
             (c1 (right-neighbor c0)) ; this is leftmost
             (c2 (right-neighbor c1)) ; this is the right neighbor of leftmost, possibly tape
             )
        (when (typep c1 'cell-end) (to-empty tape))
        (remove<cell> c0 c1 c2)
        [➜ok c1]
        )))

  (defun-typed epd*<tape> ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (lm (leftmost tape))
            )
        (setf (right-neighbor tape) tape)
        (setf (left-neighbor tape) tape)
        (to-empty tape)
        [➜ok lm]
        )))

  ;; deletes rightmost, i.e. ◨-sd
  (defun-typed p-d<tape> ((tape tape-active) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 tape)
             (c1 (left-neighbor c2)) ; this is rightmost
             (c0 (left-neighbor c1)) ; this is left neighbor of rightmost, possible tape
             )
        (when (typep c1 'cell-end) (to-empty tape))
        (remove<cell> c0 c1 c2)
        [➜ok c1]
        )))

  ;; deletes the right neighbor cell
  ;; this can not make the tape empty
  (defun-typed d<cell> ((cell cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      (let*(
             (c0 cell)
             (c1 (right-neighbor c0))
             (c2 (right-neighbor c1))
             )
        (cond
          ((eq tape c1) [➜rightmost])
          (t
            (remove<cell> c0 c1 c2)
            [➜ok c1]
            )))))

  (defun-typed -d<cell> ((cell cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜leftmost (λ()(error 'dealloc-on-leftmost)))
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 cell)
             (c1 (left-neighbor c2))
             (c0 (left-neighbor c1))
             )
        (cond
          ((eq tape c1) [➜leftmost])
          (t
            (remove<cell> c0 c1 c2)
            [➜ok c1]
            )))))

  ;; References to the right neghbor of leftmost get messed up,
  ;; but (cons-bilist tape) will be ok, so no tape sharing issues
  (defun-typed ◧d.<tape> ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (d.<cell> (leftmost tape)
        {
          :➜ok ➜ok
          :➜rightmost
          (λ()
            (let(
                  (cell-0 (cons-bilist tape))
                  )
              (setf (cons-bilist tape) ∅)
              (to-empty tape)
              [➜ok (make-instance 'cell-bilist :cons-cell cell-0)]
              ))
          })))

  (defun-typed d*<cell> (cell (tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (rn (right-neighbor cell))
            )
        (cond
          ((eq tape rn)  [➜rightmost])
          (t
            (let(
                  (rm (left-neighbor tape))
                  )
              (setf (right-neighbor rm) ∅) ; cleaved part of tape ends
              (setf (left-neighbor rn) ∅)  ; cleaved part of tape ends
              (setf (right-neighbor cell) tape) 
              (setf (left-neighbor tape) cell)
              [➜ok rn]
              )))))
