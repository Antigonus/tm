#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Conceptually this is cleaner, but it doesn't bind to Lisp's linked list.  Rather it
creates its own list form.  Perhaps this is what lists would have looked like had CLOS
been part of the original language.

Inheritance structure.


                    right-neighbor (holds a link to the right-neighbor)
                     /       ^
             left-neighbor    \
                ^           right-link-with-contents (these are the tape cells)
                |    
                |
                |
          list-tape (this is the tape header)
                ^  ^
               |   |
 list-tape-empty   list-tape-active


The list header will be a list-tape type. The header will hold links to right-bound and
left-bound.

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
  ;; Our tape header looks like a cell in that when the tape is not empty it has a right
  ;; neigibhor.  The right neigbor of the tape header is the left-bound cell of the tape, and
  ;; the left neighbor is the right-bound cell of the tape. But it differs from a cell in that
  ;; it can not be read or written.
  ;;
  ;; see src-cell/list.lisp for link types
  ;;
    (def-type tape-list (right-neighbor left-neighbor tape)())

  (def-type tape-list-abandoned (tape-list)())
  (def-type tape-list-valid (tape-list tape-valid)())

  (def-type tape-list-empty
    (
      tape-list-valid
      tape-empty
      )
    ()
    )
  (def-type tape-list-active
    (
      tape-list-valid
      tape-active
      )
    ()
    )

  (defun-typed to-abandoned ((tape tape-list)) (change-class tape 'tape-list-abandoned))
  (defun-typed to-empty     ((tape tape-list)) (change-class tape 'tape-list-empty))
  (defun-typed to-active    ((tape tape-list)) (change-class tape 'tape-list-active))

;;--------------------------------------------------------------------------------
;; init
;;
  (defun-typed init ((tape tape-list) (init null) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (cap tape)
      (to-empty tape)
      [➜ok tape]
      ))

;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun-typed left-bound ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (right-neighbor tape)] ; blist tape is also a bilink
      ))

  (defun-typed right-bound ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (left-neighbor tape)]
      ))

  (defun-typed bound ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜bad-direction (λ()(error 'bad-direction)))
        (d *right*)
        &allow-other-keys
        )
      ➜
      (cond
        ((= d *right*) (right-bound tape ➜))
        ((= d *left*) (left-bound tape ➜))
        (t [➜bad-direction])
        )))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
;;
  (defun-typed epa<tape> ((tape1 tape-list-active) (tape0 tape-list-active))
    (let(
          (left-bound-tape0  (right-neighbor tape0))
          (right-bound-tape0 (left-neighbor tape0))
          (left-bound-tape1  (right-neighbor tape1))
          )
      ;; prepend tape0
      ;;
        (connect right-bound-tape0 left-bound-tape1)
        (to-interior right-bound-tape0)
        (to-interior left-bound-tape1)

      ;; fix the header
      ;;
        (connect tape1 left-bound-tape0)

      ;; tape0 has been consumed
      ;;
        (cap-off tape0) ; so we don't have any gc problems later
        (to-abandoned tape0)
      ))

  ;; accepts a tape-list and a cell, makes the cell the new left-bound
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epa<cell> ((tape tape-list-empty) (new-cell cell-list))
    (to-active tape)
    (to-solitary new-cell) ; the new cell becomes right-bound
    (insert-between tape tape new-cell)
    )
  (defun-typed epa<cell> ((tape tape-list-active) (new-cell cell-list))
    (let(
          (c0 tape) ; note the tape header is inherited from bilink, it looks like a tape node
          (c1 (right-neighbor tape))
          )
      (to-interior c1) ; old left-bound is no longer left-bound
      (to-left-bound new-cell) ; the new cell becomes left-bound
      (insert-between c0 c1 new-cell)
      ))

  ;; accepts a tape-list and an instance, makes a new left-bound initialized with the
  ;; instance will be a problem if (cons-list tape) is shared
  (defun-typed epa<instance> ((tape tape-list-valid) instance)
    (let(
          (new-cell (make-instance 'cell-list :contents instance))
          )
      (epa<cell> tape new-cell)
      ))

  ;; accepts a tape-list and a cell, makes the cell the new right-bound
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed ◨a<cell> ((tape tape-list-empty) (new-cell cell-list))
    (epa<cell> tape new-cell)
    )
  (defun-typed ◨a<cell> ((tape tape-list-active) (new-cell cell-list))
    (let(
          (c0 (left-neighbor tape))
          (c1 tape) ; note the tape header is inherited from bilink, it looks like a tape node
          )
      (to-interior c0) ; old right-bound is no longer right-bound
      (to-right-bound new-cell) ; the new cell becomes right-bound
      (insert-between c0 c1 new-cell)
      ))

  ;; accepts a tape-list and an instance, makes a new right-bound initialized with the
  ;; instance will be a problem if (cons-list tape) is shared
  (defun-typed ◨a<instance> ((tape tape-list-valid) instance)
    (let(
          (new-cell (make-instance 'cell-list :contents instance))
          )
      (◨a<cell> tape new-cell)
      ))


  ;; removes the left-bound cell and returns it
  ;; an active tape always has a left-bound to be deleted
  ;; will be a problem if the tape is intangled, as partners will not have correct left-bound afterward
  (defun-typed epd<tape> ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c0 tape)
             (c1 (right-neighbor c0)) ; this is left-bound
             (c2 (right-neighbor c1)) ; this is the right neighbor of left-bound, possibly tape
             )
        (when (typep c1 'solitary) (to-empty tape))
        (extract c0 c1 c2)
        [➜ok c1]
        )))

  (defun-typed epd+<tape> ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (lm (left-bound tape))
            )
        (cap-off tape)
        (to-empty tape)
        [➜ok lm]
        )))

  ;; deletes right-bound, i.e. ◨-sd
  (defun-typed ep-d<tape> ((tape tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 tape)
             (c1 (left-neighbor c2)) ; this is right-bound
             (c0 (left-neighbor c1)) ; this is left neighbor of right-bound, possible tape
             )
        (when (typep c1 'solitary) (to-empty tape))
        (extract c0 c1 c2)
        [➜ok c1]
        )))

  (defun-typed d+<tape> ((tape tape-list-active) cell &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜right-bound (λ()(error 'dealloc-on-right-bound)))
        &allow-other-keys
        )
      ➜
      (d+<cell> cell
        {
          :➜ok (λ(tail)
                 (connect cell tape) ; connect up the lose tape
                 [➜ok tail]
                 )
          :➜right-bound ➜right-bound
          })
      ))

;;--------------------------------------------------------------------------------
;; length
;;
  (defun-typed length-is-one ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (right-neighbor (left-bound tape)
        {
          :➜ok (λ(cell)(declare (ignore cell))[➜∅])
          :➜right-bound ➜t
          })))

  (defun-typed length-is-two ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (right-neighbor (left-bound tape)
        {
          :➜ok (λ(cell)
                 (right-neighbor cell
                   {
                     :➜ok (λ(cell)(declare (ignore cell))[➜∅])
                     :➜right-bound ➜t
                     }))
          :➜right-bound ➜∅
          })))
