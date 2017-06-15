#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Conceptually this is cleaner, but it doesn't bind to Lisp's linked list.  Rather it
creates its own list form.  Perhaps this is what lists would have looked like had CLOS
been part of the original language.  Thus leaving this for later.  Perhaps it will bcome
list2-tape or some such.

.. bringing this back as a doubly linked list


Inheritance structure.

               bilink
                ^  ^
                |   \ 
                |    bilist-with-cargo
                |
             list-tape
               ^  ^
               |   |
 list-tape-empty   list-tape-active


Due to list-tape being inherited from link, the list-tape header can be passed to
functions that manipulate links. This simplifies end case processing.

Because list-tape-empty and list-tape-active are inherited from list-tape without the
addiition of slots, we can change-type between them without penalty.  Status information
about the list is carried in their type.

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
        :initarg right-neighbor 
        :accessor right-neighbor
        )
      ))

  (def-type cell (bilink)
    (
      (cargo :initarg cargo :accessor cargo)
      ))

  (def-type bilist-tape (link) ())

  (def-type bilist-tape-empty (bilist-tape))
  (def-type bilist-tape-active (bilist-tape))

  (defun-typed to-active ((tape bilist-tape)) (change-class tape 'bilist-tape-active))
  (defun-typed to-empty  ((tape bilist-tape)) (change-class tape 'bilist-tape-empty))

;;--------------------------------------------------------------------------------
;; initializer
;;
  (defun-typed init ((tape list-tape) (seq sequence) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let (rightmost i)
        (labels(
                 (init-1 (i)
                   (cond
                     ((≥ i (length seq))
                       ∅
                       )
                     (t
                       (let(
                             (cell (make-instance 'link-with-cargo))
                             )
                         (setf (cargo cell) (elt seq i))
                         (setf (link cell) (init-1 (1+ i)))
                         ))))
                 (init-0 () ; returns a first cons cell
                   (let(
                         (cell-0 (leftmost tape-0))
                         )
                     (cons (r<cell> cell-0) (init-1 cell-0))
                     ))
                 )
          (cond
            ((∨ (¬ seq) (= 0 (length seq)))
              (to-empty tape)
              [➜ok tape]
              )
            (t
              (setf (right-neighbor (bilink tape)) (init-1 0))
              (list-tape-to-active tape]
              [➜ok tape]
              ))
          )))
