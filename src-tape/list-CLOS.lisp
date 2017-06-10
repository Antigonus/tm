#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Conceptually this is cleaner, but it doesn't bind to Lisp's linked list, but
rather creates its own list form.  Perhaps this is what lists would have looked
like had CLOS been part of the original language.  Thus leaving this for later.
Perhaps it will bcome list2-tape or some such.


Inheritance structure.

                link
                ^  ^
                |   \ 
                |    list-with-cargo
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
  (def-type link ()
    (
      (target :initarg target :accessor target) ; the link target
      ))

  (def-type link-with-cargo (link)
    (
      (cargo :initarg cargo :accessor cargo)
      ))

  (def-type list-tape (link) ())

  (def-type list-tape-empty (list-tape))
  (def-type list-tape-active (list-tape))

  (defun-typed list-tape-to-active ((tape list-tape)) (change-class tape 'list-tape-active))
  (defun-typed list-tape-to-empty  ((tape list-tape)) (change-class tape 'list-tape-empty))

  (def-type list-cell (cell)
    (
      (a-link-with-cargo :initarg :a-link-with-cargo :accessor a-link-with-cargo)
      ))

;;--------------------------------------------------------------------------------
;; initializer
;;
  (defun-typed init ((tape list-tape) (init null) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (list-tape-to-empty tape)
      [➜ok tape]
      )

  (defun-typed init ((tape list-tape) (seq sequence) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
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
             )
      (cond
        ((= 0 (length seq))
          (list-tape-to-empty tape)
          [➜ok tape]
          )
        (t
          (setf (link tape) (init-1 0))
          (list-tape-to-active tape]
          [➜ok tape]
          ))
      )))
