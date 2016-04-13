#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
  (in-package #:tm)

;;--------------------------------------------------------------------------------
;; o
;;   This function's call arguments are to be placed into the parent list.  I.e.
;;   "open" this list and insert its contents.
;;
;;   Calls to o are recognized by #'meta-unwrap, which opens up the o list at 
;;   
;;   -- reserves use of this function for L (L macro expands it out but ...)
;;  
  (defun o (&rest objects)
    (declare (ignore objects))
    (error 'use-of-o :text "'o function only has meaning inside of L or Ln")
    )

;;--------------------------------------------------------------------------------
;;  q - makes a quoted list that is not a literal
;;

  (defun meta-q (e)
    (cond
      ((atom e) (list 'quote e))
      (t
        (cons 'list (mapcar #'meta-q e )))
        ))

  (defmacro q (&rest objects)
     (cond
       ((null objects) ∅)
       (t
         (meta-q objects)
         )))

;;--------------------------------------------------------------------------------
;;  unwrap
;;     given a list returns a new list
;;     removes one level of parens from objects that happen to be lists
;;       non-list objects are copied over directly
;;       top level null list objects are not included in the new list
;;
  (defun unwrap (l)
    "Given a list, returns a new list with one less level of parens per constiuent list object."
    (cond
      ((null l) '())
      (t
        (let(
              (i (car l))
              (r (cdr l))
              )
          (cond
            ((null i) (unwrap r))
            ((not (consp i)) (cons i (unwrap r)))
            (t (append i (unwrap r)))
            )))
     ))

;;--------------------------------------------------------------------------------
;; meta-wrap
;;   given a src list, returns a dst list
;;   non-destructive to src list
;;
;;   meta-wrap is intended to be used inside of macros where the items in the src-list are
;;   to eventually be evalued, but haven't been yet.  Hence list heads are function calls,
;;   but those functions have not yet been called.
;;
;;   'o as a list head is treated specially. It is used to mark a list of items which
;;   actually belong to the parent list.  'o is in the function channel, but in fact we
;;   process it in meta-wrap, so it is gone before eval sees it.  Hence, when meta-wrap is
;;   used properly, 'o is in the function channel, and it can not alias with data.
;;   (Pure data will be quoted, so 'quote will be in the function channel.)
;;
;;   meta-wrap makes all objects not to be opened into meta list objects.  A meta list is
;;   one that starts with a literal 'list.  ... Rather than wrapping every element in a
;;   metalist, we gather successive elements to be wrapped, then wrap them all at once.
;;   Hence,
;;
;;     (defparam a '(4 5))
;;     (meta-wrap '(1 2 3) (o a)) --> '(list (list 1 2 3) a).
;;
;;   Here the inner list, i.e. (list 1 2 3), shows that 1 2 and 3 were gathered up 
;;   rather than wrapped individually.  In the prior version of meta-wrap we did 
;;   actually generate '(list (list 1) (list 2) (list 3), but that made for more work
;;   for unwrap. 
;;
;;   To complete the example above, When unwrap is called at run time, on the output of
;;   meta-wrap as a macro return value, we get:
;;
;;   (unwrap (LIST (LIST 1 2 3) A)) -->  (1 2 3 4 5)
;;
;;
  (defun to-be-opened (i) (and (consp i) (eq 'o (car i))))

  (defun meta-wrap (src)
    (cond
      ((null src) '(list))
      ((atom src) `(list ,src))
      (t
        (let(
              (dst (list 'list))
              (gathered ∅)
              )
          (let(
                (dst◨ dst)
                (src◨ src)
                (gathered◨ ∅)
                )
            (labels(
                     (gather-object()
                       (unless
                         gathered 
                         (setq gathered (list 'list))
                         (setq gathered◨ gathered)
                         )
                       (let(
                             (new-cell (cons (car src◨) ∅))
                             )
                         (rplacd gathered◨ new-cell)
                         (setq gathered◨ new-cell)
                         ))
                     (append-gathered(); append current gathered list to dst, reset gathered
                       (when gathered
                         (let(
                               (new-cell (cons gathered ∅))
                               )
                           (rplacd dst◨ new-cell)
                           (setq dst◨ new-cell)
                           )
                         (setq gathered ∅)
                         ))
                     (append-opened-list()
                       (rplacd dst◨ (cdar src◨)) ; d dropps the 'o
                       (loop ; seek rightmost
                         (unless (cdr dst◨) (return))
                         (setq dst◨ (cdr dst◨))
                         ))
                     (work()
                       (if
                         (to-be-opened (car src◨))
                         (progn
                           (append-gathered)
                           (append-opened-list)
                           )
                         (gather-object)
                         ))
                     (work-loop()
                       (work)
                       (setq src◨ (cdr src◨))
                       (when src◨  (work-loop))
                       )
                     )

              (work-loop)
              (append-gathered)
              dst
              ))))))


;;--------------------------------------------------------------------------------
;;  L - make a list
;;
;;  Similar to 'list', though with extra functionality.  If an object is wrapped
;;  in the 'o (oh) function, and that object is a list, then the list is opened and
;;  the contained objects are put directly in the list being created.
;;
;;    (define a-list '(3 4))
;;
;;    (L 1 2 a-list 5)--> '(1 2 (3 4) 5)
;;    (L 1 2 (o a-list) 5) --> '(1 2 3 4 5)
;;
;;  L differs from quote in two respects, firstly it does not allocate as a literal,
;;  secondly the objects in the list are evaluated before the list is returned.  Because
;;  the objects are evaluated they are forms, and because they are forms, we can have an
;;  unambiguous iterpretation for our 'o operator.
;;
  (defmacro L (&rest objects)
    (cond
      ((¬ objects) ∅)
      ((¬ (member-if #'to-be-opened objects))
        `(list ,@objects)
        )
      (t
        `(unwrap ,(meta-wrap objects))
        )))

