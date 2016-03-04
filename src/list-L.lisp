#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
|#
  (in-package #:le)

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
    (error 'use-of-o-error :text "'o function only has meaning inside of L or Ln")
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

  (defun test-unwrap-0 ()
    (and
      (equal (unwrap '()) '())
      (equal (unwrap '(1)) '(1))
      (equal (unwrap '(1 2 3)) '(1 2 3))
      (equal (unwrap '((1))) '(1))
      (equal (unwrap '(1 (2 3) 4)) '(1 2 3 4))
      (equal (unwrap '((1 2) (3 (4 (5 6)) 7) 8))  '(1 2 3 (4 (5 6)) 7 8) )
      (equal (unwrap '((1 2) () (3 (4 (5 6)) 7) 8))  '(1 2 3 (4 (5 6)) 7 8) )
      ))
  (test-hook test-unwrap-0)    

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

  (defun meta-wrap (src-list)
    (cond
      ((null src-list) '(list))
      ((atom src-list) `(list ,src-list))
      (t
        (let(
              (dst (mk-tm-list-0))
              (gathered ∅)
              (src (mk-tm-list-0 src-list))
              )
          (labels(
                   (gather-object()
                     (unless gathered (setq gathered (mk-tm-list-0)))
                     (a◨ gathered (r src))
                     )
                   (append-gathered()
                     (when gathered
                       (a◨ dst (tape gathered))
                       (setq gathered ∅)
                       ))
                   (append-opened-list()
                     (d* (mk-tm-list-0 (r src)) dst) ; move the opened list directly to dst
                     )
                   (work()
                     (if
                       (to-be-opened (r src))
                       (progn
                         (append-gathered) ; close out the prior gathered objects (list a b ...)
                         (append-opened-list)
                         )
                       (gather-object)
                       )))

            (⟳ src #'s #'work #'append-gathered)
            (tape dst)
            )))))

    (defun test-meta-wrap-0 ()
      (and
        (equal (meta-wrap '(1 2 3)) '(list (list 1 2 3)))
        (equal (meta-wrap `(∅ 2 (o (a b) (c d)) 3)) '(list (list ∅ 2) (a b) (c d) (list 3)))
        (equal (meta-wrap `(∅ 2 (o x))) '(list (list ∅ 2) x))
        ))
    (test-hook test-meta-wrap-0)


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
;;  L differs from quote in two respects, firstly it is not a literal, secondly
;;  the objects in the list are evaluated before the list is returned.  Because
;;  the objects are evaluated they are forms, and because they are forms, we can
;;  have an unambiguous iterpretation for our 'o operator.
;;
  (defmacro L (&rest objects)
    (cond
      ((null objects) ∅)
      ((¬∃ (mk-tm-list-0 objects) #'le::to-be-opened)
        `(list ,@objects)
        )
      (t
        `(unwrap ,(meta-wrap objects))
        )))

;; see test-le.lisp for further tests. (sbcl has issues with macro definition and
;; use being in the same file)
  
