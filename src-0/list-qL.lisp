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
  (defun o (&rest instances)
    (declare (ignore instances))
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

  (defmacro q (&rest instances)
     (cond
       ((null instances) ∅)
       (t
         (meta-q instances)
         )))

;;--------------------------------------------------------------------------------
;;  unwrap
;;     given a list returns a new list
;;     new lists has one fewer level of parens from items that happen to be lists
;;       non-list instances are copied over directly
;;       top level null list instances are not included in the new list
;;
  (defun unwrap (l)
    "Given a list, returns a new list with one less level of parens per constiuent list instance."
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
;;  given a list returns a meta list
;;
;;    The meta list will be turned into a result list when evaluated.
;;
;;    The result list will have one more level of parens than the given list, except for
;;    for items in the given list that are to be opened (are lists, and their heads are
;;    'o).  
;;  
;;    The net effect when a result list is given to 'unwrap' is to bring 'o marked lists
;;    up a level.
;;
  (defun has-to-open-mark (i) (and (consp i) (eq 'o (car i))))
  (defun remove-mark (i) (cdr i)) ; i must be marked  (o 1 2 3) --> (1 2 3)
  (defun wrap (i) (cons 'list (reverse i)))

  ;; relay through items in the list marked for being opened
  (defun meta-wrap-relay (open-stuff a-list)
    (cond
      (open-stuff
        (let(
              (i (car open-stuff))
              (r (cdr open-stuff))
              )
          (cons i (meta-wrap-relay r a-list))
          ))
      (t
        (meta-wrap-gather ∅ a-list)
        )))

  ;; gathers up stuff to be wrapped in another layer of list
  (defun meta-wrap-gather (gather-list a-list)
    (cond
      ((and (not a-list)(not gather-list)) nil)
      ((not a-list) (list (wrap gather-list)))
      (t
        (let(
              (i (car a-list))
              (r (rest a-list))
              )
          (if
            (has-to-open-mark i)
            (if gather-list
              (cons (wrap gather-list) (meta-wrap-relay (remove-mark i) r))
              (meta-wrap-relay (remove-mark i) r)
              )
            (meta-wrap-gather (cons i gather-list) r)
            )))))

  (defun meta-wrap (src)
    (cons 'list (meta-wrap-gather ∅ src))
    )

;;--------------------------------------------------------------------------------
;;  L - make a list
;;
;;  Similar to 'list', though with extra functionality.  If an instance is wrapped
;;  in the 'o (oh) function, and that instance is a list, then the list is opened and
;;  the contained instances are put directly in the list being created.
;;
;;    (define a-list '(3 4))
;;
;;    (L 1 2 a-list 5)--> '(1 2 (3 4) 5)
;;    (L 1 2 (o a-list) 5) --> '(1 2 3 4 5)
;;
;;  L differs from quote in two respects, firstly it does not allocate as a literal,
;;  secondly the instances in the list are evaluated before the list is returned.  Because
;;  the instances are evaluated they are forms, and because they are forms, we can have an
;;  unambiguous iterpretation for our 'o operator.
;;
  (defmacro L (&rest instances)
    (cond
      ((¬ instances) ∅)
      ((¬ (member-if #'has-to-open-mark instances))
        `(list ,@instances)
        )
      (t
        `(unwrap ,(meta-wrap instances))
        )))

