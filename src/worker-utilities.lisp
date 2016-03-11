#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Useful worker functions.

|#


;;--------------------------------------------------------------------------------
;; list filter
;;
;;  Takes a source tm and a pred, bins source object into either tm-true or tm-false.
;;  The tm-false bin is optional
;;
  (def-worker bifurcate pred tm-src (tm-true &optional tm-false)
    (&optional   
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (let(
          (object (r tm-src))
          )
      (if 
        (funcall pred object)
        (as tm-true object
          (λ()(s tm-src cont-ok cont-righmost))
          cont-no-alloc
          )
        (when
          tm-false
          (as tm-true object
            (λ()(s tm-src cont-ok cont-righmost))
            cont-no-alloc
            )))))

;;--------------------------------------------------------------------------------
;; list copy
;;
;;  When a list object is moved by a program such as bifurcate, only the head
;;  of the list is copied, the cdr of that cons cell still points to the same 
;;  data as in the source list. By making a copy we get a list stucture clean of
;;  such shared list tails.
;;
  (defstruct copy-box-struct
    (src-buf (mk-stack-list))
    (dst-buf (mk-stack-list))
    )

  (def-worker copy copy-box tm-src tm-dst
    (&optional   
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )


    )

;;--------------------------------------------------------------------------------
;;  bifurcates a tree into true and false subtrees
;;
;;  note that the pred is handed lists, not atoms.
;;
;;     when a subtree is marked as true, we do not descend search into it
;;     when a subtree is marked as false, we descend to look for more true subtrees
;;
;;   subtrees that contain no true subtrees are returned as false subtrees
;;   subtress that are false, but contain true subtrees are not returned
;;
;;   thus true subtrees are the largest possible subtree for the predicate.
;;   and false subtrees are the smallest possible for the ¬predicate
;;
#|
  (defun bifurcate-tree (tree pred)
    (let(
          (true-results (list 'list)) ; a padding cell
          (false-results (list 'list))
          )
      (let(
            (true-results-rm true-results)
            (false-results-rm false-results)
            )
        (labels(
                 (bifurcate-tree-1 (tree)
                   (cond
                     ((atom tree) ∅)
                     ((funcall pred tree)
                       (ah◨ true-results-rm tree)
                       t
                       )
                     ((¬∃* tree #'bifurcate-tree-1)
                       (ah◨ false-results-rm tree)
                       ∅
                       )
                     (t t)
                     ))
                 )
          (bifurcate-tree-1 tree)
          ))
      (values (cdr true-results) (cdr false-results))
      ))
|#
