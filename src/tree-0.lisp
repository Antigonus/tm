#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt
  
|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; tree quantifiers
;;
;;
  ;; these should work via virtue of having defined #'s on a depth first traversal
  (defun test-tree-quantifiers-0 ()
    (and
       (not (∃ (mk-tm-depth-list-0 '(b c)))                   (λ(tm)(eq 'a (r tm)))))
            (∃ (mk-tm-depth-list-0 '(a b c)))                 (λ(tm)(eq 'a (r tm))))
            (∃ (mk-tm-depth-list-0 '(b c (1 (c (a 1)) 2) e))) (λ(tm)(eq 'a (r tm))))
      (not (¬∃ (mk-tm-depth-list-0 '(b c (1 (c (a 1)) 2) e))) (λ(tm)(eq 'a (r tm)))))
           (¬∃ (mk-tm-depth-list-0 '(b c (1 (c (q 1)) 2) e))) (λ(tm)(eq 'a (r tm))))
      ))
  (test-hook test-tree-quantifiers-0)


;;--------------------------------------------------------------------------------
;;  map-tree
;;
;;  given an input tree, builds an output tree
;;
;;  op is given an object, and returns one or two values, a filtered object and optionally
;;  a command.  command includes, ∅, which causes the result to be copied to the output
;;  tree, 'drop, 'forward-contents, and 'push-back-contents.
;;
;;  If op simply echos every object, then the tree will be copied.
;;
;;  After each step of the source tree, there is a new node to read. So we write
;;  it into the destination tree.   When the source tree stacks a traversal point
;;  to return to, the dest tree stacks an attachment point to build from.  Once
;;  the build is finished, the tape is turned into a list, and given as an object
;;  to the cell in the higher up machine.
;;

;; add copy of history for tm-tree cue-to

   (defmethod a-map
     (
       (tm-src tm-depth)
       (tm-dst tm-tree)
       &optional
       (op #'echo)
       (cont-ok (be t))
       (cont-rightmost (be ∅))
       )

     )


  (defun test-map-step-2 ()
    (let*(
           (tm0 (mk-tm-depth-list-0 '(1 (2 3) 4)))
           (tm1 (mk-tm-depth-list-0))
           )
      (∧
        (equal (tape tm1) '(list))
        (map-step tm0 tm1 #'echo)
        (equal (tape tm1) '(list 1))
        (map-step tm0 tm1 #'echo) 
        )))


   (defmethod map-step
     (
       (tm-src tm-breadth)
       (tm-dst tm-tree)
       &optional
       (op #'echo)
       (cont-ok (be t))
       (cont-rightmost (be ∅))
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
  need to up date to use tms

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

    (defun test-bifurcate-tree-0 ()
      (multiple-value-bind (a b) 
        (bifurcate-tree 
          (list 
            2 
            (list
              3
              2
              (list 1 7)
              (list 10 12)
              ) 
            (list 1 5) 
            (list 2 4 6)
            )
          (λ(i)(eql (car i) 1))
          )
        (equal
          (list a b)
          '( ((1 7)(1 5)) ((10 12)(2 4 6)))
          )))
    (test-hook test-bifurcate-tree-0)


|#
