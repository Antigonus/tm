#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The machine's tape has been weaved through a tree.

|#

(in-package #:tm)

(defun test-s-depth-0 ()
  (let*(
         (a-tree (mount {1 {2 {3 4}} 5}))
         (tm (tm-mk 'tm-depth a-tree))
         )
    (∧
      (= (r tm) 1)
      (eq (s-depth-ru (tape tm) (HA tm)) 's)
      (equal (r tm) '(2 (3 4)))
      (eq (s-depth-ru (tape tm) (HA tm)) 'si)
      (= (r tm) 2)
      (eq (s-depth-ru (tape tm) (HA tm)) 's)
      (equal (r tm) '(3 4))
      (eq (s-depth-ru (tape tm) (HA tm)) 'si)
      (= (r tm) 3)
      (eq (s-depth-ru (tape tm) (HA tm)) 's)
      (= (r tm) 4)
      (eq (s-depth-ru (tape tm) (HA tm)) 'dequeue)
      (= (r tm) 5)
      (eq (s-depth-ru (tape tm) (HA tm)) 'rightmost)
      )))
(test-hook test-s-depth-0)

(defun test-tm-depth-s-1 ()
  (let*(
         (a-tree (mount {1 {2 {3 4}} 5}))
         (tm (tm-mk 'tm-depth a-tree))
         )
    (∧
      (= (r tm) 1)
      (s tm)
      (equal (r tm) '(2 (3 4)))
      (s tm)
      (= (r tm) 2)
      (s tm)
      (equal (r tm) '(3 4))
      (s tm)
      (= (r tm) 3)
      (s tm)
      (= (r tm) 4)
      (s tm)
      (= (r tm) 5)
      (not (s tm))
      )))
(test-hook test-tm-depth-s-1)

;; Intersting twise with tree iteration, as we see elements in subnodes twice,
;; First in a list returned as a value, then as a value in the list
;;
(defun test-tree-quantifiers-0 ()
  (let(
        (a0 (tm-mk 'tm-depth (mount [b c])))
        (a1 (tm-mk 'tm-depth (mount [a b c])))
        (a2 (tm-mk 'tm-depth (mount [b c (1 (c (a 1)) 2) e])))
        (a3 (tm-mk 'tm-depth (mount [b c (1 (c (a 1)) 2) e])))
        (a4 (tm-mk 'tm-depth (mount [b c (1 (c (q 1)) 2) e])))
        )

  (and
    (¬ ( ∃ a0 (λ(tm)(eq 'a (r tm)))))
       ( ∃ a1 (λ(tm)(eq 'a (r tm))))
       ( ∃ a2 (λ(tm)(eq 'a (r tm))))
    (¬ (¬∃ a3 (λ(tm)(eq 'a (r tm)))))
       (¬∃ a4 (λ(tm)(eq 'a (r tm))))
    )))
(test-hook test-tree-quantifiers-0)
