#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The machine's tape has been weaved through a tree.

|#

(in-package :tm)

    (defun test-s-depth ()
      (let*(
             (a-tree '(1 (2 (3 4)) 5))
             (tm (mk-tm-depth-list a-tree))
             )
        (∧
          (= (r tm) 1)
          (eq (s-depth-ru tm (history tm)) 'so)
          (equal (r tm) '(2 (3 4)))
          (eq (s-depth-ru tm (history tm)) 'si)
          (= (r tm) 2)
          (eq (s-depth-ru tm (history tm)) 'so)
          (equal (r tm) '(3 4))
          (eq (s-depth-ru tm (history tm)) 'si)
          (= (r tm) 3)
          (eq (s-depth-ru tm (history tm)) 'so)
          (= (r tm) 4)
          (eq (s-depth-ru tm (history tm)) 'dequeue)
          (= (r tm) 5)
          (eq (s-depth-ru tm (history tm)) 'rightmost)
          )))
    (test-hook test-s-depth)

  (defun test-tm-depth-s-1 ()
    (let*(
           (a-tree '(1 (2 (3 4)) 5))
           (tm (mk-tm-depth-list a-tree))
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

   (defun test-tm-breadth-s-1 ()
     (let*(
            (a-tree '(1 (2 (3 4)) 5))
            (tm (mk-tm-breadth-list a-tree))
            )
       (and
         (= (r tm) 1)

         (s tm)
         (equal (r tm) '(2 (3 4)))

         (s tm)
         (= (r tm) 5)

         (s tm)
         (= (r tm) 2)

         (s tm)
         (equal (r tm) '(3 4))

         (s tm)
         (= (r tm) 3)

         (s tm)
         (= (r tm) 4)

         (not (s tm))
         )))
   (test-hook test-tm-breadth-s-1)

  ;; Intersting twise with tree iteration, as we see elements in subnodes twice,
  ;; First in a list returned as a value, then as a value in the list
  ;;
    (defun test-tree-quantifiers-0 ()
      (and
         (¬ (∃ (mk-tm-depth-list '(b c))                   (λ(tm)(eq 'a (r tm)))))
            (∃ (mk-tm-depth-list '(a b c))                 (λ(tm)(eq 'a (r tm))))
            (∃ (mk-tm-depth-list '(b c (1 (c (a 1)) 2) e)) (λ(tm)(eq 'a (r tm))))
        (¬ (¬∃ (mk-tm-depth-list '(b c (1 (c (a 1)) 2) e)) (λ(tm)(eq 'a (r tm)))))
           (¬∃ (mk-tm-depth-list '(b c (1 (c (q 1)) 2) e)) (λ(tm)(eq 'a (r tm))))
        ))
    (test-hook test-tree-quantifiers-0)
