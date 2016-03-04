#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch

|#

(in-package #:le)

;;--------------------------------------------------------------------------------
;;  non destructively bins the objects in source list based on pred
;;  returns the two bins as values
;;
  (defun bifurcate (src pred)
    (cond
      ((¬ src) (values ∅ ∅))
      (t
        (let(
              (bin-true (mk-tm-list-0))
              (bin-false (mk-tm-list-0))
              )
          (let(
                (tm0 (mk-tm-list-0 src))
                (tm-true (mk-tm-list-0 bin-true))
                (tm-false (mk-tm-list-0 bin-false))
              )
            (labels(
                     (categorize()
                       (let(
                             (i (r tm0))
                             )
                         (if (funcall pred i)
                           (a◨ tm-true i)
                           (a◨ tm-false i)
                           )))
                       )
              (⟳ tm0 #'s #'categorize)
              (values
                (if (s bin-true) bin-true ∅)
                (if (s bin-false) bin-false ∅)
                )))))))

    (defun test-bifurcate-0 ()
      (multiple-value-bind 
        (a b) 
        (bifurcate '(1 2 3 4 5) #'oddp)
        (and
          (equal (HA a) '(1 3 5))
          (equal (HA b) '(2 4))
          )))
     (test-hook test-bifurcate-0)

