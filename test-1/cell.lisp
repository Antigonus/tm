#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-cell-0 ()
  (let*(
         (tp0 (mk 'cell {:value 1}))
         (tp1 (mk 'cell {:value 3}))
         (tp2 (mk 'cell {:value 5}))
         )

    (w<neighbors> tp0 tp1)
    (w<neighbors> tp0 ∅ {:address *left})

    (w<neighbors> tp1 tp2)
    (w<neighbors> tp1 tp0 {:address *left})

    (w<neighbors> tp2 ∅)
    (w<neighbors> tp2 tp1 {:address *left})

    (∧
      (eq (r<neighbors> tp0) tp1)
      (eq (r<neighbors> tp1) tp2)
      (= (r<neighbors> tp2 {:➜empty (be 27)}) 27)

      (eq (r<neighbors> (r<neighbors> tp0)) tp2)
      (= (r<neighbors> (r<neighbors> (r<neighbors> tp0)) {:➜empty (be 29)}) 29)

      (eq (r<neighbors> tp1 {:address *left}) tp0)
      (= (r<neighbors> (r<neighbors> tp1 {:address *left}) {:address *left :➜empty (be 33)}) 33)

      (eq (r<neighbors> (r<neighbors> tp2 {:address *left}) {:address *left}) tp0)
      (= (r<neighbors>
           (r<neighbors>
             (r<neighbors> tp2 {:address *left})
             {:address *left})
           {:address *left :➜empty (be 31)}
           )
        31
        )

      (= (r<content> tp0) 1)
      (= (r<content> tp1) 3)
      (= (r<content> tp2) 5)

      (w<content> tp0 7)
      (w<content> tp1 9)
      (w<content> tp2 11)

      (= (r<content> tp0) 7)
      (= (r<content> tp1) 9)
      (= (r<content> tp2) 11)
    )))
(test-hook test-cell-0)
