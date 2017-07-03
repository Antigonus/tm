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
         (tp0 (mk 'cell-bilist 1))
         (tp1 (mk 'cell-bilist 3))
         (tp2 (mk 'cell-bilist 5))
         (tp3 (mk 'cell-bilist 7))
         (tp4 (mk 'cell-bilist 9))
         flag1 flag2
         )

    (setf (right-neighbor tp0) tp0)
    (setf (left-neighbor tp0) tp0)

    (setf (right-neighbor tp1) tp3)
    (setf (right-neighbor tp3) tp3)
 
    (setf (left-neighbor tp1) tp1)
    (setf (left-neighbor tp3) tp1)

    (place-inbetween tp0 tp0 tp4)
    (setf flag1
      (∧
        (eq (right-neighbor tp0) tp4)
        (eq (left-neighbor  tp0) tp4)
        (eq (right-neighbor tp4) tp0)
        (eq (left-neighbor  tp4) tp0)
        ))

    (place-inbetween tp1 tp3 tp2)
    (setf flag2
      (∧
        (eq (right-neighbor tp1) tp2)
        (eq (right-neighbor tp2) tp3)
        (eq (right-neighbor tp3) tp3)
        (eq (left-neighbor tp1) tp1)
        (eq (left-neighbor tp2) tp1)
        (eq (left-neighbor tp3) tp2)
        ))

    (∧
      flag1 flag2
      )

    ))
(test-hook test-cell-0)

(defun test-cell-1 ()
  (let*(
        (c0 (mk 'cell-bilist 0 {:type 'leftmost}))
        (c1 (mk 'cell-bilist 1 {:left-neighbor c0}))
        (c2 (mk 'cell-bilist 20 {:left-neighbor c1 :type 'rightmost}))
        )
    (w<cell> c1 10)
    (∧
      (= (r<cell> c1) 10)
      (=
        (right-neighbor-n c0 2
          {
            :➜ok (λ(rn)(r<cell> rn))
            :➜rightmost #'cant-happen
            })
        20
        )
      (=
        (right-neighbor-n c0 3
          {
            :➜ok #'cant-happen
            :➜rightmost (be 27)
            })
        27
        )
      (=
        (left-neighbor-n c2 2
          {
            :➜ok (λ(rn)(r<cell> rn))
            :➜leftmost #'cant-happen
            })
        0
        )
      (=
        (left-neighbor-n c2 3
          {
            :➜ok #'cant-happen
            :➜leftmost (be 29)
            })
        29
        )
      )))
(test-hook test-cell-1)

(defun test-cell-2 ()
  (let*(
        (c0 (mk 'cell-bilist 0 {:type 'leftmost}))
        (c1 (mk 'cell-bilist 1 {:left-neighbor c0}))
        (c2 (mk 'cell-bilist 20 {:left-neighbor c1 :type 'rightmost}))
        )
    (∧
      (esr<cell> c1 {:➜ok (λ(v) (= v 20)) :➜rightmost #'cant-happen})
      (= (esr<cell> c1) 20)
      (= (esr<cell> c2 {:➜ok (be 5) :➜rightmost (be 21)}) 21)
      (= (esnr<cell> c0 2 {:➜ok (λ(v)v) :➜rightmost (be 17)}) 20)
      (= (esnr<cell> c0 3 {:➜ok (λ(v)v) :➜rightmost (be 17)}) 17)
      )))
(test-hook test-cell-2)

(defun test-cell-3 ()
  (let*(
        (c0 (mk 'cell-bilist 0 {:type 'leftmost}))
        (c1 (mk 'cell-bilist 1 {:left-neighbor c0}))
        (c2 (mk 'cell-bilist 20 {:left-neighbor c1 :type 'rightmost}))
        )
    (∧
      (= (r<cell> c2) 20)
      (= (esr<cell> c0) 1)
      (= (esw<cell> c0 5) 5)
      (= (esr<cell> c0) 5)
      (= (esnr<cell> c0 2) 20)
      (= (esnw<cell> c0 2 22
           {
             :➜ok (λ(v)(+ v 1))
             :➜rightmost (λ(cell n)(declare (ignore cell n))24)
             })
        23)
      (= (esnr<cell> c0 2) 22)
      (= (esnw<cell> c0 3 33
           {
             :➜ok (λ(v)(+ v 1))
             :➜rightmost
             (λ(cell n)
               (if
                 (∧
                   (= (r<cell> cell) 22)
                   (= n 1)
                   )
                 35
                 31
                 ))})
        35)
      )))
(test-hook test-cell-3)

(defun test-cell-4 ()
  (let(
        (c0 (mk 'cell-bilist 0 {:type 'solitary}))
        (c1 (mk 'cell-bilist 1))
        (c2 (mk 'cell-bilist 2))
        (c3 (mk 'cell-bilist 3))
        flag1 flag2
        )
    (a<cell> c0 c1)
    (setf flag1
      (∧
        (typep c0 'leftmost)
        (typep c1 'rightmost)
        ))

    (a<cell> c1 c2)  
    (setf flag2
      (∧
        (typep c0 'leftmost)
        (typep c1 'interior) 
        (typep c2 'rightmost)
        ))

    (a<cell> c1 c3)
    (∧
      flag1
      flag2
      (= (esnr<cell> c0 0) 0)
      (= (esnr<cell> c0 1) 1)
      (= (esnr<cell> c0 2) 3)
      (= (esnr<cell> c0 3) 2)
      )))
(test-hook test-cell-4)

(defun test-cell-5 ()
  (let(
        (c0 (mk 'cell-bilist 0 {:type 'solitary}))
        (c1 (mk 'cell-bilist 1))
        (c2 (mk 'cell-bilist 20))
        (c3 (mk 'cell-bilist 30))
        flag1 flag2
        )
    (a<cell> c0 c1)
    (setf flag1 (∧ (typep c0 'leftmost) (typep c1 'rightmost)))

    (a<cell> c1 c2)  
    (setf flag2
      (∧
        (typep c0 'leftmost)
        (typep c1 'interior) 
        (typep c2 'rightmost)
        ))

    (-a<cell> c2 c3)
    (∧
      flag1
      flag2
      (= (esnr<cell> c0 0) 0)
      (= (esnr<cell> c0 1) 1)
      (= (esnr<cell> c0 2) 30)
      (= (esnr<cell> c0 3) 20)
      (= (r<cell> (left-neighbor-n c2 3)) 0)
      (= (left-neighbor-n c2 4 {:➜ok (be -1) :➜leftmost (be -2)}) -2)
      )))
(test-hook test-cell-5)


(defun test-cell-6 ()
  (let(
        (c0 (mk 'cell-bilist 0 {:type 'solitary}))
        (c1 (mk 'cell-bilist 1))
        (c2 (mk 'cell-bilist 2))
        (c3 (mk 'cell-bilist 3))
        flag1 flag2 flag3 flag4 flag5 flag6 flag7 flag8
        )
    (-a<cell> c0 c1)
    (setf flag1 (∧ (typep c0 'rightmost) (typep c1 'leftmost)))

    (-a<cell> c1 c2)  
    (setf flag2
      (∧
        (typep c0 'rightmost)
        (typep c1 'interior)
        (typep c2 'leftmost)
        ))

    (a<cell> c2 c3)
    (setf flag2
      (∧
        (typep c0 'rightmost)
        (typep c1 'interior)
        (typep c3 'interior)
        (typep c2 'leftmost)
        ))

    (setf flag3
      (∧
        (= (esnr<cell> c2 0) 2)
        (= (esnr<cell> c2 1) 3)
        (= (esnr<cell> c2 2) 1)
        (= (esnr<cell> c2 3) 0)
        (= (r<cell> (left-neighbor-n c0 3)) 2)
        (= (left-neighbor-n c0 4 {:➜ok (be -1) :➜leftmost (be -2)}) -2)
        ))

    (setf flag4
      (∧
        (= (-d<cell> c2 {:➜ok #'cant-happen :➜leftmost (be 33)}) 33)
        (= (r<cell> (-d<cell> c0)) 1)
        ))
    
    (setf flag5
      (∧
        (= (esnr<cell> c2 0) 2)
        (= (esnr<cell> c2 1) 3)
        (= (esnr<cell> c2 2) 0)
        (= (esnr<cell> c2 3 {:➜ok #'cant-happen :➜rightmost (be 71)}) 71)
        ))

    (setf flag6
      (∧
        (= (d<cell> c0 {:➜ok #'cant-happen :➜rightmost (be 44)}) 44)
        (= (r<cell> (d<cell> c2)) 3)
        ))

    (setf flag7
      (∧
        (= (esnr<cell> c2 0) 2)
        (= (esnr<cell> c2 1) 0)
        (= (esnr<cell> c2 3 {:➜ok #'cant-happen :➜rightmost (be 71)}) 71)
        ))

    (setf flag8
      (∧
        (= (r<cell> (d.<cell> c2)) 2)
        (typep c2 'solitary)
        ))

    (∧ flag1 flag2 flag3 flag4 flag5 flag6 flag7 flag8)
    ))
(test-hook test-cell-6)

