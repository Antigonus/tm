#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-cell-list-0 ()
  (let*(
         (tp0 (mk 'cell-list 1))
         (tp1 (mk 'cell-list 3))
         (tp2 (mk 'cell-list 5))
         (tp3 (mk 'cell-list 7))
         (tp4 (mk 'cell-list 9))
         flag1 flag2
         )

    (to-leftmost tp0)
    (connect tp0 tp1)
    (to-rightmost tp1)
    
    (to-leftmost tp2)
    (connect tp2 tp3)
    (to-rightmost tp3)

    (setf (right-neighbor tp4) tp4)
 
    (setf flag1
      (∧
        (eq (right-neighbor tp0) tp1)
        (eq (right-neighbor tp2) tp3)
        ))

    (a<cell> tp1 tp2)
    (a<cell> tp3 tp4)
    (setf flag2
      (∧
        (eq (right-neighbor tp1) tp2)
        (eq (right-neighbor tp2) tp3)
        (eq (right-neighbor tp3) tp4)
        ))

    (∧
      flag1 flag2
      )

    ))
(test-hook test-cell-list-0)


(defun test-cell-list-1 ()
  (let*(
        (c2 (mk 'cell-list 20 {:status 'rightmost}))
        (c1 (mk 'cell-list 1  {:status 'interior :right-neighbor c2}))
        (c0 (mk 'cell-list 0  {:status 'leftmost :right-neighbor c1 }))
        )
    (w c1 10)
    (∧
      (= (r c1) 10)
      (=
        (neighbor c0
          {
            :distance 2
            :➜ok (λ(rn)(r rn))
            :➜rightmost #'cant-happen
            })
        20
        )
      (=
        (neighbor c0 
          {
            :distance 3
            :➜ok #'cant-happen
            :➜rightmost (be 27)
            })
        27
        )
      )))
(test-hook test-cell-list-1)

(defun test-cell-list-2 ()
  (let*(
        (c2 (mk 'cell-list 20 {:status 'rightmost}))
        (c1 (mk 'cell-list 1  {:status 'interior :right-neighbor c2}))
        (c0 (mk 'cell-list 0  {:status 'leftmost :right-neighbor c1 }))
        )
    (∧
      (esr c1 {:➜ok (λ(v) (= v 20)) :➜rightmost #'cant-happen})
      (= (esr c1) 20)
      (= (esr c2 {:➜ok (be 5) :➜rightmost (be 21)}) 21)
      (= (esr c0 {:distance 2 :➜ok (λ(v)v) :➜rightmost (be 17)}) 20)
      (= (esr c0 {:distance 3 :➜ok (λ(v)v) :➜rightmost (be 17)}) 17)
      )))
(test-hook test-cell-list-2)

(defun test-cell-list-3 ()
  (let*(
        (c2 (mk 'cell-list 20 {:status 'rightmost}))
        (c1 (mk 'cell-list 1  {:status 'interior :right-neighbor c2}))
        (c0 (mk 'cell-list 0  {:status 'leftmost :right-neighbor c1 }))
        )
    (∧
      (= (r c2) 20)
      (= (esr c0) 1)
      (esw c0 5)
      (= (esr c0) 5)
      (= (esr c0 {:distance 2}) 20)
      (= (esw c0 22
           {
             :distance 2
             :➜ok (be 23)
             :➜rightmost (λ(cell n)(declare (ignore cell n))24)
             })
        23)
      (= (esr c0 {:distance 2}) 22)
      (= (esw c0 33
           {
             :distance 3
             :➜ok (be 34)
             :➜rightmost
             (λ(cell n)
               (if
                 (∧
                   (= (r cell) 22)
                   (= n 1)
                   )
                 35
                 31
                 ))})
        35)
      )))
(test-hook test-cell-list-3)

(defun test-cell-list-4 ()
  (let(
        (c0 (mk 'cell-list 10 {:status 'solitary}))
        (c1 (mk 'cell-list 11))
        (c2 (mk 'cell-list 12))
        (c3 (mk 'cell-list 13))
        (c4 (mk 'cell-list 14))
        flag1 flag2 flag3 flag4 flag5
        )
    (a<cell> c0 c1)
    (a<cell> c1 c2)  
    (a<cell> c2 c3)
    (a<cell> c3 c4)

    (setf flag1 
      (∧
        (= (r (d.<cell> c0)) 10) ; after d. c0 is still the leftmost, but contents is now 11
        (= (r c0) 11)
        (typep c0 'leftmost)
        ))

    (setf flag2
      (∧
        (= (esr c0 {:distance 0}) 11)
        (= (esr c0 {:distance 1}) 12)
        (= (esr c0 {:distance 2}) 13)
        (= (esr c0 {:distance 3}) 14)
        (= (esr c0 {:distance 4 :➜rightmost (be 199)}) 199)
        ))
      
    (setf flag3
      (∧
        (= (r (d<cell> c2)) 13) ; deletes c3
        (typep c2 'interior)
        (= (r (d<cell> c2)) 14) ; deletes c4
        (typep c2 'rightmost)
        ))

    (setf flag4
      (∧
        (= (esr c0 {:distance 0}) 11)
        (= (esr c0 {:distance 1}) 12)
        (= (esr c0 {:distance 2 :➜rightmost (be 122)}) 122)
        ))

    (setf flag5
      (∧
        (= (d.<cell> c2 {:➜rightmost (be 127)}) 127)
        (= (r (d<cell> c0)) 12)
        (typep c0 'solitary)
        (= (d<cell> c0 {:➜rightmost (be 129)}) 129)
        ))

    (∧
      flag1 flag2 flag3 flag4 flag5
      )
    ))
(test-hook test-cell-list-4)

