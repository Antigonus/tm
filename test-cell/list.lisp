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

    (to-solitary tp0)
    (a<cell> tp0 tp1)
    
    (to-solitary tp2)
    (a<cell> tp2 tp3)

    (setf flag1
      (∧
        (eq (right-neighbor tp0) tp1)
        (typep tp0 'bound-left)
        (typep tp1 'bound-right)
        (eq (right-neighbor tp2) tp3)
        (typep tp2 'bound-left)
        (typep tp3 'bound-right)
        ))

    (a<cell> tp0 tp4)
    (a<cell> tp1 tp2)
    (setf flag2
      (∧
        (eq (right-neighbor tp0) tp4)
        (eq (right-neighbor tp4) tp1)
        (eq (right-neighbor tp1) tp2)
        (typep tp0 'bound-left)
        (typep tp4 'interior)
        (typep tp1 'interior)
        (typep tp2 'bound-right)
        ))

    (∧
      flag1 flag2
      )

    ))
(test-hook test-cell-list-0)


(defun test-cell-list-1 ()
  (let*(
        (c2 (mk 'cell-list 20 {:status 'bound-right}))
        (c1 (mk 'cell-list 1  {:status 'interior :right-neighbor c2}))
        (c0 (mk 'cell-list 0  {:status 'bound-left :right-neighbor c1 }))
        )
    (w c1 10)
    (∧
      (= (r<cell> c1) 10)
      (=
        (neighbor c0
          {
            :n 2
            :➜ok (λ(rn)(r<cell> rn))
            :➜bound-right #'cant-happen
            })
        20
        )
      (=
        (neighbor c0 
          {
            :n 3
            :➜ok #'cant-happen
            :➜bound-right (be 27)
            })
        27
        )
      )))
(test-hook test-cell-list-1)

(defun test-cell-list-2 ()
  (let*(
        (c2 (mk 'cell-list 20 {:status 'bound-right}))
        (c1 (mk 'cell-list 1  {:status 'interior :right-neighbor c2}))
        (c0 (mk 'cell-list 0  {:status 'bound-left :right-neighbor c1 }))
        )
    (∧
      (r c1 {:n 1 :➜ok (λ(v) (= v 20)) :➜bound-right #'cant-happen})
      (= (r c1 {:n 1}) 20)
      (= (r c2 {:n 1 :➜ok (be 5) :➜bound-right (be 21)}) 21)
      (= (r c0 {:n 2 :➜ok (λ(v)v) :➜bound-right (be 17)}) 20)
      (= (r c0 {:n 3 :➜ok (λ(v)v) :➜bound-right (be 17)}) 17)
      )))
(test-hook test-cell-list-2)

(defun test-cell-list-3 ()
  (let*(
        (c2 (mk 'cell-list 20 {:status 'bound-right}))
        (c1 (mk 'cell-list 1  {:status 'interior :right-neighbor c2}))
        (c0 (mk 'cell-list 0  {:status 'bound-left :right-neighbor c1 }))
        )
    (∧
      (= (r<cell> c2) 20)
      (= (r c0 {:n 1}) 1)
      (w c0 5 {:n 1})
      (= (r c0 {:n 1}) 5)
      (= (r c0 {:n 2}) 20)
      (= (w c0 22
           {
             :n 2
             :➜ok (be 23)
             :➜bound-right (λ(cell n)(declare (ignore cell n))24)
             })
        23)
      (= (r c0 {:n 2}) 22)
      (= (w c0 33
           {
             :n 3
             :➜ok (be 34)
             :➜bound-right
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
    (a<cell> c0 c2)
    (a<cell> c0 c1)  
    (a<cell> c2 c4)
    (a<cell> c2 c3)

    (setf flag1 
      (∧
        (= (r (d.<cell> c0)) 10) ; after d. c0 is still the bound-left, but contents is now 11
        (= (r c0) 11)
        (typep c0 'bound-left)
        ))

    (setf flag2
      (∧
        (= (r c0 {:n 0}) 11)
        (= (r c0 {:n 1}) 12)
        (= (r c0 {:n 2}) 13)
        (= (r c0 {:n 3}) 14)
        (= (r c0 {:n 4 :➜bound-right (be 199)}) 199)
        ))
      
    (setf flag3
      (∧
        (= (r (d<cell> c2)) 13) ; deletes c3
        (typep c2 'interior)
        (= (r (d<cell> c2)) 14) ; deletes c4
        (typep c2 'bound-right)
        ))

    (setf flag4
      (∧
        (= (r c0 {:n 0}) 11)
        (= (r c0 {:n 1}) 12)
        (= (r c0 {:n 2 :➜bound-right (be 122)}) 122)
        ))

    (setf flag5
      (∧
        (= (d.<cell> c2 {:➜bound-right (be 127)}) 127)
        (= (r (d<cell> c0)) 12)
        (typep c0 'solitary)
        (= (d<cell> c0 {:➜bound-right (be 129)}) 129)
        ))

    (∧
      flag1 flag2 flag3 flag4 flag5
      )
    ))
(test-hook test-cell-list-4)

