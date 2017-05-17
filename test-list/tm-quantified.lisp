#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package #:tm)

  (defun test-w*-0 ()
    (let*(
           (tm (mk 'list-tm {:tape {7 9 11}}))
           (fill (mk 'list-tm {:tape {21 23 25}}))
           )
      (∧
        (w* tm fill)
        (equal (tape tm) (tape fill))
        (on-rightmost tm)
        (on-rightmost fill)
        (◧ tm)
        (◧ fill)
        (s tm)
        (w* tm fill {:➜ok (be ∅) :➜rightmost-tm (be t)})
        )))
  (test-hook test-w*-0)

  (defun test-s*-0 ()
    (let*(
           (tm0 (mk 'list-tm {:tape {7 9 11}}))
           )
      (∧
        (on-leftmost tm0)
        (s* tm0)
        (on-rightmost tm0)
        (-s* tm0)
        (on-leftmost tm0)
        (s tm0)
        (¬ (on-leftmost tm0))
        (¬ (on-rightmost tm0))
        )))
  (test-hook test-s*-0)

  (defun test-a*-0 ()
    (let*(
           (tm (mk 'list-tm {:tape {7 9 11}}))
           (fill (mk 'list-tm {:tape {21 23 25}}))
           )
      (∧
        (a* tm fill)
        (equal (tape tm) {7 25 23 21 9 11})
        (on-leftmost tm)
        (= (r tm) 7)
        (= (r fill) 25)
        (on-rightmost fill)
        )))
  (test-hook test-a*-0)

  (defun test-as*-0 ()
    (let*(
           (tm (mk 'list-tm {:tape {7 9 11}}))
           (fill (mk 'list-tm {:tape {21 23 25}}))
           )
      (∧
        (as* tm fill)
        (equal (tape tm) {7 21 23 25 9 11})
        (= (r tm) 25)
        )))
  (test-hook test-as*-0)

  (defun test-as*-1 ()
    (let(
          (tm0 (mk 'list-tm {:tape {1 2 3}}))
          (tm1 (mk 'list-tm {:tape (q a b c)}))
          )
      (s* tm0)
      (s* tm1)
      (as* tm0 (mk 'list-tm {:tape {4 5 6}}))
      (a*  tm1 (mk 'list-tm {:tape (q e f g)}))
      (∧
        (= (r tm0) 6)
        (eq (r tm1) 'c)
        (equal (tape tm0) {1 2 3 4 5 6})
        (equal (tape tm1) (q a b c g f e))
        )))
  (test-hook test-as*-1)

  (defun test-sn-0 ()
    (let*(
           (y '(1 3 6 5))
           (tmy (mk 'list-tm {:tape y}))
           )
      (and
        (sn tmy 2
          {
            :➜ok (λ()(= (r tmy) 6))
            :➜rightmost (be ∅)
            })
        (sn tmy 27
          {
            :➜ok (be ∅)
            :➜rightmost (λ(cnt) (and (= (r tmy) 5) (= cnt 26)))
            })
        )))
  (test-hook test-sn-0)

  (defun test-sn-1 ()
    (let(
          (k0 (mk 'list-tm {:tape (list 10 11 12)}))
          (k1 (mk 'list-tm {:tape (list 13 14 15)}))
          )
      (∧
        (= (r k0) 10)
        (sn k0 1)
        (= (r k0) 11)
        (= (sn k0 22 {:➜ok (be ∅) :➜rightmost #'echo}) 21)

        (= (r k1) 13)
        (sn k1 2)
        (= (r k1) 15)
        (sn k1 1 {:➜ok (be ∅) :➜rightmost (be t)})
        )))
  (test-hook test-sn-1)

  (defun test-asn-0 ()
    (let*(
           (tm (mk 'list-tm {:tape {7 9 11}}))
           (fill (mk 'list-tm {:tape {21 23 25}}))
           )
      (∧
        (asn tm 2 fill)
        (equal (tape tm) {7 21 23 9 11})
        (= (r tm) 23)
        (= (r fill) 25)
        )))
  (test-hook test-asn-0)

  (defun test-equiv-0 ()
    (let(
          (tm0 (mk 'list-nd-tm {:tape {10 11 12}}))
          (tm1 (mk 'list-nd-tm {:tape {12 13 14}}))
          (tm2 (mk 'list-nd-tm {:tape {10 11 12}}))
          (tm3 (mk 'list-nd-tm {:tape {10 11 12 13 14}}))
          (equiv-fun
            (λ(a b ct c∅)
              (if 
                (∨
                  (∧ (oddp (r a)) (oddp (r b)))
                  (∧ (evenp (r a)) (evenp (r b)))
                  )
                [ct]
                [c∅]
                ))))
      (∧
        (equiv tm0 tm2)
        (◧ tm0)
        (◧ tm2)
        (¬ (equiv tm0 tm1))
        (◧ tm0)
        (◧ tm1)
        (¬ (equiv tm1 tm2))
        (◧ tm1)
        (◧ tm2)
        (equiv tm0 tm1 {:equiv equiv-fun})
        (◧ tm0)
        (◧ tm1)
        (= (equiv tm2 tm3 {:➜tm0 (be 5)}) 5)
        (◧ tm2)
        (◧ tm3)
        (= (equiv tm3 tm2 {:➜tm1 (be 7)}) 7)
        )))
  (test-hook test-equiv-0)
          
    
