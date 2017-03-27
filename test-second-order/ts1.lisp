#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-ts1-0 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {1 2 3}}))
         (tm1 (mk 'ts1-tm {:base tm0}))
         (tm2 (entangle tm1))
         )
    (∧
      (typep tm1 'status-active)
      (typep tm2 'status-active)
      (= (r tm1) 1)
      (= (r tm2) 1)
      (= (address tm1) 0)
      (= (address tm2) 0)
      (= (address-rightmost tm1) 2)
      (= (address-rightmost tm2) 2)
      (= (d tm1) 2)
      (= (address-rightmost tm1) 1)
      (= (address-rightmost tm2) 1)
      (s tm2)
      (= (r tm2) 3)
      (= (address tm2) 1)
      (clean-entanglements tm1)
      )))
(test-hook test-ts1-0)

(defun test-ts1-1 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {1 2 3}}))
         (tm1 (mk 'ts1-tm {:base tm0}))
         )
    (∧
      (= (address tm1) 0) 
      (= (address-rightmost tm1) 2) 
      (park tm1)
      (= (address-rightmost tm1) 2) 
      (= (d◧ tm1) 1)
      (= (address-rightmost tm1) 1) 
      (= (d◧ tm1) 2)
      (= (address-rightmost tm1) 0) 
      (= (d◧ tm1) 3)
      (typep tm1 'status-empty)
      )
    ))
(test-hook test-ts1-1)

(defun test-ts1-2 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {1 2 3}}))
         (tm1 (mk 'ts1-tm {:base tm0}))
         )
    (∧
      (typep tm0 'tape-machine)
      (typep tm1 'ts1-tm)
      (typep tm1 'status-tm)
      (typep tm1 'status-active)
      (= (address tm1) 0) 
      (= (address-rightmost tm1) 2) 
      (park tm1)
      (typep tm1 'ts1-parked)
      (typep tm1 'status-parked)
      (d tm1)
      (d tm1)
      (d tm1)
      (typep tm1 'ts1-empty)
      (typep tm1 'status-empty)
      )))
(test-hook test-ts1-2)

(defun test-ts1-3 ()
  (let*(
         (n 20)
         (data (loop for i from 0 to n collect i))
         (tm10 (mk 'list-solo-tm {:tape data}))
         (tm20 (mk 'list-haz-tm {:tape {∅}}))
         (tm21 (mk 'ts1-tm {:base tm20 :empty t}))
         retries0 retries1 (max-retries 1000)
         actual-sum
         (expected-sum  (/ (* n (+ n 1)) 2))
         )
    (let(
          (t1 (bt:make-thread 
                (λ()
                  (sleep .01)
                  (∀* tm10
                    (λ(tm10)
                      (as tm21 (r tm10))
                      (sleep .005)
                      )))
                :name "t1"
                ))
          (t2 (bt:make-thread
                (λ()
                  (let(
                        (tm22 (entangle tm21))
                        )

                    (setf retries0 0)
                    (⟳
                      (λ(again)
                        (when
                          (∧
                            (typep tm22 'status-empty)
                            (≠ retries0 max-retries)
                            )
                          (sleep .003)
                          (incf retries0)
                          [again]
                          )
                        (when (¬ (typep tm22 'status-empty)) (c◧ tm22))
                        ))

                    (setf retries1 0)
                    (⟳
                      (λ(again)
                        (c◧ tm22)
                        (setf actual-sum 0)
                        (∀* tm22 (λ(tm22) (setf actual-sum (+ actual-sum (r tm22)))))
                        (when
                          (∧
                            (≠ actual-sum expected-sum)
                            (≠ retries1 max-retries)
                            )
                          (sleep .002)
                          (incf retries1)
                          [again]
                          )))

                    ))
                :name "t2"
                ))
          )
      (bt:join-thread t1)
      (bt:join-thread t2)
;;      (print {actual-sum expected-sum {retries0 retries1}})
      (∧ 
        (= actual-sum expected-sum) 
        (> retries0 0) 
        (> retries1 0)
        (≠ retries0 max-retries)
        (≠ retries1 max-retries)
        )
      )))
(test-hook test-ts1-3)

(defun test-ts1-4 ()
  (let*(
         (data (loop for i from 1 to 5 collect i))

         (tm10 (mk 'list-haz-tm {:tape (append data {'end})}))
         (tm11 (mk 'status-tm {:base tm10}))

         (tm20 (mk 'list-haz-tm {:tape {∅}}))
         (tm21 (mk 'ts1-tm {:base tm20 :empty t}))

         (tm30 (mk 'list-haz-tm {:tape {∅}}))
         (tm31 (mk 'status-tm {:base tm30 :empty t}))

         (t1-finished ∅)
         retries-next retries-empty retries-collision (max-retries 10)
         )

    (prins 
        (nl)
        (princ "tm11: ")
        (tm-print tm11)
        )
      
    (let(
          ;; thread t1 copies instance references from t11 to t21
          (t1 (bt:make-thread 
                (λ()
                  (sleep .01)
                  (∀* tm11
                    (λ(tm11)
                      (as tm21 (r tm11))
                      (sleep .005)
                      ))
                  (park tm21) ;; this frees t2 to move the last instance from tm21
                  (setf t1-finished t)
                  )
                :name "t1"
                ))

          ;; thread t2 moves references from t21 to  tm31
          ;; t21 will be empty when t2 is done
          (t2 (bt:make-thread
                (λ() ; in this thread ..
                  (setf retries-next 0)
                  (setf retries-empty 0)
                  (setf retries-collision 0)
                  (⟳
                    (λ(again)

                      (when (¬ t1-finished) 
                        (prins (print "waiting for t1"))
                        (sleep .001)
                        [again]
                        )
                      (prins 
                        (print "in t2 before the d◧, tm21 and tm31 ")
                        (nl)
                        (tm-print tm21)
                        (tm-print tm31)
                        (print "in t2 before the d◧, address rightmost tm21 and tm31 ")
                        (princ (address-rightmost tm21))
                        (princ " ")
                        (princ (address-rightmost tm31))
                        (nl)
                        )

                      (d◧ tm21 tm31
                        {
                          :➜ok (λ(instance)
                                 (prins
                                   (print "in t2 d◧ ok cont, tm21 and tm31 ")
                                   (nl)
                                   (tm-print tm21)
                                   (tm-print tm31))
                                 (sleep .003)
                                 (if (eq instance 'end)
                                   (prins
                                     (print "in t2 d◧ ok cont, found 'end instance, exiting")
                                     )
                                   (if (≠ max-retries retries-next)
                                     (progn
                                       (prins (print "in t2 d◧ ok cont, pulling next instance"))
                                       (incf retries-next)
                                       [again]
                                       )
                                     (prins (print "in t2 d◧ ok cont, pulled all the data"))
                                     )))

                          :➜empty (λ()
                                    (prins (print "in t2 d◧ empty cont"))
                                    (sleep .001)
                                    (if (≠ max-retries retries-empty)
                                      (progn
                                        (prins (print "in t2 d◧  empty cont, will retry"))
                                        (incf retries-empty)
                                        [again]
                                        )
                                      (prins (print "in t2 d◧ empty cont, max retries giving up"))
                                      ))
                          :➜collision (λ()
                                        (sleep .002)
                                        (prins (print "in t2 collision cont"))
                                        (if (≠ max-retries retries-collision)
                                          (progn
                                            (prins (print "in t2 d◧ collision cont, will retry"))
                                            (incf retries-collision)
                                            [again]
                                            )
                                          (prins (print "in t2 d◧ collision cont, max retries giving up"))
                                          ))
                          }))))
                  :name "t2"
                  ))
          )
      (bt:join-thread t1)
      (bt:join-thread t2)

      (print "final tm21 and tm31:")
      (nl)
      (tm-print tm21)
      (tm-print tm31)
      (print {
           "max-retries: " max-retries 
           "retries-next: " retries-next
           "retries-empty: " retries-empty
           "retries-collision: " retries-collision 
           })
      (finish-output nil)
      )))

(test-hook test-ts1-4)

