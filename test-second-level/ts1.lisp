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
         tm2-stolen
         flag
         )
    (with-entangled tm1
      (λ(tm2)
        (setf tm2-stolen tm2)
        (setf flag 
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
            ))))
    (∧
      (typep tm2-stolen 'status-abandoned)
      flag
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

(defun test-ts1-3 (&optional (max-retries 1000))
  (let*(
         (n 20)
         (data (loop for i from 0 to n collect i))
         (tm10 (mk 'list-solo-tm {:tape data}))
         (tm20 (mk 'list-haz-tm {:tape {∅}}))
         (tm21 (mk 'ts1-tm {:base tm20 :empty t}))
         (tm-wait-tries (mk-interval 1 max-retries))
         (tm-sum (mk-interval 1 max-retries))
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
                  (with-entangled tm21
                    (λ(tm22)

                      (prins (print "entering tm-wait-tries loop"))
                      (∀ tm-wait-tries
                        (λ(tm-retries ct c∅)
                          (declare (ignore tm-retries))
                          (if
                            (typep tm22 'status-empty)
                            (progn
                              (sleep .002)
                              [ct]
                              )
                            [c∅] ; stop waiting, tm22 is not empty
                            ))
                        (λ()(prins (print "too many retries tm-wait-tries")))
                        )

                      (prins (print "entering tm-sum loop"))
                      (∀ tm-sum
                        (λ(tm-sum ct c∅)
                          (declare (ignore tm-sum))
                          (c◧ tm22)
                          (setf actual-sum 0)
                          (∀* tm22 (λ(tm22) (setf actual-sum (+ actual-sum (r tm22)))))
                          (if
                            (≠ actual-sum expected-sum)
                            (progn
                              (sleep .003)
                              [ct]
                              )
                            [c∅]
                            ))
                          (λ()(prins (print "too many retries tm-sum")))
                          )
                    )))
                :name "t2"
                ))
          )
      (bt:join-thread t1)
      (bt:join-thread t2)
      (prins
        (print "tm-wait-tries")(princ (r tm-wait-tries))
        (print "tm-sum")(princ (r tm-sum))
        )
      (∧ 
        (= actual-sum expected-sum) 
        (> (r tm-wait-tries) 1) 
        (¬ (on-rightmost tm-wait-tries))
        (> (r tm-sum) 1)
        (¬ (on-rightmost tm-sum))
        )
      )))
(test-hook test-ts1-3)

#|

This test caught a lot of bugs during development.  Yet, the two threads are still serial.
t1 runs, sets a semphore, then t2 runs.

tm11 is a data source.  tm21 is a pipe.  tm31 is a sink.  t1 fills the pipe, t2 consumes
instances from the pipe and puts them in tm31.  The pipe is shared between threads so
it is a thread safe machine of type 'ts1'.  tm11 and tm31 are not shared, and are thus
of type 'status'.

At the end we compare tm31 with tm11. 

|#

(defun test-ts1-4-t1 (tm11 tm-pipe t1-finished-in-box)
  (prins
    (nl)(princ "filling tm-pipe from tm11")
    )
  (∀* tm11
    (λ(tm11)
      (as tm-pipe (r tm11))
      (sleep .001)
      ))
  (park tm-pipe) ;; this frees t2 to move the last instance from tm-pipe
  (prins
    (nl)(princ "tm-pipe full:")
    (nl)(tm-print tm-pipe)
    )
  (setf (unbox t1-finished-in-box) t)
  )
    
(defun test-ts1-4-t2-wait (tm-wait-tries t1-finished-in-box)
  (prins (print "tm-wait-tries for t1 to finish filling the pipe"))
  (∀ tm-wait-tries
    (λ(tm-wait-tries ct c∅)(declare (ignore tm-wait-tries)) ; not used in loop
      (if (¬ (unbox t1-finished-in-box)) 
        (progn
          (prins (print "waiting for t1"))
          (sleep .001)
          [ct]
          )
        c∅
        ))
    (λ()
      (prins (print "hit maximum tm-wait-tries"))
      )))
  
(defun test-ts1-4-t2-drain
  (
    tm-drain-pipe-tries
    tm-pipe 
    tm31
    experienced-empty-in-box
    experienced-collision-in-box
    )
  (prins (print "entering tm-drain-pipe-tries loop"))
  (∀ tm-drain-pipe-tries
    (λ(tm-drain-pipe-tries ct c∅)(declare (ignore tm-drain-pipe-tries))
      (d◧ tm-pipe tm31
        {
          :➜ok
          (λ(instance)
            (prins
              (nl)(princ "in t2 d◧ ok cont, tm-pipe and tm31 ")
              (nl)(tm-print tm-pipe)
              (nl)(tm-print tm31))
            (sleep .003)
            (if (eq instance 'end)
              (progn 
                (prins (nl)(princ "in t2 d◧ ok cont, found 'end instance, exiting"))
                [c∅]
                )
              (progn
                (prins (nl)(princ "in t2 d◧ ok cont, pulling next instance"))
                [ct]
                )))
          :➜empty
          (λ()
            (prins (nl)(princ "in t2 d◧  empty cont, will retry"))
            (setf (unbox experienced-empty-in-box) t)
            (sleep .001)
            [ct]
            )
          :➜collision
          (λ()
            (prins (nl)(princ "in t2 d◧ collision cont, will retry"))
            (setf (unbox experienced-collision-in-box) t)
            (sleep .002)
            [ct]
            )
          }))
    (λ() (prins (print "hit maximum retries while trying to drain tm-pipe")))
    ))


(defun test-ts1-4 (&optional (max-tries 20))
  (let*(
         (data (loop for i from 1 to 5 collect i))

         (tm10 (mk 'list-haz-tm {:tape (append data {'end})}))
         (tm11 (mk 'status-tm {:base tm10}))

         (tm20 (mk 'list-haz-tm {:tape {∅}}))
         (tm-pipe (mk 'ts1-tm {:base tm20 :empty t}))

         (tm30 (mk 'list-haz-tm {:tape {∅}}))
         (tm31 (mk 'status-tm {:base tm30 :empty t}))

         (t1-finished ∅)
         (tm-wait-tries (mk-interval 1 max-tries))
         (tm-drain-pipe-tries (mk-interval 1 max-tries))
         (experienced-empty ∅)
         (experienced-collision ∅)
         )

    (prins 
        (nl)
        (princ "tm11: ")
        (tm-print tm11)
        )
      
    (let(
          ;; t1 fills the pipe with data from tm11, then signals it is finished
          ;; i.e. it loads th epipe
          (t1 (bt:make-thread 
                (λ()
                  (test-ts1-4-t1 tm11 tm-pipe (box t1-finished))
                  )
                :name "t1"
                ))

          ;; after t1 is finished t2 move the data from the pipe into tm31
          ;; after t1 is done, the pipe will be empty, and tm31 will be identical to tm11
          (t2 (bt:make-thread
                (λ() ; in this thread ..
                  (test-ts1-4-t2-wait tm-wait-tries (box t1-finished))
                  (test-ts1-4-t2-drain 
                    tm-drain-pipe-tries
                    tm-pipe
                    tm31
                    (box experienced-empty)
                    (box experienced-collision)
                    )
                  )
                :name "t2"
                ))
          )
      (bt:join-thread t1)
      (bt:join-thread t2)
      )

    (prins
      (nl)(princ "ts1-4 final tm-pipe and tm31 ")
      (nl)(tm-print tm-pipe)
      (nl)(tm-print tm31)
      (print "tm-wait-tries")(princ (r tm-wait-tries))
      (print "tm-drain-pipe-tries")(princ (r tm-drain-pipe-tries))
      (if experienced-empty
        (print "experienced empty")
        (print "did not experience empty")
        )
      (if experienced-collision
        (print "experienced collision")
        (print "did not experience collision")
        ))

    (if (typep tm31 'status-empty)
      ∅
      (let(
            (tm-ensemble (mk 'ensemble-tr {:list {tm11 tm31}}))
            )
        (c◧ tm11)
        (c◧ tm31)
        (∧
          (∀ tm-ensemble
            (λ(tm ct c∅)(declare (ignore tm))
              (if (eq (r tm11) (r tm31)) [ct] [c∅])
              ))
          (on-rightmost tm11)
          (on-rightmost tm31)
          )))
  ))
  (test-hook test-ts1-4)


#|

  test-ts1-r ran t1 then t2 in order, now we run them simultaneously. t1 is slow, 
  and t2 is hungry, so we see a lot of empty and collision retries.

  Comment out the 'prins' surrounding the final print message to see the retry counter
  values.  Or run the command (turn-prins-on) to turn on the debug messages.

  Check the retry counters to make sure that the timing is set such that the threads are
  running in parallel. I set the sleeps in milliseconds, so that hopefully this test will
  not be too sensitive to the platform it is run on. Though I am seeing a variation on my
  laptop even at this course of granularity.
  
|#

(defun test-ts1-5-t1 (tm11 tm-pipe)
  (prins
    (nl)(princ "filling tm-pipe from tm11")
    )
  (sleep .003)
  (∀* tm11
    (λ(tm11)
      (as tm-pipe (r tm11))
      (sleep .0025)
      ))
  (park tm-pipe) ;; this frees t2 to move the last instance from tm-pipe
  (prins
    (nl)(princ "tm-pipe full:")
    (nl)(tm-print tm-pipe)
    )
  )

(defun test-ts1-5-t2-drain
  (
    tm-drain-pipe-tries
    tm-pipe 
    tm31
    experienced-empty-in-box
    experienced-collision-in-box
    )
  (prins (print "entering tm-drain-pipe-tries loop"))
  (∀ tm-drain-pipe-tries
    (λ(tm-drain-pipe-tries ct c∅)(declare (ignore tm-drain-pipe-tries))
      (d◧ tm-pipe tm31
        {
          :➜ok
          (λ(instance)
            (prins
              (nl)(princ "in t2 d◧ ok cont, tm-pipe and tm31 ")
              (nl)(tm-print tm-pipe)
              (nl)(tm-print tm31))
            (sleep .001)
            (if (eq instance 'end)
              (progn 
                (prins (nl)(princ "in t2 d◧ ok cont, found 'end instance, exiting"))
                [c∅]
                )
              (progn
                (prins (nl)(princ "in t2 d◧ ok cont, pulling next instance"))
                [ct]
                )))
          :➜empty
          (λ()
            (prins (nl)(princ "in t2 d◧  empty cont, will retry"))
            (setf (unbox experienced-empty-in-box) t)
            (sleep .002)
            [ct]
            )
          :➜collision
          (λ()
            (prins (nl)(princ "in t2 d◧ collision cont, will retry"))
            (setf (unbox experienced-collision-in-box) t)
            (sleep .002)
            [ct]
            )
          }))
    (λ() (prins (print "hit maximum retries while trying to drain tm-pipe")))
    ))


(defun test-ts1-5 (&optional (max-tries 2000))
  (let*(
         (data (loop for i from 1 to 5 collect i))

         (tm10 (mk 'list-haz-tm {:tape (append data {'end})}))
         (tm11 (mk 'status-tm {:base tm10}))

         (tm20 (mk 'list-haz-tm {:tape {∅}}))
         (tm-pipe (mk 'ts1-tm {:base tm20 :empty t}))

         (tm30 (mk 'list-haz-tm {:tape {∅}}))
         (tm31 (mk 'status-tm {:base tm30 :empty t}))

         (tm-drain-pipe-tries (mk-interval 1 max-tries))
         (experienced-empty ∅)
         (experienced-collision ∅)
         )

    (prins 
        (nl)
        (princ "tm11: ")
        (tm-print tm11)
        )
      
    (let(
          ;; t1 fills the pipe with data from tm11, then signals it is finished
          ;; i.e. it loads th epipe
          (t1 (bt:make-thread 
                (λ()
                  (test-ts1-5-t1 tm11 tm-pipe)
                  )
                :name "t1"
                ))

          ;; after t1 is finished t2 move the data from the pipe into tm31
          ;; after t1 is done, the pipe will be empty, and tm31 will be identical to tm11
          (t2 (bt:make-thread
                (λ() ; in this thread ..
                  (test-ts1-5-t2-drain 
                    tm-drain-pipe-tries
                    tm-pipe
                    tm31
                    (box experienced-empty)
                    (box experienced-collision)
                    )
                  )
                :name "t2"
                ))
          )
      (bt:join-thread t1)
      (bt:join-thread t2)
      )

    (prins
      (nl)(princ "ts1-5 final tm-pipe and tm31 ")
      (nl)(tm-print tm-pipe)
      (nl)(tm-print tm31)
      (print "tm-drain-pipe-tries")(princ (r tm-drain-pipe-tries))
      (if experienced-empty
        (print "experienced empty")
        (print "did not experience empty")
        )
      (if experienced-collision
        (print "experienced collision")
        (print "did not experience collision")
        ))

    (if (typep tm31 'status-empty)
      ∅
      (let(
            (tm-ensemble (mk 'ensemble-tr {:list {tm11 tm31}}))
            )
        (c◧ tm11)
        (c◧ tm31)
        (∧
          (∀ tm-ensemble
            (λ(tm ct c∅)(declare (ignore tm))
              (if (eq (r tm11) (r tm31)) [ct] [c∅])
              ))
          (on-rightmost tm11)
          (on-rightmost tm31)
          experienced-empty
          experienced-collision
          )))
  ))
  (test-hook test-ts1-5)
