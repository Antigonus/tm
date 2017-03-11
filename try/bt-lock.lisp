;;--------------------------------------------------------------------------------
;; don't know why they call it a lock,  it is a dataflow token
;;
;;  (defsynonym bt:make-lock    make-token-arbiter)
;;  (defsynonym bt:acquire-lock request-token)
;;  (defsynonym bt:release-lock return-token)
;;
  (defun try-lock ()
    (let(
          (token-arbiter (bt:make-lock))
          )

      (print "thread 0 - acquring the token")(nl)
      (finish-output nil)
      (bt:acquire-lock token-arbiter t) ; get the token
      (print "thread 0 - we have the token, next we starting thread 1")
      (finish-output nil)

      (let(
            (thread-1 
              (bt:make-thread
                (λ()

                  (print "thread 1 - start and acquiring the token")(nl)
                  (finish-output nil)
                  (bt:acquire-lock token-arbiter t)
                  (print "thread 1 - we have the token!")(nl)
                  (finish-output nil)

                  (print "thread 1 - returning the token for redistribution")(nl)
                  (finish-output nil)
                  (bt:release-lock token-arbiter)
                  (print "thread 1 terminating")(nl)
                  (finish-output nil)

                  )))
            )

        ;; let thread 1 print its start message cleanly
        (sleep 3) 
        (finish-output nil)

        (print "thread 0 - returning the token for redistribution ..")(nl)
        (finish-output nil)
        (bt:release-lock token-arbiter)

        ;; let thread 1 print its 'got the token' message cleanly
        (sleep 3)
        (finish-output nil)

        (print "thread 0 waiting for thread 1 to terminate")(nl)
        (finish-output nil)
        (bt:join-thread thread-1)
        'done
        )))


;;--------------------------------------------------------------------------------
;;
  (defun try-lock-2 ()
    (let(
          (token-arbiter (bt:make-lock))
          )

      (print "thread 0 - acquring the token")(nl)
      (finish-output nil)

      (bt:with-lock-held (token-arbiter) ; get the token
        (print "thread 0 - we have the token, next we starting thread 1")
        (finish-output nil)

        (bt:make-thread
          (λ()
            (print "thread 1 - start and acquiring the token")(nl)
            (finish-output nil)
            (bt:acquire-lock token-arbiter t)
            (print "thread 1 - we have the token!")(nl)
            (finish-output nil)

            (print "thread 1 - returning the token for redistribution")(nl)
            (finish-output nil)
            (bt:release-lock token-arbiter)
            (print "thread 1 terminating")(nl)
            (finish-output nil)
            ))

          ;; let thread 1 print its start message cleanly
          (sleep 3) 
          (finish-output nil)

          (print "thread 0 - returning the token for redistribution ..")(nl)
          (finish-output nil)
          )

        ;; let thread 1 print its 'got the token' message cleanly
        (sleep 3)
        (finish-output nil)

        'done
        ))
