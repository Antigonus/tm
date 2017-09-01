#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

When a Turing machine is allowed to proceed forward via multiple control arcs, we say that
the machine becomes 'multiplexed'.  Each branch of control proceeds forward with its own
version of the tape.  In the tape machine library we have 'tape machines' rather than
Turing Machines.  With tape machines control is provided by the program.  We accomplish
multiplexing by calling the function #'fork.

This terminology of mulitplexing comes from communications theory.  In communcations
theory a given channel may be divided into multiple channels by using techniques of
frequency division or time division multiplexing.  We typically identify the multiple
multiplexed channels with natural numbers.  Hence, channel 0, channel 1, etc.

Here in this file we have a datatype for what was a single instance during simplex
execuction, but is now broken into multiple instances due to the multiplexed execution.  I
call such a group of instances a 'plex'. Here a plex is built over a tape array.

When no instance is available for reading on a given channel, that means no write has been
done on that channel since the fork, and thus data must be found on the parent channel.
This might fall through recursively until reaching channel 0, which must have a value.

Hence, each mutiplexed tape machine has a parent table.  Here the parent table is
implemented from a tape-array.  Reading the parent table using a channel number as an
address results in the parent channel number, for all channels except for channel 0, which
has no parent.

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; plex
;;
  ;; a plex is made from empty

;;--------------------------------------------------------------------------------
;; parent table
;;
  ;; A parent table is made from empty.  Because there is no parent for channel zero we
  ;; subtract one from table addresses, and knock the zero row out of the table.

  ;; allocates a new channel as the child of the given parent channel
  ;; parent must be less than the new-child (assures recursive lookup always tends to zero)
  ;; adds an entry in the parent table for the new channel
  (defmacro get-channel (parent-table &optional (parent 0)) 
    `(max<tape-array> ,parent-table
       {
         :➜empty
         (λ()
           (w<tape-array> ,parent-table ,parent)
           1 ; channel 1
           )

         :➜ok
         (λ(max)
           (w<tape-array> ,parent-table ,parent {:address (1+ max)})
           (+ 2 max) ; because channel numbers have one subtracted from them before accessing the table
           )
         }))

  ;; returns the parent for the given channel
  (defun parent (parent-table channel &optional ➜)
    (destructuring-bind
      (&key
        ;; ok continuation passed through to r<tape-array> via (o ➜)
        (➜no-parent (λ()(error 'no-parent)))
        (➜bad-channel (λ()(error 'bad-channel)))
        &allow-other-keys
        )
      ➜
      (cond
        ((= channel 0) [➜no-parent])
        ((> (1- channel) (max<tape-array> parent-table)) [➜bad-channel])
        (t
          (r<tape-array> parent-table {:address (1- channel) (o ➜)})
          ))
      ))

;;--------------------------------------------------------------------------------
;; plex
;;
  ;; reading a plex can not modify the underlying tape topology, so this need not be a macro
  ;;
    (defun r<plex> (plex &optional ➜)
      (destructuring-bind
        (&key
          (channel 0)
          parent-table ; not needed on channel 0
          (➜ok #'echo)
          (➜bad-channel (λ()(error 'bad-channel)))
          (➜empty #'accessed-empty)
          &allow-other-keys
          )
        ➜
        (⟳(λ(➜again)
            (r<tape-array> plex
              {
                :address channel
                :➜ok ➜ok
                :➜empty
                (λ()
                  (if
                    (= channel 0)
                    [➜empty]
                    (parent parent-table channel
                      {
                        :➜ok
                        (λ(parent)
                          (setf channel parent)
                          [➜again]
                          )
                        :➜no-parent #'cant-happen ; we just removed the case of channel=0
                        :➜bad-channel ➜bad-channel
                        })))
                })
            ))))

  ;; Writing the plex with an channel beyond the end of the plex causes the plex to expand.
  ;; We provide an iota of protection against a bad channel number causing a huge expansion
  ;; by checking that the channel number is valid.  The channel is considered to be valid
  ;; if it is zero, or if it has an entry in the parents table.
  ;;
  ;; I was tempted to have the parent look up return a bool, but was concerned about a continuation
  ;; I don't know about returning a value, and that being interpreted as a bool.
  ;;
    (defmacro w<plex> (plex instance &optional ➜)
      `(destructuring-bind
         (&key
           (channel 0)
           parent-table ; not needed on channel 0
           (➜ok #'echo)
           (➜bad-channel (λ()(error 'bad-channel)))
           (➜alloc-fail #'alloc-fail)
           &allow-other-keys
           )
         ,➜

         (parent parent-table channel ; this is called just as a channel validity test
           {
             :➜ok ; then channel has a parent so it is valid
             (λ(parent)
               (declare (ignore parent))
               (w<tape-array> ,plex ,instance
                 {
                   :address channel
                   :➜ok ➜ok
                   :➜alloc-fail ➜alloc-fail
                   })
               [➜ok]
               )

             :➜no-parent ; only channel 0 has no parent
             (λ()
               (when (≠ channel 0) [➜bad-channel]); optimizer should remove this
               (w<tape-array> ,plex ,instance
                 {
                   :address channel
                   :➜ok ➜ok
                   :➜alloc-fail ➜alloc-fail
                   })
               [➜ok]
               )

             :➜bad-channel ➜bad-channel
             })))

