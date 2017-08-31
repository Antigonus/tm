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
  ;; a parent table is made from empty.  As there is no parent for channel zero, so we do not keep
  ;; a row in the table for channel 0, consequently we subtract one from addresses into the
  ;; table.

  ;; allocates a new channel as the child of the given parent channel
  ;; parent must be less than the new-child (assures recursive lookup always tends to zero)
  (defun get-channel (parent-table &optional (parent 0)) 
    (let(
          (new-channel (+ 2 (max<tape-array> parent-table)))
          )
      (w<tape-array> parent-table parent {:address (1- new-channel)})
      (when (≥ parent new-channel) (error 'bad-parent))
      new-channel
      ))

  ;; returns the parent for the given channel
  (defun parent (parent-table channel &optional ➜)
    (destructuring-bind
      (&key
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
  ;; Under normal circumstances this should reliably return a read value
  ;; because channel 0 always has contents, and a parent table has a parent
  ;; for every channel except channel 0.
  ;;
    (defun r<plex> (plex channel parent-table &optional ➜)
      (destructuring-bind
        (&key
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

  ;; The channel is considered to be valid if it has an entry in the parents table.  The
  ;; write might cause the plex to expand, which might lead to a parents table array copy
  ;; and replacement, which is why this is a macro.
  ;;
    (defmacro w<plex> (plex instance channel parent-table &optional ➜)
      `(destructuring-bind
         (&key
           (➜ok #'echo)
           (➜bad-channel (λ()(error 'bad-channel)))
           &allow-other-keys
           )
         ,➜
         (parent ,parent-table ,channel
           {
             :➜ok
             (λ(parent)
               (declare (ignore parent))
               (w<tape-array> ,plex ,instance ,➜)
               )
             :➜no-parent ➜bad-channel
             :➜bad-channel ➜bad-channel
             }))
      )
