#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Interpret a natural number as a sequence of fixed length tiles.  One might think of
  tiles as digits in a number, where those digits must have a base that is an integral
  power of 2.  We call them tiles because we they structurally placing bits into
  adjacent groups.

  Does not allow topology modification, but considering adding it as moving
  bits in a number is a reasonably high performance method for emulating
  topology modification.  (And after all, all topology modification on standard
  computers is emulated.)

  Can be customerized through parameters:
     1. :length-tile sets the length of a tile.

  The natural is least significant tile at leftmost, then extending rightward.  
  This is the opposite direction that we are accustomed to writing natural numbers.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type cell-tiled-natural (cell)
    (
      (tape
        :initarg :tape
        :accessor tape
        )
      (bit-dex
        :initarg :bit-dex
        :accessor bit-dex
        )
      ))

  ;; this is potentially shared by many tape-tiled-naturals
  ;; stuff one would like to see compiled away, would like to put in sym table instead
  (def-type tape-tiled-natural-parms ()
    (
      (name ; name for the parms
        :initarg :name
        :accessor name
        )
      (length-tile
        :initarg :length-tile
        :accessor length-tile
        )
      ))
  (defparameter *tape-tiled-natural-parms-byte*
    (make-instance 'tape-tiled-natural-parms
      :name 'tape-tiled-natural-parms-byte
      :length-tile 8
      ))

  (def-type tape-tiled-natural (tape)
    (
      (natural
        :initarg :natural
        :accessor natural
        )
      (parms ;; this should be a field in the symbol table rather than being 'introspective'
        :initarg :parms
        :accessor parms
        )
      ))

  (def-type tape-tiled-natural-active (tape-tiled-natural tape-active)())
  (def-type tape-tiled-natural-empty (tape-tiled-natural tape-empty)())
  (defun-typed to-active ((tape tape-tiled-natural)) (change-class tape 'tape-tiled-natural-active))
  (defun-typed to-empty  ((tape tape-tiled-natural)) (change-class tape 'tape-tiled-natural-empty))

  ;; reads a tile
  ;; base is the bit address of the first bit in the tile
  (defun read-tiled-natural (tape base)
    (let(
          (length-tile (length-tile (parms tape)))
          )
      (ldb (byte length-tile base) (natural tape))    
      ))

  ;; .. would like a have a more efficient implementation..
  ;; writes a tile
  (defun write-tiled-natural (tape base instance)
    (let(
          (length-tile (length-tile (parms tape)))
          )
      (setf (natural tape)
        (dpb instance (byte length-tile base) (natural tape))    
        )))

  (defun-typed init ((tape tape-tiled-natural) (seq sequence) &optional ➜)
    (destructuring-bind
      (&key
        ;; parms
        length-tile
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (setf (natural tape) 0)
      (cond
        (length-tile
          (setf 
            (parms tape)
            (make-instance
              'tape-tiled-natural-parms
              :name 'local
              :length-tile length-tile
              )))
        (t
          (setf (parms tape) *tape-tiled-natural-parms-byte*)
          (setf length-tile (length-tile (parms tape)))
          ))
      (cond
        ((∨ (¬ seq) (= 0 (length seq)))
          (to-empty tape)
          [➜ok tape]
          )
        (t
          (labels(
                   (init-1 (i base)
                     (when (< i (length seq))
                       (write-tiled-natural tape base (elt seq i))
                       (init-1 (1+ i) (+ length-tile base))
                       ))
                   )
            (init-1 0 0)
            (to-active tape)
            [➜ok tape]
            )))))

  (defun-typed init ((tape-1 tape-tiled-natural) (tape-0 tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (setf (parms tape-1) (parms tape-0))
      (setf (natural tape-1) (natural tape-0))
      (to-active tape-1)
      [➜ok tape-1]
      ))

;;--------------------------------------------------------------------------------
;; accessing instances
;;


;;--------------------------------------------------------------------------------
;; tape queries
;;
  (defun-typed =<cell> ((cell-0 cell-tiled-natural) (cell-1 cell-tiled-natural) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (if
        (= (bit-dex cell-0) (bit-dex cell-1))
        [➜t]
        [➜∅]
        )))

  (defun-typed r<cell> ((cell cell-tiled-natural))
    (read-tiled-natural (tape cell) (bit-dex cell))
    )

  ;; Writing a zero into the rightmost tile makes the natural shorter.  But this is a cell
  ;; operation not a tape operation, so the outer tape operation will have to take this
  ;; into account.
  (defun-typed w<cell> ((cell cell-tiled-natural) instance)
    (write-tiled-natural (tape cell) (bit-dex cell) instance)
    )

  (defun-typed leftmost ((tape tape-tiled-natural-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok 
        (make-instance 
          'cell-tiled-natural
          :tape tape
          :bit-dex 0
          )]
      ))

  (defun-typed right-neighbor ((cell cell-tiled-natural) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (let*(
             (tape (tape cell))
             (length-tile (length-tile (parms tape)))
             (natural (natural tape))
             (bit-dex (bit-dex cell))
             (next-bit-dex (+ bit-dex length-tile))
             )
        (if
          (> 0 (ash natural (- next-bit-dex)))
          [➜ok (read-tiled-natural tape next-bit-dex)]
          [➜rightmost]
          ))))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;   for now not supporting topology manipulation for tiled-natural
;; 
