#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Interpret a natural number as a sequence of fixed length tiles.

  The natural is least significant tile at leftmost, then extending rightward.  
  This is the opposite direction that we are accustomed to writing integers.

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

  (def-type tape-tiled-natural (tape)
    (
      (natural
        :initarg :natural
        :accessor natural
        )
      (length-tile
        :initarg :length-tile
        :accessor length-tile
        )
      ))

  (def-type tape-tiled-natural-active (tape-tiled-natural tape-active)())
  (def-type tape-tiled-natural-empty (tape-tiled-natural tape-empty)())
  (defun-typed to-active ((tape tape-tiled-natural)) (change-class tape 'tape-tiled-natural-active))
  (defun-typed to-empty  ((tape tape-tiled-natural)) (change-class tape 'tape-tiled-natural-empty))

  ;; reads a tile
  ;; base is the bit address of the first bit in the tile
  (defun read-tiled-natural (tape base)
    (ldb (byte (length-tile tape) base) (natural tape))    
    )

  ;; .. would like a have a more efficient implementation..
  ;; writes a tile
  (defun write-tiled-natural (tape base instance)
    (setf (natural tape)
      (dpb instance (byte (length-tile tape) base) (natural tape))    
      ))

  (defun-typed init ((tape tape-tiled-natural) (seq sequence) &optional ➜)
    (destructuring-bind
      (&key
        (length-tile 8)
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (setf (natural tape) 0)
      (setf (length-tile tape) length-tile)
      (labels(
               (init-1 (i base)
                 (when (< i (length seq))
                   (write-tiled-natural tape base (elt seq i))
                   (init-1 (1+ i) (+ length-tile base))
                   ))
               )
        (cond
          ((∨ (¬ seq) (= 0 (length seq)))
            (to-empty tape)
            [➜ok tape]
            )
          (t
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
      (setf (length-tile tape-1) (length-tile tape-0))
      (setf (natural tape-1) (natural tape-0))
      (to-active tape-1)
      [➜ok tape-1]
      ))

;;--------------------------------------------------------------------------------
;; accessing instances
;;


;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun-typed =<cell> ((cell-0 cell-tiled-natural) (cell-1 cell-tiled-natural))
    (= (bit-dex cell-0) (bit-dex cell-1))
    )

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
             (length-tile (length-tile tape))
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