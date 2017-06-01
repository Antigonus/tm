
;;--------------------------------------------------------------------------------
;; accessing data
;;
  (def-function-class ◧r (tape &optional ➜))
  (def-function-class ◧sr (tape &optional ➜))

  (def-function-class ◧w (tape instance &optional ➜))
  (def-function-class ◧sw (tape instance &optional ➜))


;;--------------------------------------------------------------------------------
;; length-tape
;;
  (def-function-class tape-length-is-one (tm &optional ➜))
  (def-function-class tape-length-is-two (tm &optional ➜))


;;--------------------------------------------------------------------------------
;; epa epd should be tape operations, but we have issues with head collisions
;; solo can do this without a problem, but nd machines might have affected entangled
;; copies.
;;
