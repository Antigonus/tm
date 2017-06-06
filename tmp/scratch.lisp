(LET (AD BD)
  (UNWIND-PROTECT 
    (PROGN 
      (SETQ AD-s* (DUP A)) 
      (SETQ s*BD (DUP B)) 
      (L (R s*AD) (R BD-s*))
      )
    (WHEN AD (-s*DIS-s*ENTANGLE AD))
    (WHEN BD (DISENTANGLE BD))
    ))
