      PROGRAM EPSILON
      REAL A, EPS32
      DOUBLE PRECISION B, EPS64
C     Calcolo in precisione singola
      EPS32 = 1.0
10    A = 1.0 + EPS32
      IF (A.GT.1) THEN
        EPS32 = EPS32 * 0.5
        GOTO 10
      ENDIF
      EPS32 = EPS32 * 2.0
      PRINT*, EPS32
C     Calcolo in precisione doppia
      EPS64 = 1.0D0
20    B = 1.0D0 + EPS64
      IF (B.GT.1D0) THEN
        EPS64 = EPS64 * 0.5D0
        GOTO 20
      ENDIF
      EPS64 = EPS64 * 2.0D0
      PRINT*, EPS64
      END
