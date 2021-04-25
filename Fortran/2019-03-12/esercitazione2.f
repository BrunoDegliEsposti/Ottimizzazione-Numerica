      PROGRAM NORMA2
      REAL A(50)
      PRINT*, 'Dimensione del vettore: '
      READ*, N
      READ*, (A(I), I=1,N)
      S = 0
      DO I=1,N
        S = S + A(I)*A(I)
      ENDDO
      S = SQRT(S)
      PRINT*, 'Norma 2 del vettore: ', S
      END
