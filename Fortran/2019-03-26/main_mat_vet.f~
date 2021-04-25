      PROGRAM MAIN_MAT_VET
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(100,100), W(100), V(100)
      PRINT*, 'Numero di righe: '
      READ*, M
      PRINT*, 'Numero di colonne: '
      READ*, N
      PRINT*, 'Inserisci la matrice (per righe): '
      READ*, ((A(I,J), J=1,N), I=1,M)
      PRINT*, 'Inserisci il vettore: '
      READ*, (W(I), I=1,N)
      CALL MAT_VET(A,W,N,M,V,100)
      PRINT*, 'Risultato del prodotto: ', (V(I), I=1,M)
C     PRINT*, 'Risultato del prodotto: ', V
C     PRINT*, 'Norma 2 del vettore: ', DNRM2(2,V,1)
      END
