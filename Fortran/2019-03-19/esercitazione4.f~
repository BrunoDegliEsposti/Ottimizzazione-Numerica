      PROGRAM ESERCITAZIONE4
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(2,2), W(2), V(2)
C     PRINT*, 'Numero di righe: '
C     READ*, M
C     PRINT*, 'Numero di colonne: '
C     READ*, N
      PRINT*, 'Inserisci la matrice (2x2, per righe): '
      READ*, ((A(I,J), J=1,2), I=1,2)
      PRINT*, 'Inserisci il vettore: '
      READ*, (W(I), I=1,2)
      CALL MAT_VET(A,W,2,2,V)
C     PRINT*, 'Risultato del prodotto: ', (V(I), I=1,2)
      PRINT*, 'Risultato del prodotto: ', V
      PRINT*, 'Norma 2 del vettore: ', DNRM2(2,V,1)
      END
