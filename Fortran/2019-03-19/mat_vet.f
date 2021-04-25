      SUBROUTINE MAT_VET(A,W,N,M,V)
C     CALCOLA IL PRODOTTO V=AW
C     CON A MATRICE IN INGRESSO MxN,
C         W VETTORE IN INGRESSO Nx1,
C         V VETTORE IN USCITA Mx1
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION W(N), V(M), A(M,N)
      DO I=1,M
        V(I) = 0.D0
        DO J=1,N
            V(I) = V(I) + A(I,J)*W(J)
        ENDDO
      ENDDO
      END
