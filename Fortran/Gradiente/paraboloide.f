      function funzione(x,n)
C     f(x) = sum(x^2)
      implicit double precision(a-h,o-z)
      dimension x(n)
      funzione = DDOT(n,x,1,x,1)
      end function

      subroutine gradiente(x,n,dfx)
C     df(x) = 2x
      implicit double precision(a-h,o-z)
      dimension x(n), dfx(n)
      dfx = 2*x
      end subroutine
