      function funzione(x,n)
C     f(x,y) = (a-x)^2 + b(y-x^2)^2
C     valori tipici: a=1, b=100
      implicit double precision(a-h,o-z)
      dimension x(n)
      parameter(a=1, b=1)
      funzione = (a-x(1))**2 + b*((x(2)-x(1)**2)**2)
      end function

      subroutine gradiente(x,n,dfx)
C     df(x,y) = ( -2(a-x)-4bx(y-x^2)), 2b(y-x^2) )
C     valori tipici: a=1, b=100
      implicit double precision(a-h,o-z)
      dimension x(n), dfx(n)
      parameter(a=1, b=1)
      dfx(1) = -2*(a-x(1))-4*b*x(1)*(x(2)-x(1)**2)
      dfx(2) = 2*b*(x(2)-x(1)**2)
      end subroutine
