      subroutine backtrack(k,x,n,f,fx,dfx,alpha_tilde,kbtmax,ibtflag)
C     input:      k,x,n,f,fx,dfx,alpha_tilde,kbtmax
C     output:     x,fx,ibtflag
C     function    f(x,n)        calcola il valore di f in x
C     ibtflag contiene 0 (errore), oppure il numero di valutazioni di f
      implicit double precision(a-h,o-z)
      dimension x(n,0:1), fx(0:9), dfx(n,0:1)
      double precision alpha, fxmax, dfx_squared
      alpha = alpha_tilde
      fxmax = maxval(fx)
      dfx_squared = DDOT(n,dfx(:,mod(k-1,2)),1,dfx(:,mod(k-1,2)),1)
      ibtflag = 0
      do i=1,kbtmax
          x(:,mod(k,2)) = x(:,mod(k-1,2)) - alpha*dfx(:,mod(k-1,2))
          fx(mod(k,10)) = f(x(:,mod(k,2)),n)
          ibtflag = ibtflag + 1
          if (fx(mod(k,10)) <= fxmax - 1e-4*alpha*dfx_squared) then
              return
          endif
          alpha = 0.5*alpha
      enddo
      ibtflag = 0
      end subroutine
