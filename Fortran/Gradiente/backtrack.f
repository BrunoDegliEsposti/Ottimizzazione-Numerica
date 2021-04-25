      subroutine backtrack(x,n,f,fx,dfx,
     *      p,alpha_tilde,btgamma,c1,kbtmax,ibtflag)
C     input:      x,n,f,fx,dfx,p,alpha_tilde,btgamma,c1,kbtmax
C     output:     x,fx,ibtflag
C     function    f(x,n)        calcola il valore di f in x
C     ibtflag contiene 0 (errore), oppure il numero di valutazioni di f
      implicit double precision(a-h,o-z)
      dimension x(n), dfx(n), p(n), x_new(n)
      alpha = alpha_tilde
      ibtflag = 0
      do k=1,kbtmax
          x_new = x + alpha*p
          fx_new = f(x_new,n)
          ibtflag = ibtflag + 1
          if (fx_new <= fx + c1*alpha*DDOT(n,dfx,1,p,1)) then
              x = x_new
              fx = fx_new
              return
          endif
          alpha = btgamma*alpha
      enddo
      ibtflag = 0
      end subroutine
