      subroutine line_search(x,n,f,fx,df,dfx,dfx_nrm2,k,kmax,
     *  toll,kbtmax,nvf,iflag)
C     input:    x,n,f,df,kmax,toll,kbtmax
C     output:   x,fx,dfx,dfx_nrm2,k,nvf,iflag
C     function    f(x,n)        calcola il valore di f in x
C     subroutine  df(x,n,dfx)   calcola il gradiente di f in x
      implicit double precision(a-h,o-z)
      dimension x(n,0:1), fx(0:9), dfx(n,0:1), S(n), y(n)
      double precision alpha_tilde

      k = 0
      fx(0) = f(x(:,0),n)
      fx(1:9) = -1d100
      nvf = 1
      call df(x(:,0),n,dfx(:,0))

      k = 1
      alpha_tilde = 1
      ! backtrack aggiorna x e fx
      call backtrack(k,x,n,f,fx,dfx,alpha_tilde,kbtmax,ibtflag)
      nvf = nvf + ibtflag
      call df(x(:,1),n,dfx(:,1))

      do k=2,kmax
          S = x(:,1) - x(:,0)
          y = dfx(:,1) - dfx(:,0)
          alpha_tilde = DDOT(n,S,1,S,1)/DDOT(n,S,1,y,1)
          if (alpha_tilde < 1d-3) then
              alpha_tilde = 1
          endif
          call backtrack(k,x,n,f,fx,dfx,alpha_tilde,kbtmax,ibtflag)
          if (ibtflag == 0) then
              iflag = -1
              nvf = nvf + kbtmax
              return
          endif
          nvf = nvf + ibtflag
          call df(x(:,mod(k,2)),n,dfx(:,mod(k,2)))
          dfx_nrm2 = DNRM2(n,dfx(:,mod(k,2)),1)
          if (dfx_nrm2 <= toll) then
              iflag = 1
              return
          endif
      enddo
      iflag = 0
      end subroutine










