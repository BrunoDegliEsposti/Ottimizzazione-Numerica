      subroutine discesa_gradiente(x,n,f,fx,df,dfx,dfx_nrm2,k,kmax,
     *  toll,alpha,btgamma,c1,kbtmax,nvf,iflag)
C     input:    x,n,f,df,kmax,toll,alpha,btgamma,c1,kbtmax
C     output:   x,fx,dfx,dfx_nrm2,k,nvf,iflag
C     function    f(x,n)        calcola il valore di f in x
C     subroutine  df(x,n,dfx)   calcola il gradiente di f in x
      implicit double precision(a-h,o-z)
      dimension x(n), dfx(n), p(n)
      fx = f(x,n)
      nvf = 1
      do k=0,kmax
          call df(x,n,dfx)
          dfx_nrm2 = DNRM2(n,dfx,1)
          if (dfx_nrm2 <= toll) then
              iflag = 1
              return
          endif
          p = -dfx
          ! backtrack aggiorna x e fx
          call backtrack(x,n,f,fx,dfx,p,alpha,btgamma,c1,kbtmax,ibtflag)
          if (ibtflag == 0) then
              iflag = -1
              return
          endif
          nvf = nvf + ibtflag
      enddo
      iflag = 0
      end subroutine
