      function f_class(x,n)
      ! x è il parametro del modello
      ! Ogni valutazione costa circa nfeatures*ntraining flops
      use mush
      implicit double precision(a-h,o-z)
      dimension x(n)
      f_class = 0
      do j=1,ntraining
          b = dataset(j,1)
          ax = DDOT(n, dataset(j,2:m),1, x,1)
          f_class = f_class + (b - 1/(1+exp(-ax)))**2
      enddo
      f_class = f_class/ntraining
      end function

      subroutine f_class_lm(m_input,n,x,fvec,iflag)
      use mush
      implicit double precision(a-h,o-z)
      dimension x(n), fvec(ntraining)
      do j=1,ntraining
          b = dataset(j,1)
          ax = DDOT(n, dataset(j,2:m),1, x,1)
          fvec(j) = (b - 1/(1+exp(-ax)))**2
      enddo
      fvec = fvec/sqrt(real(ntraining))
      end subroutine

      subroutine grad_class(x,n,dfx)
      ! Ogni valutazione costa circa nfeatures*ntraining flops
      use mush
      implicit double precision(a-h,o-z)
      dimension x(n), dfx(n)
      dfx = 0
      do j=1,ntraining
          b = dataset(j,1)
          ax = DDOT(n, dataset(j,2:m),1, x,1)
          e = exp(-ax)
          r = 1/(1+e)
          dfx = dfx + 2*(b-r)*r*r*e*(-dataset(j,2:m))
      enddo
      dfx = dfx/ntraining
      end subroutine








