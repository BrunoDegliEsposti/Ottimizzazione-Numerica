      function f_class(x,n)
      ! x è il parametro del modello
      ! f(x) = (1/N)*sum_{j=1}^{N}{log(1+exp(-b_j*DDOT(a_j,x)))}
      !      + 0.5*rlambda*DDOT(x,x), con N=ntraining
      ! Ogni valutazione costa circa nfeatures*ntraining flops
      use mush
      implicit double precision(a-h,o-z)
      dimension x(n)
      f_class = 0
      do j=1,ntraining
          b = dataset(j,1)
          ax = DDOT(n, dataset(j,2:m),1, x,1)
          f_class = f_class + log(1+exp(-b*ax))
      enddo
      f_class = f_class/ntraining + 0.5d0*rlambda*DDOT(n,x,1,x,1)
      end function

      subroutine grad_class(x,n,dfx)
      ! Ogni valutazione costa circa nfeatures*ntraining flops
      use mush
      implicit double precision(a-h,o-z)
      dimension x(n), dfx(n)
      dfx = 0
      do j=1,ntraining
          b = dataset(j,1)
          ax = DDOT(n, dataset(j,2:m),1, x,1)
          e = exp(-b*ax)
          dfx = dfx + (e/(1+e))*(-b*dataset(j,2:m))
      enddo
      dfx = dfx/ntraining + rlambda*x
      end subroutine








