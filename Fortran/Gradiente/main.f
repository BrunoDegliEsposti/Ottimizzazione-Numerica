      program main
      implicit double precision(a-h,o-z)
      external funzione, gradiente
      parameter(n=2, kmax=300, toll=1d-3)
      parameter(alpha=1.0d0, btgamma=0.5d0, c1=1d-4, kbtmax=15)
      dimension x(n), dfx(n)
      x(:) = 1.1d0
      call discesa_gradiente(x,n,funzione,fx,gradiente,dfx,
     * dfx_nrm2,k,kmax,toll,alpha,btgamma,c1,kbtmax,nvf,iflag)
      print*, 'Valore del flag: ', iflag
      print*, 'Numero di iterazioni effettuate: ', k
      print*, 'Numero di valutazioni di funzione: ', nvf
      print*, 'Ultimo valore di x: ', x
      print*, 'Ultimo valore della funzione in x: ', fx
      print*, 'Ultima norma del gradiente della funzione: ', dfx_nrm2
      end program
