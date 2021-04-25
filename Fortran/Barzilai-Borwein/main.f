      program main
      implicit double precision(a-h,o-z)
      external funzione, gradiente
      parameter(n=2, kmax=3000, toll=1d-3, kbtmax=15)
      dimension x(n,0:1), fx(0:9), dfx(n,0:1)
      x(:,0) = 1.1d0
      call line_search(x,n,funzione,fx,gradiente,dfx,dfx_nrm2,k,kmax,
     *  toll,kbtmax,nvf,iflag)
      print*, 'Valore del flag: ', iflag
      print*, 'Numero di iterazioni effettuate: ', k
      print*, 'Numero di valutazioni di funzione: ', nvf
      print*, 'Ultimo valore di x: ', x(:,mod(k,2))
      print*, 'Ultimo valore della funzione in x: ', fx(mod(k,10))
      print*, 'Ultima norma del gradiente della funzione: ', dfx_nrm2
      end program
