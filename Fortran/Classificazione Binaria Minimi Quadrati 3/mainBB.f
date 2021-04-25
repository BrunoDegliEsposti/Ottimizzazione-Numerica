      module mush
          ! Il dataset contiene almeno ntraining+nvalidation righe,
          ! ciascuna dotata di esattamente 1+nfeatures elementi.
          implicit double precision(a-h,o-z)
          parameter(nfeatures=112, ntraining=3124, nvalidation=5000)
          parameter(m=1+nfeatures)
          dimension dataset(ntraining+nvalidation, m)
      end module mush

      program main
      use mush
      implicit double precision(a-h,o-z)
      ! Parametri per il metodo di Barzilai-Borwein
      parameter(n=nfeatures, kmax=300, toll=1d-3, kbtmax=10)
      dimension x(n,0:1), fx(0:9), dfx(n,0:1)
      external f_class, grad_class

      ! LOAD DATASET FROM FILE
      open(unit=1, file='mushrooms-dataset.txt', status='old')
      do i=1,ntraining+nvalidation
          read(1,*) dataset(i,:)
      enddo
      close(unit=1)

      ! MODEL TRAINING
      x(:,0) = 0
      call line_search(x,n,f_class,fx,grad_class,dfx,dfx_nrm2,k,kmax,
     *toll,kbtmax,nvf,iflag)
      open(unit=2, file='output-parameters.txt', status='replace')
      write(2,*) x(:,mod(k,2))
      close(unit=2)
      print*, 'Algoritmo di ottimizzazione: Barzilai-Borwein'
      print*, 'Valore del flag: ', iflag
      print*, 'Numero di iterazioni effettuate: ', k
      print*, 'Numero di valutazioni di funzione: ', nvf
      print*, "L'ultimo valore di x è stato salvato su file."
      print*, 'Ultimo valore della funzione in x: ', fx(mod(k,10))
      print*, 'Ultima norma del gradiente della funzione: ', dfx_nrm2

      ! MODEL VALIDATION
      ntrue_positive = 0
      ntrue_negative = 0
      nfalse_positive = 0
      nfalse_negative = 0
      do i=ntraining+1,ntraining+nvalidation
          b = dataset(i,1)
          ax = DDOT(nfeatures, dataset(i,2:m),1, x(:,mod(k,2)),1)
          prediction = 1/(1+exp(-ax))
          if (abs(b-prediction) < 0.5) then
              if (b > 0.5) then
                  ntrue_positive = ntrue_positive + 1
              else
                  ntrue_negative = ntrue_negative + 1
              endif
          else
              if (b > 0.5) then
                  nfalse_negative = nfalse_negative + 1
              else
                  nfalse_positive = nfalse_positive + 1
              endif
          endif
      enddo
      print*,
      print*, 'Dimensione del training set: ', ntraining
      print*, 'Dimensione del validation set: ', nvalidation
      write(*,50) 100*real(nfalse_positive+nfalse_negative)/nvalidation
50    format(' Errore di predizione: ', 11X, F7.4, '%')
      print*, 'Numero di veri positivi: ', ntrue_positive
      print*, 'Numero di veri negativi: ', ntrue_negative
      print*, 'Numero di falsi positivi: ', nfalse_positive
      print*, 'Numero di falsi negativi: ', nfalse_negative
      end program









