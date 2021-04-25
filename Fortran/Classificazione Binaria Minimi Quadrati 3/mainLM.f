      module mush
          ! Il dataset contiene almeno ntraining+nvalidation righe,
          ! ciascuna dotata di esattamente m := 1+nfeatures elementi.
          implicit double precision(a-h,o-z)
          parameter(nfeatures=112, ntraining=3124, nvalidation=5000)
          parameter(m=1+nfeatures)
          dimension dataset(ntraining+nvalidation, m)
      end module mush

      program main
      use mush
      implicit double precision(a-h,o-z)
      ! Parametri per il metodo di Levenberg-Marquardt
      parameter(n=nfeatures, kmax=50, tol=1d-3,
     *          lwa=ntraining*n+5*n+ntraining)
      dimension x(n), fvec(ntraining), iwa(n), wa(lwa)
      external f_class_lm

      ! LOAD DATASET FROM FILE
      open(unit=1, file='mushrooms-dataset.txt', status='old')
      do i=1,ntraining+nvalidation
          read(1,*) dataset(i,:)
      enddo
      close(unit=1)

      ! MODEL TRAINING
      x = 0
      call lmdif1(f_class_lm,ntraining,n,x,fvec,tol,info,iwa,wa,lwa)
      open(unit=2, file='output-parameters.txt', status='replace')
      write(2,*) x
      close(unit=2)
      print*, 'Algoritmo di ottimizzazione: Levenberg-Marquardt'
      print*, 'Valore di info: ', info
      !print*, 'Numero di iterazioni effettuate: ', !FIXME
      !print*, 'Numero di valutazioni di funzione: ', !FIXME
      print*, "L'ultimo valore di x è stato salvato su file."
      !print*, 'Ultimo valore della funzione in x: ', !FIXME
      !print*, 'Ultima norma del gradiente della funzione: ', !FIXME

      ! MODEL VALIDATION
      ntrue_positive = 0
      ntrue_negative = 0
      nfalse_positive = 0
      nfalse_negative = 0
      do i=ntraining+1,ntraining+nvalidation
          b = dataset(i,1)
          ax = DDOT(nfeatures, dataset(i,2:m),1, x,1)
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









