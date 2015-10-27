PROGRAM obs2combine
!===============================================================================
! PROGRAM: obs2combine
! 
!combines all processed observations of obsout
!   i.e. 
!   obsout.dat = obsout_sst.dat + obsout_sss.dat + obsout_eta.dat + obsout_temp.dat + obsout_salt.dat
!NOTE: This program expects only SST observations in obsout_sst.dat, only SSS observations in obsout_sss.dat etc..
!      Hence irrevalent observations from a file are simply ignored
! 
!USES:
!  param_obs
!-------------------------------------------------------------------------------
! $Author: Sivareddy $
!Email: ssiva@incois.gov.in
!===============================================================================
 USE common, ONLY: r_sngl
 USE params_obs, ONLY: obs1nrec, obs2nrec, id_u_obs,id_v_obs,id_t_obs,id_s_obs,id_sst_obs,id_sss_obs,id_eta_obs
 
 IMPLICIT NONE
 REAL(r_sngl), ALLOCATABLE :: wk(:)
 INTEGER :: ios,n1,n2,i

 if (obs2nrec==8) then
   ALLOCATE(wk(8))
 elseif (obs2nrec==9) then
   ALLOCATE(wk(9))
 else
    WRITE(6,*) "obs2combine.f90:: no read_obs option for obs2nrec = ", obs2nrec
    STOP 95
 endif

 OPEN(unit=11,file='obsout_sst.dat',FORM='unformatted')
 OPEN(unit=12,file='obsout_sss.dat',FORM='unformatted')
 OPEN(unit=13,file='obsout_eta.dat',FORM='unformatted')
 OPEN(unit=14,file='obsout_temp.dat',FORM='unformatted')
 OPEN(unit=15,file='obsout_salt.dat',FORM='unformatted')
 OPEN(unit=16,file='obsout.dat',FORM='unformatted',status='unknown')
 n1=0
 n2=0
 DO i=11,15,1	!loop through each file
 Print*,'Entering file no: ',i
  DO
    READ(unit=i,IOSTAT=ios)wk	!READ until End of file reached
    if (ios /= 0) THEN
      print*,'END OF FILE:', i
      EXIT
    endif
    n1=n1+1
    SELECT CASE(i)	
     case(11)
      if (wk(1) == id_sst_obs) then !Idea is to consider only sst observations from obsout_sst.dat
       n2=n2+1
       write(16)wk
      endif
     case(12)
      if (wk(1) == id_sss_obs) then
       n2=n2+1
       write(16)wk
      endif
     case(13)
      if (wk(1) == id_eta_obs) then
       n2=n2+1
       write(16)wk
      endif
     case(14)
      if (wk(1) == id_t_obs) then
       n2=n2+1
       write(16)wk
      endif
     case(15)
      if (wk(1) == id_s_obs) then
       n2=n2+1
       write(16)wk
      endif
     END SELECT
    ENDDO
   CLOSE(i)	!Reading of the file is over. Hence close the file
  ENDDO
  CLOSE(16)	!Now, obsout.dat has all valid records obtained form all obsout_*.dat files
  print*,'Total number of observations read: ',n1
  print*,'Total number of valid observations: ', n2
  DEALLOCATE(wk)
  STOP
END PROGRAM obs2combine
