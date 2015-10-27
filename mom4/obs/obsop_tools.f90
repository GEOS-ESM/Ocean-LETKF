MODULE obsop_tools
!This module contains few sub-routines of common_mom4.f90 and common_obs_mom4.f90 customized for various obsop_* programs
!-------------------------------------------------------------------------------
! $Author: Sivareddy, Steve Penny, Takemasa Miyoshi $
!===============================================================================
  USE common
  USE common_mom4
  USE params_obs

  IMPLICIT NONE

  PUBLIC

CONTAINS
!-----------------------------------------------------------------------
! Set the parameters
!-----------------------------------------------------------------------
SUBROUTINE set_common_mom4_allvars(elemid)
!===============================================================================
! Initialize the module
!===============================================================================
  USE netcdf
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: elemid	!SIVA: Element id to process appropriate block
  INTEGER :: i,j,k
  INTEGER :: ncid,istat,varid,dimid
  CHARACTER(NF90_MAX_NAME) :: dimname
  LOGICAL :: ex

  WRITE(6,'(A)') 'Hello from set_common_mom4_allvars'
 if (elemid == id_eta_obs) then	!SIVA: we don't want to execute this block if it is not called from obsop_eta.f90
  if (DO_ALTIMETRY) then
    INQUIRE(FILE=trim(SSHclm_file),EXIST=ex)
    IF(ex) THEN
      ! Read in the model climatology
      CALL read_etaclm(SSHclm_file,SSHclm_m)
    ELSE
      WRITE(6,*) "The file ", SSHclm_file, " does not exist:"
      WRITE(6,*) "Exiting obsop_eta.f90..."
      STOP(1)
    ENDIF
  endif
 endif	! SIVA: End of ETA block
  !
  ! Lon, Lat, f, orography
  !
!STEVE: this part adapted from ROMS, update from MOM4 netcdf files:
!STEVE: GOAL: to utilize all netcdf grid data to completely define the grid and all grid-dependent operations
  INQUIRE(FILE=trim(gridfile),EXIST=ex)
  IF(.not. ex) THEN
    WRITE(6,*) "The file does not exist: ", gridfile 
    WRITE(6,*) "Exiting obsop_eta.f90..."
    STOP(2)
  ENDIF
  WRITE(6,'(A)') '  >> accessing file: ', gridfile
  call check( NF90_OPEN(gridfile,NF90_NOWRITE,ncid) )
  call check( NF90_INQ_VARID(ncid,'grid_x_T',varid) )   ! Longitude for T-cell
  call check( NF90_GET_VAR(ncid,varid,lon) )
  WRITE(6,*) "lon(1) = ", lon(1)
  WRITE(6,*) "lon(nlon) = ", lon(nlon)
  call check( NF90_INQ_VARID(ncid,'grid_y_T',varid) )   ! Latitude for T-cell
  call check( NF90_GET_VAR(ncid,varid,lat) )
  WRITE(6,*) "lat(1) = ", lat(1)
  WRITE(6,*) "lat(nlat) = ", lat(nlat)
  call check( NF90_INQ_VARID(ncid,'zt',varid) )      ! depth of T-cell
  call check( NF90_GET_VAR(ncid,varid,lev) )
  WRITE(6,*) "lev(1) = ", lev(1)
  WRITE(6,*) "lev(nlev) = ", lev(nlev)
! call check( NF90_INQ_VARID(ncid,'num_levels',varid) ) ! number of vertical levels
! call check( NF90_GET_VAR(ncid,varid,phi0) )
! WRITE(6,*) "ncid = ", ncid
! WRITE(6,*) "varid = ", varid
! WRITE(6,*) "phi0(1,1) = ", phi0(1,1)
! WRITE(6,*) "phi0(nlon,nlat) = ", phi0(nlon,nlat)
  !
  ! dx and dy
  !
  call check( NF90_INQ_VARID(ncid,'ds_01_21_T',varid) )    ! width of T_cell (meters)
  call check( NF90_GET_VAR(ncid,varid,dx) ) 
  call check( NF90_INQ_VARID(ncid,'ds_10_12_T',varid) )    ! height of T_cell (meters)
  call check( NF90_GET_VAR(ncid,varid,dy) ) 
  call check( NF90_INQ_VARID(ncid,'area_T',varid) )        ! area of T_cell
  call check( NF90_GET_VAR(ncid,varid,area_t) ) 
  WRITE(6,*) "common_mom4:: grid_spec.nc MIN(dx) = ", MINVAL(dx)
  WRITE(6,*) "common_mom4:: grid_spec.nc MAX(dx) = ", MAXVAL(dx)
  WRITE(6,*) "common_mom4:: grid_spec.nc MIN(dy) = ", MINVAL(dy)
  WRITE(6,*) "common_mom4:: grid_spec.nc MAX(dy) = ", MAXVAL(dy)
  WRITE(6,*) "common_mom4:: grid_spec.nc MIN(area_t) = ", MINVAL(area_t)
  WRITE(6,*) "common_mom4:: grid_spec.nc MAX(area_t) = ", MAXVAL(area_t)

  !
  ! kmt data
  !
  call check( NF90_INQ_VARID(ncid,'num_levels',varid) ) ! number of vertical T-cells
  call check( NF90_GET_VAR(ncid,varid,kmt0) )
  WRITE(6,*) "kmt0(1,1) = ", kmt0(1,1)
  WRITE(6,*) "kmt0(nlon,nlat) = ", kmt0(nlon,nlat)
  kmt = NINT(kmt0)
  call check( NF90_INQ_VARID(ncid,'wet',varid) )        ! land/sea flag (0=land) for T-cell
  call check( NF90_GET_VAR(ncid,varid,wet) )
  WRITE(6,*) "wet(1,1) = ", wet(1,1)
  WRITE(6,*) "wet(nlon,nlat) = ", wet(nlon,nlat)

  WRITE(6,*) "Using dx and dy from netcdf file: ", gridfile
  WRITE(6,*) "dx(1,1) = ", dx(1,1)
  WRITE(6,*) "dx(nlon,nlat) = ", dx(nlon,nlat)
  WRITE(6,*) "dy(1,1) = ", dy(1,1)
  WRITE(6,*) "dy(nlon,nlat) = ", dy(nlon,nlat)


  !STEVE: needed for computing the AMOC based on the streamfunction calculation:
  call check( NF90_INQ_VARID(ncid,'zb',varid) )      ! depth of T-cell
  call check( NF90_GET_VAR(ncid,varid,zb) )
  WRITE(6,*) "zb(1) = ", zb(1)
  WRITE(6,*) "zb(nlev) = ", zb(nlev)

  ! Compute dz:
  dz(1) = zb(1)
  do k=2,nlev
    dz(k) = zb(k)-zb(k-1)
  enddo

  !
  ! Corioris parameter
  !
!$OMP PARALLEL WORKSHARE
  fcori(:) = 2.0d0 * r_omega * sin(lat(:)*pi/180.0d0)
!$OMP END PARALLEL WORKSHARE

  ! Close the grid_spec.nc file:
  call check( NF90_CLOSE(ncid) )

  ! STEVE: for (more) generalized (longitude) grid:
  lon0 = lon(1)
  lonf = lon(nlon)
  lat0 = lat(1)
  latf = lat(nlat)
  wrapgap = 360.0d0 - abs(lon0) - abs(lonf)

  RETURN
END SUBROUTINE set_common_mom4_allvars

!SIVA: Subroutine to read a 2-D variable from corresponding restatrt file
SUBROUTINE read_grd_2dvars(infile,v2d,elemid)
  USE netcdf
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: elemid	!SIVA: Element id to process appropriate block
  CHARACTER(*),INTENT(IN) :: infile
  REAL(r_size),INTENT(OUT) :: v2d(nlon,nlat)
  REAL(r_sngl) :: buf4(nlon,nlat)
  CHARACTER(slen) :: bfile ! (barotropic - eta)
  CHARACTER(slen) :: sffile !(SFC)
  INTEGER :: i,j,k
  INTEGER :: ncid,istat,varid

  bfile  = trim(infile)//'.ocean_barotropic.res.nc'
  sffile = trim(infile)//'.ocean_sbc.res.nc'

 if (elemid == id_sst_obs) then ! Check elemid for SST and do necessary
    call check( NF90_OPEN(sffile,NF90_NOWRITE,ncid) )
    WRITE(6,*) "read_grd_2d:: just opened file ", sffile
    buf4=0.0
    call check( NF90_INQ_VARID(ncid,'t_surf',varid) )
    call check( NF90_GET_VAR(ncid,varid,buf4(:,:)) )
    if (dodebug) WRITE(6,*) "read_grd_2d:: just got data for variable sfc temp"
    DO j=1,nlat
      DO i=1,nlon
        if (kmt(i,j) .ge. 1) v2d(i,j) = REAL(buf4(i,j),r_size) - t0c !kelvin
      END DO
    END DO
    if (dodebug) WRITE(6,*) "read_grd_2d:: finished processing data for variable SST"

    ! !STEVE: debug
    if (dodebug) then
      WRITE(6,*) "POST-SST"
      WRITE(6,*) "read_grd:: sffile = ", sffile
      WRITE(6,*) "max val for level v2d(:,:) = ",MAXVAL(v2d(:,:))
    endif
! !STEVE: end
    call check( NF90_CLOSE(ncid) )
 endif  !SIVA: End of SST block

 if (elemid == id_sss_obs) then !Check elemid for SSS and do necessary
    call check( NF90_OPEN(sffile,NF90_NOWRITE,ncid) )
    WRITE(6,*) "read_grd_2d:: just opened file ", sffile

    !!! SSS
    buf4=0.0
    call check( NF90_INQ_VARID(ncid,'s_surf',varid) )
    call check( NF90_GET_VAR(ncid,varid,buf4(:,:)) )
    if (dodebug) WRITE(6,*) "read_grd_2d:: just got data for variable sfc salt"
    DO j=1,nlat
      DO i=1,nlon
        v2d(i,j) = REAL(buf4(i,j),r_size)
      END DO
    END DO
    if (dodebug) WRITE(6,*) "read_grd_2d:: finished processing data for variable SSS"

! !STEVE: debug
    if (dodebug) then
      WRITE(6,*) "POST-SSS"
      WRITE(6,*) "read_grd:: sffile = ", sffile
      WRITE(6,*) "max SSS val = ", MAXVAL(v2d(:,:))
    endif
! !STEVE: end
   call check( NF90_CLOSE(ncid) )
 endif !SIVA: End of SSS block

 if (elemid == id_eta_obs) then	!Check elemid for ETA and do necessary
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Open the ALTIMETRY netcdf restart file (eta)
  ! (These are the modeled sfc height perturbations used by GODAS)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  altimetry : if(DO_ALTIMETRY) then
    !STEVE: use the sea level perturbation from ocean_barotropic.res.nc
    call check( NF90_OPEN(bfile,NF90_NOWRITE,ncid) )
    WRITE(6,*) "read_grd_2d in ETA section:: just opened file ", bfile

    !!! SSH
    buf4=0.0
    call check( NF90_INQ_VARID(ncid,'eta_t',varid) )
    call check( NF90_GET_VAR(ncid,varid,buf4(:,:)) )
    if (dodebug) WRITE(6,*) "read_grd_2d in ETA section:: just got data for variable eta_t"
    DO j=1,nlat
      DO i=1,nlon
        !STEVE: Hopefully reading in meters here... (data might be in cm)
        v2d(i,j) = REAL(buf4(i,j),r_size)
      END DO
    END DO
    if (dodebug) WRITE(6,*) "read_grd_2d in ETA section:: finished processing data for variable SSH"

    ! Convert SSH eta stored in v2d to climatological Sea Level Anomaly (SLA) by subtracting pre-computed model climatology
    v2d(:,:) = v2d(:,:) - SSHclm_m(:,:)

    ! !STEVE: debug
    if (dodebug) then
      WRITE(6,*) "POST-eta"
      WRITE(6,*) "read_grd_2d in ETA:: bfile = ", bfile
      WRITE(6,*) "max val for level v2d(:,:) = ", MAXVAL(v2d(:,:))
      WRITE(6,*) "min val for level v2d(:,:) = ", MINVAL(v2d(:,:))
    endif
    ! !STEVE: end
    call check( NF90_CLOSE(ncid) )
  else
   WRITE(6,*) "read_grd_2d in ETA section:: DO_ALTIMETRY is set to false. Hence, not processing any more"
   WRITE(6,*) "Skipping the ETA processing...."
   STOP(4)
  endif altimetry
 endif	!SIVA: End of ETA block

 RETURN
END SUBROUTINE read_grd_2dvars

!SIVA: Subroutine to read a 3-D variable from corresponding restatrt file
SUBROUTINE read_grd_3dvars(infile,v3d,elemid)
  USE netcdf
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: elemid	!SIVA: Element id to process appropriate block
  CHARACTER(*),INTENT(IN) :: infile
  REAL(r_size),INTENT(OUT) :: v3d(nlon,nlat,nlev)
  REAL(r_sngl) :: buf4(nlon,nlat,nlev)
  CHARACTER(slen) :: tsfile ! (TS) 
  CHARACTER(slen) :: uvfile ! (UV) 
  INTEGER :: i,j,k
  INTEGER :: ncid,istat,varid

  tsfile = trim(infile)//'.ocean_temp_salt.res.nc'
  uvfile = trim(infile)//'.ocean_velocity.res.nc'

 if (elemid == id_t_obs) then !SIVA: Check elemid for temperature id and do necessary
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Open the T/S netcdf restart file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call check( NF90_OPEN(tsfile,NF90_NOWRITE,ncid) )
  WRITE(6,*) "read_grd_3dvars in Temperature section:: just opened file ", tsfile

  !!! t
  buf4=0.0
  call check( NF90_INQ_VARID(ncid,'temp',varid) )
  call check( NF90_GET_VAR(ncid,varid,buf4) )
  if (dodebug) WRITE(6,*) "read_grd_3dvars in Temperature section:: just got data for variable temp"
  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        v3d(i,j,k) = REAL(buf4(i,j,k),r_size)
      END DO
    END DO
  END DO
  if (dodebug) WRITE(6,*) "read_grd_3dvars in Temperature section:: finished processing data for variable temp"

! !STEVE: debug
  if (dodebug) then
    WRITE(6,*) "POST-SALT"
    WRITE(6,*) "read_grd_3dvars in Temperature section:: tsfile = ", tsfile
    do k=1,nlev
      WRITE(6,*) "max val for level v3d(:,:,", k, ") = ", MAXVAL(v3d(:,:,k))
    enddo 
  endif
! !STEVE: end

  call check( NF90_CLOSE(ncid) )
 endif	!SIVA: End of Temperature section

 if (elemid == id_s_obs) then !SIVA: Check elemid for salinity id and do necessary
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Open the T/S netcdf restart file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call check( NF90_OPEN(tsfile,NF90_NOWRITE,ncid) )
  WRITE(6,*) "read_grd_3dvars in Salinity section:: just opened file ", tsfile

  !!! s
  buf4=0.0
  call check( NF90_INQ_VARID(ncid,'salt',varid) )
  call check( NF90_GET_VAR(ncid,varid,buf4) )
  if (dodebug) WRITE(6,*) "read_grd_3dvars in Salinity section:: just got data for variable salt"
  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        v3d(i,j,k) = REAL(buf4(i,j,k),r_size)
      END DO
    END DO
  END DO
  if (dodebug) WRITE(6,*) "read_grd_3dvars in Salinity section:: finished processing data for variable salt"

! !STEVE: debug
  if (dodebug) then
    WRITE(6,*) "POST-SALT"
    WRITE(6,*) "read_grd_3dvars in Salinity section:: tsfile = ", tsfile
    do k=1,nlev
      WRITE(6,*) "max val for level v3d(:,:,", k, ") = ", MAXVAL(v3d(:,:,k))
    enddo 
  endif
! !STEVE: end

  call check( NF90_CLOSE(ncid) )
 endif	!SIVA: End of salinity section

 if (elemid == id_u_obs) then !SIVA: Check elemid for U-current id and do necessary
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Open the U/V netcdf restart file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call check( NF90_OPEN(uvfile,NF90_NOWRITE,ncid) )
  WRITE(6,*) "read_grd_3dvars in U-current section:: just opened file ", uvfile

  !!! u
  buf4=0.0
  call check( NF90_INQ_VARID(ncid,'u',varid) )
  call check( NF90_GET_VAR(ncid,varid,buf4) )
  if (dodebug) WRITE(6,*) "read_grd_3dvars in U-current section:: just got data for variable u"
  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        v3d(i,j,k) = REAL(buf4(i,j,k),r_size)
      END DO
    END DO
  END DO
  if (dodebug) WRITE(6,*) "read_grd_3dvars in U-current section:: finished processing data for variable u"

! !STEVE: debug
  if (dodebug) then
    WRITE(6,*) "POST-U"
    WRITE(6,*) "read_grd_3dvars in U-current section:: uvfile = ", uvfile
    do k=1,nlev
      WRITE(6,*) "max val for level v3d(:,:,", k, ") = ", MAXVAL(v3d(:,:,k))
    enddo 
  endif
! !STEVE: end

  call check( NF90_CLOSE(ncid) )
 endif	!SIVA: End of U-current section

 if (elemid == id_v_obs) then !SIVA: Check elemid for V-current id and do necessary
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Open the U/V netcdf restart file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call check( NF90_OPEN(uvfile,NF90_NOWRITE,ncid) )
  WRITE(6,*) "read_grd_3dvars in V-current section:: just opened file ", uvfile

  !!! v
  buf4=0.0
  call check( NF90_INQ_VARID(ncid,'v',varid) )
  call check( NF90_GET_VAR(ncid,varid,buf4) )
  if (dodebug) WRITE(6,*) "read_grd_3dvars in V-current section:: just got data for variable v"
  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        v3d(i,j,k) = REAL(buf4(i,j,k),r_size)
      END DO
    END DO
  END DO
  if (dodebug) WRITE(6,*) "read_grd_3dvars in V-current section:: finished processing data for variable v"

! !STEVE: debug
  if (dodebug) then
    WRITE(6,*) "POST-V"
    WRITE(6,*) "read_grd_3dvars in U-current section:: uvfile = ", uvfile
    do k=1,nlev
      WRITE(6,*) "max val for level v3d(:,:,", k, ") = ", MAXVAL(v3d(:,:,k))
    enddo 
  endif
! !STEVE: end

  call check( NF90_CLOSE(ncid) )
 endif	!SIVA: End of V-current section

  RETURN
END SUBROUTINE read_grd_3dvars


!-- Read a grid file for 2d variables---------------------------------------------------
SUBROUTINE read_bingrd_2dvars(filename,v2d)
!===============================================================================
! Read in an letkf grd-format binary file
!===============================================================================
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: filename
  REAL(r_size),INTENT(OUT) :: v2d(nlon,nlat)
  INTEGER :: iunit,iolen

  iunit=11
  INQUIRE(IOLENGTH=iolen) iolen
  OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)

  READ(iunit,REC=1)v2d

  CLOSE(iunit)

  RETURN
END SUBROUTINE read_bingrd_2dvars

!-- Read a grid file for 3d variables---------------------------------------------------
SUBROUTINE read_bingrd_3dvars(filename,v3d)
!===============================================================================
! Read in an letkf grd-format binary file
!===============================================================================
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: filename
  REAL(r_size),INTENT(OUT) :: v3d(nlon,nlat,nlev)
  REAL(r_sngl), ALLOCATABLE :: buf4(:,:) !(nlon,nlat)
  INTEGER :: iunit,iolen
  INTEGER :: k,n,irec

  ALLOCATE(buf4(nlon,nlat))

  iunit=11
  INQUIRE(IOLENGTH=iolen) iolen
  OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)

  irec=1
    DO k=1,nlev
      READ(iunit,REC=irec) buf4
      irec = irec + 1
      v3d(:,:,k) = REAL(buf4,r_size)
    END DO

  CLOSE(iunit)

  RETURN
END SUBROUTINE read_bingrd_3dvars

END MODULE obsop_tools

