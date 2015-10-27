#!/bin/ksh --login
#===============================================================================
# SCRIPT:
# letkf_prepc.ksh
#
# PURPOSE:
# This script prepares background and observation data for letkf 
# This 'compact' version runs all processing for each member 
# (as opposed to a separate instance not each member & timeslot)
#
# MODULES USED:
#  (e.g. on Gaea)
#  module swap PrgEnv-pgi PrgEnv-intel
#  module load netcdf
#
# INPUTS:
#  YYYYMMDDHH    :: string containing 4-digit year, 2-digit month, 2-digit day, 2-digit hour
#  MEMBERID      :: Ensemble member number
#  EXP_DATA      :: directory containing experiment output data
#  NSLOTS        :: number of timeslots to use for 4D-LETKF (e.g. "5" for 5 days)
#  days          :: forecast length (in integer days)
#  OBSOPexe      :: execuatable for LETKF observation operator
#  OBSDIR1       :: Observation directory, primary
#  OBSDIR5       :: Observation directory to use for analysis time only
#  INPUT_INIT    :: Directory containing static model input files
#  LDIR          :: directory of letkf executable and obsoperator executable
#  USE_ALTIMETRY :: flag to assimilate altimetry data (1==true,0==false)
#  altimetry_climatology_file       :: model eta climatology, for assimilating AVISO altimetry  
# 
#===============================================================================
# Author      :: Stephen G. Penny
# Institution :: University of Maryland (UMD) 
#                Department of Atmospheric and Oceanic Science (AOSC), and
#                National Centers for Environmental Prediction (NCEP)
#                National Oceanograpic and Atmospheric Administration (NOAA)
# Email       :: Steve.Penny@noaa.gov
#===============================================================================
set -e
ncatted=/gpfs1/home/Libs/GNU/NCO/nco-4.4.2/bin/ncatted

echo "LETKF preparation step"
echo "processing cycle: ${YYYYMMDDHH}"
echo "I am member ${MEMBERID}"
workdir_fcst=${EXP_DATA}/${YYYYMMDDHH}/model/${MEMBERID}/RESTART
workdir2=${EXP_DATA}/${YYYYMMDDHH}/letkf
mkdir -p ${workdir2}

MEM3=`printf %.3d ${MEMBERID}`

#STEVE: active code:
TMPDIR=${EXP_DATA}/${YYYYMMDDHH}
IY=${YYYYMMDDHH:0:4}
IM=${YYYYMMDDHH:4:2}
ID=${YYYYMMDDHH:6:2}
IH=${YYYYMMDDHH:8:2}
IN=00
IS=00

FCST=0
while test $FCST -lt $NSLOTS
do
  FCST=`expr $FCST + 1`
  ISLOT2=`printf %.2d ${FCST}`
  echo "Posting forecast ${ISLOT2}"
  workdir=${EXP_DATA}/${YYYYMMDDHH}/letkf_prep/${MEMBERID}/${ISLOT2}
  mkdir -p ${workdir}
  cd ${workdir}
  echo "This is the LETKF preparation step for member ${MEMBERID}, for cycle ${YYYYMMDDHH}" > $workdir/letkf_prep_${MEMBERID}.out

  # Update the ISLOT date
  date=/bin/date
  #sinc=`expr $FCST - 1` #STEVE: use this if I want the dates offset
  sinc=1
  sinc_units=days
  NY=`$date -d "$IY-$IM-$ID $sinc $sinc_units" +%Y`
  NM=`$date -d "$IY-$IM-$ID $sinc $sinc_units" +%m`
  ND=`$date -d "$IY-$IM-$ID $sinc $sinc_units" +%d`
  NH=`$date -d "$IY-$IM-$ID $sinc $sinc_units" +%H`
  IY=$NY
  IM=$NM
  ID=$ND
  IH=$NH
  echo "processing cycle as: $IY$IM$ID$IH"

  #-----------------------------------------------------------------------------
  # FIRST, link the background model data
  #-----------------------------------------------------------------------------

  ln -f $workdir_fcst/$IY$IM$ID.$IH$IN$IS.ocean_temp_salt.res.nc     gs${ISLOT2}$MEM3.ocean_temp_salt.res.nc
  ln -f $workdir_fcst/$IY$IM$ID.$IH$IN$IS.ocean_velocity.res.nc      gs${ISLOT2}$MEM3.ocean_velocity.res.nc
  ln -f $workdir_fcst/$IY$IM$ID.$IH$IN$IS.ocean_sbc.res.nc           gs${ISLOT2}$MEM3.ocean_sbc.res.nc
  if [ "$USE_ALTIMETRY" -eq "1" ]; then
    ln -f $workdir_fcst/$IY$IM$ID.$IH$IN$IS.ocean_barotropic.res.nc  gs${ISLOT2}$MEM3.ocean_barotropic.res.nc
  fi

  #STEVE: add 'fill value' to netcdf files for identification of missing values
  cp $ncatted .
  ncatted -O -a _FillValue,temp,o,f,-1.e+34 gs${ISLOT2}$MEM3.ocean_temp_salt.res.nc
  ncatted -O -a _FillValue,salt,o,f,-1.e+34 gs${ISLOT2}$MEM3.ocean_temp_salt.res.nc
  ncatted -O -a _FillValue,u,o,f,-1.e+34 gs${ISLOT2}$MEM3.ocean_velocity.res.nc
  ncatted -O -a _FillValue,v,o,f,-1.e+34 gs${ISLOT2}$MEM3.ocean_velocity.res.nc
  ncatted -O -a _FillValue,sea_lev,o,f,-1.e+34 gs${ISLOT2}$MEM3.ocean_sbc.res.nc
  if [ "$USE_ALTIMETRY" -eq "1" ]; then
    ncatted -O -a _FillValue,eta_t,o,f,-1.e+34 gs${ISLOT2}$MEM3.ocean_barotropic.res.nc
  fi

  #STEVE: need a template for the output analysis files:
  #       MUST COPY, NOT LINK. The files WILL be overwritten.
  if [ "${ISLOT2}" -eq "${ATIME}" ]; then #STEVE: trying to only do it once per member...
    cp ${workdir}/gs${ISLOT2}$MEM3.ocean_temp_salt.res.nc ${workdir2}/anal$MEM3.ocean_temp_salt.res.nc
    cp ${workdir}/gs${ISLOT2}$MEM3.ocean_velocity.res.nc  ${workdir2}/anal$MEM3.ocean_velocity.res.nc
    cp ${workdir}/gs${ISLOT2}$MEM3.ocean_sbc.res.nc       ${workdir2}/anal$MEM3.ocean_sbc.res.nc
    if [ "$USE_ALTIMETRY" -eq "1" ]; then
      cp ${workdir}/gs${ISLOT2}$MEM3.ocean_barotropic.res.nc  ${workdir2}/anal$MEM3.ocean_barotropic.res.nc
    fi
    #STEVE: may want to zero these out for peace of mind...
  fi

  #-----------------------------------------------------------------------------
  # SECOND, link the observations
  #-----------------------------------------------------------------------------

  # The next conditional gives the ability to use different observation source 
  # collection for different slots. (e.g. only use surface obs at time of 
  # analysis, use profiles through analysis cycle window)
  echo "For ISLOT2=$ISLOT2, and ATIME=${ATIME}"
  if [ "$ISLOT2" -eq "${ATIME}" ]; then
    OBSDIR_SST=$OBSDIR5_SST
    OBSDIR_SSS=$OBSDIR5_SSS
    OBSDIR_ETA=$OBSDIR5_ETA
    OBSDIR_TEMP=$OBSDIR5_TEMP
    OBSDIR_SALT=$OBSDIR5_SALT
  else
    OBSDIR_SST=$OBSDIR1_SST
    OBSDIR_SSS=$OBSDIR1_SSS
    OBSDIR_ETA=$OBSDIR1_ETA
    OBSDIR_TEMP=$OBSDIR1_TEMP
    OBSDIR_SALT=$OBSDIR1_SALT
  fi

  echo "Processing obs: ${OBSDIR_SST}/$IY$IM$ID.dat to obs${ISLOT2}${MEM3}.dat and so on"
  ln -f $INPUT_INIT/grid_spec.nc .
#SIVA: copy all individual obsop_* executables and the one which combines all these observations
  cp $LDIR/$OBSOPexe_sst .	
  cp $LDIR/$OBSOPexe_sss .	
  cp $LDIR/$OBSOPexe_eta .	
  cp $LDIR/$OBSOPexe_temp .	
  cp $LDIR/$OBSOPexe_salt .
  cp $LDIR/$OBSOPexe_combine .	
#
#SIVA:
#Link observations to the respective obsin_*.dat. If the data is not available, create a dummy file so that obsop_* runs no matter original data available or not
  if [ -f "$OBSDIR_SST/$IY$IM$ID.dat" ]; then	
   ln -f $OBSDIR_SST/$IY$IM$ID.dat obsin_sst.dat
  else
   echo "No SST data for $IY$IM$ID.dat found. Continuing with dummy file"
   touch obsin_sst.dat
  fi

  if [ -f "$OBSDIR_SSS/$IY$IM$ID.dat" ]; then	
   ln -f $OBSDIR_SSS/$IY$IM$ID.dat obsin_sss.dat
  else
   echo "No SSS data for $IY$IM$ID.dat found. Continuing with dummy file"
   touch obsin_sss.dat
  fi

  if [ -f "$OBSDIR_ETA/$IY$IM$ID.dat" ]; then	
   ln -f $OBSDIR_ETA/$IY$IM$ID.dat obsin_eta.dat
  else
   echo "No ETA data for $IY$IM$ID.dat found. Continuing with dummy file"
   touch obsin_eta.dat
  fi

  if [ -f "$OBSDIR_TEMP/$IY$IM$ID.dat" ]; then	
   ln -f $OBSDIR_TEMP/$IY$IM$ID.dat obsin_temp.dat
  else
   echo "No TEMP data for $IY$IM$ID.dat found. Continuing with dummy file"
   touch obsin_temp.dat
  fi

  if [ -f "$OBSDIR_SALT/$IY$IM$ID.dat" ]; then	
   ln -f $OBSDIR_SALT/$IY$IM$ID.dat obsin_salt.dat
  else
   echo "No SALT data for $IY$IM$ID.dat found. Continuing with dummy file"
   touch obsin_salt.dat
  fi
#SIVA: END of linking observation data sets
#
  ln -f ${workdir}/gs${ISLOT2}$MEM3.ocean_temp_salt.res.nc gues.ocean_temp_salt.res.nc
  ln -f ${workdir}/gs${ISLOT2}$MEM3.ocean_velocity.res.nc  gues.ocean_velocity.res.nc
  ln -f ${workdir}/gs${ISLOT2}$MEM3.ocean_sbc.res.nc       gues.ocean_sbc.res.nc
  if [ "$USE_ALTIMETRY" -eq "1" ]; then
    ln -f ${workdir}/gs${ISLOT2}$MEM3.ocean_barotropic.res.nc       gues.ocean_barotropic.res.nc
    echo "Linking: $altimetry_climatology_file to here..."
    if [ -f "$altimetry_climatology_file" ]; then
      ln -f $altimetry_climatology_file .
    else
      echo "$altimetry_climatology_file does not exist..."
      echo "Exiting..."
      exit 1
    fi
  fi

  ####################################################################################################################################
  # Running LETKF Obs Operator executable to generate the observation innovations for each member at each timestep:
  ####################################################################################################################################
# $OBSOPexe -obsin $OBSDIR/$IY$IM$ID.dat -gues gs${ISLOT2}$MEM3 -obsout ${workdir2}/obs${ISLOT2}${MEM3}.dat > obsope.log
  #STEVE: (perhaps make parallel and call with aprun)
  rm -f obsout_*.dat obsout.dat	#SIVA: Just to make sure we process latest data
  $OBSOPexe_sst 
  $OBSOPexe_sss 
  $OBSOPexe_eta 
  $OBSOPexe_temp 
  $OBSOPexe_salt 
#  touch obsin_sst.dat obsin_sss.dat obsin_eta.dat obsin_temp.dat obsin_salt.dat   #SIVA: Just want to make sure OBSOPexe_combine run successful
  $OBSOPexe_combine
#  rm -f obsout_*.dat #SIVA: can be used if necessary

  if [ -f "obsout.dat" ]; then
    ln -f obsout.dat ${workdir2}/obs${ISLOT2}${MEM3}.dat
  else
    echo "output obs2 formatted file not created by $OBSOPexe_combine."
    pwd
    ls
    echo "Exiting..."
    exit 2
  fi
  ln -f ${workdir}/gs${ISLOT2}$MEM3.ocean_temp_salt.res.nc ${workdir2}/gs${ISLOT2}$MEM3.ocean_temp_salt.res.nc
  ln -f ${workdir}/gs${ISLOT2}$MEM3.ocean_velocity.res.nc  ${workdir2}/gs${ISLOT2}$MEM3.ocean_velocity.res.nc
  ln -f ${workdir}/gs${ISLOT2}$MEM3.ocean_sbc.res.nc       ${workdir2}/gs${ISLOT2}$MEM3.ocean_sbc.res.nc
  if [ "$USE_ALTIMETRY" -eq "1" ]; then
    ln -f ${workdir}/gs${ISLOT2}$MEM3.ocean_barotropic.res.nc       ${workdir2}/gs${ISLOT2}$MEM3.ocean_barotropic.res.nc
  fi

  #STEVE: the hard-link limit is running out (65000 max), so best to delete unnecessary links
  rm -f grid_spec.nc
  rm -f ncatted
done #DONE FCST loop
exit 0
