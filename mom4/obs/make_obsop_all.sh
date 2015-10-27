#!/bin/sh
set -exv

# sh make_obsop.sh $MEM
source ../../config/machine.sh
source ../../config/$MACHINE.fortran.sh
source ../../config/$MACHINE.netcdf.sh

sh ulnkcommon.sh
sh lnkcommon.sh
rm -f *.mod
rm -f *.o

# Ensemble size
# STEVE: figure out how to read from params_letkf.f90 and put here (e.g. with awk/perl/etc.)
#        -> grep and sed seem to work ok:
#        (Set the ensemble size in params_letkf.f90, it will read it in here)
MEM=`grep nbv= params_letkf.f90 | sed -r 's/INTEGER,PARAMETER :: nbv=([0-9]+)/\1/'`
echo "MEM=$MEM"
MEM3=`printf %.3d ${MEM}`

PGM=obsop_sss.$MEM3
#F90OPT='-ftz -ip -ipo -O2 -parallel -i_dynamic -what -fpp -fno-alias -stack_temps -safe_cray_ptr -fast'

$F90 $OMP $F90_OPT $INLINE $F90_OBJECT_FLAG SFMT.f90
$F90 $OMP $F90_OPT $INLINE $F90_OBJECT_FLAG common.f90
$F90 $OMP $F90_OPT $F90_OBJECT_FLAG params_model.f90
$F90 $OMP $F90_OPT $F90_OBJECT_FLAG vars_model.f90
$F90 $OMP $F90_OPT $F90_OBJECT_FLAG params_letkf.f90
$F90 $OMP $F90_OPT $F90_DEBUG $F90_INLINE $NETCDF_INC $F90_OBJECT_FLAG common_mom4.f90
$F90 $OMP $F90_OPT $F90_OBJECT_FLAG params_obs.f90
$F90 $OMP $F90_OPT $F90_OBJECT_FLAG vars_obs.f90
$F90 $OMP $F90_OPT $F90_OBJECT_FLAG common_obs_mom4.f90
#--
$F90 $OMP $F90_OPT $F90_DEBUG $F90_OBJECT_FLAG gsw_oceanographic_toolbox.f90
$F90 $OMP $F90_OPT $F90_DEBUG $F90_OBJECT_FLAG gsw_pot_to_insitu.f90
#--
#SIVA: obsop_tools is added
$F90 $OMP $F90_OPT $F90_OBJECT_FLAG $NETCDF_INC obsop_tools.f90
#--
#SIVA: compile and get executable for each of the obsop_*.f90 programs 
$F90 $OMP $F90_OPT $F90_OBJECT_FLAG obsop_sst.f90 
$F90 $OMP $F90_OPT -o obsop_sst.$MEM3 *.o $NETCDF_LIB
rm -f obsop_sst.o

$F90 $OMP $F90_OPT $F90_OBJECT_FLAG obsop_sss.f90 
$F90 $OMP $F90_OPT -o obsop_sss.$MEM3 *.o $NETCDF_LIB
rm -f obsop_sss.o

$F90 $OMP $F90_OPT $F90_OBJECT_FLAG obsop_eta.f90 
$F90 $OMP $F90_OPT -o obsop_eta.$MEM3 *.o $NETCDF_LIB
rm -f obsop_eta.o

$F90 $OMP $F90_OPT $F90_OBJECT_FLAG obsop_temp.f90 
$F90 $OMP $F90_OPT -o obsop_temp.$MEM3 *.o $NETCDF_LIB
rm -f obsop_temp.o

$F90 $OMP $F90_OPT $F90_OBJECT_FLAG obsop_salt.f90 
$F90 $OMP $F90_OPT -o obsop_salt.$MEM3 *.o $NETCDF_LIB
rm -f obsop_salt.o

$F90 $OMP $F90_OPT $F90_OBJECT_FLAG obs2combine.f90
$F90 $OMP $F90_OPT -o obs2combine *.o $NETCDF_LIB
rm -f obs2combine.o
#
#Clean..
rm -f *.mod
rm -f *.o
sh ulnkcommon.sh
#--
#--

echo "NORMAL END"
