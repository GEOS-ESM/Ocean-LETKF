#!/bin/csh -f

#-----------------------------------------------------------------------
# Modify below according to your environment
#-----------------------------------------------------------------------
#set OBSDIR=/incois/siva/OBS/SUPEROBS/LETKF
#set OBSDIR=/incois/siva/OBS/AQUARIUS_L2/LETKF
#set OBSDIR=/incois/siva/OBS/ALTIMETER/LETKF
#set OBSDIR=/incois/siva/OBS/IG_TSPRFS/LETKF
set OBSDIR=$1
set var=$2
set MTYPE=$3
#set var=salt
#set MTYPE=GUES
#set MTYPE=ANAL
### directory settings
set WDIR=`pwd`
set OUTPUT=/incois/arya/mom4p1_letkf/RMSE_TEST/$MTYPE
set OBSOPE=obsop_$var.016
set IG_grid=/incois/siva/mom4p1_letkf/mom4p1/work/IG_grid
set MEAN=$IG_grid/ARCHIVE/$MTYPE/MEAN
#
# Work directory
#
echo '>>>'
echo " >>"
echo " >> Entering Loop"
echo " >>"
echo $MEAN
### inputs
ln -fs ../$OBSOPE $OBSOPE
ln -fs $IG_grid/INPUT/grid_spec.nc grid_spec.nc
foreach fname (`ls $MEAN/*sbc.res.nc`)
 echo $fname
 set mdate=`echo $fname | awk '{print substr($0,65,10)}'`
 set obsdate=`echo $fname | awk '{print substr($0,65,8)}'`
 echo $mdate
 echo '>>>'
 echo "OBSOPE Starting for $mdate"
 ln -fs $OBSDIR/$obsdate.dat obsin_$var.dat
 ln -fs $MEAN/$mdate.ocean_temp_salt.res.nc gues.ocean_temp_salt.res.nc
 ln -fs $MEAN/$mdate.ocean_velocity.res.nc gues.ocean_velocity.res.nc
 ln -fs $MEAN/$mdate.ocean_barotropic.res.nc gues.ocean_barotropic.res.nc
 ln -fs $MEAN/$mdate.ocean_sbc.res.nc gues.ocean_sbc.res.nc
 ./$OBSOPE  > obsope.log
 mv obsout_$var.dat $OUTPUT/${obsdate}_$var.dat
 rm -f gues.ocean_temp_salt.res.nc 
 rm -f gues.ocean_velocity.res.nc
 rm -f gues.ocean_barotropic.res.nc
 rm -f gues.ocean_sbc.res.nc
 rm -f obsin_$var.dat
end
echo "Normal End"
