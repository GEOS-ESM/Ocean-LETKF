#!/bin/csh -f

echo "File = $0"
echo "Work Directory  = $1"
echo "Experiment Name = $2 (make sure to include ensemble member id subscript)"
echo "Model Type      = $3"
echo "Number of Procs = $4"
#echo "llfile          = $5 (mom4p1.ll)"

#Source the system initialization scripts (but not the user's so as to avoid interactive settings that can poison us)
source /etc/csh.cshrc

set platform      = ifc      # A unique identifier for your platform
set name          = $2 #00            # Name of the experiment you want to run
                                   # One of box1, box_channel1, bowl1, dome1, gyre1, iom1, mk3p51, symmetric_box1, torus1
set npes          = $4             # number of processors
                                   # Note: If you change npes you may need to change
                                   # the layout in the corresponding namelist
set type          = $3 #mom4p1_solo_prod            # type of the experiment
set workdir       = $1             # where the model is run and model output is produced
                                   # This is recommended to be a link to the $WORKDIR of the platform.
set expdir        = $workdir/$name 
set inputDataDir  = $expdir/INPUT   # This is path to the directory that contains the input data for this experiment.
                                    # You should have downloaded and untared this directory from MOM4p1 FTP site.
set diagtable     = $inputDataDir/diag_table  # path to diagnositics table
set datatable     = $inputDataDir/data_table  # path to the data override table.
set fieldtable    = $inputDataDir/field_table # path to the field table
set namelist      = $inputDataDir/input.nml   # path to namelist file

set root          = /incois/siva/mom4p1_letkf/mom4p1 #$cwd:h         # The directory in which you checked out src
set executable    = $root/exec_$platform/$type/fms_$type.x      # executable created after compilation
set mppnccombine  = $root/bin/mppnccombine.$platform  # path to executable mppnccombine
set time_stamp    = $root/bin/time_stamp.csh          # path to cshell to generate the date

#===========================================================================
# The user need not change any of the following
#===========================================================================

#
# Users must ensure the correct environment file exists for their platform.
#

# Check if the user has extracted the input data
  if ( ! -d $inputDataDir ) then
    echo "ERROR: the experiment directory '$inputDataDir' does not exist or does not contain input and preprocessing data directories!"
    exit 1
  endif

set echo

# setup directory structure
  if ( ! -d $expdir )         mkdir -p $expdir
  if ( ! -d $expdir/RESTART ) mkdir -p $expdir/RESTART

#
#Check the existance of essential input files
#
   if ( ! -e $inputDataDir/grid_spec.nc ) then
     echo "ERROR: required input file does not exist $inputDataDir/grid_spec.nc "
     exit 1
   endif
   if ( ! -e $inputDataDir/ocean_temp_salt.res.nc ) then
     echo "ERROR: required input file does not exist $inputDataDir/ocean_temp_salt.res.nc "
     exit 1
   endif

#Change to expdir

  cd $expdir
  echo "pwd:: `pwd`"
  echo "Should be: $expdir"

# Create INPUT directory. Make a link instead of copy
# 
if ( ! -d $expdir/INPUT   ) mkdir -p $expdir/INPUT

  if ( ! -e $namelist ) then
    echo "ERROR: required input file does not exist $namelist "
    exit 1
  endif
  if ( ! -e $datatable ) then
    echo "ERROR: required input file does not exist $datatable "
    exit 1
  endif
  if ( ! -e $diagtable ) then
    echo "ERROR: required input file does not exist $diagtable "
    exit 1
  endif
  if ( ! -e $fieldtable ) then
    echo "ERROR: required input file does not exist $fieldtable "
    exit 1
  endif

  cp $namelist   $expdir/input.nml
  cp $datatable  $expdir/data_table
  cp $diagtable  $expdir/diag_table
  cp $fieldtable $expdir/field_table 


#   --- run the model ---

  cd $expdir

  if($npes > 1) then
#    mpiexec_mpt -np $npes $executable > $expdir/fmt.out
     echo "Calling: mpirun -n $npes $executable > $expdir/fms.out"
     cp $executable $expdir/
     #ISSUE: aprun is not universal, update to make this 
     #       mpi run command an input
     mpirun -n $npes $expdir/fms_$type.x > $expdir/fms.out
  else
     $executable:t > fms.out
  endif

#----------------------------------------------------------------------------------------------
# generate date for file names ---
    set begindate = `$time_stamp -bf digital`
	echo "TIME STAMP IS $begindate"
    if ( $begindate == "" ) set begindate = tmp`date '+%j%H%M%S'`
    set enddate = `$time_stamp -ef digital`
    if ( $enddate == "" ) set enddate = tmp`date '+%j%H%M%S'`
    if ( -f time_stamp.out ) rm -f time_stamp.out
#----------------------------------------------------------------------------------------------
# get a tar restart file
  cd RESTART
  cp $expdir/input.nml $expdir/RESTART 
  cp $expdir/*_table $expdir/RESTART
# combine netcdf files
  if ( $npes > 1 ) then
    set file_previous = ""
    set multires = (`ls *.nc.????`)
    foreach file ( $multires )
	if ( $file:r != $file_previous:r ) then
	    set input_files = ( `ls $file:r.????` )
              if ( $#input_files > 0 ) then
                 $mppnccombine $file:r $input_files
                 if ( $status != 0 ) then
                   echo "ERROR: in execution of mppnccombine on restarts"
                   exit 1
                 endif
                 rm $input_files
              endif
           else
              continue
           endif
           set file_previous = $file
       end
  endif
  cd $expdir
  mkdir -p history
  mkdir -p ascii
#----------------------------------------------------------------------------------------------
# rename ascii files with the date
  foreach out (`ls *.out`)
     mv $out ascii/$begindate.$out
  end

#----------------------------------------------------------------------------------------------
# combine netcdf files
  if ( $npes > 1 ) then
    set file_previous = ""
    set multires = (`ls *.nc.????`)
    foreach file ( $multires )
	if ( $file:r != $file_previous:r ) then
	    set input_files = ( `ls $file:r.????` )
              if ( $#input_files > 0 ) then
                 $mppnccombine $file:r $input_files
                 if ( $status != 0 ) then
                   echo "ERROR: in execution of mppnccombine on restarts"
                   exit 1
                 endif
                 rm $input_files
              endif
           else
              continue
           endif
           set file_previous = $file
       end
  endif

#----------------------------------------------------------------------------------------------
# rename nc files with the date
  foreach ncfile (`/bin/ls *.nc`)
     mv $ncfile history/$begindate.$ncfile
  end

  unset echo
#
#SIVA: CHECK WHETHER ANY RESTART IS GENERATED
  cd $expdir/RESTART
  if ( ! -e ocean_temp_salt.res.nc ) then
   echo "SIVA: ocean_temp_salt.res.nc does not exist in $expdir/RESTART"
   echo "Model run is not Successful"
   echo "exiting with exit code 1"
   exit 1
  endif
  cd $expdir
#End of checking

echo end_of_run
echo "NOTE: mom4run_Aditya.csh:: Natural end-of-script."

exit 0
  
