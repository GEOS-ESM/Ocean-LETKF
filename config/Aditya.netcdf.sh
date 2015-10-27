#!/bin/bash

NETCDF_DIR=/gpfs1/home/Libs/INTEL/NETCDF4/netcdf-4.1.3
NETCDFF_DIR=/gpfs1/home/Libs/INTEL/NETCDF4/netcdf-4.1.3
NETCDF_LIB="-L$NETCDFF_DIR/lib -lnetcdff -L$NETCDFF_DIR/lib -lnetcdf"
NETCDF_INC="-I$NETCDFF_DIR/include -I$NETCDF_DIR/include"
