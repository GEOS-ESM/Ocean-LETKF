#!/bin/bash

NETCDF_DIR=$NETCDF
NETCDF_INC="-I${NETCDF_DIR}/include"
NETCDF_LIB="-L${NETCDF_DIR}/lib -lnetcdf -lnetcdff"
