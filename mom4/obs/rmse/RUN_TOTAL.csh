#!/bin/csh
csh run_obsop_mod.csh /incois/siva/OBS/IG_TSPRFS/LETKF salt GUES
csh run_obsop_mod.csh /incois/siva/OBS/IG_TSPRFS/LETKF salt ANAL

csh run_obsop_mod.csh /incois/siva/OBS/IG_TSPRFS/LETKF temp GUES
csh run_obsop_mod.csh /incois/siva/OBS/IG_TSPRFS/LETKF temp ANAL

csh  run_obsop_mod.csh /incois/siva/OBS/ALTIMETER/LETKF eta GUES
csh  run_obsop_mod.csh /incois/siva/OBS/ALTIMETER/LETKF eta ANAL

csh run_obsop_mod.csh /incois/siva/OBS/SUPEROBS/LETKF sst GUES
csh run_obsop_mod.csh /incois/siva/OBS/SUPEROBS/LETKF sst ANAL

csh run_obsop_mod.csh /incois/siva/OBS/AQUARIUS_L2/LETKF sss GUES
csh run_obsop_mod.csh /incois/siva/OBS/AQUARIUS_L2/LETKF sss ANAL
