#!/bin/sh
# Compile calwrf on Linux using pgf95

if [ -f calwrf.exe ]; then rm calwrf.exe; fi

pgf95 -Bstatic calwrf_v2.0.1.f -lnetcdf -lm -o calwrf.exe
