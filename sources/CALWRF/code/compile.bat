REM Compiling and linking with gfortran

@REM prevent conflicts if G95 is installed
set LIBRARY_PATH=

gfortran -o calwrf.exe calwrf_v2.0.1.f -L. -llibnetcdf-0
