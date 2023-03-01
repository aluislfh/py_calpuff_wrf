NAME
     CALWRF -- Reads NCAR’s WRF-ARW model output (netCDF format) in either
     Version 2 or Version 3 WRF format and creates a 3D.DAT file suitable for
     input into CALMET.

SYNOPSIS
     calwrf.exe

DESCRIPTION
     CALWRF assumes the input control file is "calwrf.inp".
     There are no command line options.
     
     For more information, see Section 7.5 ("CALWRF Preprocessor") of the
     CALPUFF manual:
     http://www.src.com/calpuff/download/CALPUFF_Version6_UserInstructions.pdf

COMPILING ON LINUX
     1) Get netcdf library
        http://www.unidata.ucar.edu/software/netcdf/
     
     2) Get pgf95 Fortran compiler
        http://www.pgroup.com/products/pgiworkstation.htm

     3) Use "compile.sh" BASH script to compile on Linux.
     
COMPILING ON WINDOWS
     1) Get and install gfortran for Windows.
        http://gcc.gnu.org/wiki/GFortranBinaries
        http://users.humboldt.edu/finneyb/gfortran-windows-20130301.exe

     2) Get pre-compiled NetCDF from UCAR.
        ftp://ftp.unidata.ucar.edu/pub/netcdf/contrib/win32/netcdf-3.6.2-beta5_pgi_w32bin.zip
     
     3) Extract "netcdf-3.6.2-beta5_pgi_w32bin.zip" into the same directory
        as the CALWRF source code ("code" directory).

     4) Run "compile.bat"

     5) When running CALWRF, make sure the following 4 DLL files stay in same
        directory as "calwrf.exe":

        Three gcc/gfotran DLL files:
        * libgcc_s_dw2-1.dll
        * libgfortran-3.dll
        * libquadmath-0.dll
        Available at:
        C:\Program Files\gfortran\bin\
        or
        C:\Program Files (x86)\gfortran\bin\
        See "GCC Runtime Library Exception.pdf" for license to redistribute.

        One NetCDF DLL file:
        * libnetcdf-0.dll
        See "netcdf.h" for license to redistribute.
  
KEY FILES
     binary_linux/calwrf.exe
          ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically
          linked, for GNU/Linux 2.4.0, not stripped
          (Tested on CentOS 4.4, Linux kernel 2.6.9-67)

     binary_windows/calwrf.exe
          PE32 executable for MS Windows (console) Intel 80386 32-bit
          (Tested on Windows XP and Windows 7)
          Requires the 4 DLL files and netcdf.h in the "binary_windows"
          directory file to operate.
 
     code/calwrf_v2.0.1.f
          CALWRF's main Fortran source code file. Comment lines near top of the
          file contain a version change log.

COPYRIGHT
     Copyright (c) 2013 Exponent, Inc.

LAST UPDATED
     Version: 2.0.1
     Date: 2013-08-13 08:00
