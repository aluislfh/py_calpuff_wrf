----------------------------------------------------------------------
PRTMET  Display Processor for "CALMET.DAT"
----------------------------------------------------------------------

Purpose:
--------
	  Extracts and prints selected fields from a "CALMET.DAT" file
          created by CALMET.  Any data type in the file may be
          selected for a particular range of hours, for any subset of
          the CALMET grid.

Demonstration:
--------------

          Print the winds at level 3 and the mixing heights for a 7x7
          subset of a 99x99 CALMET grid, for a 4-hour period.
          Produce domain characteristic QA plot files for landuse, terrain,
          leaf area index, and roughness length.


Files for Demonstration:
------------------------

   NAME        TYPE     SIZE(bytes)             DESCRIPTION
___________   _______  ____________     ____________________________

 PRTMET.EXE   Program     1,928,920     Executable

 PRTMET.INP   Input          14,871     Control file
(..\CALPUFF\
 CMET.DAT     Input       8,344,444     "CALMET.DAT" file)

 PRTMET.LST   Output         27,748     List-file for application

 QAMETG.BNA   Output            152     Plot-file for Met Grid boundary

 QALUSE.GRD   Output            355     Plot-file for landuse

 QATERR.GRD   Output            698     Plot-file for terrain elevation

 QALAI.GRD    Output            698     Plot-file for leaf area index

 QAZ0.GRD     Output            698     Plot-file for roughness length

 QASSTA.DAT   Output            159     Plot-file for surface station locations

 QAUSTA.DAT   Output            107     Plot-file for upper-air station locations

 QAPSTA.DAT   Output            452     Plot-file for precipitation station locations

 LUSE.CLR     Output            534     SURFER color file for land use map

 PGT.CLR      Output            326     SURFER color file for PG class map

Procedure:
----------

1.  This application uses the "CALMET.DAT" file that is located in the
    CALPUFF subdirectory for this demonstration.  If you have not
    installed this directory, you cannot run this PRTMET demonstration.
    To see if this file exists, type the following command:

    dir ..\calpuff\cmet.dat [return]

    The response to this command should list the file name, its size,
    and creation date/time.

2.  Run the program by typing:

    prtmet prtmet.inp [return]

3.  The program reads the processing instructions from PRTMET.INP, and
    overwrites the output files listed above.

4.  Review the .LST file for the configuration used, and the results
    obtained.
