----------------------------------------------------------------------
CALMET  Meteorological Model
----------------------------------------------------------------------

Purpose:
--------
          Constructs gridded meteorological fields for use in CALPUFF,
          CALGRID, and other transport and dispersion models.

Demonstration:
--------------

          Generate meteorological fields for a 3-dimensional domain
          consisting of 10 levels in the vertical, with a horizontal
          grid of 99x99 1-km cells, for a 4-hour period.  Data sources
          include surface stations, upper-air stations, over-water
          stations, precipitaion stations, and output from the MM4
          prognostic model.


Files for Demonstration:
------------------------

   NAME        TYPE                DESCRIPTION
___________   _______       ____________________________

 CALMET.INP   Input          Control file
 GEO1KM.DAT   Input          Geophysical data for domain
 SURF.DAT     Input          Surface meteorological data
 UPALBR.DAT   Input          Upper-air data for 1 station
 UPCHH.DAT    Input          Upper-air data for 1 station
 UPPWM.DAT    Input          Upper-air data for 1 station
 4007.DAT     Input          Over-water meteorological data
 PRECIP.DAT   Input          Precipitation data
 MM4.DAT      Input          Prognostic data in MM4 format

 CALMET.DAT   Output         "CALMET.DAT" file
 CALMET.LST   Output         List-file for application
 QA3D.DAT     Output         QA file of MM4 cell centers

 CALMETL.EXE  Executable     CALMET executable



Procedure:
----------

1.  This application may be run from the DOS prompt, or by means of
    the graphical user interface (GUI).  If run from DOS, follow the
    instructions in Step 2.  If using the GUI, we assume that the 
    CALMET icon is available on your desktop.

2.  Instructions for DOS (optional)

    (a)  Run the program by typing:

         CALMET calmet.inp [return]

    (b)  The program reads the processing instructions from CALMET.INP,
         overwrites the output list-file CALMET.LST, and creates a
         new output file CALMET.DAT.

    (c)  Review the .LST file for the configuration used, and other
         information from the input files read during execution.


3.  Instructions for the GUI


    (a)  Double click on the CALMET icon to access the General
         Information Screen.

    (b)  Verify that the system is using the "large" CALMET executable
         and its associated parameter-file (CALMETL.EXE and 
         PARAMSL.MET).

    (c)  If the "large" version is active, go to step (e).

    (d)  Configure the system for the "large" version as follows:
          Click on Setup.  Click on Set Executables.
          Click in the executable filename box.
                Edit the name to change CALMET to CALMETL.
          (make no other changes to the pathname!)
          Click in the parameter filename box.
                Edit the name to change PARAMS to PARAMSL.
          (make no other changes to the pathname!)
                Hit OK.
                Allow GUI to shut down (hit OK).
                Do not save current control file (hit NO).
          Return to step (a).

    (e)  Set the working directory to where the input files for this
         demonstration are located.
          Click on File.  Click on Change Directory.
                Double click on demo (under c:\calpuff\).
                Double click on calmet (under c:\calpuff\demo\).
                Hit OK.

    (f)  Click on File.  Click on Open.  Select CALMET.INP.

    (g)  Click on Run.  Click on Run CALMET

    (h)  Click on OK.  Click on Run.

    (i)  A DOS window opens for the model run; enter any key to close
         this window when the run is completed.

    (j)  To view the output list file, click on Utilities.  Click on
         View File.  Select CALMET.LST.  To exit view, hit X or ESC.
         Hit F1 for a list of commands when in the view utility.

    (k)  To exit the CALMET GUI, click on File.  Click on Exit.

 