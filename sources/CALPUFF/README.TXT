----------------------------------------------------------------------
CALPUFF  Transport and Dispersion Model
----------------------------------------------------------------------

Purpose:
--------
          Simulates the movement, dispersion, and transformation/removal
          of gases and particles released to the atmosphere, and
          computes concentration/deposition at specified locations.

Demonstration:
--------------

          Compute hourly concentration and wet and dry deposition at
          grid-cell locations (1 km resolution), allowing for chemical
          transformations.  A single point source is simulated for a 
          4-hour period.


Files for Demonstration:
------------------------

   NAME        TYPE             DESCRIPTION
___________   _______       ____________________________

 CALPUFF.INP  Input         Control file
 CALMET.DAT   Input         "CALMET.DAT" file
 OZONE.DAT    Input         Hourly ozone concentrations

 CALPUFF.LST  Output        List-file for application
 CALPUFF.CON  Output        Concentration file
 CALPUFF.DRY  Output        Dry flux file
 CALPUFF.WET  Output        Wet flux file
 CALPUFF.VIS  Output        Rel. humidity file (visibility)
 QATERR.GRD   Output        QA terrain grid file
 QAGRID.BNA   Output        QA grid boundary file
 QALUSE.GRD   Output        QA landuse grid file
 QAPNTS.DAT   Output        QA point source location file
 QARECG.DAT   Output        QA gridded receptor file
 LUSE.CLR     Output        Default landuse color scale

 CALPUFF.EXE Executable     CALPUFF executable


Procedure:
----------

1.  This application may be run from the DOS prompt, or by means of
    the graphical user interface (GUI).  If run from DOS, follow the
    directions in Step 2.  If using the GUI, we assume that the CALPUFF
    icon is available on your desktop.

2.  Instructions for DOS (optional)

    (a)  Run the program by typing:

         CALPUFF calpuff.inp [return]

    (b)  The program reads the processing instructions from CALPUF.INP,
         overwrites the output files listed above.

    (c)  Review the .LST file for the configuration used, and other
         information from the input files read during execution.  In
         this application, concentrations computed each hour are
         written to the list file CALPUFF.LST.


3.  Instructions for the GUI


    (a)  Double click on the CALPUFF icon to access the General
         Information Screen.

    (b)  Verify that the system is using the "large" CALPUFF executable
         and its associated parameter-file (CALPUFFL.EXE and
         PARAMSL.PUF).

    (c)  If the "large" version is active, go to step (e).

    (d)  Configure the system for the "large" version as follows:
          Click on Setup.  Click on Set Executables.
          Click in the executable filename box.
                Edit the name to change CALPUFF to CALPUFFL.
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
                Double click on calpuff (under c:\calpuff\demo\).
                Hit OK.

    (f)  Click on File.  Click on Open.  Select CALPUFF.INP.

    (g)  Click on Run.  Click on Run CALPUFF

    (h)  Click on OK.  Click on Run.

    (i)  A DOS window opens for the model run; enter any key to close
         this window when the run is completed.

    (j)  To view the output list file, click on Utilities.  Click on
         View File.  Select CALPUFF.LST.  To exit view, hit X or ESC.
         Hit F1 for a list of commands when in the view utility.

    (k)  To exit the CALPUFF GUI, click on File.  Click on Exit.

 