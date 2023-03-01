c----------------------------------------------------------------------
c --- PRTMET -- CALMET Printing Postprocessor
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481    Level: 080407                    MAIN
c
c     Copyright (c) 1995-2008 by TRC Environmental Corporation
c
c --- Written by:
c                  J. Scire, R. Mentzer, M. Pietro
c
c --- PURPOSE:
c                  Print selected meteorological fields from a CALMET 
c                  output file, and create output plotfiles compatible
c                  with SURFER
c
c --- Updates:
c
c --- Version 4.48 Level 070720 to Version 4.481 Level 080407
c     (1) Updated CALUTILS from v2.55 Level 070327 to v2.56 Level 080407
c         Control file entries in exponential notation were not correct
c         if decimal point was missing (2e-02 was read as 0.2e-02).
c         Modified: ALTONU
c
c --- Version 4.47 Level 070719 to Version 4.48 Level 070720
c --- D.Strimaitis
c      (1) Add control for the step used for points written to the 
c          VEC type of plot-file for winds
c          Modified:  CONTROL.PMT
c                     BLOCK DATA, SETUP, READCF, QAINP, COMP, VECTOR
c      (2) Add QA check of LCALGRD (from met file) and requests for
c          T,w in layers
c          Modified:  QAINP
c
c --- Version 4.46 Level 070327 to Version 4.47 Level 070719
c --- D.Strimaitis
c      (1) Implement automatic filename section for 3D.DAT files:
c          - introduce "NSECDT3D" for the interval between fields
c          - introduce "ITIME3D" to control interpretation of time
c            period for the fields
c          Modified:  CONTROL.PMT
c                     BLOCK DATA, SETUP, READCF, QAINP, COMP, RD3D
c      (2) Replace map projection PST in the 3D.DAT header with PS
c          for Polar Stereographic
c          Modified:  RDHD53
c      (3) Update 3D.DAT data record format for Dataset 2.1
c          Modified:  RD3D
c      (4) Remove QA checks on the number of hours requested, and
c          drop number of hours (ITHR) from common /control/
c          (process periods until data run out if requested period
c           exceeds period in file)
c          Modified:  CONTROL.PMT
c                     QAINP
c
c --- Version 4.45 Level 061215 to Version 4.46 Level 070327
c --- D.Strimaitis
c      (1) Modify treatment of planar GRD (SURFER) output fields by
c          setting min/max in the header slightly different to force
c          SURFER to make a plot
c          Modified:  GRDOUTR, GRDOUTI
c      (2) CALUTILS from v2.54 Level 061020 to v2.55 Level 070327
c          Fixed format bug in subroutine BASRUTC for the case of time
c          zone zero (output string was 'UTC+0  0' instead of 'UTC+0000'
c          Modified:  UTCBASR, BASRUTC
c
c --- Version 4.44 Level 060519 to Version 4.45 Level 061215
c --- D.Strimaitis
c      (1) CALUTILS from v2.52 Level 060519 to v2.54 Level 061020
c          Move GLOBE1 to COORDLIB
c          Allow negative increments in INCRS
c          Modified:  INCRS
c          Removed:   GLOBE1
c      (2) Increased format field for x,y coordinates written to VEC
c          files from f8.3 to f12.3
c          Modified:  VECTOR
c      (3) A pointer in the timeseries output function was advanced to
c          an undefined location if mixing height is not on the output
c          list.
c          Contributed by: Eric Delisle
c                          SNC-Lavalin Environment Inc. 
c                          Montreal (Quebec)
c          Modified:  COMP
c
c --- Version 4.43 Level 060309 to Version 4.44 Level 060519
c --- D.Strimaitis
c     (1) CALUTILS from v2.51 Level 051019 to v2.52 Level 060519
c         Variable names in control file are not processed correctly
c         if there are too many characters (including blanks) to the
c         left of the "=" sign (run stops in setup phase).
c         Modified:  READIN
c
c --- Version 4.42 Level 051118 to Version 4.43 Level 060309
c --- D.Strimaitis
c     (1) Updated to CALUTILS V2.51 (051019)
c
c --- Version 4.41 Level 051012 to Version 4.42 Level 051118 (F.ROBE)
c     (1) Use a free-format read for the number of comment records
c         in the 3D.DAT file
c     (2) Implement seconds as minutes in timestamp portion of the
c         automatic filenames for plot-files
c         Modified:  COMP
c
c --- Version 4.4 Level 041208 modified to Version 4.41 Level 051012
c --- D.Strimaitis
c     (1) Re-zero layer 1 selections set in BLOCK DATA when specific
c         NZPLOT levels are provided in control file.
c         Modified:  READCF
c     (2) Modify the LUSE.CLR file to change wetland color and add a red
c         strip for LU=99.
c         Modified:  METHDX
c     (3) Use full begin/end time data from CALMET data records
c         Modified:  HEADER.PMT
c                    RDR1D, RDR2D, RDI1D, RDI2D, METHDX, RDMET
c                    READCF, COMP
c         Removed:   RDR1DN, RDR2DN, RDI1DN, RDI2DN
c
c --- Version Version 4.3 Level 040923 modified to Version 4.4 Level 041208
c --- F. Robe
c     (1) Read CALMET format V5.6 Level 041208 with explicit beginning/ending times
c         including seconds, and modified header
c         Changes to METHDX, RDMET. New subroutines RDR1ND, RDR2DN, RDI1DN, RDI2DN
C         to read new format (old format still accepted). CALMET.DAT version number
c         stored in HEADER.PMT
c     (2) Read modified PRTMET.INP (version 2.1) with additional first  line 
c         (with input file version number), explicit beg/ending times with seconds
c     (3) Store hourly explicit times in common DAT and output explicit times 
c         instead of hour-ending times (changes to COMP, OUT,OUTFX, OUTIFX)
c         except in plots' labels (hour-ending still)
c     (4) Change messag (table labels) length from 70 to 40 characters
c         (in subroutines, out, outfx, outifx, methdx,comp,rdmet,rdhd53)
c     (5) Bug fix in READCF: dimension ctemp(132,3)
c     (6) Bug fix in RDMET: added 2 variables to calling list to OUT (as
c         PRTMET subroutine OUT is different from CALPUFF subroutine OUT)
c
c --- Version 4.22 Level 040920 modified to Version 4.3 Level 040923
c --- D.Strimaitis
c     (1) Add automatic 3D.DAT processing in place of CALMET.DAT.
c         New routines:  MET3D.PMT, METHD3D, RDHD53(after CALMET)
c                        RDMET(after CALPUFF), NEXTFIL(as dummy)
c         Modified:  HEADER.PMT, SETUP, QAINP, COMP, RDR2D
c                    
c     (2) Add GRD format control as a hidden option in the control file.  
c         This applies to GRD files that are plotted as image maps (not
c         contours) -- Landuse and PGT.  SURFER 7 required a range
c         adjustment to properly register the grid cell blocks that
c         make up the image.  SURFER 8 registers cells properly without
c         the adjustment, so the standard GRD works for both image and
c         contour maps.  The default for the control is the SURFER 8
c         convention in which all GRD files are alike.
c         (We presume that the SURFER 7 format will seldom be needed.)
c         Modified: CONTROL.PMT, BLOCKDATA, SETUP, READCF, GRDOUTR,
c                   GRDOUTI, COMP, METHDX
c                    
c     (3) Create a PRTMET.CMN file from met file header information
c         New routines:  MKCMN
c         Modified: SETUP
c
c --- Version 4.21 Level 040424 modified to Version 4.22 Level 040920
c --- D.Strimaitis
c     (1) Expand format of QATERR.DAT plot-file to allow terrain heights
c         below sea level.
c
c --- Version 4.2 Level 040330 modified to Version 4.21 Level 040424
c --- F.Robe
c     (1) Allow the timeseries output file to be named in PRTMET.INP
c         (i.e. new PRTMET.INP)
c         Backward compatible: default filename in PRTMET remains
c         Implies changes to FILNAM.PMT and PARAMS.PMT
c     (2) Bug fix for non-hourly processing timestep to prevent
c         processing beyond end time
c
c --- Version 4.14 Level 040318 modified to Version 4.2 Level 040330
c --- D. Strimaitis
c     (1) Expand automatic plot-file names, and include UTC+HHmm format
c         in the time part of the name.  For example,
c         1pm EST on March 31, 2004 is 2004_M03_D31_1300(UTC-0500)
c         6pm GMT on March 31, 2004 is 2004_M03_D31_1800(UTC+0000)
c         7pm (Paris) on March 31, 2004 is 2004_M03_D31_1900(UTC+0100)
c     (2) Replace filename strings c*70 with c*132; this also requires
c         changes in CALUTILS (updated to V2.3, Lev 040330)
c     (3) Switch automatic vector plot files to paired .GRD files to
c         enable use of SURFER vector plot functions.  The single .VEC
c         file is retained as an option controlled by new MVECT input.
c         User may also pick U,V .GRD files.
c     (4) Add a layer option for the automatic plot files to select
c         which layers of winds and temperatures are output
c     (5) Change from ASCII to BINARY for the automatic plot files of
c         real numbers to produce much smaller files.  Use the simpler
c         SURFER6 DSBB file structure for these binary .GRD files.
c         New option LSNAPTXT overrides BINARY output to produce ASCII
c         files as before.
c     (6) Output PGT class GRD file as IMAGE type, and add new color
c         scale file PGT.CLR.  Modify GRDOUTI to treat zlo,zhi
c         arguments as reals so that PGT class goes from 0.5 to 6.5.
c         (This centers the class labels 1-6)
c     (7) Add call to QAINP in SETUP
c     (8) Clamp NSNAP loop at MXSNAP in QAINP and SETUP
c     (9) Clamp NMEAN loop at MXAV in QAINP and SETUP
c
c --- Version 4.13 Level 040225 modified to Version 4.14 Level 040318
c --- D. Strimaitis
c     (1) Shift GRD ranges from cell-centers to cell-edges when GRD
c         file is to be plotted as an IMAGE (e.g. land use QA file)
c         (Subroutines GRDOUTR, GRDOUTI, COMP, METHDX)
c
c --- Version 4.12 030701 Level  modified to Version 4.13 Level 040225
c --- F.Robe
c     (1) modify the timeseries output option such that it creates an
c         output file that can easily be imported in Excel
c         (column format)
c
c --- Version 4.11 Level 030528 modified to Version 4.12, Level 030701
c --- D. Strimaitis
c     (1) Fixed READCF assignment of LCFILES from integer to logical
c
c --- Version 4.1 Level 030402 modified to Version 4.11, Level 030528
c --- J. Scire, D. Strimaitis
c     (1) CALUTILS library updated to Version: 2.2, Level: 030528
c
c --- Version 4.0, Level 020208 modified to Version 4.1, Level 030402
c --- by D. Strimaitis
c     (1) Update CALUTILS (Version 2.1, Level 030402)
c     (2) Revised CALMET header structure (using 'comments' rather than
c         explicit variables for full control file image)
c         Dropped structure introduced in 3.0(010731!a)
c     (3) Revised CALMET data structure using 2D arrays from NOOBS
c         2D arrays of sfc TEMP, IPCODE, RHO, QSW, RH
c     (4) Add control file variable LMETCF to signal printout of CALMET
c         control file image
c
c --- Version 3.1, Level 010731!a modified to Version 4.0, Level 020208
c --- by D. Strimaitis
c     (1) Restructure inputs for CALPUFF system control file
c     (2) Restructure main program as subroutine COMP
c     (3) Place system-wide utilities into an include module 
c         (calutil.for)
c     (4) Change plotfile option to include automatic file generation
c     (5) Do not generate any GRD plot file that is 'planar'
c
c --- Version 3.0, Level: 010315 modified to Version 3.1, Level: 010731!a
c --- by J. Scire,  D. Strimaitis
c     (1) Header records of input CALMET.DAT file modified to include
c         expanded records including input parameters.  All header 
c         records are read in new routine METHDX, and reported to the
c         list file.  Old CALMET.DAT structure remains supported.
c
c --- Version 3.0, Level: 000707 modified to Version 3.0, Level: 010315
c --- by D. Strimaitis
c     (1) Add Y2K processing elements to enforce YYYY format
c
c --- Version 3.0, Level: 000120 modified to Version 3.0, Level: 000707
c --- by D. Strimaitis
c     (1) Modify test for CALMET level at which new header record
c         was added (format changed)
c
c --- Version 3.0, Level: 991124 modified to Version 3.0, Level: 000120
c --- by D. Strimaitis (1/2000)
c     (1) Modify format statements to accommodate 4-digit year when
c         creating list-file output for a single grid-point.
c
c --- Version 3.0, Level: 981128 modified to Version 3.0, Level: 991124
c --- by J. Scire (11/99)
c     (1) Modify format statements to accommodate 4-digit year 
c         (compatible with CALMET V5.1).  Changes to MAIN, OUT, OUTFX,
c         and OUTIFX.
c          
c --- Version 3.0, Level: 980304 modified to Version 3.0, Level: 981128
c --- by J. Scire (11/98)
c     (1) Expand formats for output to allow more than 99 rows/columns.
c         (Use i3 format for cell indexes).  Modifications to: OUT, 
c         OUTFX, OUTIFX.
c     (2) Fix error allowing program to work when IRTYPE=0. Changes to
c         MAIN.
c     (3) Additional QA checks of control file inputs added (MAIN).
c
c     Modified: -  J. Scire (3/98)
c                  Logic added to read new CALMET.DAT format as
c                  an option
c     Modified: -  F.Robe (12/19/96) 
c                  Formatted write statements in subroutine
c                  CONTOUR (instead of free write) s.t. identical
c                  output files on Unix workstations and PC.
c                - F. Robe (11/3/96) 
c                  New option to print out gridded fields 
c                  in a format compatible with SURFER. Options include: 
c                  snapshots or average fields, contour or vector plots.
c                  11/14: option to print plot files of geophysical fields
c                  11/18: option to print plot files of only parts of the 
c                  domain.
c----------------------------------------------------------------------
c
c --- Include parameter file
      include 'params.pmt'
c --- Include common blocks
      include 'qa.pmt'
c
c --- Set version and level number of program (stored in /QA/ and
c --- checked against values set in PARAMS.GEO)
      ver='4.481'
      level='080407'
c
c --- SETUP PHASE -- read control file information
      call SETUP
c
c --- COMPUTATIONAL PHASE -- process data files
      call COMP
c
c --- TERMINATION PHASE -- program termination functions
      call FIN
c
      stop
      end
c----------------------------------------------------------------------
      BLOCK DATA
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481     Level: 070720            BLOCK DATA
c               D. Strimaitis
c
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.pmt'
c
c --- Include common blocks
      include 'filnam.pmt'
      include 'control.pmt'
      include 'grid.pmt'
      include 'met3d.pmt'

c --- FILNAM common block
c ------------------------
      data runinp/'prtmet.inp'/,runlst/'prtmet.lst'/,
     1     metdat/'calmet.dat'/,tegrd/'qaterr.grd'/,
     1     prttime/'prttime.txt'/,
     2     lugrd/'qaluse.grd'/,ligrd/'qalai.grd'/,z0grd/'qaz0.grd'/,
     3     sstadat/'qassta.dat'/,pstadat/'qapsta.dat'/,
     4     ustadat/'qausta.dat'/,gridbna/'qametg.bna'/,
     5     luclr/'luse.clr'/,pgtclr/'pgt.clr'/
c --- FILLOG common block
c ------------------------
      data lcfiles/.true./

c --- CONTROL common block
c ------------------------
c --- Processing Options
      data ichr/1/
c --- Listfile Output Options
      data lhdv/.true./,lmetcf/.false./,lsfc/.false./,lupc/.false./
      data lprc/.false./lnears/.false./,lsurf/.false./
      data lli/.false./,llu/.false./,lte/.false./,lz0/.false./
      data ifli/0/,iflu/0/,ifte/0/,ifz0/0/
      data lstab/.false./,lustr/.false./,lmoln/.false./,lwstr/.false./
      data lmxht/.false./,lprat/.false./
      data ifstab/0/,ifustr/0/,ifmoln/0/,ifwstr/0/,ifmxht/0/,ifprat/0/
      data ipws/1/,wsfac/1.0/,ifws/0/,n3d/0/
c --- Plotfile Output Options
      data lvect/.false./,ltemp/.false./,lprec/.false./
      data lmixh/.false./,lipgt/.false./
      data nsnap/0/,nmean/0/,ibegav/1/,iendav/1/
      data nzplot/0/,mvect/0/,lsnaptxt/.true./
      data iwplt(1)/1/,itplt(1)/1/
      data image/0/
      data nsecdt3d/3600/, itime3d/0/
      data nvecstep/0/

c --- GRID common block
c ------------------------
      data nbx/0/, nby/0/, nex/0/, ney/0/
      data pmap/'        '/
      data datum/'        '/
      data daten/'            '/
      data iutmzn/0/
      data utmhem/'    '/
      data feast/0.0/, fnorth/0.0/
      data rnlat0/-999./,relon0/-999./,xlat1/-999./,xlat2/-999./

c --- MET3D common block
c ------------------------
      data imet3d/2/

      end

c----------------------------------------------------------------------
c --- BRING IN CALPUFF SYSTEM UTILITY SUBROUTINES
      include 'calutils.for'
c----------------------------------------------------------------------

c----------------------------------------------------------------------
      subroutine setup
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481     Level: 070720                  SETUP
c               D. Strimaitis
c
c PURPOSE:     SETUP calls routines to read and check the control data
c              provided, to set logicals, and it reports the control
c              data, and opens the data files if inputs are valid.
c
c --- UPDATE
c --- V4.47 (070719) to V4.48 (070720)  DGS
c               - Add NVECSTEP to control file report
c --- V4.3 (040923) to V4.47 (070719)  DGS
c               - Add NSECDT3D, ITIME3D to control file report
c --- V4.21 (040424) to V4.3 (040923)  DGS
c               - Add 3D.DAT as input met file type
c               - Pass /CONTROL/ variable IMAGE to METHDX
c               - Add call to MKCMN to create a PRTMET.CMN file
c --- V4.2 (040330) to V4.21 (040424)  F.RObe
c               - Print out  timeseries filename
c --- V4.1 (030402) to V4.2 (040330)  DGS
c               - Add plot-layer option output
c               - Add PGT Class color file
c               - Add call to QAINP
c               - Clamp nsnap and nmean loops at max values
c               - Replace selected filename strings c*70 with c*132
c --- V4.0 (020208) to V4.1 (030402)  DGS
c               - Add LMETCF for printing CALMET control file image
c               - Remove CMAP
c
c ARGUMENTS:
c    PASSED:  none
c
c  RETURNED:  none
c
c CALLING ROUTINES:   MAIN
c
c EXTERNAL ROUTINES:  DATETM, COMLINE, READCF, QAINP, METHDX, MKCMN
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.pmt'
      include 'control.pmt'
      include 'filnam.pmt'
      include 'grid.pmt'
      include 'met3d.pmt'
      include 'qa.pmt'

c --- Local
      character*16 aname16
      character*12 aname12

c --- Get date and time from system
      call DATETM(rdate,rtime,rcpu)

c --- Get the name of the control file from the command line
      call COMLINE(runinp)

c --- Open the control file
      open(io5,file=runinp,status='old')

c --- Report progress
      write(iomesg,*)'SETUP PHASE'

c --- Check that the version and level number in the parameter
c --- file matches those in the code itself
      if(ver.ne.mver.or.level.ne.mlevel)then
         write(iomesg,10) ver,level,mver,mlevel
10       format(/1x,'ERROR in SUBR. SETUP -- The PRTMET version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif

c --- Read control file (open list file)
      call READCF

c --- Write header lines to list-file

      write(io6,*)
      write(io6,*) '--------------------------'
      write(io6,*) '    SETUP Information'
      write(io6,*) '--------------------------'
      write(io6,*)

c -----------------------------
c --- Report control data
c -----------------------------

      write(io6,*)
      write(io6,*) 'Control File Used -----'
      write(io6,*) runinp

      write(io6,*)
      write(io6,*) 'Input MET File    -----'
      write(io6,'(a10,a132)') 'metdat  : ',metdat

      write(io6,*) 'Output List File  -----'
      write(io6,'(a10,a132)') 'runlst  : ',runlst

      if (isp.eq.1)then
         write(io6,*) 'Timeseries output File  -----'
         write(io6,'(a10,a132)') 'prttime  : ',prttime
      endif

      write(io6,*) 'Domain Plot Files -----'
      write(io6,'(a21,a132)') 'terrain elevations : ',tegrd
      write(io6,'(a21,a132)') 'land use           : ',lugrd
      write(io6,'(a21,a132)') 'surface roughness  : ',z0grd
      write(io6,'(a21,a132)') 'leaf area index    : ',ligrd
      write(io6,'(a21,a132)') 'surface stations   : ',sstadat
      write(io6,'(a21,a132)') 'precip. stations   : ',pstadat
      write(io6,'(a21,a132)') 'upper-air stations : ',ustadat
      write(io6,'(a21,a132)') 'met grid           : ',gridbna
      write(io6,'(a21,a132)') 'land use color     : ',luclr
      write(io6,'(a21,a132)') 'PGT Class color    : ',pgtclr

2     format(1x,a60,i4)
3     format(1x,a60,l1)
4     format(1x,a60,l1,5x,'Fixed format ? ',i3)
5     format(1x,a55,f8.4)
111   format(24x,i3,12x,i1,9x,i1,7x,i1)
      write(io6,*)
      write(io6,*) 'Processing Options -----'
      write(io6,*)
      write(io6,2)'Beginning year             ',ibyr
      write(io6,2)'Beginning month            ',ibmo
      write(io6,2)'Beginning day              ',ibdy
      write(io6,2)'Beginning Julian day       ',ibjul
      write(io6,2)'Beginning time (hour)      ',ibhr
      write(io6,2)'Beginning time (second)    ',ibsec
      write(io6,2)'Ending year                ',ieyr
      write(io6,2)'Ending month               ',iemo
      write(io6,2)'Ending day                 ',iedy
      write(io6,2)'Ending Julian day          ',iejul
      write(io6,2)'Ending time (hour)         ',iehr
      write(io6,2)'Ending time (second)       ',iesec
      write(io6,2)'Print interval (timesteps) ',ichr
      write(io6,*)
      write(io6,2)'Beginning X point          ',nbx
      write(io6,2)'Beginning Y point          ',nby
      write(io6,2)'Ending X point             ',nex
      write(io6,2)'Ending Y point             ',ney
      write(io6,*)
      write(io6,2)'For 3D.DAT file:           '
      write(io6,2)'Time interval (sec)        ',nsecdt3d
      write(io6,2)'Timestamp mode             ',itime3d
      if(itime3d.EQ.0) write(io6,2)'(time period from file) '
      if(itime3d.EQ.1) write(io6,2)'(time period = interval)'
      write(io6,*)
      write(io6,3)'Display X-Y coordinates of surface sta. ?   ',lsfc
      write(io6,3)'Display X-Y coordinates of upper air sta. ? ',lupc
      write(io6,3)'Display X-Y coordinates of precip. sta. ?   ',lprc
      write(io6,3)'Display nearest surface station array ?     ',lnears
      write(io6,4)'Display surface roughness length ?          ',lz0,
     1 ifz0
      write(io6,4)'Display land use categories ?               ',llu,
     1 iflu
      write(io6,4)'Display terrain elevations ?                ',lte,
     1 ifte
      write(io6,4)'Display leaf area index ?                   ',lli,
     1 ifli
      write(io6,*)
      write(io6,2)'Control variables for printing of 3-D fields.'
      write(io6,2)'     LEVEL         U,V        W     TEMP ?'
      do i=1,mxnz
         write(io6,111)i,iuvout(i),iwout(i),itout(i)
      enddo
      write(io6,*)
      write(io6,2)'Wind components (U, V) converted to WS, WD ?',ipws
      write(io6,2)'Display wind field in fixed format ?        ',ifws
      write(io6,*)
      write(io6,5)'Multiplicative factor for wind units:       ',wsfac
      write(io6,2)'(If the factor is 1.0 then units will remain in m/s)'
      write(io6,*)
      write(io6,4)'Display PGT stability class ?               ',lstab,
     1 ifstab
      write(io6,4)'Display friction velocity ?                 ',lustr,
     1 ifustr
      write(io6,4)'Display Monin-Obukhov length ?              ',lmoln,
     1 ifmoln
      write(io6,4)'Display mixing height ?                     ',lmxht,
     1 ifmxht
      write(io6,4)'Display convective velocity scale ?         ',lwstr,
     1 ifwstr
      write(io6,4)'Display precipitation rate ?                ',lprat,
     1 ifprat
      write(io6,*)
      write(io6,3)'Display surface met. station variables ?    ',lsurf
      write(io6,*)

      write(io6,*)
      write(io6,*) 'Snapshot Options -----'
      write(io6,*)
      write(io6,*)'Automatic snapshot files selected:'
      if(mvect.EQ.0) then
         write(io6,*)'   Wind Vectors  (*.VEC)        ',lvect,
     &               '   (MVECT = 0)'
         if(nvecstep.EQ.0) then
            write(io6,*)'      Stepping Index            ',nvecstep,
     &                  '   (automatic)'
         else
            write(io6,*)'      Stepping Index            ',nvecstep
         endif
      elseif(mvect.EQ.1) then
         write(io6,*)'   Wind Vectors  (*.WSP, *.WDR) ',lvect,
     &               '   (MVECT = 1)'
      elseif(mvect.EQ.2) then
         write(io6,*)'   Wind Vectors  (*.USP, *.VSP) ',lvect,
     &               '   (MVECT = 2)'
      endif
      write(io6,*)'   Temperature   (*.DEG)        ',ltemp
      write(io6,*)'   Precipitation (*.PRC)        ',lprec
      write(io6,*)'   Mixing Height (*.MIX)        ',lmixh
      write(io6,*)'   PGT Stability (*.PGT)        ',lipgt
      write(io6,*)
      write(io6,*)'   All snapshot files are ASCII?',lsnaptxt
      write(io6,*)
      write(io6,2)'Control variables for ploting 3-D fields.'
      write(io6,2)'LEVEL        WIND      TEMP ?        '
      do i=1,mxnz
         write(io6,111)i,iwplt(i),itplt(i)
      enddo
      write(io6,*)
      write(io6,*)'Number of specific snapshot files:',nsnap
      if(nsnap.gt.0) then
        write(io6,*)'Variable, vertical slice, time, filename:'
        nnsnap=MIN(nsnap,mxsnap)
        do nn=1,nnsnap
           write(io6,116)asnap(nn),ksnap(nn),tsnap(nn),filesnap(nn)
        end do
116     format(a4,1x,i3,1x,i5,1x,a132)
      endif

      write(io6,*)
      write(io6,*) 'Average Field Options -----'
      write(io6,*)
      write(io6,*)'Number of average field files:',nmean
      if(nmean.gt.0) then
        write(io6,*)'Beginning and ending of averaging period',
     &     ibegav,iendav
        write(io6,*)'Variable, vertical slice, filename:'
        nnmean=MIN(nmean,mxav)
        do nn=1,nnmean
           write(io6,117)amean(nn),kmean(nn),fileav(nn)
        end do
117     format(a4,1x,i3,1x,a132)
      endif

c -------------------------------------------
c --- Read and report MET file header records
c -------------------------------------------

c --- Input file may be either the binary CALMET.DAT or
c --- the ASCII 3D.DAT.  Open as ASCII first and test the header record
      open(io7,file=metdat,status='old',form='formatted')
c --- First 16 characters will be "3D.DAT          " for version 2.0
      read(io7,'(a16)') aname16
      if(aname16.EQ.'3D.DAT          ') then
c ---    3D.DAT Version 2.0 or later
         metfm=2
         imet3d=2
      else
c ---    This may be an early version of 3D.DAT if the second record
c ---    starts with "3D.DAT      " or "MM53D.DAT   "
         read(io7,'(a12)') aname12
         if(aname12.EQ.'3D.DAT      ' .OR.
     &      aname12.EQ.'MM53D.DAT   ') then
            metfm=2
            imet3d=1
         else
c ---       CALMET.DAT
            metfm=1
         endif
      endif
      close(io7)

c --- Open and process headers

      if(metfm.EQ.1) then
c ---    Open CALMET output data file
         open(io7,file=metdat,status='old',form='unformatted')

c ---    Process header of CALMET file (create domain plot files)
         call methdx(image,lhdv,lmetcf,lsfc,lupc,lprc,lz0,ifzo,
     &               llu,iflu,lte,ifte,lli,ifli,lnears)

      else

c ---    Open 3D.DAT meteorological file
         open(io7,file=metdat,status='old',form='formatted')

c ---    Process header of 3D.DAT file (create domain plot files)
         call methd3d(lhdv,lmetcf,lte,ifte)

      endif

c --- QA the inputs
      call QAINP

c --- Create CMN file from information in meteorological file headers
      call MKCMN

      return
      end

c----------------------------------------------------------------------
      subroutine readcf
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481          Level: 070720            READCF
c               D. Strimaitis
c
c --- PURPOSE:  Read the control file containing the file names of
c               the input and output files of the run, and associated
c               control variables.  Open list file for output.
c
c --- UPDATE
c
c --- V4.47 (070719) to V4.48 (070720)  DGS
c               - Add user-supplied stepping index for VEC plot file
c
c --- V4.41 (051012) to V4.47 (070719)  DGS
c               - Add controls for processing a 3D.DAT file
c
c --- V4.4 (041208) to V4.41 (051012)  DGS
c               - Re-zero layer 1 selections set in BLOCK DATA when
c                 specific NZPLOT levels are provided
c               - Use starting times and convert from hour-ending if
c                 old control file is read (keep seconds)
c               - Do not calc ITHR here
c
c --- V4.3 (040923) to V4.4 (041208)  FRR
c               - Bug fix: Dimension ctemp(132,3) (instead of (132,2)
c               - NewPRTMET.INP format. First line has input file version 
c                 number (PRTMET.INP 2.1)
c               - Computation period defined in PRTMET.INP with explicit  
c                 beginning/ending times including seconds
c               - Convert those explicit times back to hour-ending times
C                 to use internally in the code
c
c
c --- V4.21 (040424) to V4.3 (040923)  DGS
c               - Add IMAGE control variable for GRD format to input
c                 group 1
c --- V4.2 (040320) to V4.21 (040424)  F.RObe
c               - Bug fix : ITHR= number of hours to process 
c               - New timeseries file - PRTTIME added to dictionary
c --- V4.12 (030701) to V4.2 (040330)  DGS
c               - Restructure automatic plot files and add layer
c                 selection option
c               - Replace filename strings c*70 with c*132
c               - Add LSNAPTXT option to group 3 to force all snapshot
c                 files to be written as ASCII
c --- V4.1 (030402) to V4.12 (030701)  DGS
c               - Fixed READCF assignment of LCFILES from integer
c                 to logical
c --- V4.0 (020208) to V4.1 (030402)  DGS
c               - Add LMETCF for printing CALMET control file image
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c
c ---    Common block /FILNAM/ variables:
c           runinp
c
c        Parameters: IO5, IO6, IOMESG
c
c --- OUTPUT:
c
c ---    Common block /FILNAM/ variables:
c           runlst,metdat,tegrd,lugrd,ligrd,z0grd,sstadat,
c           pstadat,ustadat,gridbna,luclr
c           lcfiles
c ---    Common block /CONTROL/ variables:
c           IBYR,IBMO,IBDY,IBHR,ibjul,ibsec,
c           IEYR,IEMO,IEDY,IEHR,iejul,iesec,
c           ICHR,LHDV,LMETCF,LSFC,LUPC,LPRC,LNEARS,LSURF,
c           LLI,LLU,LTE,LZ0,IFLI,IFLU,IFTE,IFZ0,
c           LSTAB,LUSTR,LMOLN,LWSTR,LMXHT,LPRAT,
c           IFSTAB,IFUSTR,IFMOLN,IFWSTR,IFMXHT,IFPRAT,
c           IPWS,WSFAC,IFWS,N3D,iuvout,iwout,itout,
c           LVECT,LTEMP,LPREC,LMIXH,LIPGT,LSNAPTXT,NSNAP,MVECT,
c           NMEAN,IBEGAV,IENDAV,ksnap,tsnap,kmean,
c           asnap,amean
c           NZPLOT,iwplt,itplt
c           nsecdt3d,itime3d,nvecstep
c ---    Common block /GRID/ variables:
c           nbx,nby,nex,ney
c
c --- READCF called by:  SETUP
c --- READCF calls:      READIN, FILCASE, JULDAY
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.pmt'
      include 'params.cal'
c
c --- Include common blocks
      include 'control.pmt'
      include 'filnam.pmt'
      include 'grid.pmt'
      include 'qa.pmt'
c
c --- Local variables
      integer ilayer(4),izplot(3)
      integer idef(2,13)
      character*4 adef(13)
      character*4 ctemp(132,3)
      character*12 cvdic(mxvar,9)
      integer ivleng(mxvar,9),ivtype(mxvar,9)
      logical lecho,lerrcf
      character*16 inputset,inputver
      character*64 inputmod

c --- The wind speed logical LWSPE is dropped, so define it locally
c --- to handle an older control file
      logical LWSPE

c --- Initialize local variables
      data lecho/.false./, lerrcf/.false./
      data names/3/
      data adef/'VECT','UVEL','VVEL','WVEL','TEMP','WDIR','WSPE',
     &          'IPGT','USTA','MONL','WSTA','MIXH','PREC'/

c --- Set Dictionary

      data cvdic/
     a  'RUNLST','METDAT','PRTTIME','LCFILES', 56*' ',
     b  'IBYR','IBMO','IBDY','IBHR','IBSEC','IEYR','IEMO','IEDY',
     b  'IEHR','IESEC','ICHR','NBX','NBY','NEX','NEY','IMAGE',
     b  'NSECDT3D','ITIME3D', 42* ' ',
     c  'LHDV','LMETCF','LSFC','LUPC','LPRC','LNEARS','LSURF',
     c  'LLI','LLU','LTE','LZ0','FLI','FLU','FTE','FZ0',
     c  'LSTAB','LUSTR','LMOLN','LWSTR','LMXHT','LPRAT',
     c  'FSTAB','FUSTR','FMOLN','FWSTR','FMXHT','FPRAT',
     c  'IPWS','WSFAC','FWS','N3D', 29* ' ',
     2  'X', 59* ' ',
     d  'LVECT','LTEMP','LWSPE','LPREC','LMIXH','LIPGT',
     d  'LSNAPTXT','NSNAP','MVECT','NZPLOT','VECSTEP', 49* ' ',
     2  'X', 59* ' ',
     3  'FILESNAP','VECT','UVEL','VVEL','WVEL','TEMP','WDIR',
     3  'WSPE','IPGT','USTA','MONL','WSTA','MIXH','PREC', 46* ' ',
     e  'NMEAN','IBEGAV','IENDAV', 57* ' ',
     2  'FILEMEAN','VECT','UVEL','VVEL','WVEL','TEMP','WDIR',
     2  'WSPE','IPGT','USTA','MONL','WSTA','MIXH','PREC', 46* ' '/

      data ivleng/
     a  3*132,1, 56*0,
     b  18*1, 42*0,
     c  31*1, 29*0,
     2  4, 59*0,
     d  11*1, 49*0,
     2  3, 59*0,
     3  132,13*2, 46*0,
     e  3*1, 57*0,
     2  132,13*2, 46*0/

c --- Variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
      data ivtype/
     a  3*4,3, 56*0,
     b  18*2, 42*0,
     c  11*3,4*2,6*3,7*2,1,2*2, 29*0,
     2  2, 59*0,
     d  7*3,4*2, 49*0,
     2  2, 59*0,
     3  4,13*2, 46*0,
     e  3*2, 57*0,
     2  4,13*2, 46*0/



c ------------------
c --- Input format 
c -------------------
c --- New format: first line includes dataset types, version number
c --- and description (starts at 2.1)
      read(io5,'(2a16,a64)') inputset,inputver,inputmod
   

c ------------------
c --- Input Group 0
c ------------------

c --- Initialize the temporary arrays
      do i=1,names
         do j=1,132
            ctemp(j,i)(1:1)=' '
         enddo
      enddo

c --- Read the group data
       call READIN(cvdic(1,1),ivleng(1,1),ivtype(1,1),io5,iomesg,
     & lecho,
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),lcfiles,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum)

c --- Prepare any filenames included in the I/O file by erasing
c --- the default filename set above
      if(ctemp(1,1)(1:1).ne.' ')runlst=' '
      if(ctemp(1,2)(1:1).ne.' ')metdat=' '
      if(ctemp(1,3)(1:1).ne.' ')prttime=' '

c --- Transfer the char*4 data into the char*132 variables
      do j=1,132
         if(ctemp(j,1)(1:1).ne.' ')runlst(j:j)=ctemp(j,1)(1:1)
         if(ctemp(j,2)(1:1).ne.' ')metdat(j:j)=ctemp(j,2)(1:1)
         if(ctemp(j,3)(1:1).ne.' ')prttime(j:j)=ctemp(j,3)(1:1)
      enddo

c --- Convert the file names to the proper case
      call FILCASE(lcfiles,runlst)
      call FILCASE(lcfiles,metdat)
      call FILCASE(lcfiles,prttime)

c --- Open listfile
      open(io6,file=runlst,status='unknown')

c --- Write banner to list file
      write(io6,5) ver,level
5     format(///,26x,'PRTMET OUTPUT SUMMARY',/,19x,'VERSION:  ',A12,
     1       ' LEVEL:  ',A12///)

c -----------------
c --- Input Group 1
c -----------------
C --- Initialize date variable to check whether old or new input file 
      ibsec=-9999
      iesec=-9999

      call readin(cvdic(1,2),ivleng(1,2),ivtype(1,2),io5,io6,lecho,
     1 IBYR,IBMO,IBDY,IBHR,IBSEC,IEYR,IEMO,IEDY,IEHR,IESEC,
     2 ICHR,NBX,NBY,NEX,NEY,IMAGE,NSECDT3D,ITIME3D,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum)

c --- Compute Julian day for each date
      call JULDAY(io6,ibyr,ibmo,ibdy,ibjul)
      call JULDAY(io6,ieyr,iemo,iedy,iejul)

c --- Special Y2K QA on starting year of simulation
      call QAYR4(io6,ibyr,0,ierr)
      if(ierr.NE.0) then
         write(io6,*) 'READCF:  Error in Input Group 1'
         lerrcf=.TRUE.
      endif

c --- Make sure ending year is YYYY (Y2K)
      call YR4(io6,ieyr,ierr)
      if(ierr.NE.0) then
         write(io6,*) 'READCF:  Error in Input Group 1'
         lerrcf=.TRUE.
      endif

      if (inputver.EQ.'2.1') then
c ---    New input format with beginning/ending times including seconds
c ---    check that new variables IBSEC,IESEC are there

         if( (ibsec.eq.-9999).or.(iesec.eq.-9999) ) then
            write(io6,*)'PRTMET.INP dataset version 2.1'
            write(io6,*)'Requires beginning/ending seconds: IBSEC,IESEC'
            write(io6,*)'Update your input file - STOP'
            STOP 'Error in control file - check LST file -STOP -'
         endif
      
c ---    Condition seconds and hours to natural range
         nhrinc=0
         if(ibsec.GE.3600) then
            nhrinc=ibsec/3600
            ibsec=ibsec-nhrinc*3600
         endif
         if(ibhr.ge.24) then
            nhrinc=nhrinc+(ibhr-23)
            ibhr=23
         endif
         call INCR(io6,ibyr,ibjul,ibhr,nhrinc)
         call GRDAY(io6,ibyr,ibjul,ibmo,ibdy)

         nhrinc=0
         if(iesec.GE.3600) then
            nhrinc=iesec/3600
            iesec=iesec-nhrinc*3600
         endif
         if(iehr.ge.24) then
            nhrinc=nhrinc+(ibhr-23)
            iehr=23
         endif
         call INCR(io6,ieyr,iejul,iehr,nhrinc)
         call GRDAY(io6,ieyr,iejul,iemo,iedy)

      elseif ((ibsec.ne.-9999).or.(iesec.ne.-9999)) then
c ---    Mix of old and new format features - Stop
c ---    should be old input file with hour ending times without seconds
         write(io6,*)'PRTMET.INP dataset version prior to 2.1'
         write(io6,*)'For PRTMET.INP version 2.1 add 1st line header'
         write(io6,*)'Update your input file - STOP'
         STOP 'Error in control file - check LST file -STOP - '

      else 
c ---    Old input file with hour-ending start time and no seconds
         ibsec=0
         iesec=0
c ---    Convert hour-ending start time to natural start time
         call INCR(io6,ibyr,ibjul,ibhr,-1)

         call GRDAY(io6,ibyr,ibjul,ibmo,ibdy)

      endif

c --- Check that ending date is after starting date
      iedathr=ieyr*100000+iejul*100+iehr
      ibdathr=ibyr*100000+ibjul*100+ibhr

      if(ibdathr.GT.iedathr .OR.
     &   (ibdathr.EQ.iedathr .AND. ibsec.GE.iesec)) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'Starting date/hour is after ending date/hr'
         write(io6,86) ibyr,ibmo,ibjul,ibhr,ibsec
         write(io6,87) ieyr,iemo,iejul,iehr,iesec
86       format('Beginning year: ',i4,' Month: ',i2,' Day: ':,i2,
     :           'Hour: ',i2,'Seconds: ',i4)
87       format('Ending year: ',i4,' Month: ',i2,' Day: ':,i2,
     :           'Hour: ',i2,'Seconds: ',i4)

         lerrcf=.TRUE.
      endif


c -----------------
c --- Input Group 2
c -----------------

c --- Subgroup 2(a) - listfile options
c ------------------------------------
      call readin(cvdic(1,3),ivleng(1,3),ivtype(1,3),io5,io6,lecho,
     1 LHDV,LMETCF,LSFC,LUPC,LPRC,LNEARS,LSURF,LLI,LLU,LTE,LZ0,
     2 IFLI,IFLU,IFTE,IFZ0,LSTAB,LUSTR,LMOLN,LWSTR,LMXHT,LPRAT,
     3 IFSTAB,IFUSTR,IFMOLN,IFWSTR,IFMXHT,IFPRAT,IPWS,WSFAC,IFWS,N3D,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum)

c --- Subgroup 2(b) - data for each layer printed
c -----------------------------------------------
      do i=1,n3d
         do j=1,4
            ilayer(j)=0
         enddo
      call readin(cvdic(1,4),ivleng(1,4),ivtype(1,4),io5,io6,lecho,
     1 ILAYER,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c ---    Cap layer at MXNZ
         ilayer(1)=MIN(ilayer(1),mxnz)
c ---    Place into print-arrays
         iuvout(ilayer(1))=ilayer(2)
         iwout(ilayer(1))=ilayer(3)
         itout(ilayer(1))=ilayer(4)        
      enddo

c -----------------
c --- Input Group 3
c -----------------

c --- Subgroup 3(a) - snapshot output options
c -------------------------------------------
      call readin(cvdic(1,5),ivleng(1,5),ivtype(1,5),io5,io6,lecho,
     1 LVECT,LTEMP,LWSPE,LPREC,LMIXH,LIPGT,LSNAPTXT,NSNAP,MVECT,
     2 NZPLOT,NVECSTEP,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum)

c --- Subgroup 3(b) - data for each layer plotted
c -----------------------------------------------
      if(nzplot.GT.0) then
c ---    Re-zero layer 1 selections set in BLOCK DATA
         iwplt(1)=0
         itplt(1)=0
      endif
      do i=1,nzplot
         do j=1,3
            izplot(j)=0
         enddo
      call readin(cvdic(1,6),ivleng(1,6),ivtype(1,6),io5,io6,lecho,
     1 IZPLOT,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c ---    Cap layer at MXNZ
         izplot(1)=MIN(izplot(1),mxnz)
c ---    Place into plot-arrays
         iwplt(izplot(1))=izplot(2)
         itplt(izplot(1))=izplot(3)        
      enddo

c --- Subgroup 3(c) - snapshot definition
c ---------------------------------------
      do k=1,nsnap
         kk=MIN(k,mxsnap)
c ---    Initialize temporary filename array
         do j=1,132
            ctemp(j,1)(1:1)=' '
         enddo
c ---    Initialize definition arrays
         do j=1,13
            do i=1,2
               idef(i,j)=0
            enddo
         enddo
      call readin(cvdic(1,7),ivleng(1,7),ivtype(1,7),io5,io6,lecho,
     1 ctemp(1,1),idef(1,1),idef(1,2),idef(1,3),idef(1,4),idef(1,5),
     2 idef(1,6),idef(1,7),idef(1,8),idef(1,9),idef(1,10),idef(1,11),
     3 idef(1,12),idef(1,13),
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c ---    Transfer the char*4 data into the char*132 variables
         do j=1,132
            if(ctemp(j,1)(1:1).ne.' ')filesnap(kk)(j:j)=ctemp(j,1)(1:1)
         enddo
c ---    Convert the file names to the proper case
         call FILCASE(lcfiles,filesnap(kk))
c ---    Select the assigned parameters for this snapshot
         do j=1,13
            if(idef(1,j).NE.0) then
               asnap(kk)=adef(j)
               ksnap(kk)=idef(1,j)
               tsnap(kk)=idef(2,j)
            endif
         enddo
      enddo

c -----------------
c --- Input Group 4
c -----------------

c --- Subgroup 4(a) - average field output options
c ------------------------------------------------
      call readin(cvdic(1,8),ivleng(1,8),ivtype(1,8),io5,io6,lecho,
     1 NMEAN,IBEGAV,IENDAV,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum)

c --- Subgroup 4(b) - average field definition
c --------------------------------------------
      do k=1,nmean
         kk=MIN(k,mxav)
c ---    Initialize temporary filename array
         do j=1,132
            ctemp(j,1)(1:1)=' '
         enddo
c ---    Initialize definition arrays
         do j=1,13
            do i=1,2
               idef(i,j)=0
            enddo
         enddo
      call readin(cvdic(1,9),ivleng(1,9),ivtype(1,9),io5,io6,lecho,
     1 ctemp(1,1),idef(1,1),idef(1,2),idef(1,3),idef(1,4),idef(1,5),
     2 idef(1,6),idef(1,7),idef(1,8),idef(1,9),idef(1,10),idef(1,11),
     3 idef(1,12),idef(1,13),
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c ---    Transfer the char*4 data into the char*132 variables
         do j=1,132
            if(ctemp(j,1)(1:1).ne.' ')fileav(kk)(j:j)=ctemp(j,1)(1:1)
         enddo
c ---    Convert the file names to the proper case
         call FILCASE(lcfiles,fileav(kk))
c ---    Select the assigned parameters for this snapshot
         do j=1,13
            if(idef(1,j).NE.0) then
               amean(kk)=adef(j)
               kmean(kk)=idef(1,j)
            endif
         enddo
      enddo


c --- STOP now if error exists in the control file
      if(LERRCF) then
         write(*,*)'ERRORS are found in the CONTROL file'
         write(*,*)'Review messages written to the LIST file'
         stop
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine qainp
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481           Level: 070720            QAINP
c               D. Strimaitis
c
c --- PURPOSE:  QA the control file information and set up subgrid
c
c
c --- UPDATE
c
c --- V4.47 (070719) to V4.48 (070720)  DGS
c               - Add NVECSTEP QA check
c               - Add check of LCALGRD (from met file) and requests
c                 for T,w in layers
c               - Print warnings to screen as well as list file
c
c --- V4.3 (040923) to V4.47 (070719)  DGS
c               - Add NSECDT3D,ITIME3D QA checks
c
c --- V4.2 (040330) to V4.3 (040923)  DGS
c               - Add 3D.DAT QA checks
c
c --- V4.0 (020208) to V4.2 (040330)  DGS
c               - Clamp NSNAP loops at MXSNAP
c               - Clamp NMEAN loops at MXAV
c               - Add MVECT check
c
c --- INPUTS:
c
c ---    Common block /CONTROL/ variables:
c                     METFM
c                     LSURF,LSTAB,LUSTR,LMOLN,LWSTR,
c                     LMXHT,LPRAT,itout,
c                     NSNAP,NMEAN,IBEGAV,IENDAV,ksnap,tsnap,kmean
c                     NSECDT3D,ITIME3D
c ---    Common block /GRID/ variables:
c                     nx,ny,nz,nbx,nby,nex,ney
c ---    Common block /HEADER/ variables:
c                     irtype,irlg,lcalgrd
c
c        Parameters: IO6
c
c --- OUTPUT:
c ---    Common block /GRID/ variables:
c                     isub,isp,nspx,nspy
c
c --- QAINP called by:  SETUP
c --- QAINP calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.pmt'
c
c --- Include common blocks
      include 'control.pmt'
      include 'grid.pmt'
      include 'header.pmt'
      include 'met3d.pmt'
c
c --- Local variables
      logical lerrcf,lreset

c --- Initialize local variables
      data lerrcf/.false./

c --- Only wind fields are stored in file if IRTYPE = 0
      lreset=.FALSE.
      if(irtype.EQ.0) then
         if(lsurf) then
            lreset=.TRUE.
            lsurf=.FALSE.
         endif
         if(lstab) then
            lreset=.TRUE.
            lstab=.FALSE.
         endif
         if(lustr) then
            lreset=.TRUE.
            lustr=.FALSE.
         endif
         if(lmoln) then
            lreset=.TRUE.
            lmoln=.FALSE.
         endif
         if(lwstr) then
            lreset=.TRUE.
            lwstr=.FALSE.
         endif
         if(lmxht) then
            lreset=.TRUE.
            lmxht=.FALSE.
         endif
         if(lprat) then
            lreset=.TRUE.
            lprat=.FALSE.
         endif
         isum=0
         do i=1,nz
            isum=isum+itout(i)
         enddo
         if(isum.gt.0)then
            lreset=.TRUE.
            do i=1,nz
               itout(i)=0
            enddo
         endif
      endif

      if(lreset)then
         write(io6,*)
         write(io6,*) 'QAINP:  Warning, only winds are computed.'
         write(io6,*) 'Other parameters requested are'
         write(io6,*) 'unavailable for output.'
         write(*,*)
         write(*,*) 'QAINP:  Warning, only winds are computed.'
         write(*,*) 'Other parameters requested are'
         write(*,*) 'unavailable for output.'
         write(*,*)
      endif

c --- Layered temperature and w are NOT stored in file if LCALGRD = F
      lreset=.FALSE.
      if(.NOT.LCALGRD) then
         if(ltemp) then
            lreset=.TRUE.
            ltemp=.FALSE.
         endif
         isum=0
         do i=1,nz
            isum=isum+itout(i)
            isum=isum+itplt(i)
            isum=isum+iwout(i)
         enddo
         if(isum.gt.0)then
            lreset=.TRUE.
            do i=1,nz
               itout(i)=0
               itplt(i)=0
               iwout(i)=0
            enddo
         endif
         isum=0
         do i=1,mxsnap
            if(asnap(i).EQ.'TEMP') isum=isum+1
            if(asnap(i).EQ.'WVEL') isum=isum+1
         enddo
         do i=1,mxav
            if(amean(i).EQ.'TEMP') isum=isum+1
            if(amean(i).EQ.'WVEL') isum=isum+1
         enddo
         if(isum.gt.0)then
            lreset=.TRUE.
         endif
      endif

      if(lreset)then
         write(io6,*)
         write(io6,*) 'QAINP:  Warning, LCALGRD in met file is FALSE.'
         write(io6,*) 'Layered temperature and vertical velocity are'
         write(io6,*) 'unavailable for output.'
         write(*,*)
         write(*,*) 'QAINP:  Warning, LCALGRD in met file is FALSE.'
         write(*,*) 'Layered temperature and vertical velocity are'
         write(*,*) 'unavailable for output.'
         write(*,*)
      endif

c --- Determine if a subset of the grid is being printed
      if(nbx.ne.1.or.nby.ne.1.or.nex.ne.nx.or.ney.ne.ny)then
         isub=1
      else
         isub=0
      endif

c --- If only a single point, set special flags & variables
      if(nbx.eq.nex.and.nby.eq.ney)then
         isp=1
         nspx=nbx
         nspy=nby
      else
         isp=0
         nspx=0
         nspy=0
      endif

c --- Met. grid range checks
      ierr=0
      if(nbx.lt.1.or.nbx.gt.nx)ierr=1
      if(nex.lt.1.or.nex.gt.nx)ierr=1
      if(nex.lt.nbx)ierr=1
      if(nby.lt.1.or.nby.gt.ny)ierr=1
      if(ney.lt.1.or.ney.gt.ny)ierr=1
      if(ney.lt.nby)ierr=1
      if(ierr.eq.1)then
         write(io6,*)'Error in specification of met. grid range'
         write(io6,*)'NX = ',nx,' Range requested: NBX = ',nbx,
     1   ' NEX = ',nex
         write(io6,*)'NY = ',ny,' Range requested: NBY = ',nby,
     1   ' NEY = ',ney
         lerrcf=.TRUE.
      endif

cc --- Check PRTMET run length vs. MET run length
c      if(ithr.gt.irlg)then
c         write(io6,*)
c         write(io6,*) 'QAINP:  Error in Input Group 1'
c         write(io6,*) 'PRTMET run length > MET run length'
c         write(io6,*) 'PRTMET run length (ITHR) = ',ithr
c         write(io6,*)'    MET run length (IRLG) = ',irlg
c         lerrcf=.TRUE.
c      endif

c --- Check number of snapshot files requested
      if(nsnap.gt.mxsnap) then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 3'
         write(io6,*) 'Too many snapshots: nsnap>mxsnap'
         write(io6,*)' Number requested (NSNAP) = ',nsnap
         write(io6,*)' Maximum allowed (MXSNAP) = ',mxsnap
         nsnap=mxsnap
         lerrcf=.TRUE.
      endif

c --- Check number of average field files requested
      if(nmean.gt.mxav) then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 4'
         write(io6,*) 'Too many average files: nmean>mxav'
         write(io6,*)' Number requested (NMEAN) = ',nmean
         write(io6,*)' Maximum allowed   (MXAV) = ',mxav
         nmean=mxav
         lerrcf=.TRUE.
      endif

c --- Check Hour/layer for snapshot plot files
c --- (disable ITHR check by setting it to requested hour)
      if(nsnap.gt.0)then
         do nn=1,nsnap     
           ithr=tsnap(nn)
           if(ksnap(nn).lt.1.or.ksnap(nn).gt.nz)then
              lerrcf=.TRUE.
              write(io6,*)
              write(io6,*)'Error -- Invalid layer specified for a ',
     1        'snapshot plot -- NZ = ',nz,' KSNAP(nn) = ',ksnap(nn),
     2        ' NN = ',nn
           endif
           if(tsnap(nn).lt.1.or.tsnap(nn).gt.ithr)then
              lerrcf=.TRUE.
              write(io6,*)
              write(io6,*)'Error -- Invalid hour specified for a ',
     1        'snapshot plot -- ITHR = ',ithr,' TSNAP(nn) = ',tsnap(nn),
     2        ' NN = ',nn
              write(io6,*)'TSNAP is the hour of the PRTMET run, not ',
     1        'the clock (0-23) hour -- i.e., TSNAP must be >= 1 and ',
     2        '<= ITHR'
           endif
         enddo
      endif

c --- Check range of time-averaged plots
c --- (disable ITHR check by setting it to max requested hour)
      if(nmean.gt.0)then
         ithr=MAX(ibegav,iendav)
         if(ibegav.lt.1.OR.ibegav.gt.ithr.OR.
     &      iendav.lt.1.OR.iendav.gt.ithr.OR.
     &      ibegav.gt.iendav) then
            lerrcf=.TRUE.
            write(io6,*)
            write(io6,*)'Error -- Invalid averaging time specified ',
     1      'for time-averaged plots'
            write(io6,*)'IBEGAV = ',ibegav,' IENDAV = ',iendav,
     2                  ' ITHR = ',ithr
            write(io6,*)'IBEGAV/IENDAV refer to the range of hours of',
     1      ' the PRTMET run, not the clock (0-23) hours -- i.e.,'
            write(io6,*)'IBEGAV and IENDAV must be >= 1 and <= ITHR'
         endif
c ---    Check the layers of the time-averaged plots
         do nn=1,nmean
           if(kmean(nn).lt.1.or.kmean(nn).gt.nz)then
              lerrcf=.TRUE.
              write(io6,*)
              write(io6,*)'Error -- Invalid layer specified for a ',
     1        'time-averaged plot -- NZ = ',nz,' KMEAN(nn) = ',
     2        kmean(nn),' NN = ',nn
           endif
         enddo
      endif

c --- Check type of automatic vector plot files
      if(MVECT.LT.0 .OR. MVECT.GT.2) then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 3'
         write(io6,*) 'Invalid type of vector plot'
         write(io6,*)' Expected (MVECT) = 0, 1, 2'
         write(io6,*)'    Found (MVECT) = ',mvect
         lerrcf=.TRUE.
      endif
c --- Check step for VEC type of automatic vector plot file
      if(MVECT.EQ.0 .AND. NVECSTEP.LT.0) then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 3'
         write(io6,*) 'Invalid step for VEC type of vector plot'
         write(io6,*)' Expected (NVECSTEP) = 0 or > 0'
         write(io6,*)'    Found (NVECSTEP) = ',nvecstep
         lerrcf=.TRUE.
      endif

c --- Process requested information if 3D.DAT file is found
c ---------------------------------------------------------
      if(metfm.EQ.2) then
         write(io6,*)
         write(io6,*) 'QAINP:  3D.DAT Met file is processed'
         write(io6,*)
         write(io6,*)
         write(io6,*) 'The following 2-D fields ARE NOT processed:'
         write(io6,*)
         write(io6,*) '   PG Class, U-star, W-star, MO Length'
         write(io6,*) '   Mix Ht, Leaf Area Index, Roughness Length'
         write(io6,*) '   Land Use'
         write(io6,*)
         write(io6,*) 'The following 2-D fields CAN BE processed:'
         write(io6,*)
         write(io6,*) '   Pressure, Precip Rate, SW Radiation'
         write(io6,*) '   LW Radiation, Temperature, Spec. Humidity'
         write(io6,*)
         write(io6,*) 'The following 3-D fields CAN BE processed:'
         write(io6,*)
         write(io6,*) '   U, V, W, Temperature, Rel. Humidity'
         write(io6,*) '   (RH is output whenever T is output)'

c ---    Turn off requests for data that are not available
         lstab=.FALSE.
         lustr=.FALSE.
         lwstr=.FALSE.
         lmoln=.FALSE.
         lmxht=.FALSE.
         lli=.FALSE.
         llu=.FALSE.
         lz0=.FALSE.

c ---    Check 3D.DAT control inputs
         if(nsecdt3d.LT.1) then
            write(io6,*)
            write(io6,*) 'QAINP:  Error in Input Group 1'
            write(io6,*) 'Invalid time interval between 3D.DAT fields'
            write(io6,*)' Expected (NSECDT3D) > 0'
            write(io6,*)'    Found (NSECDT3D) = ',nsecdt3d
            lerrcf=.TRUE.
         endif
         if(itime3d.LT.0 .OR. itime3D.GT.1) then
            write(io6,*)
            write(io6,*) 'QAINP:  Error in Input Group 1'
            write(io6,*) 'Invalid time control for 3D.DAT fields'
            write(io6,*)' Expected (ITIME3D) = 0,1'
            write(io6,*)'    Found (ITIME3D) = ',itime3d
            lerrcf=.TRUE.
         endif

      endif

c --- STOP now if error exists in the control file
      if(LERRCF) then
         write(*,*)'ERRORS are found in the CONTROL file'
         write(*,*)'Review messages written to the LIST file'
         stop
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine comp
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481          Level: 070720              COMP
c ---           J. Scire, D. Strimaitis
c
c --- PURPOSE:  Main computational routine that loops over time periods
c               in the CALMET file, and prepares output.
c
c --- UPDATES:
c --- V4.47 (070719) V4.48 (070720)  DGS
c                - Add user-supplied stepping index for VEC plot-file
c                - Restrict snapshot/mean T,W output when LCALGRD=F
c                - Enlarge format field (f9.2) for timeseries variables
c
c --- V4.45 (061215) V4.47 (070719)  DGS
c                - Implement automatic filename section for 3D.DAT file
c                  
c --- V4.42 (051118) V4.45 (061215)  DGS/ ED-SLEI
c                - The ITS pointer was advanced to an undefined location
c                  if mixing height is not on the output list
c                  Contributed by: Eric Delisle
c                          SNC-Lavalin Environment Inc. 
c                          Montreal (Quebec)
c --- V4.41 (051012) V4.42 (051118)  FRR
c               - Implement seconds as minutes in timestamp portion
c                 of the automatic filenames for plot-files
c --- V4.4 (041208) V4.41 (051012)  DGS
c               - Use begin/end times as read from CALMET.DAT file
c
c --- V4.3 (040923) V4.4 (041208)  FRR
c               - Compute hourly explicit beg/ending times and pass them to 
c                 subroutines OUT, OUTFX, OUTIFX via common/dat/
c               - output explicit times in timeseries records
c               - Change message, mess1, mess2 lengths from 70 to 40 characters
c
c --- V4.2 (040330) to V4.3 (040923)  DGS
c               - Use RDMET subroutine from CALPUFF (few modifications)
c               - Add 3D.DAT data processing
c               - Add control file flag for Image GRD format
c --- V4.14 (040318) to V4.2 (040330)  DGS
c               - Output speed & direction GRD files,
c                 or U & V GRD files, or the post-type file created in
c                 the previous version for VECT plots
c               - Create plot files only for selected layers
c               - Replace filename strings c*70 with c*132
c               - Make the automatic files of real numbers BINARY
c                 if not over-ridden by LSNAPTXT
c               - Make PGT plot file an IMAGE file (like LandUse) by
c                 shifting coordinate range from center of the corner
c                 cells to outside edge, and setting the 'z' range to
c                 0 through 6
c --- V4.13 (040225) to V4.14 (040318)  DGS
c               - Add flag for Image GRD file to GRDOUTI & GRDOUTR
c --- V4.0 (020208) to V4.1 (030402)  FRR/DGS
c               - Add 2-D sfc met arrays (NOOBS)
c
c --- INPUTS:
c       common block /CONTROL/
c                   NVECSTEP, LCALGRD
c
c       Parameters: IO1,IO6, IO7, IO9, MXNX, MXNY, MXNZ, MXSS
c                   MXSNAP, MXAV
c
c --- OUTPUT:  none
c     out via common block /DAT/
c           IBYRN, IBMON, IBDAYN, IBJULN, IBHRN, IBSECN
c           IEYRN, IEMON, IEDAYN, IEJULN, IEHRN, IESECN

c
c --- COMP called by:  MAIN
c --- COMP calls:      initar,rdmet,wswd
c                      outfx,out,xmult,outifx,INCR,GRDAY,DEDAT,
c                      INCRS, DELTSEC,
c                      grdoutr,grdouti,vector
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmt'

c --- Include common blocks
      include 'control.pmt'
      include 'filnam.pmt'
      include 'grid.pmt'
      include 'header.pmt'
c
      real xbuf(mxnx,mxny)
      real xtmp1(mxnx,mxny),xtmp2(mxnx,mxny)
      real xustar(24), xzi(24), xel(24), xwstar(24), xrmm(24)
      real xuws(24,mxnz),xvwd(24,mxnz),xw(24,mxnz),xtemp(24,mxnz)
      real u(mxnx,mxny,mxnz),v(mxnx,mxny,mxnz)
      real w(mxnx,mxny,mxnz),ztemp(mxnx,mxny,mxnz)
      real ustar(mxnx,mxny),zi(mxnx,mxny),el(mxnx,mxny)
      real wstar(mxnx,mxny),rmm(mxnx,mxny)
      real tempk(mxss),rho(mxss),qsw(mxss)
c
c frr - additional 2D arrays for temp,rh,rho,ipcode,qsw
      real    temp2d(mxnx,mxny),rho2d(mxnx,mxny),qsw2d(mxnx,mxny)
      integer irh2d (mxnx,mxny),ipcode2d(mxnx,mxny)

c --- Additional 3D.DAT arrays
      integer izrh(mxnx,mxny,mxnz)
      real zagl(mxnx,mxny,mxnz)
      real pmsl2d(mxnx,mxny),qlw2d(mxnx,mxny)

c
      integer ipgt(mxnx,mxny),irh(mxss),ipcode(mxss),istab(24)
      integer iyear(2,24),imonth(2,24),iday(2,24)
      integer ihour(2,24),isecond(2,24)

      character*40 messag,mess1,mess2
      character*132 fileauto
      character*8 clabel
      character*20 fmtr,fmti,fmtb
      character*9 cdathr
      character*8 cdate, c8timestep
      character*3 ctzone
c --- 051118 minute for plot filename timestamp
      character*2 cmin
c
      logical*4 ldate,ldt,lauto
      logical lecho
c
      real ws(mxnx,mxny),wd(mxnx,mxny)
      real vav(mxnx,mxny,mxav)
      real vavu(mxnx,mxny),vavv(mxnx,mxny)
      real wsav(mxnx,mxny),wdav(mxnx,mxny)

c frr 040225 - Timeseries print variables (stab,u*,El,W*,mixh,rmm,
c                                          nz * (u,v,w,t) )
      parameter (ntmax=6+mxnz*4)
      real var(ntmax)
      character*10 varchar(ntmax)
      integer kkvar(ntmax)

      common/dat/iyr,imo,idy,ijul,ihr,ibyrn,ibmon,ibdayn,ibjuln,ibhrn,
     :  ibsecn,ieyrn,iemon,iedayn,iejuln,iehrn,iesecn     
c
      data ldate/.false./,zero/0.0/
      data ipcode/mxss*0/
      data fmtr/'(10000(1pe11.4,1x)) '/
      data fmti/'(10000(i3,2x))      '/

c --- For RDMET calls
      nwork=mxnx*mxny
      lecho=.false.

c --- Report progress
      write(iomesg,*)'COMPUTATIONAL PHASE'

c --- Initialize arrays which may not be included in CALMET.DAT file
c --- (IPCODE is small enough to be initialized in a data statement)
      call initar(zero,mxxy,rmm)
      call initar(zero,24,xrmm)
      call initar(zero,mxxyz,w)
      call initar(zero,mxxyz,ztemp)

c --- Set automatic plot directive
      lauto=.FALSE.
      if(lvect.OR.ltemp.OR.lprec.OR.lmixh.OR.lipgt)
     &   lauto=.TRUE.

c --- Set format for snapshot files that may be binary
      fmtb='BINARY              '
      if(LSNAPTXT) fmtb=fmtr

c --- Pass the starting date and time into the 'current' variables
      iyr=ibyr
      imo=ibmo
      idy=ibdy
      ijul=ibjul
      ihr=ibhr
      isec=ibsec
c --- Code the current date/time which is expected
      idathr=iyr*100000+ijul*100+ihr
c --- Code the final date/time that is requested
      iedathr=ieyr*100000+iejul*100+iehr
c
c --- Determine if winds will be displayed
      iprtuv=0
      iprtw=0
      iprtt=0
      do 11 i=1,nz
      if(iuvout(i).eq.1)iprtuv=1
      if(iwout(i).eq.1)iprtw=1
      if(itout(i).eq.1)iprtt=1
11    continue
c
c --- iprtsum = 0 implies no 3-D fields are printed
      iprtsum=iprtuv+iprtw+iprtt
c
      if(iprtuv.eq.0)then
      write(io6,*)
      write(io6,*)' No U,V levels were selected, therefore no U,V ',
     1   'data will be displayed.'
      endif
      if(lcalgrd.and.iprtw.eq.0)then
      write(io6,*)
      write(io6,*)' No W levels were selected, therefore no W ',
     1   'data will be displayed.'
      endif
      if(lcalgrd.and.iprtt.eq.0)then
      write(io6,*)
      write(io6,*)' No temperature levels were selected, therefore ',
     1   'no temperature data will be displayed.'
      endif
c

c --- Open timeseries file (optional) and print out headers
      if(isp.eq.1)then
c ---    open separate timeseries file (column format;to be imported in Excel)
         open(io1,file=prttime)

c ---    Header #1
         write(io1,*)'Timeseries at gridpoint (',nspx,',',nspy,')'


c ---    Header #2 - Variables
         its=1
         do 2217 kk=1,nz
            if (iuvout(kk).eq.1)then
               if (ipws.eq.1) then
                  varchar(its)  = '    WS    '
                  varchar(its+1)= '    WD    '
               else
                  varchar(its)  = '    U     '
                  varchar(its+1)= '    V     '
               endif
               its=its+2
            endif

            if (itout(kk).eq.1)then
               varchar(its)= '     T    '
               its=its+1
            endif

            if (iwout(kk).eq.1)then
               varchar(its)= '    W     '
               its=its+1
            endif
2217     end do

         if (lstab) then 
            varchar(its)= '    PG    '
            its=its+1
         endif
         if (lustr) then 
            varchar(its)= '     U*   '
            its=its+1
         endif         
         if (lmoln) then 
            varchar(its)= '     Lmo  '
            its=its+1
         endif        
         if (lwstr) then 
            varchar(its)= '     W*   '
            its=its+1
         endif
         if (lprat) then 
            varchar(its)= ' Prec.Rate'
            its=its+1
         endif
         if (lmxht) then 
            varchar(its)= ' Mix.Hgt  '
c ---       ED-SLEI (061215)
            its=its+1
         endif

c ---    ED-SLEI (061215)
         its=its-1

c         write(io1,2216)' YEAR ','MONTH',' DAY ',' HOUR ',
c     :                   (varchar(n),n=1,its)
c2216     format(4(a6),80(A10))

         write(io1,2216)' YEAR ','MONTH',' DAY ','HOUR ',' SEC',
     :                  ' YEAR ','MONTH',' DAY ','HOUR ',' SEC',
     :                   (varchar(n),n=1,its)
2216     format(2(a6,a5,a5,a5,a4),2x,80(A10))

c ---    Header #3 - Units
         its=1
         do 2219 kk=1,nz
            if (iuvout(kk).eq.1)then
               if (ipws.eq.1) then
                  if (wsfac.eq.1)then
                     varchar(its)  = '   (m/s)  '
                  else
                     varchar(its)  = ' user unit'
                  endif  
                  varchar(its+1)= '   (deg)  '
               else
                  if (wsfac.eq.1)then
                     varchar(its)  = '   (m/s)  '
                     varchar(its+1)= '   (m/s)  '
                  else
                     varchar(its)  = ' user unit'
                     varchar(its+1)= ' user unit'
                  endif  
               endif
               its=its+2
            endif

            if (itout(kk).eq.1)then
               varchar(its)= '    (K)   '
               its=its+1
            endif

            if (iwout(kk).eq.1)then
               varchar(its)= '  (cm/s)  '
               its=its+1
            endif
2219     end do

            if (lstab) then 
               varchar(its)= '   class  '
               its=its+1
            endif
            if (lustr) then 
               varchar(its)= '   (m/s)  '
               its=its+1
            endif         
            if (lmoln) then 
               varchar(its)= '    (m)   '
               its=its+1
            endif        
            if (lwstr) then 
               varchar(its)= '  (cm/s)  '
               its=its+1
            endif
            if (lprat) then 
               varchar(its)= '  (mm/hr) '
               its=its+1
            endif
            if (lmxht) then 
               varchar(its)= '    (m)   '
c ---          ED-SLEI (061215)
               its=its+1
            endif

c ---    ED-SLEI (061215)
         its=its-1

         write(io1,4216)(varchar(n),n=1,its)
4216     format(50x,2x,80(A10))


c ---    Header #4 -Layers
         its=1
         do 2218 kk=1,nz
            if (iuvout(kk).eq.1)then
               if (ipws.eq.1) then
                  kkvar(its)  = kk
                  kkvar(its+1)= kk
               else
                  kkvar(its)  = kk
                  kkvar(its+1)= kk
               endif
               its=its+2
            endif

            if (itout(kk).eq.1)then
               kkvar(its)=  kk
               its=its+1
            endif

            if (iwout(kk).eq.1)then
               kkvar(its)= kk
               its=its+1
            endif
2218     end do

         its=its-1

         write(io1,3216)(kkvar(n),n=1,its)
3216     format(50x,2x,80(1x,'Layer',i2,2x))

      endif
c frr 040225: end of timeseries headers



c --------------------------------------
c --- loop over timesteps
c --------------------------------------

      loop=0
      icount=0
      ldate=.true.
c --- Initial timestep (=NSECDT3D) will be recomputed for CALMET
      nsecdt=nsecdt3d
1000  loop=loop+1

      if(metfm.EQ.1) then
         call RDMET(idathr,isec,xbuf,nwork,lecho,kdathre,ksece,
     &              u,v,ztemp,ipgt,zi,ustar,el,wstar,rmm,tempk,rho,
     &              qsw,irh,ipcode,temp2d,rho2d,qsw2d,irh2d,ipcode2d,
     &              w)

c ---    First time, process number of seconds in the CALMET timestep
c ---    (expect minutes or hours)
         if(loop.EQ.1) then
            call DELTSEC(idathr,isec,kdathre,ksece,nsecdt)
c ---       Set filename string segment
            c8timestep='        '
            if(MOD(nsecdt,3600).EQ.0) then
c ---          Hours
               n=nsecdt/3600
               c8timestep='      HR'
               k2=6
            elseif(MOD(nsecdt,60).EQ.0) then
c ---          Minutes
               n=nsecdt/60
               c8timestep='     MIN'
               k2=5
            else
c ---          Seconds
               n=nsecdt
               c8timestep='     SEC'
               k2=5
            endif
            if(n.LT.10) then
               k1=k2
               write(c8timestep(k1:k2),'(i1)') n
            elseif(n.LT.100) then
               k1=k2-1
               write(c8timestep(k1:k2),'(i2)') n
            elseif(n.LT.1000) then
               k1=k2-2
               write(c8timestep(k1:k2),'(i3)') n
            elseif(n.LT.10000) then
               k1=k2-3
               write(c8timestep(k1:k2),'(i4)') n
            else
               stop 'TIMESTEP too large for string'
            endif

c ---       Initialize filename variable
            do j=1,4
               m=1+33*(j-1)
               n=33*j
               fileauto(m:n)='                                 '
            enddo
c ---       Set the standard punctuation
            fileauto(1:32)='0000_M00_D00_0000(UTC+0000)_L00_'
c                           12345678901234567890123456789012
            kk1=33
            kk2=40-k1+1
            fileauto(kk1:kk2)=c8timestep(k1:8)
            kdot=kk2+1
            fileauto(kdot:kdot)='.'
c ---       Range for file extension (3 characters)
            kx1=kdot+1
            kx2=kdot+3
         endif

c ---    Decode datetime variable for end-time of this timestep
         call DEDAT(kdathre,kyre,kjule,khre)

c ---    Get Gregorian month and day
         call GRDAY(io6,kyre,kjule,kmoe,kdaye)

c ---    Set time logical to true (RDMET finds the requested time)
         ldt=.TRUE.

      elseif(metfm.EQ.2) then
         call RD3D(idathr,isec,kdathre,ksece,u,v,w,ztemp,izrh,zagl,
     &             temp2d,pmsl2d,qsw2d,qlw2d,irh2d,rmm)
c ---    First time, find number of seconds in the 3D.DAT average
         if(loop.EQ.1) then
            call DELTSEC(idathr,isec,kdathre,ksece,nsecavg)
c ---       Set filename string segment
            c8timestep='        '
            if(nsecavg.EQ.0) then
c ---          Instantaneous, so use 1 second
               n=1
               c8timestep='     SEC'
               k2=5
            elseif(MOD(nsecavg,3600).EQ.0) then
c ---          Hours
               n=nsecavg/3600
               c8timestep='      HR'
               k2=6
            elseif(MOD(nsecavg,60).EQ.0) then
c ---          Minutes
               n=nsecavg/60
               c8timestep='     MIN'
               k2=5
            else
c ---          Seconds
               n=nsecavg
               c8timestep='     SEC'
               k2=5
            endif
            if(n.LT.10) then
               k1=k2
               write(c8timestep(k1:k2),'(i1)') n
            elseif(n.LT.100) then
               k1=k2-1
               write(c8timestep(k1:k2),'(i2)') n
            elseif(n.LT.1000) then
               k1=k2-2
               write(c8timestep(k1:k2),'(i3)') n
            elseif(n.LT.10000) then
               k1=k2-3
               write(c8timestep(k1:k2),'(i4)') n
            else
               stop 'TIMESTEP too large for string'
            endif

c ---       Initialize filename variable
            do j=1,4
               m=1+33*(j-1)
               n=33*j
               fileauto(m:n)='                                 '
            enddo
c ---       Set the standard punctuation
            fileauto(1:32)='0000_M00_D00_0000(UTC+0000)_L00_'
c                           12345678901234567890123456789012
            kk1=33
            kk2=40-k1+1
            fileauto(kk1:kk2)=c8timestep(k1:8)
            kdot=kk2+1
            fileauto(kdot:kdot)='.'
c ---       Range for file extension (3 characters)
            kx1=kdot+1
            kx2=kdot+3
         endif

c ---    Decode datetime variable for end-time of this timestep
         call DEDAT(kdathre,kyre,kjule,khre)

c ---    Get Gregorian month and day
         call GRDAY(io6,kyre,kjule,kmoe,kdaye)

c ---    Set time logical to true (RD3D finds the requested time)
         ldt=.TRUE.

      else
         write(*,*)'FATAL Error in COMP --- Invalid METFM'
         write(*,*)'                       Expected: 1, 2'
         write(*,*)'                          Found: ',metfm
         stop
      endif

c --- Transfer date/times for current timestep into /DAT/ variables
      ibyrn=iyr
      ibmon=imo
      ibdayn=idy
      ibjuln=ijul
      ibhrn=ihr
      ibsecn=isec
      ieyrn=kyre
      iemon=kmoe
      iedayn=kdaye
      iejuln=kjule
      iehrn=khre
      iesecn=ksece
c
c frr 040225 - Column format for timeseries (compatible with Excel)

c --- Increment counter of arrays holding data for one grid pt
      if(ldt.and.isp.eq.1)then
         icount=icount+1
c
c ---   if 24-timesteps have accumulated in the array, print the table
        if(icount.gt.24)then
           icount=1

c ---      print 24-timesteps 
           do 220 jj=1,24

c             fill in the variables to print
              its=1
              do 217 kk=1,nz
                 if (iuvout(kk).eq.1)then
                    var(its)  = xuws(jj,kk)
                    var(its+1)=xvwd(jj,kk)
                    its=its+2
                  endif

                  if (itout(kk).eq.1)then
                    var(its)= xtemp(jj,kk)
                    its=its+1
                  endif

c                 vertical velocity in cm/s
                  if (iwout(kk).eq.1)then
                    var(its)= xw(jj,kk)*100.
                    its=its+1
                  endif
217            end do

               if (lstab) then 
                  var(its)= istab(jj)
                  its=its+1
               endif
               if (lustr) then 
                  var(its)= xustar(jj)
                  its=its+1
               endif         
               if (lmoln) then 
                  var(its)= xel(jj)
                  its=its+1
               endif        
c              W* in cm/s
               if (lwstr) then
                  var(its)= xwstar(jj)*100.
                  its=its+1
               endif
               if (lprat) then 
                 var(its)= xrmm(jj)
                  its=its+1
               endif
               if (lmxht) then 
                  var(its)= xzi(jj)
c ---             ED-SLEI (061215)
                  its=its+1
               endif

c ---          ED-SLEI (061215)
               its=its-1

c ---          Print out 24 timestepss worth of timeseries
               write(io1,215)iyear(1,jj),imonth(1,jj),iday(1,jj),
     &                       ihour(1,jj),isecond(1,jj),
     &                       iyear(2,jj),imonth(2,jj),iday(2,jj),
     &                       ihour(2,jj),isecond(2,jj),
     &                       (var(kt),kt=1,its)
215            format(2(1x,i4,3x,i2,2x,i2,3x,i2,2x,i4),80(1x,f9.2))       

220         continue

         endif
c
c ---    Store date/time of this entry in timeseries table
c ---    Start date/time
         iyear(1,icount)=iyr
         imonth(1,icount)=imo
         iday(1,icount)=idy
         ihour(1,icount)=ihr
         isecond(1,icount)=isec
c ---    End date/time
         iyear(2,icount)=kyre
         imonth(2,icount)=kmoe
         iday(2,icount)=kdaye
         ihour(2,icount)=khre
         isecond(2,icount)=ksece
      endif
c
c frr 040225 : END of timeseries printing


c --- print wind field for levels and time periods requested
c --- (iprtsum = 0 implies that no 3-D fields are printed)
      if(iprtsum.eq.0)go to 143
      if(.not.ldt)go to 143
      do 142 kk=1,nz
c
c --- ioutkk = 0 implies no 3-D fields printed for this layer
      ioutkk=iuvout(kk)+iwout(kk)+itout(kk)
      if(ioutkk.eq.0)go to 142
c
      if(ipws.eq.1)then
c
c ---    convert U, V to wind speed, wind direction for
c ---    printing
      call wswd(u(1,1,kk),v(1,1,kk),nx,ny,wsfac,xtmp1,xtmp2)
c
      if(isp .eq. 1)then
c
c ---    one grid point only -- save WS, WD for table
         xuws(icount,kk)=xtmp1(nspx,nspy)
         xvwd(icount,kk)=xtmp2(nspx,nspy)
         xw(icount,kk)=w(nspx,nspy,kk)
         xtemp(icount,kk)=ztemp(nspx,nspy,kk)
      else
c
c ---    Gridded fields -- set messages for printing wind speed
c ---                      & direction
         mess2='Wind Direction (deg.) -- Level: '
         write(mess2(33:35),'(i3)')kk
         if(wsfac.eq.1.0)then
c
c ---       Wind speed in m/s
            mess1='Wind Speed (m/s) -- Level: '
            write(mess1(28:30),'(i3)')kk
         else
c
c ---       Wind speed in USER UNITS
            write(io6,32) wsfac
32          format(/1x,'Factor to convert wind speed ',
     1      'from m/s to user units (WSFAC) = ',f10.4)
            mess1='Wind Speed (user units) -- Level: '
            write(mess1(35:37),'(i3)')kk
         endif
c
c --   Write gridded WS, WD fields
       if(iuvout(kk).eq.1)then
         if(ifws.eq.1)then
            call outfx(xtmp1,1,ldate,mess1,nbx,nby,nex,ney)
            call outfx(xtmp2,0,ldate,mess2,nbx,nby,nex,ney)
         else
            call out(xtmp1,idum,1,5,ldate,mess1,nbx,nby,
     1         nex,ney)
            call out(xtmp2,idum,1,5,ldate,mess2,nbx,nby,
     1         nex,ney)
         endif
       endif
c
c --   Write gridded W fields
       if(lcalgrd.and.iwout(kk).eq.1)then
          messag='W-component (m/s) -- Level: '
          write(messag(29:31),'(i3)')kk
          call out(w(1,1,kk),idum,1,5,ldate,messag,nbx,nby,
     1       nex,ney)
       endif
c
c --   Write gridded temperature field
       if(lcalgrd.and.itout(kk).eq.1)then
          messag='Temperature (deg. K) -- Level: '
          write(messag(32:34),'(i3)')kk
          call out(ztemp(1,1,kk),idum,1,5,ldate,messag,nbx,nby,
     1       nex,ney)
c ---     Put out RH profile if this is 3D.DAT
          if(metfm.EQ.2) then
             messag='Rel. Humidity (%)    -- Level: '
             write(messag(32:34),'(i3)')kk
             call out(rdum,izrh,2,3,ldate,messag,nbx,nby,nex,ney)
          endif
       endif
      endif
      else
c
c ---    U, V wind components printed
c
c ---    Transfer U, V to work arrays (XTMP1, XTMP2) & convert
c ---    to user units (if necessary)
      call xmult(u(1,1,kk),nx,ny,wsfac,xtmp1)
      call xmult(v(1,1,kk),nx,ny,wsfac,xtmp2)
c
      if(isp.eq.1)then
c
c ---    one grid point only -- save WS, WD for table
         xuws(icount,kk)=xtmp1(nspx,nspy)
         xvwd(icount,kk)=xtmp2(nspx,nspy)
         xw(icount,kk)=w(nspx,nspy,kk)
         xtemp(icount,kk)=ztemp(nspx,nspy,kk)
      else
c
c ---    gridded fields -- set labels
         if(wsfac.eq.1.0)then
            mess1='U-component (m/s) -- Level: '
            write(mess1(29:31),'(i3)')kk
            mess2=mess1
            mess2(1:1)='V'
         else
            write(io6,32)wsfac
            mess1='U-component (user units) -- Level: '
            write(mess1(36:38),'(i3)')kk
            mess2=mess1
            mess2(1:1)='V'
         endif
c
c ---  write the gridded field -- U, V components
       if(iuvout(kk).eq.1)then
         if(ifws.eq.1)then
            call outfx(xtmp1,2,ldate,mess1,nbx,nby,nex,ney)
            call outfx(xtmp2,2,ldate,mess2,nbx,nby,nex,ney)
         else
            call out(xtmp1,idum,1,5,ldate,mess1,nbx,nby,
     1         nex,ney)
            call out(xtmp2,idum,1,5,ldate,mess2,nbx,nby,
     1         nex,ney)
         endif
       endif
c
c --   write gridded W fields
       if(lcalgrd.and.iwout(kk).eq.1)then
          messag='W-component (m/s) -- Level: '
          write(messag(29:31),'(i3)')kk
          call out(w(1,1,kk),idum,1,5,ldate,messag,nbx,nby,
     1       nex,ney)
       endif
c
c --   write gridded temperature field
       if(lcalgrd.and.irtype.eq.1.and.itout(kk).eq.1)then
          messag='Temperature (deg. K) -- Level: '
          write(messag(32:34),'(i3)')kk
          call out(ztemp(1,1,kk),idum,1,5,ldate,messag,nbx,nby,
     1       nex,ney)
c ---     Put out RH profile if this is 3D.DAT
          if(metfm.EQ.2) then
             messag='Rel. Humidity (%)    -- Level: '
             write(messag(32:34),'(i3)')kk
             call out(rdum,izrh,2,3,ldate,messag,nbx,nby,nex,ney)
          endif
       endif
      endif
      endif
142   continue
143   continue
c
c --- Write other 2-D meteorological fields -- PGT stability class,
c       Friction velocity (m/s), Mixing height (m), Monin-Obukhov
c       length (m), Convective velocity scale (m/s), Precip.
c       rate (mm/hr)
c ---   (if run type = 0, i.e., only winds computed, skip rest of loop)
      if(irtype.eq.0)go to 996
c
      if(ldt.and.lstab)then
      messag='PGT stability class'
      if(isp .eq. 1)then
         istab(icount)=ipgt(nspx,nspy)
      else
         if(ifstab.eq.1)then
            call outifx(ipgt,1,ldate,messag,nbx,nby,nex,ney)
         else
            call out(xdum,ipgt,2,5,ldate,messag,nbx,nby,nex,ney)
         endif
      endif
      endif
c
      if(ldt.and.lustr)then
      messag='Friction velocity (m/s)'
      if(isp .eq. 1)then
         xustar(icount)=ustar(nspx,nspy)
      else
         if(ifustr.eq.1)then
            call outfx(ustar,3,ldate,messag,nbx,nby,nex,ney)
         else
            call out(ustar,idum,1,5,ldate,messag,nbx,nby,nex,ney)
         endif
      endif
      endif
c
      if(ldt.and.lmxht)then
      messag='Mixing height (m)'
      if(isp .eq. 1)then
         xzi(icount)=zi(nspx,nspy)
      else
         if(ifmxht.eq.1)then
            call outfx(zi,0,ldate,messag,nbx,nby,nex,ney)
         else
            call out(zi,idum,1,5,ldate,messag,nbx,nby,nex,ney)
         endif
      endif
      endif
c
      if(ldt.and.lmoln)then
      messag='Monin-Obukhov length (m)'
      if(isp .eq. 1)then
         xel(icount)=el(nspx,nspy)
      else
         if(ifmoln.eq.1)then
            call outfx(el,0,ldate,messag,nbx,nby,nex,ney)
         else
            call out(el,idum,1,5,ldate,messag,nbx,nby,nex,ney)
         endif
      endif
      endif
c
      if(ldt.and.lwstr)then
      messag='Convective velocity scale (m/s)'
      if(isp .eq. 1)then
         xwstar(icount)=wstar(nspx,nspy)
      else
         if(ifwstr.eq.1)then
            call outfx(wstar,1,ldate,messag,nbx,nby,nex,ney)
         else
            call out(wstar,idum,1,5,ldate,messag,nbx,nby,nex,ney)
         endif
      endif
      endif
c
      if(npsta.gt.0)then
      if(ldt.and.lprat)then
         messag='Precipitation rate (mm/hr)'
         if(isp .eq. 1)then
            xrmm(icount)=rmm(nspx,nspy)
         else
            if(ifprat.eq.1)then
            call outfx(rmm,2,ldate,messag,nbx,nby,nex,ney)
            else
            call out(rmm,idum,1,5,ldate,messag,nbx,nby,nex,ney)
            endif
         endif
      endif
      endif
c
c --- Write the 1-D or 2-D meteorological fields -- Air temp. (deg. K),
c ---   Air density (kg/m**3), Short-wave solar radiation (W/m**2),
c ---   Relative humidity (percent), Precipitation code
c
      if(ldt.and.lsurf.and.ifilver.EQ.0.and.metfm.eq.1)then
c --- print the 1-D met. fields
      write(io6,992)iyr,imo,idy,ijul,ihr
992      format(/1x,'SURFACE STATION DATA -- Year: ',i4,3x,'Month: ',
     1   i2,3x,'Day: ',i2,3x,'Julian day: ',i3,3x,'Hour: ',i2//
     2   1x,'STATION',3x,'TEMPERATURE',3x,'AIR DENSITY',
     3   3x,'SHORT-WAVE RADIATION',3x,'REL. HUMIDITY',3x,'PRECIP. CODE'/
     4   2x,'NUMBER',5x,'(Deg K)',6x,'(kg/m**3)',9x,'(W/m**2)',15x,
     3   '(%)'/)
      do 995 iss=1,nssta
      write(io6,994)iss,tempk(iss),rho(iss),qsw(iss),irh(iss),
     1   ipcode(iss)
994      format(1x,i4,9x,f5.1,9x,f5.3,10x,f8.2,15x,i4,13x,i2)
995   continue
      endif

      if(ldt.and.lsurf.and.ifilver.EQ.1.and.metfm.EQ.1)then
c ---    CALMET.DAT
c ---    Write the 2-D meteorological fields -- Air temp. (deg. K),
c ---    Air density (kg/m**3), Short-wave solar radiation (W/m**2),
c ---    Relative humidity (percent), Precipitation code

         messag='Surface Temperature (K)'
         call out(temp2d,idum,1,5,ldate,messag,nbx,nby,nex,ney)

         messag='Surface Air Density (kg/m3)'
         call out(rho2d,idum,1,5,ldate,messag,nbx,nby,nex,ney)

         messag='Short Wave Radiation (W/m2)'
         call out(qsw2d,idum,1,5,ldate,messag,nbx,nby,nex,ney)

         messag='Surface Relative Humidity (%)'
         call out(rdum,irh2d,2,3,ldate,messag,nbx,nby,nex,ney)

         messag='Precipitation code (0: no rain-10: rain)'
         call out(rdum,ipcode2d,2,3,ldate,messag,nbx,nby,nex,ney)
      endif

      if(ldt.and.lsurf.and.ifilver.EQ.1.and.metfm.EQ.2)then
c ---    3D.DAT
c ---    Write the 2-D meteorological fields -- Air temp. (deg. K),
c ---    Short- (and Long-) wave solar radiation (W/m**2),
c ---    Relative humidity (percent), Sea Level Pressure (mb)

         messag='Surface Temperature (K)'
         call out(temp2d,idum,1,5,ldate,messag,nbx,nby,nex,ney)

         messag='Short Wave Radiation (W/m2)'
         call out(qsw2d,idum,1,5,ldate,messag,nbx,nby,nex,ney)

         messag='Long Wave Radiation  (W/m2)'
         call out(qlw2d,idum,1,5,ldate,messag,nbx,nby,nex,ney)

         messag='Surface Relative Humidity (%)'
         call out(rdum,irh2d,2,3,ldate,messag,nbx,nby,nex,ney)

         messag='Mean Sea Level Pressure (mb)'
         call out(pmsl2d,idum,1,5,ldate,messag,nbx,nby,nex,ney)

      endif

996   continue

c Contour and vector plots
c Snapshots

c --- Automatic plots
      if(LAUTO) then
c ---    Build date-time portion of filename
c ---    Decode datetime variable
         call DEDAT(idathr,kyr,kjul,khr)
c ---    Get Gregorian month and day
         call GRDAY(io6,kyr,kjul,kmo,kday)
c ---    Form corresponding date integer (YYYYMMDD)
         idate=kyr*10000+kmo*100+kday
c ---    Set date/hour as character variables
         write(cdathr,'(i9)') idathr
         write(cdate,'(i8)') idate
c ---    YYYY
         fileauto(1:4)=cdate(1:4)
c ---    MM
         fileauto(7:8)=cdate(5:6)
c ---    DD
         fileauto(11:12)=cdate(7:8)
c ---    HH (LST)
         fileauto(14:15)=cdathr(8:9)
c ---    mm (LST) - 051118
         minute=isec/60
         write(cmin,'(i2)')minute
         if (minute.lt.10) then
          fileauto(16:16)='0'
          fileauto(17:17)=cmin(2:2)
         else
          fileauto(16:17)=cmin(1:2)
         endif

c ---    sHHmm (-Time Zone)
         write(ctzone,'(i3)') jbtz
         if(jbtz.GT.9) then
            fileauto(22:22)='-'
            fileauto(23:24)=ctzone(2:3)
         elseif(jbtz.GE.0) then
            fileauto(22:23)='-0'
            fileauto(24:24)=ctzone(3:3)
         elseif(jbtz.GT.-10) then
            fileauto(22:23)='+0'
            fileauto(24:24)=ctzone(3:3)
         else
            fileauto(22:22)='+'
            fileauto(23:24)=ctzone(2:3)
         endif

         if(LVECT.OR.LTEMP) then
c ---       Process layers
            do kk=1,nz
               fileauto(30:31)='00'
               if(kk.GT.9) then
                  write(fileauto(30:31),'(i2)') kk
               else
                  write(fileauto(31:31),'(i1)') kk
               endif
               if(LVECT .AND. iwplt(kk).EQ.1) then
                  if(mvect.EQ.0) then
                     call wswd(u(1,1,kk),v(1,1,kk),nx,ny,1.,ws,wd)
                     fileauto(kx1:kx2)='vec'
                     call vector(io9,fileauto,ws(1,1),wd(1,1),nvecstep)
                  elseif(mvect.EQ.1) then
                     call wswd(u(1,1,kk),v(1,1,kk),nx,ny,1.,ws,wd)
                     fileauto(kx1:kx2)='wsp'
                     call grdoutr(io9,fileauto,ws(1,1),fmtb,0)
                     fileauto(kx1:kx2)='wdr'
                     call grdoutr(io9,fileauto,wd(1,1),fmtb,0)
                  elseif(mvect.EQ.2) then
                     fileauto(kx1:kx2)='usp'
                     call grdoutr(io9,fileauto,u(1,1,kk),fmtb,0)
                     fileauto(kx1:kx2)='vsp'
                     call grdoutr(io9,fileauto,v(1,1,kk),fmtb,0)
                  endif
               endif
               if(LTEMP .AND. itplt(kk).EQ.1) then
                  fileauto(kx1:kx2)='deg'
                  call grdoutr(io9,fileauto,ztemp(1,1,kk),fmtb,0)
                  if(metfm.EQ.2) then
c ---                Set out RH(z) from 3D.DAT files also
                     fileauto(kx1:kx2)='hum'
                     call grdouti(io9,fileauto,izrh(1,1,kk),fmti,0,0,0)
                  endif
               endif
            enddo
         endif
         fileauto(30:31)='00'
         if(LPREC) then
            fileauto(kx1:kx2)='prc'
            call grdoutr(io9,fileauto,rmm,fmtb,0)
         endif
         if(LMIXH) then
            fileauto(kx1:kx2)='mix'
            call grdoutr(io9,fileauto,zi,fmtb,0)
         endif
         if(LIPGT) then
            fileauto(kx1:kx2)='pgt'
            call grdouti(io9,fileauto,ipgt,fmti,0.5,6.5,image)
         endif
      endif

c --- Specific plots requested
      do 555 ip=1,nsnap
         if (tsnap(ip).eq.loop) then 
            kk=ksnap(ip)
            if (asnap(ip).eq.'uvel'.or.asnap(ip).eq.'UVEL') then
            call grdoutr(io9,filesnap(ip),u(1,1,kk),fmtb,0)
            goto 555
            endif
            if (asnap(ip).eq.'vvel'.or.asnap(ip).eq.'VVEL') then
            call grdoutr(io9,filesnap(ip),v(1,1,kk),fmtb,0)
            goto 555
            endif
            if (asnap(ip).eq.'wvel'.or.asnap(ip).eq.'WVEL') then
            if(LCALGRD) call grdoutr(io9,filesnap(ip),w(1,1,kk),
     &                               fmtb,0)
            goto 555
            endif
            if (asnap(ip).eq.'temp'.or.asnap(ip).eq.'TEMP') then
            if(LCALGRD) call grdoutr(io9,filesnap(ip),ztemp(1,1,kk),
     &                               fmtb,0)
            goto 555
            endif
            if (asnap(ip).eq.'ipgt'.or.asnap(ip).eq.'IPGT') then
            call grdouti(io9,filesnap(ip),ipgt,fmti,0.5,6.5,image)
            goto 555
            endif
            if (asnap(ip).eq.'usta'.or.asnap(ip).eq.'USTA') then
            call grdoutr(io9,filesnap(ip),ustar,fmtb,0)
            goto 555
            endif
            if (asnap(ip).eq.'mixh'.or.asnap(ip).eq.'MIXH') then
            call grdoutr(io9,filesnap(ip),zi,fmtb,0)
            goto 555
            endif
            if (asnap(ip).eq.'monl'.or.asnap(ip).eq.'MONL') then
            call grdoutr(io9,filesnap(ip),el,fmtb,0)
            goto 555
            endif
            if (asnap(ip).eq.'wsta'.or.asnap(ip).eq.'WSTA')then 
            call grdoutr(io9,filesnap(ip),wstar,fmtb,0)
            goto 555
            endif
            if (asnap(ip).eq.'prec'.or.asnap(ip).eq.'PREC') then
            call grdoutr(io9,filesnap(ip),rmm,fmtb,0)
            goto 555
            endif
            call wswd(u(1,1,kk),v(1,1,kk),nx,ny,1.,ws,wd)
            if (asnap(ip).eq.'wspe'.or.asnap(ip).eq.'WSPE')then
            call grdoutr(io9,filesnap(ip),ws(1,1),fmtb,0)
            goto 555
            endif
            if (asnap(ip).eq.'wdir'.or.asnap(ip).eq.'WDIR') then
            call grdoutr(io9,filesnap(ip),wd(1,1),fmtb,0)
            goto 555
            endif
            if (asnap(ip).eq.'vect'.or.asnap(ip).eq.'VECT') then
            call vector(io9,filesnap(ip),ws(1,1),wd(1,1),nvecstep)
            goto 555
            endif
         endif
555   continue

c
c Contour and vector plots
c Average fields
      if ((loop.ge.ibegav).and.(loop.le.iendav)) then
        do 444 im=1,nmean
           nout=1
           kk=kmean(im)
           if (amean(im).eq.'uvel'.or.amean(im).eq.'UVEL') then
             do j=1,ny
             do i=1,nx
               vav(i,j,im)=vav(i,j,im)+u(i,j,kk)
             end do
             end do
             goto 666
           endif
           if (amean(im).eq.'vvel'.or.amean(im).eq.'VVEL') then
             do j=1,ny
             do i=1,nx
               vav(i,j,im)=vav(i,j,im)+v(i,j,kk)
             end do
             end do
             goto 666
           endif
           if (amean(im).eq.'wvel'.or.amean(im).eq.'WVEL') then
              if(LCALGRD) then
                do j=1,ny
                do i=1,nx
                  vav(i,j,im)=vav(i,j,im)+w(i,j,kk)
                end do
                end do
              else
                nout=0
              endif
              goto 666
           endif
           if (amean(im).eq.'temp'.or.amean(im).eq.'TEMP') then
              if(LCALGRD) then
                do j=1,ny
                do i=1,nx
                  vav(i,j,im)=vav(i,j,im)+ztemp(i,j,kk)
                end do
                end do
              else
                nout=0
              endif
              goto 666
           endif
           if (amean(im).eq.'ipgt'.or.amean(im).eq.'IPGT') then
             do j=1,ny
             do i=1,nx
               vav(i,j,im)=vav(i,j,im)+ipgt(i,j)
             end do
             end do
             goto 666
           endif
           if (amean(im).eq.'usta'.or.amean(im).eq.'USTA') then
             do j=1,ny
             do i=1,nx
               vav(i,j,im)=vav(i,j,im)+ustar(i,j)
             end do
             end do
             goto 666
           endif
           if (amean(im).eq.'mixh'.or.amean(im).eq.'MIXH') then
             do j=1,ny
             do i=1,nx
               vav(i,j,im)=vav(i,j,im)+zi(i,j)
             end do
             end do
             goto 666
           endif
           if (amean(im).eq.'monl'.or.amean(im).eq.'MONL') then
             do j=1,ny
             do i=1,nx
               vav(i,j,im)=vav(i,j,im)+el(i,j)
             end do
             end do
             goto 666
           endif
           if (amean(im).eq.'wsta'.or.amean(im).eq.'WSTA') then
             do j=1,ny
             do i=1,nx
               vav(i,j,im)=vav(i,j,im)+wstar(i,j)
             end do
             end do
             goto 666
           endif
           if (amean(im).eq.'prec'.or.amean(im).eq.'PREC') then
             do j=1,ny
             do i=1,nx
               vav(i,j,im)=vav(i,j,im)+rmm(i,j)
             end do
             end do
             goto 666
           endif
           call wswd(u(1,1,kk),v(1,1,kk),nx,ny,1.,ws,wd)   
           if (amean(im).eq.'wspe'.or.amean(im).eq.'WSPE') then
             do j=1,ny
             do i=1,nx
               vav(i,j,im)=vav(i,j,im)+ws(i,j)
             end do
             end do
             goto 666
           endif
           if (amean(im).eq.'wdir'.or.amean(im).eq.'WDIR') then
             do j=1,ny
             do i=1,nx
               vav(i,j,im)=vav(i,j,im)+wd(i,j)
             end do
             end do
             goto 666
           endif
           if (amean(im).eq.'vect'.or.amean(im).eq.'VECT') then
             do j=1,ny
             do i=1,nx
               vavu(i,j)=vavu(i,j)+u(i,j,kk)
               vavv(i,j)=vavv(i,j)+v(i,j,kk)
             end do
             end do
           endif
666        continue
c--------------Compute the average fields and write a plot file
            if (loop.eq.iendav) then
            if (amean(im).eq.'vect'.or.amean(im).eq.'VECT') then
              do j=1,ny
              do i=1,nx
                vavu(i,j)=vavu(i,j)/(iendav-ibegav+1.)
                vavv(i,j)=vavv(i,j)/(iendav-ibegav+1.)
              end do
              end do
              call wswd(vavu,vavv,nx,ny,1.,wsav,wdav)
              call vector(io9,fileav(im),wsav,wdav,nvecstep)
            elseif(nout.GT.0) then
              do j=1,ny
              do i=1,nx 
                 vav(i,j,im)=vav(i,j,im)/(iendav-ibegav+1.)
              end do
              end do
              call grdoutr(io9,fileav(im),vav(1,1,im),fmtr,0)
            endif
            endif
444       continue
      endif

c *************************************************************
c
c --- Increment time -- set for next timestep to print
      if(ldt)then
         nsecdt2=ichr*nsecdt
         call INCRS(io6,iyr,ijul,ihr,isec,nsecdt2)

         call GRDAY(io6,iyr,ijul,imo,idy)

c ---    Code the current date/hr which is expected
         idathr=iyr*100000+ijul*100+ihr

c ---    Check against end of requested period and jump to top
c ---    of loop if more timesteps are needed
         if(idathr.LT.iedathr) then
            goto 1000
         elseif(idathr.EQ.iedathr .AND. isec.LT.iesec .AND.
     &          metfm.EQ.1) then
            goto 1000
c ---    Include data for end-time as well if 3D.DAT
         elseif(idathr.EQ.iedathr .AND. isec.LE.iesec .AND.
     &          metfm.EQ.2) then
            goto 1000
         endif
      endif

c ---------------------------------
c --- End of loop over timesteps
c ---------------------------------

c --- print remaining data in table for single grid point
c frr 040225 (timeseries file - column format)
      if(isp.eq.1)then
         if(icount.le.0)go to 1226
c
           do 1220 jj=1,icount

c             fill in the variables to print
              its=1
              do 1217 kk=1,nz
                 if (iuvout(kk).eq.1)then
                    var(its)  = xuws(jj,kk)
                    var(its+1)= xvwd(jj,kk)
                    its=its+2
                  endif

                  if (itout(kk).eq.1)then
                    var(its)= xtemp(jj,kk)
                    its=its+1
                  endif

c                 vertical velocity in cm/s
                  if (iwout(kk).eq.1)then
                    var(its)= xw(jj,kk)*100.
                    its=its+1
                  endif
1217            end do

               if (lstab) then 
                  var(its)= istab(jj)
                  its=its+1
               endif
               if (lustr) then 
                  var(its)= xustar(jj)
                  its=its+1
               endif         
               if (lmoln) then 
                  var(its)= xel(jj)
                  its=its+1
               endif
c              W* in cm/s
               if (lwstr) then 
                  var(its)= xwstar(jj)*100.
                  its=its+1
               endif
               if (lprat) then 
                 var(its)= xrmm(jj)
                  its=its+1
               endif
               if (lmxht) then 
                  var(its)= xzi(jj)
c ---             ED-SLEI (061215)
                  its=its+1
               endif

c ---          ED-SLEI (061215)
               its=its-1

c ---          Print out remaining hours of timeseries
c               write(io1,215)iyear(jj),imon(jj),idy(jj),ihour(jj),
c     :                       (var(kt),kt=1,its)
               write(io1,215)iyear(1,jj),imonth(1,jj),iday(1,jj),
     &                       ihour(1,jj),isecond(1,jj),
     &                       iyear(2,jj),imonth(2,jj),iday(2,jj),
     &                       ihour(2,jj),isecond(2,jj),
     &                       (var(kt),kt=1,its)
1220         continue
      endif

1226  continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine rdi1d(iomet,mtver,idat,nwords,clabel,ndathrb,
     &                 nsecb,ndathre,nsece)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 051012                   RDI1D
c ---          J. Scire, Earth Tech
c
c --- PURPOSE:  Read "NWORDS" of a one-dimensional integer array
c
c --- UPDATE
c --- V4.4-V4.41    051012  (DGS): resolve times to the second, and
c                                  include begin/end times
c                                  (remains compatible with
c                                  older end-time version)
c
c --- INPUTS:
c         IOMET - integer       - Fortran unit number of input file
c         MTVER - integer       - Time-mark flag
c                                 0: end-time (no seconds)
c                                 1: begin-time / end-time
c
c --- OUTPUT:
c  IDAT(nwords) - integer array - Array read from file
c        NWORDS - integer       - Number of words to read
c        CLABEL - character*8   - Variable name
c       NDATHRB - integer       - Beginning date and time (YYYYJJJHH)
c         NSECB - integer       - Beginning seconds (SSSS)
c       NDATHRE - integer       - Ending date and time (YYYYJJJHH)
c         NSECE - integer       - Ending seconds (SSSS)
c
c --- RDI1D called by:  METHDX, RDMET
c --- RDI1D calls:      none
c----------------------------------------------------------------------
c
      integer idat(nwords)
      character*8 clabel
c
      if(mtver.EQ.1) then
         read(iomet)clabel,ndathrb,nsecb,ndathre,nsece,idat
      elseif(mtver.EQ.0) then
         read(iomet)clabel,ndathre,idat
         nsece=0
         ndathrb=0
         nsecb=0
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine rdr1d(iomet,mtver,x,nwords,clabel,
     &                 ndathrb,nsecb,ndathre,nsece)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 051012                   RDR1D
c ---          J. Scire, Earth Tech
c
c --- PURPOSE:  Read "NWORDS" of a one-dimensional real array
c
c --- UPDATE
c --- V4.4-V4.41    051012  (DGS): resolve times to the second, and
c                                  include begin/end times for CALMET
c                                  Version 6 (remains compatible with
c                                  older version of CALMET)
c
c --- INPUTS:
c         IOMET - integer     - Fortran unit number of input file
c         MTVER - integer     - Time-mark flag
c                               0: end-time (no seconds)
c                               1: begin-time / end-time
c
c --- OUTPUT:
c     X(nwords) - real array  - Array read from file
c        NWORDS - integer     - Number of words to read
c        CLABEL - character*8 - Variable name
c       NDATHRB - integer     - Beginning date and time (YYYYJJJHH)
c         NSECB - integer     - Beginning seconds (SSSS)
c       NDATHRE - integer     - Ending date and time (YYYYJJJHH)
c         NSECE - integer     - Ending seconds (SSSS)
c
c --- RDR1D called by:  METHDX, RDMET
c --- RDR1D calls:      none
c----------------------------------------------------------------------
      real x(nwords)
      character*8 clabel
c
      if(mtver.EQ.1) then
         read(iomet)clabel,ndathrb,nsecb,ndathre,nsece,x
      elseif(mtver.EQ.0) then
         read(iomet)clabel,ndathre,x
         nsece=0
         ndathrb=0
         nsecb=0
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine rdr2d(iomet,mtver,x,xbuf,mxnx,mxny,nx,ny,clabel,
     &                 ndathrb,nsecb,ndathre,nsece,ieof)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 051012                   RDR2D
c              J. Scire, Earth Tech
c
c --- PURPOSE:  Read NX * NY words of a 2-D real array
c
c --- UPDATE 
c --- V4.4-V4.41    051012 (DGS): resolve times to the second, and
c                                 include begin/end times for CALMET
c                                 Version 6 (remains compatible with
c                                 older version of CALMET)
c --- 940830  to    040923 (DGS): add IEOF to recover from end-of-file
c
c --- INPUTS:
c            IOMET - integer     - Fortran unit number of input file
c            MTVER - integer     - Time-mark flag
c                                  0: end-time (no seconds)
c                                  1: begin-time / end-time
c      XBUF(nx,ny) - real array  - Buffer to hold input data
c      MXNX,MXNY   - integers    - Dimensions of data array
c            NX,NY - integers    - Actual size of grid to read
c
c --- OUTPUT:
c     X(mxnx,mxny) - real array  - Input data array (padded if nec.)
c           CLABEL - character*8 - Variable name
c          NDATHRB - integer     - Beginning date and time (YYYYJJJHH)
c            NSECB - integer     - Beginning seconds (SSSS)
c          NDATHRE - integer     - Ending date and time (YYYYJJJHH)
c            NSECE - integer     - Ending seconds (SSSS)
c             IEOF - integer     - End-of-File status
c                                  0 = pointer within file
c                                  1 = EOF reached on read
c
c --- RDR2D called by:  METHDX, RDMET
c --- RDR2D calls:      none
c----------------------------------------------------------------------
      real x(mxnx,mxny),xbuf(nx,ny)
      character*8 clabel

c --- Set EOF
      ieof=0
c
      if(nx.eq.mxnx.and.ny.eq.mxny)then
c
c ---    entire array is being used -- read full grid
         if(mtver.EQ.1) then
            read(iomet,end=999)clabel,ndathrb,nsecb,ndathre,nsece,x
         elseif(mtver.EQ.0) then
            read(iomet,end=999)clabel,ndathre,x
            nsece=0
            ndathrb=0
            nsecb=0
         endif
      else
c
c ---    only a portion of grid being used -- read and
c ---    transfer from buffer
         if(mtver.EQ.1) then
            read(iomet,end=999)clabel,ndathrb,nsecb,ndathre,nsece,xbuf
         elseif(mtver.EQ.0) then
            read(iomet,end=999)clabel,ndathre,xbuf
            nsece=0
            ndathrb=0
            nsecb=0
         endif
c
         do 10 i=1,nx
         do 10 j=1,ny
         x(i,j)=xbuf(i,j)
10       continue
      endif
c
      return

999   ieof=1
      return

      end
c----------------------------------------------------------------------
      subroutine wswd(u,v,nx,ny,xfact,ws,wd)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.42      Level: 940830                    WSWD
c ---          J. Scire, SRC
c
c --- Include parameter file
      include 'params.pmt'
c
c --- PURPOSE:  Compute wind speed and wind direction from U & V wind
c               components
c
c --- INPUTS:
c       U(mxnx,mxny) - real array - U wind components (m/s)
c       V(mxnx,mxny) - real array - V wind components (m/s)
c                 NX - integer    - No. X grid cells
c                 NY - integer    - No. Y grid cells
c              XFACT - real       - Multiplicative factor to convert
c                                   wind speed in m/s to user units
c
c --- OUTPUT:
c      WS(mxnx,mxny) - real array - Wind speed (user units)
c      WD(mxnx,mxny) - real array - Wind direction (degrees)
c                                   (N wind = 0 deg., E wind = 90 deg.,
c                                    etc.)
c
c --- WSWD called by:  MAIN
c --- WSWD calls:      none
c----------------------------------------------------------------------
c
      real u(mxnx,mxny),v(mxnx,mxny)
      real ws(mxnx,mxny),wd(mxnx,mxny)
c
      data rad/57.295778/
c
      do 100 i=1,nx
      do 100 j=1,ny
c
      ws(i,j)=xfact*sqrt(u(i,j)**2+v(i,j)**2)
      if(ws(i,j).gt.1.e-9)then
      xwd=270.-rad*atan2(v(i,j),u(i,j))
      wd(i,j)=amod(xwd,360.)
      else
      wd(i,j)=0.0
      endif
 100   continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine rdi2d(iomet,mtver,idat,ibuf,mxnx,mxny,nx,ny,clabel,
     &                 ndathrb,nsecb,ndathre,nsece)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 051012                   RDI2D
c              J. Scire, Earth Tech
c
c --- PURPOSE:  Read NX * NY words of a 2-D integer array
c
c --- UPDATE
c --- V4.4-V4.41    051012  (DGS): resolve times to the second, and
c                                  include begin/end times for CALMET
c                                  Version 6 (remains compatible with
c                                  older version of CALMET)
c
c --- INPUTS:
c               IOMET - integer       - Fortran unit number of input
c                                       file
c               MTVER - integer       - Time-mark flag
c                                       0: end-time (no seconds)
c                                       1: begin-time / end-time
c         IBUF(nx,ny) - integer array - Buffer to hold input data
c           MXNX,MXNY - integers      - Dimensions of data array
c               NX,NY - integers      - Actual size of grid to read
c
c --- OUTPUT:
c     IDAT(mxnx,mxny) - integer array - Input data array (padded if
c                                       necessary)
c              CLABEL - character*8   - Variable name
c             NDATHRB - integer       - Beginning date and time (YYYYJJJHH)
c               NSECB - integer       - Beginning seconds (SSSS)
c             NDATHRE - integer       - Ending date and time (YYYYJJJHH)
c               NSECE - integer       - Ending seconds (SSSS)
c
c --- RDI2D called by:  METHDX, RDMET
c --- RDI2D calls:      none
c----------------------------------------------------------------------
      integer idat(mxnx,mxny),ibuf(nx,ny)
      character*8 clabel
c
      if(nx.eq.mxnx.and.ny.eq.mxny)then
c
c ---    entire array is being used -- read full grid
         if(mtver.EQ.1) then
            read(iomet)clabel,ndathrb,nsecb,ndathre,nsece,idat
         elseif(mtver.EQ.0) then
            read(iomet)clabel,ndathre,idat
            nsece=0
            ndathrb=0
            nsecb=0
         endif
      else
c
c ---    only a portion of grid being used -- read and
c ---    transfer from buffer
c
         if(mtver.EQ.1) then
            read(iomet)clabel,ndathrb,nsecb,ndathre,nsece,ibuf
         elseif(mtver.EQ.0) then
            read(iomet)clabel,ndathre,ibuf
            nsece=0
            ndathrb=0
            nsecb=0
         endif
c
         do 10 i=1,nx
         do 10 j=1,ny
         idat(i,j)=ibuf(i,j)
10       continue
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine out(rarray,iarray,ityp,nsigd,ldate,messag,nbx,nby,
     1 nex,ney)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 041208                     OUT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write a gridded field of real or integer numbers in
c               self-scaling format
C
C --- UPDATES:
c     Level 991124 to V4.4 (041208) (FRR)
c           - Replace hour-ending labels with explicit beginning/ending
c             times (with seconds)  (stored in Common/dat/)
c           - change messag character length from 70 to 40
c
c --- INPUTS:
c     RARRAY(MXNX,MXNY) - Real array  - Array of real numbers to print
c                                       (used only if ITYP = 1)
c     IARRAY(MXNX,MXNY) - Int. array  - Array of integer numbers to
c                                       print (used only if ITYP = 2)
c                  ITYP - Integer     - Array type (1=real, 2=integer)
c                 NSIGD - Integer     - No. digits to print (valid range
c                                       for NSIGD is 1 to 5)
c                 LDATE - Logical     - Control variable for printing of
c                                       date (.true. = print date in
c                                       common /dat/, .false. = do not
c                                       print date)
c                MESSAG - Char.*40    - Label of table
c                   NBX - Integer     - Starting X cell to print
c                   NBY - Integer     - Starting Y cell to print
c                   NEX - Integer     - Ending X cell to print
c                   NEY - Integer     - Ending Y cell to print
c
c     Common block /DAT/ variables: (Used only if LDATE=.true.)
c           IBYRN, IBMON, IBDAYN, IBJULN, IBHRN,IBSECN
c           IEYRN, IEMON, IEDAYN, IEJULN, IEHRN,IESECN
c
c     Parameters:
c          MXNX, MXNY, IO6
c
c --- OUTPUT:  none
c
c --- OUT    called by:  MAIN
c --- OUT    calls:      WRT, WRT2
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmt'
c
      real rarray(mxnx,mxny)
c
      integer iarray(mxnx,mxny),icol(5)
      integer iout(mxnx)
c
      logical ldate
c
      character*40 messag
      character*1 csign(mxnx),plus,minus
      character*24 form1(5)
      character*21 form2(5)
      character*18 form3(5)
c
      common/dat/iyr,imo,iday,ijul,ihr,ibyrn,ibmon,ibdayn,ibjuln,ibhrn,
     :  ibsecn,ieyrn,iemon,iedayn,iejuln,iehrn,iesecn     
c
      data icol /40,40,30,25,20/
      data plus,minus /'+','-'/
      data form1 /'(1x,i3,1x,1hI,40(i3,1x))',
     1            '(1x,i3,1x,1hI,40(i3,1x))',
     2            '(1x,i3,1x,1hI,40(i3,1x))',
     3            '(1x,i3,1x,1hI,40(i4,1x))',
     4            '(1x,i3,1x,1hI,40(i5,1x))'/
      data form2 /'(5x,1hI,40(2x,a1,1x))',
     1            '(5x,1hI,40(2x,a1,1x))',
     2            '(5x,1hI,40(2x,a1,1x))',
     3            '(5x,1hI,40(3x,a1,1x))',
     4            '(5x,1hI,40(4x,a1,1x))'/
      data form3 /'(6x,40(i3,1x))',
     1            '(6x,40(i3,1x))',
     2            '(6x,40(i3,1x))',
     3            '(6x,40(i4,1x))',
     4            '(6x,40(i5,1x))'/
c
c --- check that valid values of array type (ityp) and print digits
c --- (nsigd) have been passed to routine
      if(ityp.ne.1.and.ityp.ne.2)then
      write(io6,*)'ERROR in SUBR. OUT -- invalid value of ITYP -- ',
     1   'ITYP = ',ityp
      stop
      endif
      if(nsigd.lt.1.or.nsigd.gt.5)then
      write(io6,*)'ERROR in SUBR. OUT -- invalid value of NSIGD -- ',
     1   'NSIGD = ',nsigd
      stop
      endif
c
c --- compute no. X cells to print
      nx=nex-nbx+1
c
      icr=2
      if(nsigd.eq.1)icr=1
      if(mod(nx,icol(nsigd)).eq.0)then
      npass=nx/icol(nsigd)
      else
      npass=nx/icol(nsigd)+1
      endif
c
c --- real array -- find min. & max. values
      if(ityp.ne.1)go to 50
      xmax=-1.e-25
      xmin=1.e25
      do 10 i=nbx,nex
      do 10 j=nby,ney
      if(rarray(i,j).gt.xmax)xmax=rarray(i,j)
      if(rarray(i,j).lt.xmin)xmin=rarray(i,j)
10    continue
      if(xmin.ne.0.0.or.xmax.ne.0.0)go to 12
C     if(ldate)write(io6,94)messag,iyr,imo,iday,ijul,ihr
      if(ldate)write(io6,94)messag,ibyrn,ibmon,ibdayn,ibjuln,ibhrn,
     :             ibsecn,ieyrn,iemon,iedayn,iejuln,iehrn,iesecn
      if(.not.ldate)write(io6,95)messag
      write(io6,11)
11    format(1x,'GRID NOT PRINTED -- all values zero')
      return
c
12    continue
      xexp=xmax
      if(abs(xmin).gt.xmax)xexp=abs(xmin)
      iexp=alog10(xexp)
      if(xexp.lt.1.0)iexp=iexp-1
      nexp=iexp-(nsigd-icr)
      xscale=10.**(-nexp)
c
      ic1=nbx
      ic2=ic1+icol(nsigd)-1
      if(ic2.gt.nex)ic2=nex
c
      do 30 ipass=1,npass
c
      if(ldate)then
c     write(io6,94)messag,iyr,imo,iday,ijul,ihr
      write(io6,94)messag,ibyrn,ibmon,ibdayn,ibjuln,ibhrn,ibsecn,
     :                    ieyrn,iemon,iedayn,iejuln,iehrn,iesecn  
94       format(/1x,a40,2x,'Beginning year: ',i4,2x,'month: ',i2,2x,
     1   'day: ',i2,2x,'Julian day: ',i3,2x,'hour: ',i2,2x,'second:',
     2   i4/,46x,'Ending year: ',i4,2x,'month: ',i2,2x,'day: ',i2,
     1    2x,'Julian day: ',i3,2x,'hour: ',i2,2x,'second:',i4/)
      else
      write(io6,95)messag
95       format(/1x,a40/)
      endif
      write(io6,109)nexp
109   format(1x,'Multiply all values by 10 ** ',i3/)
c
      do 20 jj=ney,nby,-1
      icnt=0
c
         do 18 i=ic1,ic2
         icnt=icnt+1
         if(rarray(i,jj).lt.0)then
            iout(icnt)=-(rarray(i,jj)*xscale-0.5)
            csign(icnt)=minus
         else
            iout(icnt)=rarray(i,jj)*xscale+0.5
            csign(icnt)=plus
         endif
18          continue
      call wrt(form1(nsigd),form2(nsigd),jj,iout,csign,icnt,io6)
20       continue
      nund=(nsigd+1)*icnt-1
      if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
      write(io6,101)(minus,n=1,nund)
101   format(5x,128a1)
      call wrt2(form3(nsigd),ic1,ic2,io6)
c
      ic1=ic1+icol(nsigd)
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nex)ic2=nex
30    continue
      return
c
c --- integer array -- find min. & max. values
50    continue
      kmax=-9999999
      kmin=9999999
      do 110 i=nbx,nex
      do 110 j=nby,ney
      if(iarray(i,j).gt.kmax)kmax=iarray(i,j)
      if(iarray(i,j).lt.kmin)kmin=iarray(i,j)
110   continue
      if(kmin.ne.0.or.kmax.ne.0)go to 102
c     if(ldate)write(io6,94)messag,iyr,imo,iday,ijul,ihr
      if(ldate)write(io6,94)messag,ibyrn,ibmon,ibdayn,ibjuln,
     :           ibhrn,ibsecn,ieyrn,iemon,iedayn,iejuln,iehrn,iesecn  
      if(.not.ldate)write(io6,95)messag
      write(io6,11)
      return
c
102   continue
      xexp=kmax
      if(iabs(kmin).gt.kmax)xexp=iabs(kmin)
      iexp=alog10(xexp)
      if(xexp.lt.1.0)iexp=iexp-1
      nexp=iexp-(nsigd-icr)
      xscale=10.**(-nexp)
c
      ic1=nbx
      ic2=ic1+icol(nsigd)-1
      if(ic2.gt.nex)ic2=nex
c
      do 130 ipass=1,npass
c
      if(ldate)then
c     write(io6,94)messag,iyr,imo,iday,ijul,ihr
      write(io6,94)messag,ibyrn,ibmon,ibdayn,ibjuln,ibhrn,ibsecn,
     :                    ieyrn,iemon,iedayn,iejuln,iehrn,iesecn  
      else
      write(io6,95)messag
      endif
      write(io6,109)nexp
c
      do 120 jj=ney,nby,-1
      icnt=0
c
         do 118 i=ic1,ic2
         icnt=icnt+1
         if(iarray(i,jj).lt.0)then
            iout(icnt)=-(iarray(i,jj)*xscale-0.5)
            csign(icnt)=minus
         else
            iout(icnt)=iarray(i,jj)*xscale+0.5
            csign(icnt)=plus
         endif
118         continue
      call wrt(form1(nsigd),form2(nsigd),jj,iout,csign,icnt,io6)
120      continue
      nund=(nsigd+1)*icnt-1
      if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
      write(io6,101)(minus,n=1,nund)
      call wrt2(form3(nsigd),ic1,ic2,io6)
c
      ic1=ic1+icol(nsigd)
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nex)ic2=nex
130   continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrt(form1,form2,jj,iout,csign,n,io6)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 940830                     WRT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write one Y row of gridded data
c
c --- INPUTS:
c              FORM1 - Char.*24    - Format field for Y label and data
c                                    to be printed
c              FORM2 - Char.*21    - Format field for sign of data
c                 JJ - Integer     - Y grid cell number
c            IOUT(N) - Int. array  - Array of data to be printed
c                                    (one Y row)
c           CSIGN(N) - Char.*1     - Array containing sign of data
c                                    ('+' or '-')
c                  N - Integer     - Number of cells in this row
c                IO6 - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRT called by:  OUT
c --- WRT calls:      none
c----------------------------------------------------------------------
      integer iout(n)
c
      character*1 csign(n)
      character*24 form1
      character*21 form2
c
      write(io6,form1)jj,iout
      write(io6,form2)csign
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrt2(form,n1,n2,io6)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 940830                    WRT2
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write a line labeling grid cell numbers
c
c --- INPUTS:
c               FORM - Char.*18    - Format field of data to be printed
c                 N1 - Integer     - Starting grid cell number
c                 N2 - Integer     - Ending grid cell number
c                IO6 - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRT2 called by:  OUT
c --- WRT2 calls:      none
c----------------------------------------------------------------------
      character*18 form
c
      write(io6,form)(i,i=n1,n2)
      return
      end
c----------------------------------------------------------------------
      subroutine xmult(xin,nx,ny,xfact,xout)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 940830                   XMULT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Scale a 2-D array with a multiplicative factor
c
c --- INPUTS:
c       XIN(mxnx,mxny) - real array - 2-D input field
c                   NX - integer    - No. X grid cells
c                   NY - integer    - No. Y grid cells
c                XFACT - real       - Multiplicative factor
c        Parameters:
c           MXMX, MXNY
c
c --- OUTPUT:
c      XOUT(mxnx,mxny) - real array - 2-D output field
c
c --- XMULT called by:  COMP
c --- XMULT calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter file
      include 'params.pmt'
c
      real xin(mxnx,mxny),xout(mxnx,mxny)
c
      do 100 i=1,nx
      do 100 j=1,ny
      xout(i,j)=xfact*xin(i,j)
100   continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine outfx(rarray,ndec,ldate,messag,nbx,nby,nex,ney)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 041208              OUTFX
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write a gridded field of real numbers with fixed format
c
c     Level 991124 to V4.4 (041208) (FRR)
c           - Replace hour-ending labels with explicit beginning/ending
c             times (with seconds)  (stored in Common/dat/)
c           - change messag length from 70 to 40 characters
c
c --- INPUTS:
c     RARRAY(MXNX,MXNY) - Real array  - Array of real numbers to print
c                  NDEC - Integer     - Number of decimal places (0-3)
c                                       (print format is F4."NDEC")
c                 LDATE - Logical     - Control variable for printing
c                                       of date (.true. = print date in
c                                       common /dat/, .false. = do not
c                                       print date)
c                MESSAG - Char.*40    - Label of table
c                   NBX - Integer     - Starting X cell to print
c                   NBY - Integer     - Starting Y cell to print
c                   NEX - Integer     - Ending X cell to print
c                   NEY - Integer     - Ending Y cell to print
c
c     Common block /DAT/ variables: (Used only if LDATE=.true.)
c           IBYRN, IBMON, IBDAYN, IBJULN, IBHRN,IBSECN
c           IEYRN, IEMON, IEDAYN, IEJULN, IEHRN,IESECN
c
c     Parameters:
c          MXNX, MXNY, ICOLS, IO6
c
c --- OUTPUT:  none
c
c --- OUTFX  called by:  MAIN
c --- OUTFX  calls:      WRTFX,WRT2
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmt'
c
      real rarray(mxnx,mxny),rout(icols)
      logical ldate
      character*40 messag
      character*1 csign(icols),plus,minus
      character*26 form1(0:3)
      character*21 form2
      character*18 form3
c
      common/dat/iyr,imo,iday,ijul,ihr,ibyrn,ibmon,ibdayn,ibjuln,ibhrn,
     :  ibsecn,ieyrn,iemon,iedayn,iejuln,iehrn,iesecn     
c
      data form1 /'(1x,i3,1x,1hI,25(f4.0,1x))',
     1            '(1x,i3,1x,1hI,25(f4.1,1x))',
     2            '(1x,i3,1x,1hI,25(f4.2,1x))',
     3            '(1x,i3,1x,1hI,25(f4.3,1x))'/
      data form2 /'(5x,1hI,25(3x,a1,1x))'/
      data form3 /'(6x,25(i4,1x))'/
      data plus,minus /'+','-'/
      data nsigd/4/
c
c --- compute total no. cells to print
      nx=nex-nbx+1
c
c --- check for invalid value of "NDEC"
      if(ndec.lt.0.or.ndec.gt.3)then
      write(io6,*)'ERROR in SUBR. OUTFX -- invalid value of ',
     1   'NDEC -- NDEC = ',ndec
      stop
      endif
c
      if(mod(nx,icols).eq.0)then
      npass=nx/icols
      else
      npass=nx/icols+1
      endif
c
      ic1=nbx
      ic2=ic1+icols-1
      if(ic2.gt.nex)ic2=nex
c
      do 30 ipass=1,npass
c
      if(ldate)then
      write(io6,94)messag,ibyrn,ibmon,ibdayn,ibjuln,ibhrn,ibsecn,
     :                    ieyrn,iemon,iedayn,iejuln,iehrn,iesecn  
94       format(/1x,a40,2x,'Beginning year: ',i4,2x,'month: ',i2,2x,
     1    'day: ',i2,2x,'Julian day: ',i3,2x,'hour: ',i2,2x,'second:',
     2   i4/,46x,'Ending year: ',i4,2x,'month: ',i2,2x,'day: ',i2,
     1    2x,'Julian day: ',i3,2x,'hour: ',i2,2x,'second:',i4/)
      else
      write(io6,95)messag
95       format(/1x,40/)
      endif
c
      do 20 jj=ney,nby,-1
      icnt=0
c
         do 18 i=ic1,ic2
         icnt=icnt+1
         if(rarray(i,jj).lt.0)then
            rout(icnt)=abs(rarray(i,jj))
            csign(icnt)=minus
         else
            rout(icnt)=rarray(i,jj)
            csign(icnt)=plus
         endif
18          continue
      call wrtfx(form1(ndec),form2,jj,rout,csign,icnt,io6)
20       continue
      nund=(nsigd+1)*icnt-1
      if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
      write(io6,101)(minus,n=1,nund)
101   format(5x,128a1)
      call wrt2(form3,ic1,ic2,io6)
c
      ic1=ic1+icols
      ic2=ic2+icols
      if(ic2.gt.nex)ic2=nex
30    continue
      return
      end
c----------------------------------------------------------------------
      subroutine wrtfx(form1,form2,jj,rout,csign,n,io6)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 940830                   WRTFX
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write one Y row of gridded real data in fixed format
c
c --- INPUTS:
c              FORM1 - Char.*26    - Format field for Y label and data
c                                    to be printed
c              FORM2 - Char.*21    - Format field for sign of data
c                 JJ - Integer     - Y grid cell number
c            ROUT(N) - Real array  - Array of data to be printed
c                                    (one Y row)
c           CSIGN(N) - Char.*1     - Array containing sign of data
c                                    ('+' or '-')
c                  N - Integer     - Number of cells in this row
c                IO6 - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRTFX called by:  OUTFX
c --- WRTFX calls:      none
c----------------------------------------------------------------------
      real rout(n)
c
      character*1 csign(n)
      character*26 form1
      character*21 form2
c
      write(io6,form1)jj,rout
      write(io6,form2)csign
c
      return
      end
c----------------------------------------------------------------------
      subroutine outifx(iarray,nifm,ldate,messag,nbx,nby,nex,ney)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 041208             OUTIFX
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write a gridded field of integer numbers with fixed
c               integer format
c --- UPDATES:
c     Level 991124 to V4.4 (041208) (FRR)
c           - Replace hour-ending labels with explicit beginning/ending
c             times (with seconds)  (stored in Common/dat/)
c           - Change messag length from 70 to 40 characters
c
c --- INPUTS:
c     IARRAY(MXNX,MXNY) - Integer arr.- Array of integers to print
c                  NIFM - Integer     - Integer format (1-4)
c                                       (print format is I"NIFM")
c                 LDATE - Logical     - Control variable for printing
c                                       of date (.true. = print date in
c                                       common /dat/, .false. = do not
c                                       print date)
c                MESSAG - Char.*40    - Label of table
c                   NBX - Integer     - Starting X cell to print
c                   NBY - Integer     - Starting Y cell to print
c                   NEX - Integer     - Ending X cell to print
c                   NEY - Integer     - Ending Y cell to print
c
c     Common block /DAT/ variables: (Used only if LDATE=.true.)
c           IBYRN, IBMON, IBDAYN, IBJULN, IBHRN, IBSECN
c           IEYRN, IEMON, IEDAYN, IEJULN, IEHRN, IESECN
c
c      Parameters:
c          MXNX, MXNY, ICOLS, IO6
c
c --- OUTPUT:  none
c
c --- OUTIFX  called by:  MAIN
c --- OUTIFX  calls:      WRTIFX,WRT2
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmt'
c
      integer iarray(mxnx,mxny),iout(icols)
      logical ldate
      character*40 messag
      character*1 csign(icols),plus,minus
      character*27 form1(4)
      character*21 form2
      character*18 form3
c
      common/dat/iyr,imo,iday,ijul,ihr,ibyrn,ibmon,ibdayn,ibjuln,ibhrn,
     :  ibsecn,ieyrn,iemon,iedayn,iejuln,iehrn,iesecn     
c
      data form1 /'(1x,i3,1x,1hI,25(3x,i1,1x))',
     1            '(1x,i3,1x,1hI,25(2x,i2,1x))',
     2            '(1x,i3,1x,1hI,25(1x,i3,1x))',
     3            '(1x,i3,1x,1hI,25(i4,1x))'/
      data form2 /'(5x,1hI,25(3x,a1,1x))'/
      data form3 /'(6x,25(i4,1x))'/
      data plus,minus /'+','-'/
      data nsigd/4/
c
c --- determine total no. cells to print
      nx=nex-nbx+1
c
c --- check for invalid value of "NIFM"
      if(nifm.lt.1.or.nifm.gt.4)then
      write(io6,*)'ERROR in SUBR. OUTIFX -- invalid value of ',
     1   'NIFM -- NIFM = ',nifm
      stop
      endif
c
      if(mod(nx,icols).eq.0)then
      npass=nx/icols
      else
      npass=nx/icols+1
      endif
c
      ic1=nbx
      ic2=ic1+icols-1
      if(ic2.gt.nex)ic2=nex
c
      do 30 ipass=1,npass
c
      if(ldate)then
      write(io6,94)messag,ibyrn,ibmon,ibdayn,ibjuln,ibhrn,ibsecn,
     :                    ieyrn,iemon,iedayn,iejuln,iehrn,iesecn  
94       format(/1x,a40,2x,'Beginning year: ',i4,2x,'month: ',i2,2x,
     1    'day: ',i2,2x,'Julian day: ',i3,2x,'hour: ',i2,2x,'second:',
     2   i4/,46x,'Ending year: ',i4,2x,'month: ',i2,2x,'day: ',i2,
     1    2x,'Julian day: ',i3,2x,'hour: ',i2,2x,'second:',i4/)
      else
      write(io6,95)messag
95       format(/1x,a40/)
      endif
c
      do 20 jj=ney,nby,-1
      icnt=0
c
         do 18 i=ic1,ic2
         icnt=icnt+1
         if(iarray(i,jj).lt.0)then
            iout(icnt)=abs(iarray(i,jj))
            csign(icnt)=minus
         else
            iout(icnt)=iarray(i,jj)
            csign(icnt)=plus
         endif
18          continue
      call wrtifx(form1(nifm),form2,jj,iout,csign,icnt,io6)
20       continue
      nund=(nsigd+1)*icnt-1
      if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
      write(io6,101)(minus,n=1,nund)
101   format(5x,128a1)
      call wrt2(form3,ic1,ic2,io6)
c
      ic1=ic1+icols
      ic2=ic2+icols
      if(ic2.gt.nex)ic2=nex
30    continue
      return
      end
c----------------------------------------------------------------------
      subroutine wrtifx(form1,form2,jj,iout,csign,n,io6)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 940830                  WRTIFX
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write one Y row of gridded integer data in I format
c
c --- INPUTS:
c              FORM1 - Char.*27    - Format field for Y label and data
c                                    to be printed
c              FORM2 - Char.*21    - Format field for sign of data
c                 JJ - Integer     - Y grid cell number
c            IOUT(N) - Integer arr.- Array of data to be printed
c                                    (one Y row)
c           CSIGN(N) - Char.*1     - Array containing sign of data
c                                    ('+' or '-')
c                  N - Integer     - Number of cells in this row
c                IO6 - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRTIFX called by:  OUTIFX
c --- WRTIFX calls:      none
c----------------------------------------------------------------------
      integer iout(n)
c
      character*1 csign(n)
      character*27 form1
      character*21 form2
c
      write(io6,form1)jj,iout
      write(io6,form2)csign
c
      return
      end
c----------------------------------------------------------------------
      subroutine initar(xvalue,nwords,xarr)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481     Level: 940830                  INITAR
c              J. Scire, SRC
c
c --- PURPOSE:  Initialize an array with a constant value
c
c --- INPUTS:
c
c          XVALUE - real       - Value to used for initialization
c          NWORDS - integer    - Lengths of the data array (words)
c
c --- OUTPUT:
c
c    XARR(nwords) - real array - Array initialized
c
c --- INITAR called by:  MAIN
c --- INITAR calls:      none
c
c----------------------------------------------------------------------
c
      real xarr(nwords)
c
      x=xvalue
      do 10 i=1,nwords
      xarr(i)=x
10    continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine grdoutr(io,filenam,var,fmt,img)
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481     Level: 070327                 GRDOUTR
c               F. Robe, D. Strimaitis  Earth Tech, Inc.
c
c --- PURPOSE:  Write out a 'GRD' file compatible with SURFER for
c               making plots ---- REAL field version
c
c --- UPDATE
c --- V4.3 (040923) to V4.46 (070327)  DGS
c               - Allow planar GRD files to be written, but make min/max
c                 in header slightly different to force plot
c --- V4.2 (040330) to V4.3 (040923)  DGS
c               - Change definition of argument IMG
c --- V4.14 (040312) to V4.2 (040330)  DGS
c               - Replace filename strings c*70 with c*132
c               - Add option for BINARY File Format (SURFER 6)
c --- V4.0 (020208) to V4.14 (040312)  DGS
c               - Add flag for Image GRD file -- plotted as an
c                 image file so range needs to go from edge to edge
c                 of grid (not cell centers)
c
c --- INPUTS:
c             IO - integer                - output unit number
c        FILENAM - character*132          - name of output file
c            VAR - real array (mxnx,mxny) - 2-D variable to plot
c            FMT - character*40           - format for output record
c                                           (set to 'BINARY' if file
c                                            type is binary)
c            IMG - integer                - image file flag
c                                            0: Image uses standard GRD
c                                               or file used for contour
c                                              (data at cell center)
c                                            1: Image uses shifted range
c                                              (data cover entire cell)
c
c --- COMMON: /GRID/
c ---  XGKM    - real array(mxnx,mxny)  - X coord. of gridpoints (km)
c      YGKM    - real array(mxnx,mxny)  - Y coord. of gridpoints (km)
c      DELX    - real                   - Length of grid cell (km)
c      NBX     - integer                - X Index of the lower left
c                                         gridpoint of subdomain
c      NEX     - integer                - X Index of the lower right
c                                         gridpoint of subdomain
c      NBY     - integer                - Y Index of the lower left
c                                         gridpoint of subdomain
c      NEY     - integer                - Y Index of the upper left
c                                         gridpoint of subdomain
c
c --- OUTPUT: output file (unit=io)
c
c --- GRDOUTR called by:  COMP, METHDX
c --- GRDOUTR calls:      none
c
c---------------------------------------------------------------------------
      include 'params.pmt'
      include 'grid.pmt'

      real var(mxnx,mxny)
      character*132 filenam
      character*40 fmt
      character*4 cid

      real*8 xlo,xhi,ylo,yhi,zlo,zhi
      integer*2 nx2,ny2
      logical lascii

      data rbig/9.9e+20/
      data cid/'DSBB'/

c --- Set logical for either ASCII or BINARY file type
      if(fmt(1:6).EQ.'BINARY') then
         lascii=.FALSE.
      else
         lascii=.TRUE.
      endif

c --- Find minimum and maximum values:
      rmax=-rbig
      rmin=rbig
      do j=nby,ney
         do i=nbx,nex
           if(var(i,j).gt.rmax)rmax=var(i,j)
           if(var(i,j).lt.rmin)rmin=var(i,j)
         enddo
      enddo

c --- Reset rmax>rmin if there is no spatial gradient to
c --- force a SURFER plot frame
      if(rmin.EQ.rmax) then
         if(rmin.GT.0.) then
            rmax=1.001*rmin
         elseif(rmin.LT.0.) then
            rmax=0.999*rmin
         else
            rmax=rmin+.0001
         endif
      endif

c --- Shift (km) from cell center to edge for IMAGE plot
      ds=0.5*delx*FLOAT(img)

      if(LASCII) then
         open(io,file=filenam,status='REPLACE')
c ---    Header
         write(io,'(a4)')'DSAA'
         write(io,'(2i12)')nex-nbx+1,ney-nby+1
         write(io,'(2f12.4)')xgkm(nbx,nby)-ds,xgkm(nex,nby)+ds
         write(io,'(2f12.4)')ygkm(nbx,nby)-ds,ygkm(nbx,ney)+ds
         write(io,'(2e12.4)')rmin,rmax
c ---    Data records
         do j=nby,ney
            write(io,fmt)(var(i,j),i=nbx,nex)
         enddo

      else

         open(io,file=filenam,form='UNFORMATTED',
     &        access='TRANSPARENT',status='REPLACE')
c ---    Set real*8 components of the header
         xlo=DBLE(xgkm(nbx,nby)-ds)
         ylo=DBLE(ygkm(nbx,nby)-ds)
         zlo=DBLE(rmin)
         xhi=DBLE(xgkm(nex,nby)+ds)
         yhi=DBLE(ygkm(nbx,ney)+ds)
         zhi=DBLE(rmax)
c ---    Set integer*4 components of the header
         nx2=nex-nbx+1
         ny2=ney-nby+1
c ---    Header
         write(io)cid
         write(io)nx2,ny2
         write(io)xlo,xhi
         write(io)ylo,yhi
         write(io)zlo,zhi
c ---    Data records
         do j=nby,ney
            write(io)(var(i,j),i=nbx,nex)
         enddo
      endif

      close(io)

      return
      end
c----------------------------------------------------------------------
      subroutine grdouti(io,filenam,ivar,fmt,zlo,zhi,img)
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481     Level: 070327                 GRDOUTI
c               F. Robe, D. Strimaitis  Earth Tech, Inc.
c
c --- PURPOSE:  Write out a 'GRD' file compatible with SURFER for
c               making plots ---- ABS(INTEGER) field version
c
c --- UPDATE
c --- V4.3 (040923) to V4.46 (070327)  DGS
c               - Allow planar GRD files to be written, but make min/max
c                 in header slightly different to force plot
c                 (only if zlo=zhi=0)
c --- V4.21 (040424) to V4.3 (040923)  DGS
c               - Change definition of argument IMG
c --- V4.2 (040330) to V4.21 (040424)  DGS
c               - Drop file output if array is all zero
c --- V4.14 (040312) to V4.2 (040330)  DGS
c               - Replace filename strings c*70 with c*132
c               - Change zlo/zhi to real
c --- V4.0 (020208) to V4.14 (040312)  DGS
c               - Add flag for Image GRD file -- plotted as an
c                 image file so range needs to go from edge to edge
c                 of grid (not cell centers)
c
c --- INPUTS:
c             IO - integer                - output unit number
c        FILENAM - character*132          - name of output file
c           IVAR - int. array (mxnx,mxny) - 2-D variable to plot
c            FMT - character*40           - format for output record
c            ZLO - real                   - lower limit of range
c            ZHI - real                   - upper limit of range
c                                           (do not use if lo=hi=0)
c            IMG - integer                - image file flag
c                                            0: Image uses standard GRD
c                                               or file used for contour
c                                              (data at cell center)
c                                            1: Image uses shifted range
c                                              (data cover entire cell)
c         
c
c --- COMMON: /GRID/
c ---  XGKM    - real array(mxnx,mxny)  - X coord. of gridpoints (km)
c      YGKM    - real array(mxnx,mxny)  - Y coord. of gridpoints (km)
c      DELX    - real                   - Length of grid cell (km)
c      NBX     - integer                - X Index of the lower left
c                                         gridpoint of subdomain
c      NEX     - integer                - X Index of the lower right
c                                         gridpoint of subdomain
c      NBY     - integer                - Y Index of the lower left
c                                         gridpoint of subdomain
c      NEY     - integer                - Y Index of the upper left
c                                         gridpoint of subdomain
c
c --- OUTPUT: output file (unit=io)
c
c --- GRDOUTI called by:  COMP, METHDX
c --- GRDOUTI calls:      none
c
c---------------------------------------------------------------------------
      include 'params.pmt'
      include 'grid.pmt'

      integer ivar(mxnx,mxny)
      character*132 filenam
      character*40 fmt

      data ibig/9999999/

c --- Open file
      open(io,file=filenam,status='REPLACE')

c --- Find minimum and maximum value in array:
      imax=-ibig
      imin=ibig
      do j=nby,ney
         do i=nbx,nex
c ---      Impose absolute value of integer variable
           ivar(i,j)=IABS(ivar(i,j))
           if(ivar(i,j).gt.imax)imax=ivar(i,j)
           if(ivar(i,j).lt.imin)imin=ivar(i,j)
         enddo
      enddo
      zmax=FLOAT(imax)
      zmin=FLOAT(imin)

c --- Special case:  force plot if planar
      if(imax.EQ.imin) then
         imax=imin+1
         zmax=FLOAT(imax)
      endif

c --- Impose limits from call arguments
      ilo=NINT(zlo)
      ihi=NINT(zhi)
      if(ilo.NE.0 .OR. ihi.NE.0) then
         zmax=zhi
         zmin=zlo
      endif

c --- Do not produce file if there is no spatial gradient
      imin=NINT(zmin)
      imax=NINT(zmax)
      if(imin .EQ. imax) then
         close(io, status='DELETE')
         return
      endif

c --- Shift (km) from cell center to edge for IMAGE plot
      ds=0.5*delx*FLOAT(img)

c --- Header
      write(io,'(a4)')'DSAA'
      write(io,'(2i12)')nex-nbx+1,ney-nby+1
      write(io,'(2f12.4)')xgkm(nbx,nby)-ds,xgkm(nex,nby)+ds
      write(io,'(2f12.4)')ygkm(nbx,nby)-ds,ygkm(nbx,ney)+ds
      write(io,'(2f12.4)')zmin,zmax

c --- Data records
      do j=nby,ney
         write(io,fmt)(ivar(i,j),i=nbx,nex)
      enddo

      close(io)

      return
      end

c----------------------------------------------------------------------
      subroutine vector(io,filenam,ws,wd,istep)
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481     Level: 070720                VECTOR
c               F. Robe, D. Strimaitis
c
c
c --- PURPOSE:  Write out a gridded file in a format compatible with
c               SURFER in order to create a vector plot (post map) of
c               the wind field
c               SURFER steps:
c                      - select POST in the MAP menu and create a 
c                        vector plot using the .dat file created by
c                        PRTMET
c --- UPDATE
c --- V4.45 (061215) to V4.48 (070720)  DGS
c               - Add user-supplied stepping index for VEC file output
c --- V4.2 (040330) to V4.45 (061215)  DGS
c               - Change x,y, format from f8.3 to f12.3
c --- V4.0 (020208) to V4.2 (040330)  DGS
c               - Replace filename strings c*70 with c*132
c
c --- INPUTS:
c             IO - integer                - output unit number
c        FILENAM - character*132          - name of output file
c             WS - real array (mxnx,mxny) - Wind speed (m/s)
c             WD - real array (mxnx,mxny) - Wind direction
c                                           (0: from the North)
c          ISTEP - integer                - output step index for
c                                           both y and z
c
c --- COMMON: /GRID/
c ---  XGKM    - real array(mxnx,mxny)  - X coord. of gridpoints (km)
c      YGKM    - real array(mxnx,mxny)  - Y coord. of gridpoints (km)
c      NBX     - integer                - X Index of the lower left
c                                         gridpoint of subdomain
c      NEX     - integer                - X Index of the lower right
c                                         gridpoint of subdomain
c      NBY     - integer                - Y Index of the lower left
c                                         gridpoint of subdomain
c      NEY     - integer                - Y Index of the upper left
c                                         gridpoint of subdomain
c
c --- OUTPUT: output file (unit=io)
c
c
c --- VECTOR called by:  COMP
c --- VECTOR calls:      none
c
c--------------------------------------------------------------------
      include 'params.pmt'
      include 'grid.pmt'

      real ws(mxnx,mxny),wd(mxnx,mxny)
      character*132 filenam

c --- Set the target number for the maximum number of vectors/line
      data mxvec/45/

c --- Thin out vectors if there are too many for a decent plot
      if(istep.EQ.0) then
         numx=1+nex-nbx
         nxstep=numx/mxvec + 1
         numy=1+ney-nby
         nystep=numy/mxvec + 1
      elseif(istep.GT.0) then
         nxstep=istep
         nystep=istep
      else
         write(io6,*)'ERROR in VECTOR:  invalid ISTEP'
         write(io6,*)'  Expected ISTEP 0 or >0'
         write(io6,*)'  Found    ISTEP = ',istep
         stop 'Halted in VECTOR --- see list file'
      endif

      open(io,file=filenam,status='REPLACE')
      write(io,*)'   x      y  arrow  angle(-wd)  length(ws)'
      do j=ney,nby,-nystep
         do i=nbx,nex,nxstep
            write(io,50)xgkm(i,j),ygkm(i,j),'symbol:175',
     %                  -wd(i,j),ws(i,j)
         enddo
      enddo

50    format (2(f12.3,1x),a11,2(2x,f8.2))
      close(io)

      return
      end

c----------------------------------------------------------------------
      subroutine methdx(img,lhdv,lmetcf,lsfc,lupc,lprc,lz0,iffzo,
     &                  llu,ifflu,lte,iffte,lli,iffli,lnears)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481      Level: 051012                  METHDX
c ---          J. Scire, D. Strimaitis   Earth Tech, Inc.
c
c --- PURPOSE:  Read header records of CALMET.DAT input file, including
c               the 'comment records' containing the CALMET control
c               file and echo back inputs to the list file.
c               Write domain plot files.
c
c --- UPDATES:
c     V4.4 (041208) to V4.41 (051012)  DGS
c               - Modify the LUSE.CLR file to change wetland color and
c                 add a red strip for LU=99.
c               - Add begin/end times to arg list for rdi1d,rdi2d,
c                 rdr1d,rdr2d
c               - Add ITIME flag to signal begin/end times (/HEADER/)
c               - Save start-time as time at START of first timestep
c               - Remove obsolete extended documentation code (LXTND)
c     V4.3 (040923) to V4.4 (041208)  FRR
c               - Read CALMET.DAT version 2.1) with new #NCOM+3 header 
c                 (explicit beginning/ending times, UTC time zone)
c                 and new record formats (call to new subroutines
c                  rdi1d,rdi2d,rdr1d,rdr2d)
c               - Can still read older CALMET.DAT files
c               - change messag length from 70 to 40 characters
c
c     V4.22 (040920) to V4.3 (040923)  DGS
c               - Add control file flag for Image GRD format
c               - Add IEOF arg to RDR2D call
c               - Pass extra variables to /HEADER/ for RDMET
c                 (nxm,nym,nzm,i2dmet)
c     V4.2 (040330) to V4.22 (040920)  DGS
c               - Expand format of QATERR.DAT plot-file to allow
c                 terrain heights below sea level
c     V4.14 (040312) to V4.2 (040330)  DGS
c               - Replace filename strings c*70 with c*132
c               - Add PGT.CLR file for PG class image plots
c
c     V4.13 (040216) to V4.14 (040312)  DGS
c               - Add flag for Image GRD file to GRDOUTI & GRDOUTR
c
c     V4.0 (020208) to V4.1 (030402) --- (DGS)
c            - Add echo of CALMET control file (new header format)
c
c     V3.1 (010731!a) to V4.0 (020208) --- (DGS)
c            - Moved grid variables from arg list to /GRID/
c            - Use logical variables from new control file
c            - Domain arrays used locally (removed from arg list)
c            - Condition processing range to comply with actual grid
c            - Add automatic plot file output for domain fields and
c              station locations
c
c --- INPUTS:
c               IMG - integer    - Flag for GRD IMAGE format
c              LHDV - logical    - Flag to output CALMET parameters
c            LMETCF - logical    - Flag to output CALMET control file
c              LSFC - logical    - Flag for surface station coords
c              LUPC - logical    - Flag for upper-air station coords
c              LPRC - logical    - Flag for precip station coords
c               LZ0 - logical    - Flag for Surface Roughness grid
c             IFFZO - integer    - Format (0:self-scaling, 1:fixed)
c               LLU - logical    - Flag for Land Use grid
c             IFFLU - integer    - Format (0:self-scaling, 1:fixed)
c               LTE - logical    - Flag for Terrain grid
c             IFFTE - integer    - Format (0:self-scaling, 1:fixed)
c               LLI - logical    - Flag for Leaf Area Index grid
c             IFFLI - integer    - Format (0:self-scaling, 1:fixed)
c            LNEARS - logical    - Flag for Nearest Met Station grid
c
c      common /GRID/
c             nbx,nby,nex,ney
c
c      Parameters: IO6, IO7, IO0, MXNX, MXNY, MXNZ, MXNZP1,
c                  MXSS, MXUS, MXPS
c
c --- OUTPUT:  (header information used by calling program)
c
c      common /HEADER/
c             irlg,irtype,nssta,npsta,nusta,lcalgrd,ifilver,
c             nxm,nym,nzm,i2dmet,dataver
c      common /GRID/
c             nbx,nby,nex,ney,nx,ny,nz,xorg,yorg,delx,zface(mxnzp1)
c             xgkm(mxnx,mxny),ygkm(mxnx,mxny)
c
c      Output Files
c             QATERR.GRD - gridded terrain elevations (mMSL)
c             QALUSE.GRD - gridded land use
c             QALAI.GRD  - gridded leaf area index
c             QAZ0.GRD   - gridded roughness length (m)
c             QAMETG.BNA - borders for the METEOROLOGICAL grid domain
c             QASSTA.DAT - location of surface met stations
c             QAPSTA.DAT - location of precipitation stations
c             QAUSTA.DAT - location of upper-air stations
c             LUSE.CLR   - color scale for land use
c             PGT.CLR    - color scale for PG class
c
c --- METHDX called by:  main
c --- METHDX calls:      RDR1D, RDI1D, RDR2D, RDI2D,
c                        OUT, OUTFX, OUTIFX, GRDOUTR, GRDOUTI
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmt'
c
c --- Include commons
      include 'grid.pmt'
      include 'filnam.pmt'
      include 'header.pmt'

c --- Local Parameters
c --------------------
c ---   MXBAR   - Maximum number of barriers allowed
c ---   MXBOX   - Maximum number of seabreeze regions allowed
c ---   MXOWS   - Maximum number of overwater stations
c ---   MXWB    - Maximum number of water bodies that will be treated
c ---             separately in the temperature interpolation
c ---             (currently must be 1!)
      parameter(mxbar=200,mxbox=50,mxows=50,mxwb=1)
      parameter(mxwnd=mxss+mxows+mxus)

      real xbuf(mxnx,mxny)
      real elev(mxnx,mxny),z0(mxnx,mxny)
      real xssta(mxss),yssta(mxss), xusta(mxus),yusta(mxus)
      real xpsta(mxps),ypsta(mxps),xlai(mxnx,mxny)
c
      integer ilandu(mxnx,mxny),nears(mxnx,mxny)
c
      character*80 title(3)
      character*8 vermet,levmet
      character*8 clabel,clabxs,clabys,clabxu,clabyu,clabxp,clabyp
      character*8 clabz0,clablu,clabte,clablai,clabnss
      character*40 messag
      character*40 fmt
      character*8 axtz
c
      logical*4 ldate
      logical LLCONF
      logical lhdv,lmetcf,lsfc,lupc,lprc,lz0,llu,lte,lli,lnears

c --- Define variables and arrays for reading extended met.
c ---------------------------------------------------------
      logical lsave,lvary,llbreze
      character*4 csnam(mxss),cunam(mxus),cpnam(mxps),cownam(mxows)

      integer nintr2(mxnz),nsmth(mxnz),nlb(mxwnd),iboxid(mxbox,mxwnd)
      integer idssta(mxss),idusta(mxus),idpsta(mxps),idowsta(mxows)
      integer jwat1(mxwb),jwat2(mxwb)

      real zupwnd(2),bias(mxnz),fextr2(mxnz),xybar(4,mxbar)
      real xg1(mxbox),xg2(mxbox),yg1(mxbox),yg2(mxbox),xbcst(mxbox)
      real ybcst(mxbox),xecst(mxbox),yecst(mxbox)
      real xstz(mxss),zanem(mxss),xutz(mxus)


c --- Local Variables
c      character*16 dataset,dataver
c --- dataver defined in HEADER.PMT
      character*16 dataset
      character*33 blank33
      character*64 datamod
      character*80 doc1
      character*132 comment1,blank

      data ldate/.FALSE./
      data blank33/'                                 '/

c --- Set blank (132 characters)
      blank(1:33)=blank33
      blank(34:66)=blank33
      blank(67:99)=blank33
      blank(100:132)=blank33

c --- Initialize begin/end time flag
c ---    0: end-time (no seconds)
c ---    1: begin-time / end-time
      itime=1

c --- Read and test first record to determine header format
c --- Record #1 - File Declaration -- 24 words
      read(io7) dataset,dataver,datamod
      ifilver=0
      if(dataset.EQ.'CALMET.DAT') ifilver=1
      REWIND(io7)

c --- Read records
c ----------------
c
      if(ifilver.EQ.1) then
c
c ---   Record #1 - File Declaration -- 24 words
        read(io7) dataset,dataver,datamod
c
c ---   Record #2 - Number of comment lines -- 1 word
        read(io7) ncom
c ---   Loop over comment records (3 to NCOM+2)
        if(LMETCF) then
          write(io6,*)
          write(io6,*)'CALMET Control file information:'
          write(io6,*)
        endif
        do i=1,ncom
          comment1=blank
          read(io7) comment1
          if(i.EQ.1) then
c ---       Save model version line
            doc1=comment1(1:80)
          elseif(i.LE.4) then
c ---       Save 3 title lines
            title(i-1)=comment1(1:80)
          endif
          if(LMETCF) write(io6,*) comment1
        enddo
c
c ---   record #NCOM+3 - run control parameters --
        if (dataver.eq.'2.1') then
c ---      CALMET.DAT with explicit beg./ending times, seconds, and
c ---      UTC timezone as string
           read(io7) jbyr,jbmo,jbdy,jbhr,jbsec,jeyr,jemo,jedy,
     1     jehr,jesec,axtz,irlg,irtype,
     1     nx, ny, nz, dgrid, xorigr, yorigr, iwfcod, nssta,
     2     nusta, npsta, nowsta, nlu, iwat1, iwat2, lcalgrd,
     3     pmap,datum,daten,feast,fnorth,utmhem,iutmzn,
     4     rnlat0,relon0,xlat1,xlat2
c ---      Convert UTC time zone to IBTZ time zone
           call utcbasr(axtz,xbtz)
           jbtz=int(xbtz)
        else
c ---      CALMET with hour-ending times (1-hour timestep)
           itime=0
           read(io7) jbyr,jbmo,jbdy,jbhr,jbtz,irlg,irtype,
     1     nx, ny, nz, dgrid, xorigr, yorigr, iwfcod, nssta,
     2     nusta, npsta, nowsta, nlu, iwat1, iwat2, lcalgrd,
     3     pmap,datum,daten,feast,fnorth,utmhem,iutmzn,
     4     rnlat0,relon0,xlat1,xlat2
        endif
      endif
c
      if(ifilver.EQ.0) then
c
c ---   Read older CALMET header records
c
c ---   CALMET with hour-ending times
        itime=0
c
c ---   record #1 - run title -- 60 words
        read(io7)title
c
c ---   record #2 - run control parameters -- 26 words
c ---   (vermet, levmet are both 8 bytes)
        read(io7)vermet,levmet,jbyr,jbmo,jbdy,jbhr,jbtz,irlg,irtype,
     1   nx, ny, nz, dgrid, xorigr, yorigr, iutmzn, iwfcod, nssta,
     2   nusta, npsta, nowsta, nlu, iwat1, iwat2, lcalgrd
c
c ---   New record -- #3 - additional run control data -- 8 words
c ---   This record was introduced in CALMET Version 5.0 (980304)
        read(levmet(1:6),'(i6)') ilevmet
        read(vermet(1:6),'(f6.0)') rvermet
        if(ilevmet.GE.980304 .OR. rvermet.GT.5.1) then
c ---      New header record format
           read(io7)xlat0,xlon0,llconf,conec,xlat1,xlat2,
     &              rlat0,rlon0
        endif
c
c ---   Recast map projection information
        if(LLCONF) then
           pmap='LCC     '
        else
           pmap='UTM     '
        endif

      endif

      if(itime.EQ.0) then
c ---    Convert hour-ending time at the start to time at the
c ---    start of the first hour
         call julday(io6,jbyr,jbmo,jbdy,jbjul)
         nhrinc=-1
         call incr(io6,jbyr,jbjul,jbhr,nhrinc)
         call grday(io6,jbyr,jbjul,jbmo,jbdy)
         jbsec=0
      endif

c --- Check that array dimensions have been sized properly
      if(nx.gt.mxnx.or.ny.gt.mxny.or.nz.gt.mxnz.or.
     1 nssta.gt.mxss.or.nusta.gt.mxus.or.npsta.gt.mxps)then
      write(io6,2354)nx,mxnx,ny,mxny,nz,mxnz,nssta,mxss,
     1    nusta,mxus,npsta,mxps
2354     format(/1x,'ERROR -- Array dimensions are too small for ',
     1   'the data in the CALMET.DAT file'/
     2   5x,'NX    = ',i5,4x,'MXNX = ',i5/
     3   5x,'NY    = ',i5,4x,'MXNY = ',i5/
     4   5x,'NZ    = ',i5,4x,'MXNZ = ',i5/
     5   5x,'NSSTA = ',i5,4x,'MXSS = ',i5/
     6   5x,'NUSTA = ',i5,4x,'MXUS = ',i5/
     7   5x,'NPSTA = ',i5,4x,'MXPS = ',i5)
      stop
      endif
c
      nzp1=nz+1
c
c --- record #4 - cell face heights (NZ + 1 words)
      call rdr1d(io7,itime,zface,nzp1,clabel,idum,idum,idum,idum)
c
c --- records #5 & 6 - x, y coordinates of surface stations
c --- (NSSTA words each record)
      if(nssta.gt.0)then
        call rdr1d(io7,itime,xssta,nssta,clabxs,idum,idum,idum,idum)
        call rdr1d(io7,itime,yssta,nssta,clabys,idum,idum,idum,idum)
      endif
c
c --- records #7 & 8 - x, y coordinates of upper air stations
c --- (NUSTA words each record)
      if(nusta.gt.0)then
         call rdr1d(io7,itime,xusta,nusta,clabxu,idum,idum,idum,idum)        
         call rdr1d(io7,itime,yusta,nusta,clabyu,idum,idum,idum,idum)
      endif
c
c --- records #9 & 10 - x, y coordinates of precipitation stations
c --- (NPSTA words each record)
      if(npsta.gt.0)then
         call rdr1d(io7,itime,xpsta,npsta,clabxp,idum,idum,idum,idum)
         call rdr1d(io7,itime,ypsta,npsta,clabyp,idum,idum,idum,idum)
      endif

c --- record #11 - surface roughness lengths (NX * NY words)
      call rdr2d(io7,itime,z0,xbuf,mxnx,mxny,nx,ny,clabz0,
     &           idum,idum,idum,idum,ieof)
c
c --- record #12 - land use categories (NX * NY words)
      call rdi2d(io7,itime,ilandu,xbuf,mxnx,mxny,nx,ny,clablu,
     &           idum,idum,idum,idum)
c
c --- record #13 - elevations (NX * NY words)
      call rdr2d(io7,itime,elev,xbuf,mxnx,mxny,nx,ny,clabte,
     &           idum,idum,idum,idum,ieof)
c
c --- record #14 - leaf area index (NX * NY words)
      call rdr2d(io7,itime,xlai,xbuf,mxnx,mxny,nx,ny,clablai,
     &           idum,idum,idum,idum,ieof)
c
c --- record #15 - nearest surface station to each grid point
c               (NX * NY words)
      if(nssta.ge.1)then
         call rdi2d(io7,itime,nears,xbuf,mxnx,mxny,nx,ny,clabnss,
     &              idum,idum,idum,idum)
      endif


c --- Perform a few operations using data from CALMET header
c ----------------------------------------------------------

c --- Reset the grid range now that the full size is known
c --- Check for a zero index
      ntest=nbx*nby*nex*ney
      if(ntest.EQ.0) then
c ---    'Use full grid' option is intended
         nbx=1
         nby=1
         nex=nx
         ney=ny
      else
c ---    Condition output grid range
         if(nbx.LT.1) nbx=1
         if(nby.LT.1) nby=1
         if(nex.LT.1) nex=1
         if(ney.LT.1) ney=1
         nbx=MIN(nbx,nx)
         nby=MIN(nby,ny)
         nex=MIN(nex,nx)
         ney=MIN(ney,ny)
      endif

c --- Set /grid/ information in km
      xorg=xorigr*.001
      yorg=yorigr*.001
      delx=dgrid*.001

c --- Set X-Y coordinates of the gridpoints (km)
c --- (store in /grid/)
      do i=1,nx
         do j=1,ny
           xgkm(i,j)=xorg+(i-0.5)*delx
           ygkm(i,j)=yorg+(j-0.5)*delx
         enddo
      enddo

c --- Set Upper Right met grid corner (km)
c --- (Lower Left is xorg,yorg)
      xUR=xorg+FLOAT(nx)*delx
      yUR=yorg+FLOAT(ny)*delx


c --- Create plot files (automatic!) of domain fields
c --- and station locations
c ---------------------------------------------------

c --- Terrain
      fmt='(10000(1pe11.4,1x))'
      call GRDOUTR(io9,tegrd,elev,fmt,0)

c --- Land Use
      fmt='(10000(i3,2x))'
      call GRDOUTI(io9,lugrd,ilandu,fmt,10.,100.,img)

c --- Surface Roughness
      fmt='(10000(1pe10.4,2x))'
      call GRDOUTR(io9,z0grd,z0,fmt,0)

c --- Leaf Area Index
      fmt='(10000(1pe10.4,2x))'
      call GRDOUTR(io9,ligrd,xlai,fmt,0)

c --- Set up default land use color file in this directory
      open(io9,file=luclr,status='REPLACE')
      write(io9,'(a)') 'ColorMap 1 1'
      write(io9,'(a)') '    0.000000 255 255   0'
      write(io9,'(a)') '   11.000000 255 255   0'
      write(io9,'(a)') '   11.000000 204 255 102'
      write(io9,'(a)') '   22.100000 204 255 102'
      write(io9,'(a)') '   22.100000 160 255 160'
      write(io9,'(a)') '   33.100000 160 255 160'
      write(io9,'(a)') '   33.100000  80 255  80'
      write(io9,'(a)') '   44.200000  80 255  80'
      write(io9,'(a)') '   44.200000 153 255 255'
      write(io9,'(a)') '   50.000000 153 255 255'
      write(io9,'(a)') '   50.000000  60 204 255'
      write(io9,'(a)') '   55.400000  60 204 255'
      write(io9,'(a)') '   55.400000 204 153 204'
      write(io9,'(a)') '   66.500000 204 153 204'
      write(io9,'(a)') '   66.500000 255 204 153'
      write(io9,'(a)') '   77.600000 255 204 153'
      write(io9,'(a)') '   77.600000 255 255 204'
      write(io9,'(a)') '   88.700000 255 255 204'
      write(io9,'(a)') '   88.700000 255 255 255'
      write(io9,'(a)') '   99.029557 255 255 255'
      write(io9,'(a)') '   99.029557 255   0   0'
      write(io9,'(a)') '  100.000000 255   0   0'
      close(io9)

c --- Set up default PGT color file in this directory
      open(io9,file=pgtclr,status='REPLACE')
      write(io9,'(a)') 'ColorMap 1 1'
      write(io9,'(a)') '    0.000000 255 255   0'
      write(io9,'(a)') '   17.000000 255 255   0'
      write(io9,'(a)') '   17.000000 255 255 204'
      write(io9,'(a)') '   34.000000 255 255 204'
      write(io9,'(a)') '   34.000000 204 255 102'
      write(io9,'(a)') '   51.000000 204 255 102'
      write(io9,'(a)') '   51.000000  80 255  80'
      write(io9,'(a)') '   67.000000  80 255  80'
      write(io9,'(a)') '   67.000000 153 255 255'
      write(io9,'(a)') '   84.000000 153 255 255'
      write(io9,'(a)') '   84.000000  60 204 255'
      write(io9,'(a)') '  100.000000  60 204 255'
      close(io9)

c --- Met grid boundary [BNA format]
      open(io9,file=gridbna,status='REPLACE')
      write(io9,'(a15)') '" Met","grid",5'
      write(io9,'(f12.4,a1,f12.4)') xorg,',',yorg
      write(io9,'(f12.4,a1,f12.4)') xorg,',',yUR
      write(io9,'(f12.4,a1,f12.4)') xUR, ',',yUR
      write(io9,'(f12.4,a1,f12.4)') xUR, ',',yorg
      write(io9,'(f12.4,a1,f12.4)') xorg,',',yorg
      close(io9)

c --- Surface station locations (km)
      open(io9,file=sstadat,status='REPLACE')
      write(io9,'(a27)') '"Xkm" "Ykm" "Sfc Met"'
      do i=1,nssta
         x=xorg+xssta(i)*.001
         y=yorg+yssta(i)*.001
         write(io9,*) x,y,i
      enddo
      if(nssta.GT.0) then
         close(io9)
      else
         close(io9, status='DELETE')
      endif

c --- Upper-air station locations
      open(io9,file=ustadat,status='REPLACE')
      write(io9,'(a27)') '"Xkm" "Ykm" "Up Met"'
      do i=1,nusta
         x=xorg+xusta(i)*.001
         y=yorg+yusta(i)*.001
         write(io9,*) x,y,i
      enddo
      if(nusta.GT.0) then
         close(io9)
      else
         close(io9, status='DELETE')
      endif

c --- Precipitation station locations
      open(io9,file=pstadat,status='REPLACE')
      write(io9,'(a27)') '"Xkm" "Ykm" "Precip"'
      do i=1,npsta
         x=xorg+xpsta(i)*.001
         y=yorg+ypsta(i)*.001
         write(io9,*) x,y,i
      enddo
      if(npsta.GT.0) then
         close(io9)
      else
         close(io9, status='DELETE')
      endif


c --- Report selected header information to list file
c ---------------------------------------------------
      
c --- Condition print-range
      nx1=MAX(1,nbx)
      nx2=MIN(nx,nex)
      ny1=MAX(1,nby)
      ny2=MIN(ny,ney)

c --- If only a single point is reported to list file, set special flags & variables
      if(nx1.eq.nx2.and.ny1.eq.ny2)then
         isp=1
         nspx=nx1
         nspy=ny1
      else
         isp=0
         nspx=0
         nspy=0
      endif

      write(io6,217)
217   format(//1x,13('----------')/1x,13('----------')//)
c
c --- Write run parameters from CALMET header records
      if(lhdv)then
      write(io6,*)' Data read from header records of CALMET file: '
      write(io6,'(1x,a132)')metdat 
      write(io6,*)
      if(ifilver.EQ.1) then
         write(io6,'(2a16,a64)')dataset,dataver,datamod
         write(io6,*)
      endif
      write(io6,102)title
102      format(3(1x,a80/))
      if(ifilver.EQ.0) then
         write(io6,104)vermet,levmet
104      format(1x,'CALMET Version: ',a8,3x,'Level: ',a8)
      else
         write(io6,'(a80)') doc1
      endif
      write(io6,*)
      write(io6,*)' Input Group #0 parameters ---'
      write(io6,*)' NUSTA   = ',nusta
      write(io6,*)' NOWSTA  = ',nowsta
      write(io6,*)
      if(dataver.eq.'2.1') then
         write(io6,*)' Input Group #1 parameters ---'
         write(io6,*)' IBYR    = ',jbyr
         write(io6,*)' IBMO    = ',jbmo
         write(io6,*)' IBDY    = ',jbdy
         write(io6,*)' IBHR    = ',jbhr
         write(io6,*)' IBSEC   = ',jbsec
         write(io6,*)' IEYR    = ',jeyr
         write(io6,*)' IEMO    = ',jemo
         write(io6,*)' IEDY    = ',jedy
         write(io6,*)' IEHR    = ',jehr
         write(io6,*)' IESEC   = ',jesec
         write(io6,*)' AXTZ    = ',axtz
         write(io6,*)' IBTZ    = ',jbtz
         write(io6,*)' IRLG    = ',irlg
      else
         write(io6,*)' Input Group #1 parameters ---'
         write(io6,*)' IBYR    = ',jbyr
         write(io6,*)' IBMO    = ',jbmo
         write(io6,*)' IBDY    = ',jbdy
         write(io6,*)' IBHR    = ',jbhr
         write(io6,*)' IBSEC   = ',jbsec
         write(io6,*)' IBTZ    = ',jbtz
         write(io6,*)' IRLG    = ',irlg
      endif

      write(io6,*)' IRTYPE  = ',irtype
      write(io6,*)' LCALGRD = ',lcalgrd
      write(io6,*)
      write(io6,*)' Input Group #2 parameters ---'
      write(io6,*)' PMAP    = ',pmap
      write(io6,*)' DATUM   = ',datum
      write(io6,*)' NIMADATE= ',daten
      write(io6,*)' FEAST   = ',feast
      write(io6,*)' FNORTH  = ',fnorth
      if(PMAP.EQ.'UTM     ') then
         write(io6,*)' IUTMZN  = ',iutmzn
         write(io6,*)' UTMHEM  = ',utmhem
      else
         write(io6,*)' XLAT1   = ',xlat1
         write(io6,*)' XLAT2   = ',xlat2
         write(io6,*)' RNLAT0  = ',rnlat0
         write(io6,*)' RELON0  = ',relon0
      endif
      write(io6,*)' NX      = ',nx
      write(io6,*)' NY      = ',ny
      write(io6,*)' DGRID   = ',dgrid
      write(io6,*)' XORIGR  = ',xorigr
      write(io6,*)' YORIGR  = ',yorigr
      write(io6,*)' NZ      = ',nz
      write(io6,203)(zface(n),n=1,nzp1)
203   format(1x,'ZFACE  = ',10(f9.3,', '))
      write(io6,*)
      write(io6,*)' Land Use parameters from GEO.DAT ---'
      write(io6,*)' NLU     = ',nlu
      write(io6,*)' IWAT1   = ',iwat1
      write(io6,*)' IWAT2   = ',iwat2
      write(io6,*)
      write(io6,*)' Input Group #4 parameters ---'
      write(io6,*)' NSSTA   = ',nssta
      write(io6,*)' NPSTA   = ',npsta
      write(io6,*)
      write(io6,*)' Input Group #5 parameters ---'
      write(io6,*)' IWFCOD  = ',iwfcod

c --- End lhdv output
      endif
c
c --- Write list of station data requested
c --- Formats used
301   format(2f12.2)
302   format(a8,i8,f8.1,f10.3,2f12.2)
303   format(a8,i8)
304   format(a8,i8,f8.1,2f12.2)
305   format(a8,i8,2f12.2)
c
c --- Surface stations
      if(lsfc)then
      write(io6,*)
      if(nssta.GT.0) then
         write(io6,*)'Surface Met Stations ---'
         write(io6,*)'GRID X&Y(m) :'
         do i=1,nssta
            write(io6,301)xssta(i),yssta(i)
         enddo
      else
         write(io6,*)'NO SURFACE STATIONS'
      endif

c --- End lsfc output
      endif
c
c --- Upper air stations
      if(lupc)then
      write(io6,*)
      if(nusta.GT.0) then
         write(io6,*)'Upper-Air Met Stations ---'
         write(io6,*)'GRID X&Y(m) :'
         do i=1,nusta
            write(io6,301)xusta(i),yusta(i)
         enddo
      else
         write(io6,*)'NO UPPER AIR STATIONS'
      endif
c --- End lupc output
      endif
c
c --- Precipitation stations
      if(lprc)then
      write(io6,*)
      if(npsta.GT.0) then
         write(io6,*)'Precipitation Stations ---'
         write(io6,*)'GRID X&Y(m) :'
         do i=1,npsta
            write(io6,301)xpsta(i),ypsta(i)
         enddo
      else
         write(io6,*)'NO PRECIPITATION STATIONS'
      endif
c --- End lprc output
      endif

c
c --- Write gridded header data requested
c
c --- Nearest surface station number to each grid point
      if(lnears .AND. isp.EQ.0)then
         messag='Nearest sf. station to gridpoint'
         messag(33:40)=clabnss
         call outifx(nears,4,ldate,messag,nx1,ny1,nx2,ny2)
      endif
c
c --- record #10 - surface roughness lengths (NX * NY words)
      if(lz0 .AND. isp.EQ.0)then
         messag='Surface roughness lengths (m)'
         messag(33:40)=clabz0
         if(iffzo.eq.1)then
            call outfx(z0,3,ldate,messag,nx1,ny1,nx2,ny2)
         else
            call out(z0,idum,1,5,ldate,messag,nx1,ny1,nx2,ny2)
         endif
      endif
c
c --- record #11 - land use categories (NX * NY words)
      if(llu .AND. isp.EQ.0)then
         messag='Land use categories'
         messag(33:40)=clablu
         if(ifflu.eq.1)then
            call outifx(ilandu,4,ldate,messag,nx1,ny1,nx2,ny2)
         else
            call out(xdum,ilandu,2,5,ldate,messag,nx1,ny1,nx2,ny2)
         endif
      endif
c
c --- record #12 - elevations (NX * NY words)
      if(lte .AND. isp.EQ.0)then
         messag='Terrain heights (m)'
         messag(33:40)=clabte
         if(iffte.eq.1)then
            call outfx(elev,0,ldate,messag,nx1,ny1,nx2,ny2)
         else
            call out(elev,idum,1,5,ldate,messag,nx1,ny1,nx2,ny2)
         endif
      endif
c
c --- record #13 - leaf area index (NX * NY words)
      if(lli .AND. isp.EQ.0)then
         messag='Leaf area index'
         messag(33:40)=clablai
         if(iffli.eq.1)then
            call outfx(xlai,2,ldate,messag,nx1,ny1,nx2,ny2)
         else
            call out(xlai,idum,1,5,ldate,messag,nx1,ny1,nx2,ny2)
         endif
      endif
c
c --- Print header for single point table.
      if(isp.eq.1) then
         write(io6,217)
         write(io6,206)
206      format(//,' ** -- TABLE FOR ONE POINT -- **',/)
         write(io6,*)
         write(io6,200)nspx,nspy
200      format(1x,'The point selected is (',i3,',',i3,')'/)
         write(io6,*)
         write(io6,207)z0(nspx,nspy)
207      format(1x,'Surface Roughness Length  (m)  = ',f10.5)
         write(io6,208)ilandu(nspx,nspy)
208      format(1x,'Land Use Category              = ',7x,i3)
         write(io6,209)elev(nspx,nspy)
209      format(1x,'Terrain Elevation (m)          = ',f10.2)
         write(io6,210)xlai(nspx,nspy)
210      format(1x,'Leaf area index                = ',f10.5)
         write(io6,212)nears(nspx,nspy)
212      format(1x,'Nearest surface sta. to I,J    = ',7x,i3)
      endif

      write(io6,217)

c --- Add variables to /HEADER/ that are used by RDMET subroutine
c --- from CALPUFF
      nxm=nx
      nym=ny
      nzm=nz
      i2dmet=ifilver

      return
      end

c----------------------------------------------------------------------
      subroutine rdhd53(lprt)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481      Level: 070719                 RDHD53
c
c --- ADAPTED FROM:
c --- CALMET   Version: 5.546     Level: 031126                  RDHD53
c              J. Scire, Earth Tech
c              Adapted from RDHD52
c
c --- PURPOSE: Read the header records from a file in 3D.DAT format 
c
c --- UPDATES:
c --- V4.42 Level 051118 to V4.47 Level 070719
c         - Replace polar stereographic map projection code PST
c           with PS
c --- v4.41 Level 040923 to V4.42 Level 051118
c         - Use a free-format read for the number of comment records
c           in the 3D.DAT file 
c
c --- INPUTS:
c            LPRT - logical    - Flag to output MET file information
c
c        Parameters:
c            MXNXP, MXNYP, MXNZP, IO6, IO7
c
c --- OUTPUT:
c
c        Common block /MET3D/ variables:
c            IBYRM,IBJULM,IBHRM,IEYRM,IEJULM,IEHRM,NHRSM3D,
c            NXMET,NYMET,NZMET,DXMET,X1MET,Y1MET,PTOPMET,
c            I13D,J13D,NXP,NYP,NZP,SIGMA(mxnzp),
c            XNLAT3D(mxnxp,mxnyp),XELON3D(mxnxp,mxnyp),
c            ITE3D(mxnxp,mxnyp),
c            feast3D,fnorth3D,iutmzn3D,
c            rnlat03D,relon03D,xlat13D,xlat23D,
c            IOUTM3D,IMET3D,ISTEPPG,
c            NCOMM3D,CNAME3D,CVER3D,CTITLE3D,COMM3D,
c            MAP3D,utmhem3D
c
c --- RDHD53 called by:  METHD3D
c --- RDHD53 calls:      JULDAY, INCR, YR4
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmt'
c
      character*132 ctemp3d
      logical lprt
c
c --- Common blocks
      include 'met3d.pmt'

c --- Set map projection variables that are not in 3D.DAT file
      utmhem3d='N'
      iutmzn3D=0
      datum3d='NWS-84'
      daten3d='02-21-2003  '
      feast3D=0.0
      fnorth3D=0.0

c --- Two versions of 3D.DAT differ only in the top of the header

      if(imet3d.EQ.1) then
c ------------------------------------------------------------------
c ---    Read header record #1 (title)
c ------------------------------------------------------------------
         read(io7,'(a80)') comm3d
         ncomm3d=1
c ------------------------------------------------------------------
c ---    Read header record #2 (Data set name, version and level)
c ------------------------------------------------------------------
         read(io7,'(3a12)')cname3d,cver3d,ctitle3d
         write(io6,101)cname3d,cver3d,ctitle3d
101      format(//1x,'Information read from 3D.DAT file'/
     1   5x,'Dataset Name:    ',a16/
     2   5x,'Dataset Version: ',a16/
     3   5x,'Dataset Title:   ',a64/)
         if(LPRT) write(io6,*) comm3d

      elseif(imet3d.EQ.2) then
c ------------------------------------------------------------------
c ---    Read header record #1 (Dataset name, version and title)
c ------------------------------------------------------------------
         read(io7,10)cname3d,cver3d,ctitle3d
10       format(2a16,a64)
         write(io6,101)cname3d,cver3d,ctitle3d

c ------------------------------------------------------------------
c ---    Read header record #2 (Number of comment lines)
c ------------------------------------------------------------------
c         read(io7,11)ncomm3d
         read(io7,*)ncomm3d
11       format(i4)

c ------------------------------------------------------------------
c ---    Next "NCOMM3d" lines (comment lines)
c ------------------------------------------------------------------
         if(ncomm3d.gt.0)then
c ---       Loop over comment records
            if(LPRT) then
              write(io6,*)
              write(io6,*)'3D.DAT file comment records:'
              write(io6,*)
            endif
            do i=1,ncomm3d
               read(io7,12)ctemp3d
12             format(a132)
c ---          Save first line of text for later printing
               if(i.eq.1)comm3d=ctemp3d
               if(LPRT) write(io6,*) ctemp3d
            enddo
         endif

      endif
c --- Rest of 3D.DAT header is the same in both versions

c -------------------------------------------
c --- Next header record (MM5 output options)
c -------------------------------------------
      read(io7,43)ioutw,ioutq,ioutc,iouti,ioutg 
      ioutm3d=81+10*ioutw+ioutq+ioutc+iouti+ioutg
43    format(5(i3))

c --------------------------------------------
c --- Next header record (map projection)
c --------------------------------------------
      read(io7,45) map3d,rnlat03d,relon03d,xlat13d,xlat23d,
     &             x1met,y1met,dxmet,nxmet,nymet,nzmet
45    format(a4,f9.4,f10.4,2f7.2,2f10.3,f8.3,2i4,i3)

c --- Polar stereographic projection is called PST in 3D.DAT,
c --- and is called PS in all coordinate codes.
      if(map3d.EQ.'PST     ') map3d='PS      '

c ------------------------------------------------
c --- Next header record (M3D output options)
c     (Add 13 output options for surface variables)
c ------------------------------------------------
      read(io7,44) inhyd,imphys,icupa,ibltyp,ifrad,isoil
     :         ,ifddaan,ifddaob
     :         ,igrdt,ipbl,ishf,ilhf,iustr,iswdn
     :         ,ilwdn,ist1,ist2,ist3,ist4,ist5,ist6
44    format(30(i3))
c --------------------------------------
c --- Next header record (M3D grid data)
c --------------------------------------
      read(io7,20)ibyrm,ibmom,ibdym,ibhrm,nhrsm3d,
     1             nxp,nyp,nzp
 20   format(i4,3i2,i5,3i4)
      call YR4(io6,ibyrm,ierrb)
      if(ierrb.NE.0)then
         write(io6,*)'Execution stopping in Subr. RDHD53 ',
     1   '-- IERRB = ',ierrb
         stop
      endif
c
c --- Calculate Julian day
      call julday(io6,ibyrm,ibmom,ibdym,ibjulm)
c
c --- Compute ending date/time (comment out if using other format)
      ieyrm=ibyrm
      iejulm=ibjulm
      iehrm=ibhrm
      call incr(io6,ieyrm,iejulm,iehrm,nhrsm3d)
c
c --------------------------------------------------
c --- Next header record (extraction subdomain data)
c --------------------------------------------------
      read(io7,30)nx1,ny1,nx2,ny2,nz1,nz2,
     &             rxmin,rxmax,rymin,rymax
      i13d=nx1
      j13d=ny1
30    format(6i4,2f10.4,2f9.4)
c

c --- Check that array dimensions are not exceeded
      if(nxp.gt.mxnxp.or.nyp.gt.mxnyp.or.nzp.gt.mxnzp)then
         write(io6,*)'ERROR in subr. RDHD53 -- Array dimensions ',
     1   'are too small for data being read'
         write(io6,*)'Grid being read  (NXP, NYP, NZP) = ',
     1    nxp,nyp,nzp
         write(io6,*)'Array dimensions (MXNXP, MXNYP, MXNZP) = ',
     1    mxnxp,mxnyp,mxnzp
         write(*,*)'FATAL Error in RDHD53 -- Array dimensions ',
     1   'are too small for data being read (see LIST file)'
         stop
      endif
c --- Check consistency between nz1,nz2, and nzp
      if(nzp.ne.nz2-nz1+1) then
        write(io6,*)'Error in RDHD53: NZ1,NZ2 and NZP not consistent'
        write(io6,*)'nz1,nz2,nzp:',nz1,nz2,nzp
        write(*,*)'FATAL Error in RDHD53 -- NZ1,NZ2 and NZP are ',
     1  'not consistent (see LIST file)'
        stop
      endif
c
c ---------------------------------------------
c --- Next NZP records -- MM5 half-sigma levels
c ---------------------------------------------
      do 40 n=1,nzp
         read(io7,38)sigma(n)
38       format(f6.3)
40    continue
c
c --------------------------------------------------------
c --- Print the 3D.DAT header information to the list file
c --- (except for the gridded fields)
c --------------------------------------------------------
      if(lprt)then
         write(io6,102) inhyd,imphys,icupa,ibltyp,ifrad,isoil,
     :               ifddaan,ifddaob
102      format(/5x,'MM5 physics options: '/
     1           5x,'    Hydrostatic:           ',i2/
     1           5x,'    Moisture scheme:       ',i2/
     1           5x,'    Convection scheme:     ',i2/
     1           5x,'    Boundary layer scheme: ',i2/
     1           5x,'    Radiation scheme       ',i2/
     1           5x,'    Soil scheme:           ',i2/
     1           5x,'    Analysis FDDA:         ',i2/
     1           5x,'    Observation FDDA:      ',i2)


         write(io6,1021)igrdt,ipbl,ishf,ilhf,iustr,iswdn
     :         ,ilwdn,ist1,ist2,ist3,ist4,ist5,ist6
1021      format(/5x,'MM5 surface variable options: '/
     1           5x,'    Ground temperature:    ',i2/
     1           5x,'    PBL:                   ',i2/
     1           5x,'    Sensible heat flux:    ',i2/
     1           5x,'    Latent heat flux:      ',i2/
     1           5x,'    Frictional velocity:   ',i2/
     1           5x,'    Downward SW radiation: ',i2/
     1           5x,'    Downward LW radiation: ',i2/
     1           5x,'    Soil temp at layer 1:  ',i2/
     1           5x,'    Soil temp at layer 2:  ',i2/
     1           5x,'    Soil temp at layer 3:  ',i2/
     1           5x,'    Soil temp at layer 4:  ',i2/
     1           5x,'    Soil temp at layer 5:  ',i2/
     1           5x,'    Soil temp at layer 6:  ',i2)

         write(io6,103)1,ioutw,ioutq,ioutc,iouti,ioutg
103      format(/5x,'MM5 output fields (1 = YES; 0 = NO): '/
     1           5x,' Pressure, height,T,Wind speed and direction: ',i2/
     1           5x,'                           Vertical velocity: ',i2/
     1           5x,'                   RH and vapor mixing ratio: ',i2/
     1           5x,'                Cloud and rain mixing ratios: ',i2/
     1           5x,'                  Ice and Snow mixing ratios: ',i2/
     1           5x,'                                     Graupel: ',i2)

         write(io6,104)ibyrm,ibmom,ibdym,ibhrm,nhrsm3d,
     1                 nxp,nyp,nzp
104      format(/5x,'Date/time (YYYYMMDDHH) of M3D data: '/
     1           5x,'                  Start = ',i4,3i2,' (GMT)'/
     2           5x,'              No. hours = ',i4,/
     3           5x,'Extraction Subdomain in M3D file: '/
     4           5x,'            No. X cells = ',i4/
     5           5x,'            No. Y cells = ',i4/
     6           5x,'            No. layers  = ',i4 )
c
         write(io6,106)nx1,ny1,nz1,nx2,ny2,nz2
106      format(/5x,'            Beginning X = ',i4/
     2           5x,'            Beginning Y = ',i4/
     3           5x,'            Beginning Z = ',i4/
     4           5x,'               Ending X = ',i4/
     5           5x,'               Ending Y = ',i4/
     6           5x,'               Ending Z = ',i4)
c
         write(io6,107)rymin,rymax,rxmin,rxmax
107      format(/5x,' Latitude range : ',f9.4,'  -  ',f9.4/
     :           5x,' Longitude range: ',f10.4, ' - ' ,f10.4)

         write(io6,108)
108      format(/5x,'MM5 half-sigma levels'/5x,'Level',5x,'Sigma'/)
         do 110 i=1,nzp
            write(io6,109)i,sigma(i)
110      continue
109      format(4x,i4,6x,f6.4)
      endif
c
c ----------------------------------------------------
c --- Next NXP * NYP records -- lat., long., elevation
c ----------------------------------------------------
      do 50 j=1,nyp
      do 50 i=1,nxp
         read(io7,99)iindex,jindex,xnlat3d(i,j),xelon3d(i,j),
     &                ite3d(i,j)
 99      format(2i4,f9.4,f10.4,i5)

c ---   QA check that I,J read match expected values
        icheck=iindex-i13d+1
        jcheck=jindex-j13d+1
        if(icheck.ne.i.or.jcheck.ne.j)then
          write(io6,*)'ERROR in subr. RDHD53 -- I,J do not match ',
     1      'values read on header record'
          write(io6,*)'I, J = ',i,j
          write(io6,*)'ICHECK, JCHECK = ',icheck,jcheck
          write(*,*)'FATAL Error in RDHD53 -- I,J do not match ',
     1      'values read on header record (see LIST file)'
          stop
        endif
50    continue

c
      return
      end

c----------------------------------------------------------------------
      subroutine methd3d(lhdv,lmetcf,lte,iffte)
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481      Level: 040923                METHD3D
c ---          D. Strimaitis   Earth Tech, Inc.
c
c --- PURPOSE:  Read header records of 3D.DAT input file, including
c               the 'comment records' and echo back inputs to the list
c               file.
c               Write domain plot files.
c
c --- INPUTS:
c              LHDV - logical    - Flag to output MET parameters
c            LMETCF - logical    - Flag to output MET file comments
c               LTE - logical    - Flag for Terrain grid
c             IFFTE - integer    - Format (0:self-scaling, 1:fixed)
c
c      common /GRID/
c             nbx,nby,nex,ney
c
c      Parameters: IO6, IO9, MXNX, MXNY, MXNZ, MXNZP1
c
c --- UPDATES:
c       V4.3 (040923) to V4.4 (041208) FRR
c          - Change messag length from 70 to 40 characters
c
c --- OUTPUT:  (header information used by calling program)
c
c      common /GRID/
c             nbx,nby,nex,ney,nx,ny,nz,xorg,yorg,delx,zface(mxnzp1),
c             xgkm(mxnx,mxny),ygkm(mxnx,mxny)
c             feast,fnorth,
c             iutmzn,rnlat0,relon0,xlat1,xlat2,
c             xgkm(mxnx,mxny),ygkm(mxnx,mxny),nbx,nby,nex,ney,
c             isub,isp,nspx,nspy,
c             pmap,datum,daten,utmhem
c      common /HEADER/
c             irtype,irlg,nssta,nusta,npsta,ifilver,jbtz,lcalgrd
c
c      Output Files
c             QATERR.GRD - gridded terrain elevations (mMSL)
c             QAMETG.BNA - borders for the METEOROLOGICAL grid domain
c             QA3D.DAT   - location of cell centers in 3D file
c
c --- METHD3D called by:  main
c --- METHD3D calls:      RDHD53
c                         OUT, OUTFX, OUTIFX, GRDOUTI
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmt'
c
c --- Include commons
      include 'filnam.pmt'
      include 'grid.pmt'
      include 'header.pmt'
      include 'met3d.pmt'

      character*40 messag
      character*40 fmt

      logical*4 ldate
      logical lhdv,lmetcf,lte

      data ldate/.FALSE./

c --- Read header records
c -----------------------
c --- Data is in 3D.DAT format (dataset version 2.0 or later)
      call rdhd53(lmetcf)

c --- Perform a few operations using data from header
c ---------------------------------------------------

c --- Define variables in /HEADER/
      irlg=nhrsm3d
      jbtz=0
c --- (remaining are not relevant)
      irtype=1
      nssta=0
      npsta=0
      nusta=0
      ifilver=1
      lcalgrd=.true.

c --- Swap 3D.DAT extraction grid into /GRID/ variables
      nx=nxp
      ny=nyp
      nz=nzp
      delx=dxmet
      xorg=x1met+(FLOAT(i13d)-1.5)*dxmet
      yorg=y1met+(FLOAT(j13d)-1.5)*dxmet
      feast=feast3d
      fnorth=fnorth3d
      iutmzn=iutmzn3d
      rnlat0=rnlat03d
      relon0=relon03d
      xlat1=xlat13d
      xlat2=xlat23d
      pmap=map3d
      datum=datum3d
      daten=daten3d
      utmhem=utmhem3d
c --- Place layer number into zface array
      zface(1)=0.0
      do k=1,nzp
         zface(k+1)=zface(k)+1.0
      enddo

c --- Reset the grid range now that the full size is known
c --- Check for a zero index
      ntest=nbx*nby*nex*ney
      if(ntest.EQ.0) then
c ---    'Use full grid' option is intended
         nbx=1
         nby=1
         nex=nx
         ney=ny
      else
c ---    Condition output grid range
         if(nbx.LT.1) nbx=1
         if(nby.LT.1) nby=1
         if(nex.LT.1) nex=1
         if(ney.LT.1) ney=1
         nbx=MIN(nbx,nx)
         nby=MIN(nby,ny)
         nex=MIN(nex,nx)
         ney=MIN(ney,ny)
      endif

c --- Set X-Y coordinates of the gridpoints (km)
c --- (store in /grid/)
      do i=1,nx
         do j=1,ny
           xgkm(i,j)=xorg+(i-0.5)*delx
           ygkm(i,j)=yorg+(j-0.5)*delx
         enddo
      enddo

c --- Set Upper Right met grid corner (km)
c --- (Lower Left is xorg,yorg)
      xUR=xorg+FLOAT(nx)*delx
      yUR=yorg+FLOAT(ny)*delx


c --- Create plot files (automatic!) of domain fields
c --- and grid locations
c ---------------------------------------------------

c --- Terrain
      fmt='(10000(i6,2x))'
      call GRDOUTI(io9,tegrd,ite3d,fmt,0,0,0)

c --- Print the 3D.DAT grid points to the QA file
      open(io9,file='QA3D.DAT',status='REPLACE')
      write(io9,*)'             3D.DAT Grid Points'
      write(io9,*)'     X           Y        Longitude    Latitude'
      write(io9,*)'   (km)        (km)        (deg E)     (deg N)'
      do j=1,ny
        do i=1,nx
           write(io9,'(4f12.3)') xgkm(i,j),ygkm(i,j),
     1                           xelon3d(i,j),xnlat3d(i,j)
        enddo
      enddo
      close(io9)

c --- Met grid boundary [BNA format]
      open(io9,file=gridbna,status='REPLACE')
      write(io9,'(a15)') '" Met","grid",5'
      write(io9,'(f12.4,a1,f12.4)') xorg,',',yorg
      write(io9,'(f12.4,a1,f12.4)') xorg,',',yUR
      write(io9,'(f12.4,a1,f12.4)') xUR, ',',yUR
      write(io9,'(f12.4,a1,f12.4)') xUR, ',',yorg
      write(io9,'(f12.4,a1,f12.4)') xorg,',',yorg
      close(io9)


c --- Report selected header information to list file
c ---------------------------------------------------
      
c --- Condition print-range
      nx1=MAX(1,nbx)
      nx2=MIN(nx,nex)
      ny1=MAX(1,nby)
      ny2=MIN(ny,ney)

c --- If only a single point is reported to list file, set special flags & variables
      if(nx1.eq.nx2.and.ny1.eq.ny2)then
         isp=1
         nspx=nx1
         nspy=ny1
      else
         isp=0
         nspx=0
         nspy=0
      endif

c --- Formats used
301   format(2f12.2)
302   format(a8,i8,f8.1,f10.3,2f12.2)
303   format(a8,i8)
304   format(a8,i8,f8.1,2f12.2)
305   format(a8,i8,2f12.2)


c --- Write gridded header data requested
c
c --- elevations (NX * NY words)
      if(lte .AND. isp.EQ.0)then
         messag='Terrain heights (m)'
         if(iffte.eq.1)then
            call outifx(ite3d,6,ldate,messag,nx1,ny1,nx2,ny2)
         else
            call out(xdum,ite3d,2,5,ldate,messag,nx1,ny1,nx2,ny2)
         endif
      endif
c
c --- Print header for single point table.
      if(isp.eq.1) then
         write(io6,217)
         write(io6,206)
206      format(//,' ** -- TABLE FOR ONE POINT -- **',/)
         write(io6,*)
         write(io6,200)nspx,nspy
200      format(1x,'The point selected is (',i3,',',i3,')'/)
         write(io6,*)
         write(io6,209)ite3d(nspx,nspy)
209      format(1x,'Terrain Elevation (m)          = ',i10)
      endif

      write(io6,217)
217   format(//1x,13('----------')/1x,13('----------')//)

      return
      end

c----------------------------------------------------------------------
      subroutine fin
c----------------------------------------------------------------------
c
c --- PRTMET   Version: 4.481            Level: 030402               FIN
c ---          J. Scire, Earth Tech
c
c --- PURPOSE:  Run termination routine -- compute runtime
c
c --- UPDATE
c --- V4.0 (020208) to V4.1 (030402)  DGS
c               - Fix list file unit number to JULDAY
c
c --- INPUTS:
c       Common block /QA/
c          rdate, rtime, rcpu
c       Parameters: IO6, IOMESG
c
c --- OUTPUT:  none
c
c --- FIN called by:  MAIN
c --- FIN calls:      DATETM, JULDAY, DELTT
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.pmt'
      include 'qa.pmt'
c
      character*8 rtime2
      character*10 rdate2
c
      write(iomesg,*)'TERMINATION PHASE'
c
c --- get system date & time at end of run
      call datetm(rdate2,rtime2,rcpu)
c
c --- compute runtime
      read(rtime(1:2),10)ihr1
      read(rtime(4:5),10)imin1
      read(rtime(7:8),10)isec1
10    format(i2)
      t1=ihr1*3600.+imin1*60.+isec1
c
      read(rtime2(1:2),10)ihr2
      read(rtime2(4:5),10)imin2
      read(rtime2(7:8),10)isec2
      t2=ihr2*3600.+imin2*60.+isec2
c
      if(rdate.eq.rdate2)then
         delt=t2-t1
      else
         read(rdate(1:2),10)imo1
         read(rdate(4:5),10)iday1
         read(rdate(7:10),'(i4)')iyr1
         call julday(io6,iyr1,imo1,iday1,ijul1)
c
         read(rdate2(1:2),10)imo2
         read(rdate2(4:5),10)iday2
         read(rdate2(7:10),'(i4)')iyr2
         call julday(io6,iyr2,imo2,iday2,ijul2)
c
c ---    compute no. hours from beg. of first hour of run to
c ---    ending hour of ending day of the run
         call deltt(iyr1,ijul1,ihr1,iyr2,ijul2,ihr2,idelhr)
c
c ---    adjust for minutes and seconds
         delt=idelhr*3600.-imin1*60.-isec1+imin2*60.+isec2
      endif

c --- On the PC, the runtime and CPU time are the same
c --- (DATETM provides RCPU = 0.0 on the PC)
      if(rcpu.EQ.0.0)rcpu=delt

c --- Report current date
      write(io6,1402)rtime2,rdate2,delt,rcpu
1402  format(//2x,'End of run -- Clock time: ',a8/
     1         2x,'                    Date: ',a10//
     2         2x,'      Elapsed Clock Time: ',f10.1,' (seconds)'//
     3         2x,'                CPU Time: ',f10.1,' (seconds)')

c
      return
      end
c----------------------------------------------------------------------
      subroutine rdmet(idathrb,isecb,wrk1,nwork,lecho,kdathre,ksece,
     1 umet,vmet,tmet,ipgt,htmix,ustar,xmonin,wstar,rmm,tempss,rhoss,
     2 qswss,irhss,ipcode,temp2d,rho2d,qsw2d,irh2d,ipcode2d,wdiv)
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481     Level: 051012                  RDMET
c               D. Strimaitis, Earth Tech, Inc.
c
c --- Adapted from:
c
c --- CALPUFF    Version: 5.723    Level: 030402                  RDMET
c                J. Scire, SRC
c
c --- PURPOSE:  Read CALMET meteorological data for one timestep
c
c
c --- UPDATE (in PRTMET RDMET)
c --- V4.4-V4.41    051012  (DGS): allow for begin/end time format
c                                  resolved to seconds
c                                  (retaincompatibility with 
c                                  older CALMET)
c --- V4.3 (040923) to V4.4 (021208) (FRR)
c     - Add 2 variables (beg/ending nx,ny) to the calling list to
c       subroutine OUT. OUT in PRTMET has a different calling list
c       than OUT in CALPUFF
c     - Change messag length from 70 to 40 characters
c
c --- UPDATE (in CALPUFF RDMET)
c --- V5.5-V5.7     030402  (DGS): Add list file unit to YR4 call
c                   030119  (FRR): bug fix to make sure ipcode2d data
c                                  is read in when npsta=-1
c
c ---               090101  (FRR): read CALMET records V5.3, level 010901
c frr (09/01)
c ---  Different format after CALMET version V5.3 - Level 010901
c ---  NOOBS mode allowed (no upper and possibly no surface stations)
c ---  2D records of sf Temp, RH, IPCODE, RHO, QS (previously 1-D)
c ---  Precipitation data if NPSTA>0 or NPSTA=-1 (prognostic rain data)
c ---
c --- V5.2-V5.4     000602  (DGS): add message to "stop"
c --- V5.0-V5.2     991104  (DGS): YYYY format used for year
c --- V5.0-V5.0     990228a (DGS): add IEOF to RDR2D arguments and add
c                                  NEXTFIL to open next CALMET file
c --- V5.0-V5.0     990228  (DGS): correct dimension in call to RDR2D
c                                  for w-field
c --- V4.0-V5.0     971107  (DGS): store w for vertical divergence field
c                   971107  (DGS): add record-skip logic from COMP to
c                                  find current date-time
c
c --- INPUTS:
c       IDATHRB - integer       - Starting date & hour of required data
c                                 (YYYYJJJHH)
c         ISECB - integer       - Starting second of required data
c                                 (0000-3599)
c   WRK1(nwork) - real array    - Work array of length "NWORK" words
c         NWORK - integer       - Dimension of work array -- NOTE:
c                                 NWORK must be at least as large as
c                                 the max. of (NXM * NYM or NSSTA)
c         LECHO - logical       - Flag controlling printing of input
c                                 meteorological data fields
c    Common block /HEADER/ variables:
c         NXM, NYM, NZM, NSSTA, NPSTA, LCALGRD, I2DMET, ITIME
c    Parameters:
c         MXNZMP1, IO6
c
c
c --- OUTPUT:
c
c                KDATHRE - integer - Ending date & hour of data
c                                    (YYYYJJJHH)
c                 KSECE - integer  - Ending second of data
c                                    (0000-3599)
c   UMET(mxnx,mxny,mxnz) - real    - U component of the wind (m/s)
c                                    at each grid point
c   VMET(mxnx,mxny,mxnz) - real    - V-component of the wind (m/s)
c   TMET(mxnx,mxny,mxnz) - real    - 3-D temperature field (deg. K)
c        IPGT(mxnx,mxny) - integer - PGT stability class
c       HTMIX(mxnx,mxny) - real    - Mixing height (m)
c       USTAR(mxnx,mxny) - real    - Surface friction velocity (m/s)
c      XMONIN(mxnx,mxny) - real    - Monin-Obukhov length (m)
c       WSTAR(mxnx,mxny) - real    - Convective velocity scale (m/s)
c         RMM(mxnx,mxny) - real    - Precipitation rate (mm/hr)
c          TEMPSS(mxss) - real    - Air temperature (deg. K) at
c                                    each surface met. station
c           RHOSS(mxss) - real    - Air density (kg/m**3)
c           QSWSS(mxss) - real    - Short wave solar radiation
c                                    (W/m**2)
c           IRHSS(mxss) - integer - Relative humidity (percent)
c          IPCODE(mxss) - integer - Precipitation code at each surface
c                                    station
c      TEMP2D(mxnx,mxny) - real    - Air temperature (deg. K)
c       RHO2D(mxnx,mxny) - real    - Air density (kg/m**3)
c       QSW2D(mxnx,mxny) - real    - Short wave solar radiation
c                                    (W/m**2)
c       IRH2D(mxnx,mxny) - integer - Relative humidity (percent)
c    IPCODE2D(mxnx,mxny) - integer - Precipitation code 
c                 KDATHR - integer - Date of hour of data (YYYYJJJHH)
c   WDIV(mxnx,mxny,mxnz) - real    - Vertical divergence dw/dz; this
c                                    array holds the w-data here
c
c --- RDMET called by:  COMP
c --- RDMET calls:      RDR1D, RDR2D, RDI1D, RDI2D, OUT, NEXTFIL, YR4
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.pmt'
c
c --- Include common /METHD/ -- met. data parameters
      include 'header.pmt'
c
      real umet(mxnx,mxny,mxnz),vmet(mxnx,mxny,mxnz)
      real tmet(mxnx,mxny,mxnz)
      real htmix(mxnx,mxny),ustar(mxnx,mxny),xmonin(mxnx,mxny)
      real wstar(mxnx,mxny),rmm(mxnx,mxny)
      real wrk1(nwork)
c frr (09/01)in noobs mode, nssta can be = 0 => dimension with mxss
c     real tempss(nssta),rhoss(nssta),qswss(nssta)
      real tempss(mxss),rhoss(mxss),qswss(mxss)
      real wdiv(mxnx,mxny,mxnz)
c
c frr (09/01) - additional 2D arrays for temp,rh,rho,ipcode,qsw
      real    temp2d(mxnx,mxny),rho2d(mxnx,mxny),qsw2d(mxnx,mxny)
      integer irh2d (mxnx,mxny),ipcode2d(mxnx,mxny)
      real    rdum(mxnx,mxny)

c frr (09/01)in noobs mode, nssta can be = 0 => dimension with mxss
c     integer ipgt(mxnx,mxny),irhss(nssta),ipcode(nssta)
      integer ipgt(mxnx,mxny),irhss(mxss),ipcode(mxss)
c
      character*40 messag
      character*8 clabel
      character*8 clabexp
c
      logical lecho,ldate
c
      data ldate/.false./
c
c --- Check that enough workspace has been allocated
      nec=max0(nxm*nym,nssta)
      if(nwork.lt.nec)then
         write(io6,*)'ERROR in subr. RDMET -- insufficient workspace ',
     1   'allocated to work arrays -- Allocated (NWORK) = ',nwork,
     2   '   Required (NEC) = ',nec
         write(*,*)
         stop 'Halted in RDMET -- see list file.'
      endif

c --- Begin reading data records for one time period
1     continue

c
c --- Track date-hr of end of this period to check for problems in file
c --- (end-times are available in new and old formats)
      kdathre=0
      ksece=0
c
c --- read the U, V wind components
      do 10 iz=1,nzm
      call rdr2d(io7,itime,umet(1,1,iz),wrk1,mxnx,mxny,nxm,nym,clabel,
     1           ndathrb1,nsecb1,ndathre1,nsece1,ieof)
      if(ieof.EQ.1) then
c ---    Reached end of CALMET.DAT file; check for another file
         call NEXTFIL(nxm,nym,ifound)
         if(ifound.EQ.1) then
            goto 1
         else
            write(*,*)
            stop 'RDMET: Unexpected EOF in CALMET records'
            write(*,*)'Target Start-time: ',idathrb,isecb
            write(*,*)'Reading Date-time: ',ndathrb1,nsecb1
            write(*,*)'               to: ',ndathre1,nsece1
         endif
      endif
c
c --- Check that record label matches expected label
      clabexp='U-LEV'
      write(clabexp(6:8),'(i3)')iz
      if(clabel.ne.clabexp)go to 999
c
      call rdr2d(io7,itime,vmet(1,1,iz),wrk1,mxnx,mxny,nxm,nym,clabel,
     1           ndathrb2,nsecb2,ndathre2,nsece2,ieof)
      if(ieof.EQ.1) then
         write(*,*)
         stop 'RDMET: Unexpected EOF in CALMET records'
      endif
      clabexp(1:1)='V'
      if(clabel.ne.clabexp)go to 999
c

c --- Use the W wind component (conditional) for w-divergence
      if(lcalgrd)then
         call rdr2d(io7,itime,wdiv(1,1,iz),wrk1,mxnx,mxny,nxm,nym,
     1              clabel,ndathrb3,nsecb3,ndathre3,nsece3,ieof)
         if(ieof.EQ.1) then
            write(*,*)
            stop 'RDMET: Unexpected EOF in CALMET records'
         endif
         clabexp(1:5)='WFACE'
         if(clabel.ne.clabexp)go to 999
         if(ndathre2.ne.ndathre3 .OR. nsece2.ne.nsece3) goto 2999
      endif
c

c --- Check for consistency of date/time
      if(ndathre1.ne.ndathre2 .OR. nsece1.ne.nsece2) goto 2999
      if(kdathre.eq.0)then
c ---    first vertical level
         kdathre=ndathre1
         ksece=nsece1
      else if(ndathre1.ne.kdathre .OR. nsece1.ne.ksece)then
c ---    date/time does not match value for previous layer
         ndathre=ndathre1
         nsece=nsece1
         go to 3999
      endif
10    continue
c
c --- Read the 3-D temperature field
      if(lcalgrd)then
         clabexp='T-LEV'
         do 12 iz=1,nzm
         call rdr2d(io7,itime,tmet(1,1,iz),wrk1,mxnx,mxny,nxm,nym,
     1              clabel,ndathrb,nsecb,ndathre,nsece,ieof)
         if(ieof.EQ.1) then
            write(*,*)
            stop 'RDMET: Unexpected EOF in CALMET records'
         endif
c
         write(clabexp(6:8),'(i3)')iz
         if(clabel.ne.clabexp)go to 999
         if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
12       continue
      endif
c

c --- read other 2-D meteorological fields
c
c --- PGT stability class
      call rdi2d(io7,itime,ipgt,wrk1,mxnx,mxny,nxm,nym,clabel,
     &           ndathrb,nsecb,ndathre,nsece)
      clabexp='IPGT'
      if(clabel.ne.clabexp)go to 999
      if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
c --- FRICTION VELOCITY
      call rdr2d(io7,itime,ustar,wrk1,mxnx,mxny,nxm,nym,clabel,
     &           ndathrb,nsecb,ndathre,nsece,ieof)
      if(ieof.EQ.1) then
         write(*,*)
         stop 'RDMET: Unexpected EOF in CALMET records'
      endif
      clabexp='USTAR'
      if(clabel.ne.clabexp)go to 999
      if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
c --- MIXING HEIGHT
      call rdr2d(io7,itime,htmix,wrk1,mxnx,mxny,nxm,nym,clabel,
     &           ndathrb,nsecb,ndathre,nsece,ieof)
      if(ieof.EQ.1) then
         write(*,*)
         stop 'RDMET: Unexpected EOF in CALMET records'
      endif
      clabexp='ZI'
      if(clabel.ne.clabexp)go to 999
      if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
c --- MONIN-OBUKHOV LENGTH
      call rdr2d(io7,itime,xmonin,wrk1,mxnx,mxny,nxm,nym,clabel,
     &           ndathrb,nsecb,ndathre,nsece,ieof)
      if(ieof.EQ.1) then
         write(*,*)
         stop 'RDMET: Unexpected EOF in CALMET records'
      endif
      clabexp='EL'
      if(clabel.ne.clabexp)go to 999
      if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
c --- CONVECTIVE VELOCITY SCALE
      call rdr2d(io7,itime,wstar,wrk1,mxnx,mxny,nxm,nym,clabel,
     &           ndathrb,nsecb,ndathre,nsece,ieof)
      if(ieof.EQ.1) then
         write(*,*)
         stop 'RDMET: Unexpected EOF in CALMET records'
      endif
      clabexp='WSTAR'
      if(clabel.ne.clabexp)go to 999
      if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
c --- PRECIPITATION DATA
c frr (09/01) - Additional option: if NPSTA=-1, prognostic gridded rain data
c      if(npsta.gt.0)then
      if(npsta.ne.0)then
         call rdr2d(io7,itime,rmm,wrk1,mxnx,mxny,nxm,nym,clabel,
     :              ndathrb,nsecb,ndathre,nsece,ieof)
         if(ieof.EQ.1) then
            write(*,*)
            stop 'RDMET: Unexpected EOF in CALMET records'
         endif
      endif

c
c frr (09/01)
c --- NOOBS CALMET: full 2-D fields of the following variables instead
c --- of 1-D fields (values at the surface stations only)
c           - Air temperature (deg. K),
c           - Air density (kg/m**3),
c           - Short-wave solar radiation (W/m**2),
c           - Relative humidity (percent),
c           - Precipitation code

      if(i2dmet.EQ.1) then

c ---    New CALMET output format - 2D arrays -
c ---    CALMET pre2.1 with hour-ending time stamps
         call rdr2d(io7,itime,temp2d,wrk1,mxnx,mxny,nxm,nym,clabel,
     &              ndathrb,nsecb,ndathre,nsece,ieof)

         if(ieof.EQ.1) then
            write(*,*)
            stop 'RDMET: Unexpected EOF in CALMET records'
         endif
         clabexp='TEMPK'
         if(clabel.ne.clabexp)go to 999
         if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
         call rdr2d(io7,itime,rho2d,wrk1,mxnx,mxny,nxm,nym,clabel,
     &              ndathrb,nsecb,ndathre,nsece,ieof)

         if(ieof.EQ.1) then
            write(*,*)
            stop 'RDMET: Unexpected EOF in CALMET records'
         endif
         clabexp='RHO'
         if(clabel.ne.clabexp)go to 999
         if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
         call rdr2d(io7,itime,qsw2d,wrk1,mxnx,mxny,nxm,nym,clabel,
     &              ndathrb,nsecb,ndathre,nsece,ieof)

         if(ieof.EQ.1) then
            write(*,*)
            stop 'RDMET: Unexpected EOF in CALMET records'
         endif
         clabexp='QSW'
         if(clabel.ne.clabexp)go to 999
         if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
         call rdi2d(io7,itime,irh2d,wrk1,mxnx,mxny,nxm,nym,clabel,
     &              ndathrb,nsecb,ndathre,nsece)
         clabexp='IRH'
         if(ieof.EQ.1) then
            write(*,*)
            stop 'RDMET: Unexpected EOF in CALMET records'
         endif
         if(clabel.ne.clabexp)go to 999
         if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
c ---    Precipitation code at surface stations
c         if(npsta.gt.0)then
c ---    frr 030119 - bug fix: also precipitation code if prognostic rain
c        (i.e npsta.eq.-1)
         if(npsta.ne.0)then
           call rdi2d(io7,itime,ipcode2d,wrk1,mxnx,mxny,nxm,nym,clabel,
     &                ndathrb,nsecb,ndathre,nsece)
           clabexp='IPCODE'
           if(clabel.ne.clabexp)go to 999
           if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
         endif

      elseif(i2dmet.EQ.0) then
c ---    read 1-D meteorological fields:
c           Air temp. (deg. K),
c           Air density (kg/m**3),
c           Short-wave solar radiation (W/m**2),
c           Relative humidity (percent),
c           Precipitation code
         call rdr1d(io7,itime,tempss,nssta,clabel,
     &              ndathrb,nsecb,ndathre,nsece)
         clabexp='TEMPK'
         if(clabel.ne.clabexp)go to 999
         if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
         call rdr1d(io7,itime,rhoss,nssta,clabel,
     &              ndathrb,nsecb,ndathre,nsece)
         clabexp='RHO'
         if(clabel.ne.clabexp)go to 999
         if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
         call rdr1d(io7,itime,qswss,nssta,clabel,
     &              ndathrb,nsecb,ndathre,nsece)
         clabexp='QSW'
         if(clabel.ne.clabexp)go to 999
         if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
         call rdi1d(io7,itime,irhss,nssta,clabel,
     &              ndathrb,nsecb,ndathre,nsece)
         clabexp='IRH'
         if(clabel.ne.clabexp)go to 999
         if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
c
c ---    Precipitation code at surface stations
         if(npsta.gt.0)then
            call rdi1d(io7,itime,ipcode,nssta,clabel,
     &                 ndathrb,nsecb,ndathre,nsece)
            clabexp='IPCODE'
            if(clabel.ne.clabexp)go to 999
            if(ndathre.ne.kdathre .OR. nsece.ne.ksece)go to 3999
         endif

      else
         write(*,*)'Subr. RDMET:  Invalid I2DMET = ',i2dmet
         stop
      endif
c
c --- Set date-time for period
c ---------------------------------
      kdathre=ndathre
      ksece=nsece
      if(itime.EQ.0) then
c ---    Construct beginning time for older CALMET data
         iyy=kdathre/100000
         iyyddd=kdathre/100
         iddd=iyyddd-iyy*1000
         ihh=kdathre-iyyddd*100
         call INCR(io6,iyy,iddd,ihh,-1)
         kdathrb=iyy*100000+iddd*100+ihh
         ksecb=0
      else
         kdathrb=ndathrb
         ksecb=nsecb
      endif

c --- Enforce YYYY format for year
      ibyr=kdathrb/100000
      ibdyhr=kdathrb-ibyr*100000
      ieyr=kdathre/100000
      iedyhr=kdathre-ieyr*100000
      call YR4(io6,ibyr,ierrb)
      call YR4(io6,ieyr,ierre)
      if(ierrb.NE.0 .OR. ierre.NE.0) stop 'Halted in RDMET'
      kdathrb=ibyr*100000+ibdyhr
      kdathre=ieyr*100000+iedyhr

c --- Check to see if required date-time has been read (start time)
      if(kdathrb.lt.idathrb)then
c ---    Obtain next time period in MET file
         go to 1
      elseif(kdathrb.gt.idathrb)then
         write(io6,*)'ERROR in subr. RDMET -- current hour not found ',
     1   'in the MET data file -- Current date/hour = ',idathrb,
     2   ' Last date/hour read = ',kdathrb
         write(*,*)
         stop 'Halted in RDMET -- see list file.'
      elseif(ksecb.lt.isecb)then
c ---    Obtain next time period in MET file
         go to 1
      elseif(ksecb.gt.isecb)then
         write(io6,*)'ERROR in subr. RDMET -- current time not found ',
     1   'in the MET data file -- Current date/time = ',idathrb,isecb,
     2   ' Last date/time read = ',kdathrb,ksecb
         write(*,*)
         stop 'Halted in RDMET -- see list file.'
      endif
c
c --- Write the input fields, if requested
      if(LECHO)then
c
c ---    Date and time
         write(io6,*)
         write(io6,*)'------ MET. FIELDS BEGINNING: ',kdathrb,ksecb
         write(io6,*)'------            AND ENDING: ',kdathre,ksece
         write(io6,*)
c
c ---    3-D fields
         do 101 iz=1,nzm
         messag='U-component (m/s) -- Level: '
         write(messag(29:31),'(i3)')iz
c        call out(umet(1,1,iz),idum,1,5,ldate,messag,nxm,nym)
         call out(umet(1,1,iz),idum,1,5,ldate,messag,1,1,nxm,nym)
         messag(1:1)='V'
c        call out(vmet(1,1,iz),idum,1,5,ldate,messag,nxm,nym)
         call out(vmet(1,1,iz),idum,1,5,ldate,messag,1,1,nxm,nym)
         messag(1:1)='W'
c        call out(wdiv(1,1,iz),idum,1,5,ldate,messag,nxm,nym)
         call out(wdiv(1,1,iz),idum,1,5,ldate,messag,1,1,nxm,nym)
101      continue
c
c ---    2-D fields
         messag='PGT stability class'
c         call out(xdum,ipgt,2,1,ldate,messag,nxm,nym)
         call out(xdum,ipgt,2,1,ldate,messag,1,1,nxm,nym)
         messag='Friction velocity (m/s)'
c         call out(ustar,idum,1,5,ldate,messag,nxm,nym)
         call out(ustar,idum,1,5,ldate,messag,1,1,nxm,nym)
         messag='Mixing height (m)'
c         call out(htmix,idum,1,5,ldate,messag,nxm,nym)
         call out(htmix,idum,1,5,ldate,messag,1,1,nxm,nym)
         messag='Monin-Obukhov length (m)'
c         call out(xmonin,idum,1,5,ldate,messag,nxm,nym)
         call out(xmonin,idum,1,5,ldate,messag,1,1,nxm,nym)
         messag='Convective velocity scale (m/s)'
c         call out(wstar,idum,1,5,ldate,messag,nxm,nym)
         call out(wstar,idum,1,5,ldate,messag,1,1,nxm,nym)
         if(npsta.gt.0)then
            messag='Precipitation rate (mm/hr)'
c            call out(rmm,idum,1,5,ldate,messag,nxm,nym)
            call out(rmm,idum,1,5,ldate,messag,1,1,nxm,nym)
         endif
c
c ---    1-D fields
c frr (09/01) after level 010901, 2D CALMET fields-  Before, 1D
         if(i2dmet.EQ.1) then
            messag='Surface Air Temperature (deg. K)'
            call out(temp2d,idum,1,5,ldate,messag,1,1,nxm,nym)
            messag='Surface Air Density (Kg/m**3)'
            call out(rho2d,idum,1,5,ldate,messag,1,1,nxm,nym)
            messag='Short Wave Radiation (W/m**2)'
            call out(qsw2d,idum,1,5,ldate,messag,1,1,nxm,nym)
            messag='Surface Relative Humidity (%)'
c           call out(rdum,irh2d,2,5,ldate,messag,1,1,nxm,nym)
            if(npsta.ne.0)then
               messag='Precipitation code'
               call out(rdum,ipcode2d,2,5,ldate,messag,1,1,nxm,nym)
            endif
         elseif(i2dmet.EQ.0) then
            write(io6,*)'TEMPSS (deg. K) = ',(tempss(n),n=1,nssta)
            write(io6,*)'RHOSS (kg/m**3)  = ',(rhoss(n),n=1,nssta)
            write(io6,*)'QSWSS (W/m**2)   = ',(qswss(n),n=1,nssta)
            write(io6,*)'IRHSS (percent)  = ',(irhss(n),n=1,nssta)
            if(npsta.gt.0)write(io6,*)'IPCODE           = ',
     1       (ipcode(n),n=1,nssta)
         else
            write(*,*)'Subr. RDMET:  Invalid I2DMET = ',i2dmet
            stop
         endif
      endif
c
      return
c
c --- Write error messages -- incorrect record label read
999   continue
      write(io6,1001)clabel,clabexp
1001  format(/1x,'ERROR in subr. RDMET -- incorrect record label ',
     1 'read from MET data file'//1x,'Label read     = ',a8/
     2                               1x,'Label expected = ',a8)
      write(*,*)
      stop 'Halted in RDMET -- see list file.'
c
c --- date/time variables do not match
2999  continue
      write(io6,*)'ERROR in subr. RDMET -- date/time variables for',
     & ' wind components do not match --  '
      write(io6,*)'   NDATHRE1,NSECE1= ',ndathre1,nsece1
      write(io6,*)'   NDATHRE2,NSECE2= ',ndathre2,nsece2
      write(io6,*)'   NDATHRE3,NSECE3= ',ndathre3,nsece3
      stop 'Halted in RDMET -- see list file.'
3999  continue
      write(io6,*)'ERROR in subr. RDMET -- date/time variables ',
     1 'do not match in layers -- '
      write(io6,*)'   NDATHRE ,NSECE = ',ndathre,nsece
      write(io6,*)'   KDATHRE ,KSECE = ',kdathre,ksece
      stop 'Halted in RDMET -- see list file.'
      end

c----------------------------------------------------------------------
      subroutine nextfil(nx,ny,ifound)
c----------------------------------------------------------------------
c
c --- PRTMET    Version: 4.481     Level: 040923                NEXTFIL
c               D. Strimaitis, Earth Tech, Inc.
c
c PURPOSE:     Dummy routine that returns ifound=0 to facilitate the
c              use of RDMET from CALPUFF.  This indicates that no new
c              CALMET files are available.
c
c --- INPUTS:
c              NX,NY - integer - Met grid cell number
c
c --- OUTPUT:
c             IFOUND - integer - Flag indicating if more data records
c
c CALLING ROUTINES:   RDMET
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------

      ifound = 0

      return
      end

c----------------------------------------------------------------------
      subroutine rd3d(idathr,isec,kdathre,ksece,up,vp,wp,tp,irhp,zp,
     &                temp2d,pmsl2d,qsw2d,qlw2d,irh2d,rmm2d)
c----------------------------------------------------------------------
C
c --- PRTMET    Version: 4.481          Level: 070719              RD3D
c               D.Strimaitis
c
c --- PURPOSE:  Reads data records from the 3D.DAT meteorological file
C     
c --- UPDATES:
c
c --  Version 4.3 (040923) to Version 4.47 (070719) (DGS)
c        - Introduce begin/end time structures
c        - Update 3D.DAT data record format for Dataset 2.1
C
C
c        Parameters: MXNXP, MXNYP, MXNZP, IO6
c
c --- RD3D called by:  COMP
c --- RD3D calls:      JULDAY, ESAT, YR4, DEDAT, INCRS
c----------------------------------------------------------------------
c --- NOTE: The begin/end time structure has not been introduced in a
c ---       new 3D.DAT dataset version as of this (070719) update, and
c ---       will require changes to this subroutine.
c----------------------------------------------------------------------
c --- include parameters
      include 'params.pmt'
      include 'control.pmt'
      include 'met3d.pmt'

      character*80 buff

c --- Declare 3D profiles
      real up(mxnxp,mxnyp,mxnzp),vp(mxnxp,mxnyp,mxnzp)
      real wp(mxnxp,mxnyp,mxnzp),tp(mxnxp,mxnyp,mxnzp)
      real zp(mxnxp,mxnyp,mxnzp)
      integer irhp(mxnxp,mxnyp,mxnzp)

c --- Declare 2D arrays
      real temp2d(mxnxp,mxnyp),pmsl2d(mxnxp,mxnyp)
      real qsw2d(mxnxp,mxnyp),qlw2d(mxnxp,mxnyp)
      real rmm2d(mxnxp,mxnyp)
      integer irh2d(mxnxp,mxnyp)

      data deg2rad/0.0174533/

c --- Prepare start-time of 3D.DAT period needed
      if(itime3d.EQ.0) then
c ---    Actual begin time for period is used
         ndathr=idathr
         nsec=isec
      elseif(itime3d.EQ.1) then
c ---    Full time interval between fields is used
c ---    Compute actual begin time in file from target begin time
c ---    (currently coded for single time in 3D.DAT file)
         nsec=isec
         call DEDAT(idathr,iyr,ijul,ihr)
         call INCRS(io6,iyr,ijul,ihr,nsec,nsecdt3d)
         ndathr=iyr*100000+ijul*100+ihr
      else
         write(io6,*)'FATAL Error in RD3D -- Invalid ITIME3D'
         write(io6,*)'Expected ITIME3D: 0 or 1'
         write(io6,*)'   Found ITIME3D: ',itime3d
         write(*,*)'FATAL Error in RD3D -- Invalid ITIME3D'
         write(*,*)'Expected ITIME3D: 0 or 1'
         write(*,*)'   Found ITIME3D: ',itime3d
         stop
      endif

c --- Current 3D.DAT has one time so set end-time = target time
      kdathre=ndathr
      ksece=nsec

c --- Loop over each grid cell in extraction subdomain
1     continue
      do j=1,nyp
        do i=1,nxp
       
c ---     2D data record
c ---     --------------
          if(imet3d.EQ.0) then
            read(io7,60,end=999) myr,mmo,mday,mhr,ix,jx,
     &           pmsl2d(i,j),rmm2d(i,j),isnow
60          format(4i2,2i3,f7.1,f5.2,i2)
            qsw2d(i,j)=0.0
            qlw2d(i,j)=0.0
            temp2d(i,j)=0.0
            irh2d(i,j)=0
          else
            read(io7,61,end=999) myr,mmo,mday,mhr,ix,jx,
     &           pmsl2d(i,j),rmm2d(i,j),isnow,qsw2d(i,j),qlw2d(i,j),
     &           temp2d(i,j),spechum
61          format(i4,3i2,2i3,f7.1,f5.2,i2,3f8.1,f8.2)
          endif
       
c ---     QA date/time
          call YR4(io6,myr,ierr)
          if(ierr.NE.0) stop 'Halted in RD3D'
          call JULDAY(io6,myr,mmo,mday,mjul)
          mdathr=myr*100000+mjul*100+mhr
          if(mdathr.GT.ndathr) then
            write(io6,*)'FATAL Error in RD3D -- Missing 3D.DAT Record'
            write(io6,*)'Expected date/time: ',ndathr
            write(io6,*)'   Found date/time: ',mdathr
            write(*,*)'FATAL Error in RD3D -- Missing 3D.DAT Record'
            write(*,*)'Expected date/time: ',ndathr
            write(*,*)'   Found date/time: ',mdathr
            stop
          elseif(mdathr.LT.ndathr) then
            iskip=1
          else
            iskip=0
          endif

c ---     QA grid point (ix,jy)
          ixqa=i+i13d-1
          jxqa=j+j13d-1
          if(ix.NE.ixqa .OR. jx.NE.jxqa)then
            write(io6,960) myr,mmo,mday,mhr,ix,jx,ixqa,jxqa
960         format(1x,'Error in Subr. RD3D -- Error in MM5.DAT/',
     1      '3D.DAT file'/     
     2      1x,'MM5/3D grid point I,J do not match expected values'/
     3      1x,'MM5/3D date/time:  Yr: ',i4,1x,'Mo: ',i2,1x,'Day: ',i2,
     4      1x,'Hr: ',i2,' (GMT)'/
     5      1x,'  MM5/3D I,J: ',2i4/
     6      1x,'Expected I,J: ',2i4)
c
            print *,'Error in Subr. RD3D -- Error in MM5/3D.DAT file'
            print *,'MM5/3D grid point I,J do not match expected ',
     1             'values'
            stop
          endif

c ---     Convert precip from cm/hr to mm/hr
          rmm2d(i,j)=rmm2d(i,j)*10.

c ---     Convert specific humidity to relative humidity(%)
          if(spechum.LE.0.0) then
            irh2d(i,j)=0
          else
c ---       Specific Humidity in g/g
            q2=spechum*.001
c ---       Mixing ratio
            rmix=q2/(1.-q2)
c ---       Saturation mixing ratio
            tdegc=temp2d(i,j)-273.15
            rsat=0.622/((pmsl2d(i,j)/ESAT(tdegc))-1.)
c ---       Relative humidity (whole %)
            irh2d(i,j)=100*NINT(rmix/rsat)
          endif

c ---     3D data records
c ---     ---------------
          if(iskip.EQ.1) then
            do n=1,nzp
c ---         Skip
              read(io7,*) idum,idum
            enddo
          else
c ---       Read and process
            do n=1,nzp
              if(ioutm3d.eq.81) then
                read(io7,81) ipress,iz,tk,iwd,ws
81              format (i4,i6,f6.1,i4,f5.1)
              elseif(ioutm3d.eq.82) then
                read(io7,82) ipress,iz,tk,iwd,ws,
     :                        irh,q
82              format(i4,i6,f6.1,i4,f5.1,i3,f5.2)
              elseif(ioutm3d.eq.83) then
                if(cver3d.eq.'2.1') then
                   read(io7,830) ipress,iz,tk,iwd,ws,
     :                           irh,q,qc,qr
                else
                   read(io7,83) ipress,iz,tk,iwd,ws,
     :                          irh,q,qc,qr
                endif
830             format(i4,i6,f6.1,i4,f5.1,i3,f5.2,2(f6.3))
83              format(i4,i6,f6.1,i4,f5.1,i3,3(f5.2))
              elseif(ioutm3d.eq.84) then
                if(cver3d.eq.'2.1') then
                   read(io7,840) ipress,iz,tk,iwd,ws,
     :                           irh,q,qc,qr,qi,qs
                else
                   read(io7,84) ipress,iz,tk,iwd,ws,
     :                          irh,q,qc,qr,qi,qs
                endif
840             format(i4,i6,f6.1,i4,f5.1,i3,f5.2,4(f6.3))
84              format(i4,i6,f6.1,i4,f5.1,i3,5(f5.2))
              elseif(ioutm3d.eq.85) then
                if(cver3d.eq.'2.1') then
                   read(io7,850) ipress,iz,t,iwd,ws,
     :                           irh,q,qc,qr,qi,qs,qg
                else
                   read(io7,85) ipress,iz,tk,iwd,ws,
     :                          irh,q,qc,qr,qi,qs,qg
                endif
850             format(i4,i6,f6.1,i4,f5.1,i3,f5.2,5(f6.3))
85              format(i4,i6,f6.1,i4,f5.1,i3,6(f5.2))
              elseif(ioutm3d.eq.91) then
                read(io7,91) ipress,iz,tk,iwd,ws,w
91              format(i4,i6,f6.1,i4,f5.1,f6.2)
              elseif(ioutm3d.eq.92) then
                read(io7,92) ipress,iz,tk,iwd,ws,w,
     :                        irh,q
92              format(i4,i6,f6.1,i4,f5.1,f6.2,i3,f5.2)

              elseif(ioutm3d.ge.93) then
c ---           May be compressed record
                read(io7,'(a)')buff
c ---           Read common parts and compression flag 
                if(cver3d.eq.'2.1') then
                   read(buff,930) ipress,iz,tk,iwd,ws,w,
     :                            irh,q,fcomp
                else
                   read(buff,93) ipress,iz,tk,iwd,ws,w,
     :                           irh,q,fcomp
                endif
93              format(i4,i6,f6.1,i4,f5.1,f6.2,i3,3(f5.2))
930             format(i4,i6,f6.1,i4,f5.1,f6.2,i3,f5.2,2(f6.3))

                if(fcomp.gt.-0.0001) then
c ---             Un-compressed form
                  if(ioutm3d.eq.93) then
                     if(cver3d.eq.'2.1') then
                        read(buff,930)ipress,iz,tk,iwd,ws,w,
     :                                irh,q,qc,qr
                     else
                        read(buff,93)ipress,iz,tk,iwd,ws,w,
     :                               irh,q,qc,qr
                     endif
                  elseif(ioutm3d.eq.94) then
                     if(cver3d.eq.'2.1') then
                        read(buff,940)ipress,iz,tk,iwd,ws,w,
     :                                irh,q,qc,qr,qi,qs
                     else
                        read(buff,94)ipress,iz,tk,iwd,ws,w,
     :                               irh,q,qc,qr,qi,qs
                     endif
940                  format(i4,i6,f6.1,i4,f5.1,f6.2,i3,f5.2,4(f6.3))
94                   format(i4,i6,f6.1,i4,f5.1,f6.2,i3,5(f5.2))
                  elseif(ioutm3d.eq.95) then
                     if(cver3d.eq.'2.1') then
c ---                   new moisture format (6.3 instead of 5.2)(050504)
                        read(buff,950) ipress,iz,tk,iwd,ws,w,
     :                                 irh,q,qc,qr,qi,qs,qg
                     else
                        read(buff,95) ipress,iz,tk,iwd,ws,w,
     :                                irh,q,qc,qr,qi,qs,qg
                     endif
95                   format(i4,i6,f6.1,i4,f5.1,f6.2,i3,6(f5.2))
950                  format(i4,i6,f6.1,i4,f5.1,f6.2,i3,f5.2,5(f6.3))
                  endif
                else
c ---             Compressed form
                  if(ioutm3d.eq.93) then
                    qc=0
                    qr=0
                  elseif(ioutm3d.eq.94) then
                    qc=0
                    qr=0
                    qi=0
                    qs=0
                  elseif(ioutm3d.eq.95) then
                    qc=0
                    qr=0
                    qi=0
                    qs=0
                    qq=0
                  endif
                endif
              endif
         
c ---         3D array of relative humidity
              irhp(i,j,n)= irh
c ---         Convert wd,ws to u,v components and store in grid arrays
c ---         Store temperature in grid array
              wd=FLOAT(iwd)
              wdrad = deg2rad*wd
              up(i,j,n) = -ws * sin(wdrad)
              vp(i,j,n) = -ws * cos(wdrad)
              tp(i,j,n) = tk
c ---         Adjust heights from msl to ht above local ground
              zp(i,j,n) = FLOAT(iz - ite3d(i,j))

c ---         Future use?
c ---         Convert mixing ratio from g/kg to g/g
              q=q/1000.
c ---         Calculate potential temperature
              pt = tk * ((1000./ipress) ** 0.286)
c ---         compute specific humidity
              qw = q / (1 + q)
c ---         compute virtual potential temperature
              vpt1 = pt * (1. + 0.61 * qw)

c ---       Next nzp
            enddo

          endif

c ---   Next nxp
        enddo
c --- Next nyp
      enddo


c --- Do another iteration through the i,j loop if skipping to
c --- current time
      if(iskip.EQ.1) goto 1
      return

999   write(io6,*) ' Ran out of 3D.DAT data during run...'
      stop

      return
      end
c----------------------------------------------------------------------
      function esat(tdegc)
c----------------------------------------------------------------------
c
c --- CALMET   Version: 5.546     Level: 901130                    ESAT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Compute the saturation water vapor pressure (mb) using
c               the method of Lowe (1977) (JAM, 16, pp 100-103).
c
c --- INPUT:
c                TDEGC - real          - Air temperature (deg. C)
c
c --- OUTPUT:
c                 ESAT - real          - Saturation water vapor
c                                        pressure (mb)
c
c --- ESAT called by: WATER
c --- ESAT calls:     none
c----------------------------------------------------------------------
      data a0/6.107799961/,a1/4.436518521e-1/,a2/1.428945805e-2/
      data a3/2.650648471e-4/,a4/3.031240396e-6/,a5/2.034080948e-8/
      data a6/6.136820929e-11/
c
c --- compute saturation water vapor pressure (mb)
c --- NOTE: temperature is in deg. C
      esat=a0+tdegc*(a1+tdegc*(a2+tdegc*(a3+tdegc*(a4+tdegc*
     1 (a5+tdegc*a6)))))
c
      return
      end

c----------------------------------------------------------------------
      subroutine mkcmn
c----------------------------------------------------------------------
C
c --- PRTMET    Version: 4.481          Level: 040923             MFCMN
c               D.Strimaitis  Earth Tech, Inc.
c
c --- PURPOSE:  Create PRTMET.CMN file using /GRID/ information
c
c --- INPUTS:
c
c    Common block /GRID/ variables:
c             nx,ny,nz,zface,xorg,yorg,delx
c             feast,fnorth,
c             iutmzn,rnlat0,relon0,xlat1,xlat2,
c             pmap,datum,daten,utmhem
c    Common block /HEADER/ variables:
c             jbtz
c
c        Parameters: MXNZP1, IO9
c
c --- MKCMN called by:  SETUP
c --- MKCMN calls:      none
c----------------------------------------------------------------------
c --- include parameters
      include 'params.pmt'
      include 'grid.pmt'
      include 'header.pmt'

      character*80 cline,cblank,dashes
      character*12 clat0,clon0,clat1,clat2
      character*12 cxo,cyo,cdo,cfeast,cfnorth
      character*1 cN,cS,cE,cW

      data cN/'N'/, cS/'S'/, cE/'E'/, cW/'W'/

c --- Set up the blanking string
      ie=0
      do i=1,8
         ib=ie+1
         ie=ie+10
         cblank(ib:ie)='          '
         dashes(ib:ie)='----------'
      enddo

c --- Transfer grid data into strings
      write(cxo,'(f10.2)') xorg
      write(cyo,'(f10.2)') yorg
      write(cdo,'(f10.2)') delx

c --- Transfer lat/lon data into strings
      if(rnlat0.LT.0.) then
         rslat=-rnlat0
         write(clat0,'(f6.2,a1)') rslat,cS
      else
         write(clat0,'(f6.2,a1)') rnlat0,cN
      endif
      if(xlat1.LT.0.) then
         rslat=-xlat1
         write(clat1,'(f6.2,a1)') rslat,cS
      else
         write(clat1,'(f6.2,a1)') xlat1,cN
      endif
      if(xlat2.LT.0.) then
         rslat=-xlat2
         write(clat2,'(f6.2,a1)') rslat,cS
      else
         write(clat2,'(f6.2,a1)') xlat2,cN
      endif
      if(relon0.LT.0.) then
         rwlon=-relon0
         write(clon0,'(f6.2,a1)') rwlon,cW
      else
         write(clon0,'(f6.2,a1)') relon0,cE
      endif

c --- Transfer false offset data into strings
      write(cfeast,'(f10.1)') feast
      write(cfnorth,'(f10.1)') fnorth

c --- Open CMN file
      open(io9,file='prtmet.cmn',status='REPLACE')

c --- Write CMN information

      write(io9,'(a80)') dashes

      cline=cblank
      cline(1:33)='                     GRID FILE   '
      write(io9,'(a80)') cline

      cline=cblank
      cline(1:33)='                  ---------------'
      write(io9,'(a80)') cline

      cline=cblank
      cline='! IMAP = '//pmap//'  !'
      write(io9,'(a80)') cline

      if(pmap.EQ.'UTM     ') then
         cline=cblank
         cline='! IUTMZN = 123  !'
         write(cline(12:14),'(i3)') iutmzn
         write(io9,'(a80)') cline
         cline=cblank
         cline='! LSOHEM = '//utmhem//'     !'
         write(io9,'(a80)') cline
      elseif(pmap.EQ.'LCC     ') then
         cline=cblank
         cline='! LCCRLAT0 = '//clat0//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! LCCRLON0 = '//clon0//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! LCCRLAT1 = '//clat1//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! LCCRLAT2 = '//clat2//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! LCCFEAST = '//cfeast//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! LCCFNORTH = '//cfnorth//'  !'
         write(io9,'(a80)') cline
      elseif(pmap.EQ.'TTM     ') then
         cline=cblank
         cline='! TTMRLAT0 = '//clat0//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! TTMRLON0 = '//clon0//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! TTMFEAST = '//cfeast//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! TTMFNORTH = '//cfnorth//'  !'
         write(io9,'(a80)') cline
      elseif(pmap.EQ.'PS      ') then
         cline=cblank
         cline='! PSRLAT0 = '//clat0//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! PSRLON0 = '//clon0//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! PSRLAT1 = '//clat1//'  !'
         write(io9,'(a80)') cline
      elseif(pmap.EQ.'EM      ') then
         cline=cblank
         cline='! EMRLAT0 = '//clat0//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! EMRLON0 = '//clon0//'  !'
         write(io9,'(a80)') cline
      elseif(pmap.EQ.'LAZA    ') then
         cline=cblank
         cline='! LZRLAT0 = '//clat0//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! LZRLON0 = '//clon0//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! LAZAFEAST = '//cfeast//'  !'
         write(io9,'(a80)') cline
         cline=cblank
         cline='! LAZAFNORTH = '//cfnorth//'  !'
         write(io9,'(a80)') cline
      endif

      cline=cblank
      cline='! DATUM = '//datum//'  !'
      write(io9,'(a80)') cline

      cline=cblank
      cline='! XREFKM = '//cxo//'  !'
      write(io9,'(a80)') cline
      cline=cblank
      cline='! YREFKM = '//cyo//'  !'
      write(io9,'(a80)') cline
      cline=cblank
      cline='! DGRIDKM = '//cdo//'  !'
      write(io9,'(a80)') cline
      write(io9,*)'! NX = ',nx,' !'
      write(io9,*)'! NY = ',ny,' !'
      write(io9,*)'! NZ = ',nz,' !'
      write(io9,*)'! XBTZ = ',jbtz,' !'

      do i=1,nz+1
         write(io9,99) NINT(zface(i))
      enddo
99    format('! ZFACE = ',i6,' !')

      close(io9)
      return
      end
