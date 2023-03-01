c-----------------------------------------------------------------------
c --- COMMON BLOCK /QA/ -- Model QA parameters                   MAKEGEO
c-----------------------------------------------------------------------
      character*12 ver,level
      character*8 rtime
      character*10 rdate
c
      common/QA/ver,level,rcpu,rtime,rdate
c
c-----------------------------------------------------------------------
c     DEFINITIONS  [i]=integer   [r]=real   [l]=logical   [c]=character
c-----------------------------------------------------------------------
c ver            version number of MAKEGEO                           [c]
c level          level number of MAKEGEO                             [c]
c rcpu           computed CPU time of the run                        [r]
c rtime          system time at start of run (HH:MM:SS)              [c]
c rdate          system date at start of run (MM-DD-YY)              [c]
c-----------------------------------------------------------------------
