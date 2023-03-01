#!/usr/bin/env python
#-*- coding: utf-8 -*-
# encoding: utf-8
from __future__ import unicode_literals

import os, sys, glob
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.pyplot as plt
import numpy as np
import multiprocessing
import time
from mpl_toolkits.basemap import Basemap
import matplotlib.colors as colors
import datetime
from pyproj import Proj
import pyproj
import math
from scipy.spatial.distance import cdist
from matplotlib.patches import Polygon
from matplotlib.collections import PatchCollection
from matplotlib.patches import PathPatch
import utm

#--------------------------------------------------
# Estructura las funciones de este script:
#--------------------------------------------------

#    main()
#        domains()
#            utmToLatLng()
#            grad_to_utm()
#            wrfcheck_ij()
#            centroide()

#        starting()
#            namelist_calwrf()
#            namelist_terrel()
#            namelist_ctgproc()
#            namelist_makegeo()
#            namelist_calmet()
#            namelist_calpuff()
#            namelist_calpost()
#            namelist_prtmet()

#        fload()

#        fplot()
#            cnv_to_rgb()
#            cm_precip()

#--------------------------------------------------


def main():

    #---------------------------
    # Edit parameters
    #---------------------------

    fdate    = '2023022700' # sys.argv[1] # '2021031900'

    # static dirs

    data_dir = "/home/adrian/Desktop/calpuff/data"            # WRF data
    base_dir = "/home/adrian/Desktop/calpuff/sources"         # Calmet/Calpuff source code
    run_dir  = "/home/adrian/Desktop/calpuff/runs/"            # Runs dir
    shps_dir = "/home/adrian/Desktop/calpuff/shp"             # SHP data

    # Directory for sourcode and runs desing

    # .../calpuff_gamma/
    # .../calpuff_gamma/data
    # .../calpuff_gamma/data/YYYYMMDDHH
    # .../calpuff_gamma/sources
    # .../calpuff_gamma/sources/CALMET
    # .../calpuff_gamma/sources/CALPOST
    # .../calpuff_gamma/sources/CALPUFF
    # .../calpuff_gamma/sources/CTGPROC
    # .../calpuff_gamma/sources/CALWRF
    # .../calpuff_gamma/sources/MAKEGEO
    # .../calpuff_gamma/sources/PRTMET
    # .../calpuff_gamma/sources/TERREL
    # .../calpuff_gamma/shp
    # .../calpuff_gamma/runs
    # .../calpuff_gamma/runs/PROVINCIA
    # .../calpuff_gamma/runs/PROVINCIA/YYYYMMDDHH/

    # other parameters

    PROCESS_LIMIT = 1  # single CPU core=1, i3=4, i5=4, i7=8, etc...
    sources       = 'sources_coord.txt'  # Gamma User TXT

    # Domain generator from Gamma User TXT
    outdoms,oname,calsources = domains(sources,base_dir)


    #---------------------------
    # Run model and plot
    #---------------------------

    if PROCESS_LIMIT == 1:

        #---------------------------
        # run in serial mode
        #---------------------------

        for l in range(len(outdoms[:,0])):
#            try:
                starting(fdate,data_dir,base_dir,run_dir,shps_dir,oname[l],str(outdoms[l,0]),str(outdoms[l,1]),str(outdoms[l,2]),str(outdoms[l,3]),str(outdoms[l,4]),str(outdoms[l,5]),str(outdoms[l,6]),str(outdoms[l,7]),str(outdoms[l,8]),str(outdoms[l,9]),str(int(outdoms[l,10])),str(int(outdoms[l,11])),str(int(outdoms[l,12])),str(int(outdoms[l,13])),calsources[l,:])
#            except:
#                continue

    else:

        #---------------------------
        # run with multiprocess
        #---------------------------

        for l in range(len(outdoms[:,0])):

            process=multiprocessing.Process(target=starting,args=(fdate,data_dir,base_dir,run_dir,shps_dir,oname[l],str(outdoms[l,0]),str(outdoms[l,1]),str(outdoms[l,2]),str(outdoms[l,3]),str(outdoms[l,4]),str(outdoms[l,5]),str(outdoms[l,6]),str(outdoms[l,7]),str(outdoms[l,8]),str(outdoms[l,9]),str(int(outdoms[l,10])),str(int(outdoms[l,11])),str(int(outdoms[l,12])),str(int(outdoms[l,13])),calsources[l,:]))
            while(len(multiprocessing.active_children()) == PROCESS_LIMIT):
                time.sleep(1)
            process.start()



def starting(fdate,data_dir,base_dir,run_dir,shps_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF,calsource):


    #----------------------------------------------------------------------------------
    #
    #   RUN CALMET / CALPUFF
    #
    #----------------------------------------------------------------------------------

    runs_dir=run_dir+"/"+PROVINCIA+"/"+fdate  # Runs dir

    os.system('mkdir -p '+runs_dir)


    #-------------------
    # STARTING SIMULATION
    #-------------------

    os.chdir(runs_dir)

    # topography
    os.system('ln -sf '+base_dir+'/TERREL/v3.686/w100n40.dem '+runs_dir)

    # coastline
    os.system('ln -sf '+base_dir+'/TERREL/v3.686/gshhs_f.b '+runs_dir)
    os.system('ln -sf '+base_dir+'/TERREL/v3.686/coast.bln '+runs_dir)


    # land use
    os.system('ln -sf '+base_dir+'/CTGPROC/v2.684/nausgs2_0l.img '+runs_dir)

    # linking wrf data
    os.system('ln -sf '+data_dir+'/'+fdate+'/wrfout_d0* '+runs_dir)


    #-------------------
    # RUN TERREL
    #-------------------

    print("\n\n#############################################")
    print("TERREL ... ")
    print("#############################################\n\n")

    #write namelist
    text = namelist_terrel(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF)
    f = open(runs_dir+'/TERREL.INP','w')
    f.write(text)
    f.close()

    os.system('ln -sf '+base_dir+'/TERREL/v3.686/terrel.exe '+runs_dir)

    os.system('./terrel.exe TERREL.INP')


    #-------------------
    # RUN CTGPROC
    #-------------------

    print("\n\n#############################################")
    print("CTGPROC ... ")
    print("#############################################\n\n")


    #write namelist
    text = namelist_ctgproc(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF)
    f = open(runs_dir+'/CTGPROC.INP','w')
    f.write(text)
    f.close()

    os.system('ln -sf '+base_dir+'/CTGPROC/v2.684/ctgproc.exe '+runs_dir)

    os.system('./ctgproc.exe CTGPROC.INP')


    #-------------------
    # RUN MAKEGEO
    #-------------------

    print("\n\n#############################################")
    print("MAKEGEO ... ")
    print("#############################################\n\n")


    #write namelist
    text = namelist_makegeo(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF)
    f = open(runs_dir+'/MAKEGEO.INP','w')
    f.write(text)
    f.close()

    os.system('ln -sf '+base_dir+'/MAKEGEO/v2.291/makegeo.exe '+runs_dir)

    os.system('./makegeo.exe MAKEGEO.INP')


    #-------------------
    # RUN CALWRF
    #-------------------

    print("\n\n#############################################")
    print("CALWRF ... ")
    print("#############################################\n\n")


    #write namelist
    text = namelist_calwrf(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF)
    f = open(runs_dir+'/CALWRF.INP','w')
    f.write(text)
    f.close()

    os.system('ln -sf '+base_dir+'/CALWRF/binary_linux/calwrf.exe '+runs_dir)

    os.system('./calwrf.exe CALWRF.INP')


    #-------------------
    # RUN CALMET
    #-------------------

    print("\n\n#############################################")
    print("CALMET ... ")
    print("#############################################\n\n")


    #write namelist
    text = namelist_calmet(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF)
    f = open(runs_dir+'/CALMET.INP','w')
    f.write(text)
    f.close()

    os.system('ln -sf '+base_dir+'/CALMET/v6.326/calmet.exe '+runs_dir)

    os.system('./calmet.exe CALMET.INP')


    #-------------------
    # RUN CALPUFF
    #-------------------

    print("\n\n#############################################")
    print("CALPUFF ... ")
    print("#############################################\n\n")

    #write namelist
    text = namelist_calpuff(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF,calsource)
    f = open(runs_dir+'/CALPUFF.INP','w')
    f.write(text)
    f.close()

    os.system('ln -sf '+base_dir+'/CALPUFF/v6.262/calpuff.exe '+runs_dir)

    os.system('./calpuff.exe CALPUFF.INP')


    #-------------------
    # RUN CALPOST
    #-------------------

    print("\n\n#############################################")
    print("CALPOST ... ")
    print("#############################################\n\n")

    for spec in ['CO','NOX','SO2']:

        #write namelist
        text = namelist_calpost(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF,spec)
        f = open(runs_dir+'/CALPOST.INP','w')
        f.write(text)
        f.close()

        os.system('ln -sf '+base_dir+'/CALPOST/v6.221/calpost.exe '+runs_dir)

        os.system('./calpost.exe CALPOST.INP')


    #-------------------
    # RUN PRTMET
    #-------------------

    print("\n\n#############################################")
    print("PRTMET ... ")
    print("#############################################\n\n")

    #write namelist
    text = namelist_prtmet(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF)
    f = open(runs_dir+'/PRTMET.INP','w')
    f.write(text)
    f.close()

    os.system('ln -sf '+base_dir+'/PRTMET/v4.481/prtmet.exe '+runs_dir)

#    os.system('./prtmet.exe PRTMET.INP')


    #--------------------------------------------
    # Plotting data
    #--------------------------------------------

    print("\n\n#############################################")
    print("Python Plot ... ")
    print("#############################################\n\n")

    fload(fdate,data_dir,base_dir,runs_dir,shps_dir,PROVINCIA,CENLON,CENLAT)



def fload(fdate,data_dir,base_dir,runs_dir,shps_dir,PROVINCIA,CENLON,CENLAT):

    # Create meteogram
    os.system('ln -sf '+base_dir+'/my_meteogram3x2.py '+runs_dir)
    print('python3 '+runs_dir+'/my_meteogram3x2.py '+CENLON+' '+CENLAT+' '+PROVINCIA)
    os.system('python3 '+runs_dir+'/my_meteogram3x2.py '+CENLON+' '+CENLAT+' '+PROVINCIA)

    # Create maps
    for spec in ['so2','co','nox']:

        conc_name = 'rank(all)_'+spec+'_1hr_conc.dat'

        xg0,yg0,dg = (np.loadtxt(conc_name,skiprows=6)[:,0]*1000,np.loadtxt(conc_name,skiprows=6)[:,1]*1000,np.loadtxt(conc_name,skiprows=6)[:,2])

        xg,yg = utmToLatLng(17, xg0, yg0, northernHemisphere=True)
        xg,yg = (np.array(xg),np.array(yg))

        fplot(yg.reshape(90,120),xg.reshape(90,120),dg.reshape(90,120),fdate,spec,conc_name,data_dir,base_dir,runs_dir,shps_dir,PROVINCIA,CENLON,CENLAT)



def fplot(xg,yg,dg,fdate,spec,conc_name,data_dir,base_dir,runs_dir,shps_dir,PROVINCIA,CENLON,CENLAT):

    # save grids to txt
    np.savetxt(runs_dir+'/'+fdate+'_spec_lons.txt', xg, delimiter=',', fmt='%1.6f')
    np.savetxt(runs_dir+'/'+fdate+'_spec_lats.txt', yg, delimiter=',', fmt='%1.6f')
    np.savetxt(runs_dir+'/'+fdate+'_spec_'+spec+'.txt', dg, delimiter=',', fmt='%1.6f')

    # centrado
    xg,yg,dg = (xg[30:60,40:80],yg[30:60,40:80],dg[30:60,40:80])

    # tamaño de figura
    width = 10.125
    height = 6.6

    m = Basemap(projection='cyl', llcrnrlon=np.min(xg), llcrnrlat=np.min(yg), urcrnrlon=np.max(xg), urcrnrlat=np.max(yg),  resolution='h')

    fig = plt.figure(1,figsize=(width,height),dpi=300)
    ax = fig.add_subplot(111)

    srcpto = '\nFuente: '+str(PROVINCIA)+' ( LON: '+str(CENLON)+' LAT: '+str(CENLAT)+' )'
    plt.title('Promedio horario de concentraciones de emisiones de '+spec.upper()+' simuladas con CALMET-CALPUFF [ug/m3]'+srcpto, fontsize=8,bbox=dict(facecolor='white', alpha=0.65), x=0.5,y=.93,weight = 'demibold',style='oblique', stretch='normal', family='sans-serif')

    m.scatter(np.array([float(CENLON)],dtype=np.float32), np.array([float(CENLAT)],dtype=np.float32), marker='+',color='r', zorder=6, s=5.8, label='Fuentes Emis.')

#    m.drawmapboundary(fill_color='#98acc0')#, zorder=2)
#    m.fillcontinents(color='#dfdcd8',lake_color='#98acc0', alpha=0.9)#, zorder=2)
#    m.drawcoastlines(linewidth = 0.35)#, zorder=2)

#    m.readshapefile(shps_dir+'/gis_osm_roads_free_1', 'roads', color='grey', linewidth=0.18, zorder=4)
#    m.readshapefile(shps_dir+'/gis_osm_waterways_free_1', 'river', color='blue', linewidth=0.18, zorder=4)
#    m.readshapefile(shps_dir+'/municipios', 'municipios', color='k', linewidth=0.3, zorder=5)
#    m.readshapefile(shps_dir+'/provincias', 'roads', color='k', linewidth=0.6, zorder=6)

    ################# Corregir direcciones

    m.readshapefile(shps_dir+'/gis_osm_roads_free_1', 'roads', color='grey', linewidth=0.3, zorder=3)
    m.readshapefile(shps_dir+'/gis_osm_waterways_free_1', 'river', color='blue', linewidth=0.5, zorder=3)
    m.readshapefile(shps_dir+'/municipios', 'municipios', color='k', linewidth=0.8, zorder=4)
#    m.readshapefile(shps_dir+'/provincias', 'roads', color='k', linewidth=0.6, zorder=6)

    m.drawmapboundary(fill_color='#99b3cc')
    m.readshapefile(shps_dir+'/provincias', 'comarques', drawbounds = False)

    patches   = []
    for info, shape in zip(m.comarques_info, m.comarques):
            patches.append( Polygon(np.array(shape), True) )
    ax.add_collection(PatchCollection(patches, facecolor= '#f2efe9', edgecolor="#505050", linewidths=1.5, zorder=2))

    ################

    m.drawmeridians(range(0, 360, 3),labels=[1,0,0,1],fontsize=8, linewidth=0)
    m.drawparallels(range(-180, 180, 3),labels=[1,0,0,1],fontsize=8, linewidth=0)

    cmap1,norm1,clevs1 = cm_precip()

    masked_array = np.ma.masked_where(dg <= 0.02,dg)
    CLFRA=m.contourf(xg,yg,dg, clevs1, cmap=cmap1, zorder=5, extend="max", latlon=True, alpha=0.8) 

    CLFRA.cmap.set_over((0.39,0,0.34))
    cbar = m.colorbar(CLFRA,location='right',pad=-0.01, extend='max', extendfrac='auto', ticks=clevs1, spacing='uniform')

    plt.savefig(runs_dir+'/'+fdate+'_'+conc_name+'.png', dpi=300, bbox_inches='tight', pad_inches=0)
    plt.close()


#--------------------------------------------------------------------------------------------------------
# Tool funtions for colormap
#--------------------------------------------------------------------------------------------------------

def cnv_to_rgb(clist):

    newcolors = []
    for i in range(len(clist)):
        newcolors.append((float(clist[i][0])/255,float(clist[i][1])/255,float(clist[i][2])/255,float(clist[i][3])/255))

    return newcolors    



def cm_precip():

    a = np.array([0.005,5,10,20,30,40,50,75,100,150,200,250])
#    a = [0.5,1.0,1.5,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0]

    clevs = np.array(a)
    norm = [(float(i)-min(a))/(max(a)-min(a)) for i in a]

    C = np.array([[35,35,212],
                    [110,138,255],
                    [53,92,255],
                    [0,160,30],
                    [60,190,60],
                    [180,210,110],
                    [185,250,110],
                    [255,250,20],
                    [255,165,10],
                    [230,0,0],
                    [190,0,0],
                    [130,0,0]])
    COLORS = []
    for i, n in enumerate(norm):
        COLORS.append((n, np.array(C[i])/255.))

    cmap = colors.LinearSegmentedColormap.from_list("conc", COLORS)
#    cmap=plt.get_cmap('jet')

    return cmap,norm,clevs


#--------------------------------------------------------------------------------------------------------
# Tool funtions for namelist generators
#--------------------------------------------------------------------------------------------------------

def namelist_calwrf(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF):

    YYYY0 = str(fdate[0:4])
    MM0   = str(fdate[4:6])
    DD0   = str(fdate[6:8])
    HH0   = str(fdate[8:10])

    endTime = str(datetime.datetime.strptime(fdate, "%Y%m%d%H") + datetime.timedelta(hours=24))

    YYYY1 = str(endTime[0:4])
    MM1   = str(endTime[5:7])
    DD1   = str(endTime[8:10])
    HH1   = str(endTime[11:13])

    # This namelist is only for 00z initialization


    text='''Create 3D.DAT file for WRF output
calwrf.lst          ! Log file name
{1}.m3d ! Output file name
{13},{15},{12},{14},1,14    ! Beg/End I/J/K ("-" for all)
{16}{17}{18}{19}          ! Start datetime (UTC yyyymmddhh, "-" for all)
{20}{21}{22}{23}          ! End   datetime (UTC yyyymmddhh, "-" for all)
25                   ! Number of WRF output files
wrfout_d03_{16}-{17}-{18}_00:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_01:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_02:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_03:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_04:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_05:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_06:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_07:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_08:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_09:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_10:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_11:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_12:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_13:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_14:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_15:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_16:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_17:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_18:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_19:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_20:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_21:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_22:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{16}-{17}-{18}_23:00:00 ! File name of wrf output (Loop over files
wrfout_d03_{20}-{21}-{22}_00:00:00 ! File name of wrf output (Loop over files

*****   Below are comments *****************************************

Create 3D.DAT file for WRF output (full domain and time-period)
calwrf.lst           ! Log file name
calwrf_em.m3d        ! Output file name
-1,-1,-1,-1,-1,-1    ! Beg/End I/J/K ("-" for all)
-1                   ! Start datetime (UTC yyyymmddhh, "-" for all)
-1                   ! End   datetime (UTC yyyymmddhh, "-" for all)
1                    ! Number of WRF output files
wrfout_070427.dat    ! File name of wrf output (Loop over files)


Create 3D.DAT file for WRF output (full domain and time-period)
calwrf.lst          ! Log file name
calwrf.dat3         ! Output file name
-9,-9,-9,-9,-9,-9   ! Beg/End I/J/K ("-" for all)
-9                  ! Start datetime (UTC yyyymmddhh, "-" for all)
-9                  ! End   datetime (UTC yyyymmddhh, "-" for all)
1                   ! Number of WRF output files
wrfout_d01_2007-01-01_000000 ! File name of wrf output (Loop over files)


Create 3D.DAT file for WRF output (sub domain and time-period)
calwrf.lst          ! Log file name
calwrf_070427.m3d   ! Output file name
1,163,1,121,1,27    ! Beg/End I/J/K
2007042700          ! Start datetime (UTC yyyymmddhh, "-" for all)
2007042704          ! End   datetime (UTC yyyymmddhh, "-" for all)
1                   ! Number of WRF output files
wrfout_070427.dat   ! File name of wrf output (Loop over files)

'''.format(fdate,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF, YYYY0, MM0, DD0, HH0, YYYY1, MM1, DD1, HH1)
#           0       1         2     3     4    5    6    7    8      9      10      11     12     13     14     15      16    17   18  19    20    21    22   23

#    print(fdate,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF, YYYY0, MM0, DD0, HH0, YYYY1, MM1, DD1, HH1)
#    2021031900 la_habana -82.343642 23.091708416666663 -82.94066500540053 -81.73736299459947 22.686667912616258 23.496748920717067 300.6346439896687 424.7118322637424 2510.1372457008 2598.704617965251 118 90 158 142 2021 03 19 00 2021 03 20 00
# 300.6346439896687 424.7118322637424 2510.1372457008 2598.704617965251 118    90     158    142
# UTM_X1            UTM_X2            UTM_Y1          UTM_Y2            I1_WRF,J1_WRF,I2_WRF,J2_WRF
#int(outdoms[l,10]):int(outdoms[l,12]),int(outdoms[l,11]):int(outdoms[l,13])
#              12                 14                 13                  15

    return text



def namelist_terrel(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF):

    YYYY0 = str(fdate[0:4])
    MM0   = str(fdate[4:6])
    DD0   = str(fdate[6:8])
    HH0   = str(fdate[8:10])

    endTime = str(datetime.datetime.strptime(fdate, "%Y%m%d%H") + datetime.timedelta(hours=24))

    YYYY1 = str(endTime[0:4])
    MM1   = str(endTime[5:7])
    DD1   = str(endTime[8:10])
    HH1   = str(endTime[11:13])

    # This namelist is only for 00z initialization

    text='''-------------------------------------------------------------------------------
                     TERREL PROCESSOR CONTROL FILE
                 ------------------------------------

    TERREL accepts terrain surface elevation data from a number of 
    digital databases and forms grid-cell averages or point-values for
    use in particular dispersion modeling systems.  For the CALPUFF
    system, TERREL produces a gridded terrain file for the MAKEGEO
    processor, and it produces a file of point-values for discrete
    receptors for CALPUFF.  Use TERREL one of more times to build the 
    requested file.

-------------------------------------------------------------------------------

INPUT GROUP: 0 -- Input and Output File Names
--------------

-------------
Subgroup (0a)
-------------

    Number of Terrain Data Files provided in Subgroup 0b

    By default, no data files are expected, and running TERREL
    without input data files will allow it to complete its set-up
    procedures, and report the number of data files needed to cover
    the specified modeling domain.  This information can be helpful
    when assembling the data files for an application.

    (NTDF)                Default: 0     ! NTDF = 1  !

    Other Input and Output Files:
    -----------------------------

    Default Name  Type          File Name
    ------------  ----          ---------
    TERREL.DAT       output    ! OUTFIL = Terr1km.dat      !
    TERREL.LST       output    ! LSTFIL = Terrel.lst      !
    TERREL.GRD       output    ! PLTFIL = Qaterr.grd      !
    ---------------------------------------------------------
    (Save-files)
    PREV.SAV         input     * PREVFIL = Terr1km.sav *
    TERREL.SAV       output    * SAVEFIL = Terr1km.sav *
    ---------------------------------------------------------
    (Discrete (X,Y) Point Files)
    XYINP.DAT        input    * XYINP =            *
    XYOUT.DAT        output   * XYOUT =            *
    ---------------------------------------------------------
    (Coastal Data)
       USGS Global Self-consistent Hierarchical High-resolution
       Shoreline Database (GSHHS)    
    GSHHS_F.B        input    ! GSHHSIN = gshhs_f.b   !
       Processed coastline polygons for
       TERREL grid(BLN)
    COAST.BLN        input or  ! COASTBLN = Coast.bln    !
                     output

    ---------------------------------------------------------
    All file names will be converted to lower case if LCFILES = T
    Otherwise, if LCFILES = F, file names will be converted to UPPER CASE
    (LCFILES)       Default: T       ! LCFILES = T    !
             T = lower case
             F = UPPER CASE

    NOTE: file/path names can be up to 70 characters in length

!END!


-------------
Subgroup (0b)
-------------

    The following NTDF Terrain Data Files are processed.
    Enter NTDF lines identifying the file name for each,
    followed by a group terminator.  The type of database
    for each file is designated by the assignment name:

    (USGS90)   designates USGS 1-deg DEM files (~90m)
    (USGS30)   designates USGS 7.5-min DEM files (typically 30m)
    (ARM3)     designates ARM3 terrain data files(~900m)
    (3CD)      designates 3CD (binary) 1-deg DEM files (~90m)
    (DMDF)     designates Canadian DMDF files (~100m)
    (SRTM1)    designates 1-sec Shuttle RADAR Topo Mission files (~30m)
    (SRTM3)    designates 3-sec Shuttle RADAR Topo Mission files (~90m)
    (GTOPO30)  designates GTOPO30 30-sec data (~900m)
    (USGSLA)   designates USGS Lambert Azimuthal data (~1000m)
    (NZGEN)    designates New Zealand Generic data files
    (GEN)      designates Generic data files

     1 !GTOPO30 = W100N40.DEM !     !END!
    

-------------
Subgroup (0c)
-------------

    Datum-Region
    ------------

    The Datum-Region for coordinates in each of the input Terrain Data Files
    may be identified in the header records of the file.  Check the file
    documentation and change these defaults if needed.  The list of Datum-Regions
    with official transformation parameters is provided by the National Imagery and
    Mapping Agency (NIMA)

    Datum-Region for input Terrain Data File coordinates
    (DUSGS90)       Default: WGS-72  ! DUSGS90  = WGS-72    !
    (DUSGS30)       Default: NAS-C   ! DUSGS30  = NAS-C     !
    (DARM3)         Default: NAS-C   ! DARM3    = NAS-C     !
    (D3CD)          Default: WGS-72  ! D3CD     = WGS-72    !
    (DDMDF)         Default: NAS-C   ! DDMDF    = NAS-C     !
    (DSRTM1)        Default: WGS-96  ! DSRTM1   = WGS-96    !
    (DSRTM3)        Default: WGS-96  ! DSRTM3   = WGS-96    !
    (DGTOPO30)      Default: WGS-84  ! DGTOPO30 = WGS-84    !
    (DUSGSLA)       Default: ESR-S   ! DUSGSLA  = ESR-S     !
    (DNZGEN)        Default: WGS-84  ! DNZGEN   = WGS-84    !
    (DGEN)          Default: WGS-84  ! DGEN     = WGS-84    !

    Datum-region for input Coastal Data File coordinates
    (DWVS)          Default: WGS-84  ! DWVS     = WGS-84    !

!END!




--------------------------------------------------------------------------------

INPUT GROUP: 1 -- Processing Options
--------------

    Intermediate data for the terrain grid are saved in a binary
    file for subsequent applications of TERREL.  When TERREL is 
    applied more than once (with different terrain data files), the
    save file must be used to pass previous results along.

      Previous SAVE file used to start this run?
      (LPREV)          Default: F        ! LPREV = F    !
         T = PREV.SAV file is used
         F = PREV.SAV file is NOT used


    TERREL constructs gridded terrain elevations (m MSL), and may also
    estimate the terrain elevation at discrete points by selecting the
    peak elevation within a prescribed distance (km) from each point.    
    When processing discrete points, the XYINP.DAT provides the grid
    coordinates (km) of each point, and may also include a height above
    ground (m) for each point (e.g. for elevated receptors).  The
    structure of the XYINP.DAT file is a free-format text file with 
    either 2 columns (X,Y) or 4 columns (X, Y, Elevation, Height). When
    the 4-column form is used, data in the 3rd column are replaced
    with the elevations obtained from the terrain data base files.

      Report elevations for discrete (X,Y) points?
      (LXY)          Default: F        ! LXY = F       !
         T = Yes (XYINP.DAT and XYOUT.DAT files are used)
         F = No  (XYINP.DAT and XYOUT.DAT files are NOT used)

      Number of data columns in XYINP.DAT file
      (Used only if LXY=T)
      (NXYCOL)       Default: 4   ! NXYCOL = 2     !

  
      Search radius (km) about each (X,Y) for locating terrain peak
      (Used only if LXY=T)
      (XYRADKM)      No Default    ! XYRADKM = 0.15     !


   Some terrain data sets contain void areas where the data are
   missing.  Others may contain areas where data are inaccurate (noisy).
   Both situations occur mostly over oceans or large lakes, but for SRTM
   data it can also occur over land due to the data set still evolving.
   These void (missing) or noisy input data can be replaced in several
   ways.

   Noisy Data ---
   Noise affects SRTM data for oceans and lakes and the adjacent shores,
   due to the scattering effects of water on radar measurements.
   The most obvious occurence of noise is negative elevations for water
   and adjacent land points.  This can be filtered with the specification
   of a minimum acceptable elevation by water/land type.  Extracted
   elevations that are greater than this minimum are retained, while those
   lower than this minimum value can be re-defined as missing for
   subsequent treatment by the missing values processing, or can be
   replaced with either the minimum value or with another default value
   defined for treatment of void (missing) data.  The minimum values must
   be chosen judiciously for the region being treated since some regions
   have valid elevations below MSL.

   Missing data ---
   Cells with missing elevations can be interpolated from surrounding
   cells with valid values, and a maximum search radius is defined.
   Also, if coastline processing has been used, default elevations for
   each water/land type can be defined and used in place of voids.
   This replacement can be carried out as the final step before output
   on a cell-by-cell and receptor-by-receptor basis, or can be carried
   out for values extracted from the terrain files as missing.  This
   latter option is best used only for oceans and lakes.  For oceans
   and lakes it is also possible to not use extracted elevations but
   only use the default.

   Coastline data are used to define coarse water/land type by
   point or cell, for several of the options available for treating
   missing or noisy data.  Coarse water/land type definitions are:
       1 = ocean
       2 = mainland and marine islands
   Coastline data are accepted in the form of either the USGS Global
   Self-consistent Hierarchical High-resolution Shoreline (GSHHS) Database
   file, or a BLN file produced in a previous application for the modeling
   domain (it must have correct grid limits and polygon headers).  The
   processed coastline (BLN) file for the domain is automatically created
   when the GSHHS database is input.  No BLN is created when an existing
   BLN file is input.

     Process coastline data?
     (LCOAST)                   Default: F     ! LCOAST = T     !
       T = Process coastline data
       F = Do not process coastline data

     Read pre-processed coastline data (existing BLN file)?
     (LBLNREAD)                 Default: F      ! LBLNREAD = F !
       T = Use pre-processed coastline data
       F = Process raw coastline data

   Noisy Data Replacement Options
   ------------------------------

   --Filtering with minimum elevations by water/land type (2 values)
     (INOISEREP)                Default: 0,0
        0 = Do not check for noise
        1 = Set values lower than minimum to missing
        2 = Replace values lower than minimum with minimum value
        3 = Replace values lower than minimum with default value
            (set in TERDEF below)

     Minimum terrain elevations (m) for noise detection (2 values)
     (ZNOISE)                   Default: 0.,1.

                       |      |mainland
                       |      |& marine
                       |ocean |islands 
                       ------ ---------
          ! INOISEREP = 2,2  !
          !    ZNOISE =   0. ,      0.   !

   Missing Data Replacement Options
   --------------------------------

   --Application of default elevations by water/land type (2 values)
     (ITERREP)                  Default: 3,0
        0 = Do not replace voids
        1 = Replace voids on output only
        2 = Replace void point values on extraction and voids on output
        3 = Always replace all values for this water type with default
            (only valid for oceans and lakes)

     Default terrain elevations (m) (2 values)
     (TERDEF)                   Default: 0.,0.

                       |      |mainland |
                       |      |& marine |
                       |ocean |islands  |
                       ------ --------- 
            ! ITERREP =  3,      2  !
            !  TERDEF =  0. ,      1.   !


   --Carry out interpolation to fill void cells?
     (LVOIDFILL)                 Default: F      ! LVOIDFIL = T     !
       T = Try interpolation to fill void cells
       F = Do not try interpolation to fill void cells

   --Search radius (km) around grid cells for interpolation to fill
     voids   (Should be several times larger than DGRIDKM)
     (Used only if LVOIDFIL=T)
     (CELLRADKM)                No Default       ! CELLRADKM = 5.  !

   Terrain data may be prepared for one of several models, and the
   structure of the output data file varies accordingly.

     Structure of output TERREL.DAT file
      (IMODEL)           Default: 1     ! IMODEL = 1     !
        1 = CALMET        (grid-cell-average elevations)
        2 = MESOPAC       (grid-cell-average elevations)
        3 = ISC POLAR     (grid-cell-peak elevations)
        4 = ISC CARTESIAN (grid-cell-peak elevations)
        5 = NUATMOS       (grid-cell-average elevations)
        6 = Generic       (grid-cell-average elevations)

   Warnings are posted to the list file if grid cells contain fewer
   data points than ITHRES(%) of the mean for all cells.  Such a
   warning may indicate that insufficient data coverage is provided by
   the terrain data files that are processed.

Threshold (%) of the average number of data points in a cell
      (ITHRES)           Default: 75   ! ITHRES =  75   !
   

!END!

--------------------------------------------------------------------------------

INPUT GROUP: 2 -- Map Projection and Grid Information for Output
--------------

    Projection
    ----------

    Map projection for all X,Y (km)
    (PMAP)           Default: UTM    ! PMAP = UTM  !  Aqui
       UTM :  Universal Transverse Mercator
       TTM :  Tangential Transverse Mercator
       LCC :  Lambert Conformal Conic
       PS  :  Polar Stereographic
       EM  :  Equatorial Mercator
       LAZA:  Lambert Azimuthal Equal Area

    False Easting and Northing (km) at the projection origin
    (Used only if PMAP = TTM, LCC, or LAZA)
    (FEAST)          Default=0.0   ! FEAST  =  0.0  !
    (FNORTH)         Default=0.0   ! FNORTH =  0.0  !

    UTM ZONE (1 to 60)
    (Used only if PMAP = UTM)
    (IUTMZN)         No Default   ! IUTMZN = 17     !

    Hemisphere for UTM projection?
    (Used only if PMAP = UTM)
    (UTMHEM)         Default: N   ! UTMHEM = N    !
        N    :  Northern hemisphere projection
        S    :  Southern hemisphere projection

    Latitude and Longitude (decimal degrees) for projection
    (Used only if PMAP = TTM, LCC, PS, EM, or LAZA)
    (RLAT0)           No Default   ! RLAT0 =  0N  !
    (RLON0)           No Default   ! RLON0 =  0W   !

        TTM :  RLON0 identifies central (true N/S) meridian of projection
               RLAT0 selected for convenience
        LCC :  RLON0 identifies central (true N/S) meridian of projection
               RLAT0 selected for convenience
        PS  :  RLON0 identifies central (grid N/S) meridian of projection
               RLAT0 selected for convenience
        EM  :  RLON0 identifies central meridian of projection
               RLAT0 is REPLACED by 0.0N (Equator)
        LAZA:  RLON0 identifies longitude of tangent-point of mapping plane
               RLAT0 identifies latitude of tangent-point of mapping plane

    Matching parallel(s) of latitude (decimal degrees) for projection
    (Used only if PMAP = LCC or PS)
    (RLAT1)           No Default   ! RLAT1 = 0N  !
    (RLAT2)           No Default   ! RLAT2 = 0N  !
        LCC :  Projection cone slices through Earth's surface at RLAT1 and RLAT2
        PS  :  Projection plane slices through Earth at RLAT1
               (RLAT2 is not used)

    -------------
    NOTE:  Latitudes and longitudes should be positive, and include a
           letter N, S, E, or W indicating north or south latitude, and
           east or west longitude.  For example,
           35.9  N Latitude =   35.9N
           118.7 E Longitude = 118.7E


    Datum-Region
    ------------

    The Datum-Region for the output coordinates is identified by a character
    string.  Many mapping products currently available use the model of the 
    Earth known as the World Geodetic System 1984 (WGS-84).  Other local
    models may be in use, and their selection in TERREL will make its output 
    consistent with local mapping products.  The list of Datum-Regions with 
    official transformation parameters is provided by the National Imagery 
    and Mapping Agency (NIMA).

    Datum-Region for output coordinates
    (DATUM)           Default: WGS-84  ! DATUM =  WGS-84 !


    Grid
    ----

    Grid Type
    (IGRID)      Default = 1         ! IGRID = 1   !
       1 = Cartesian, with reference point in Lower Left CORNER 
           of cell (1,1)  -- CALMET Convention --
       2 = Cartesian, with reference point at CENTER of cell (1,1)
           (NOT compatible with CALMET)
       3 = Polar, with reference point at center of rings
    ----------
    NOTE: Cell (1,1) is at the SW corner of the grid

    Reference point coordinates X,Y (km) for grid
    where X is Easting, Y is Northing
    (XREFKM)        No Default         ! XREFKM = {8}     !
    (YREFKM)        No Default         ! YREFKM = {10}     !

    Cartesian grid definition
    (Used only if IGRID=1,2)
    No. X grid cells (NX)  No Default         ! NX = 120     !
    No. Y grid cells (NY)  No Default         ! NY = 90     !

    Grid Spacing (km) (DGRIDKM) No Default         ! DGRIDKM = 1.  !

    Polar grid definition -- enter ring distances and ray angles
                             in Input Group 3
    (Used only if IGRID=3)
    No. of rings (NRING)     No Default         ! NRING = 0    !
    No. of radials (NRAYS)   No Default         ! NRAYS = 0    !

    Elevation processing method for polar grid
    (Used only if IGRID=3)
    (IPROC)                  Default = 2        ! IPROC = 2     !
       1 = NORMAL: terrain data for point at the intersection of ring
                   and ray is extracted from the region bounded by
                   rings and radials halfway to the adjacent rings and 
                   radials
       2 = SCREEN: terrain data for point at the intersection of ring
                   and ray is extracted from the region bounded by the
                   current ring and the next larger ring, and radials 
                   halfway to the adjacent radials

!END!

--------------------------------------------------------------------------------

INPUT GROUP: 3 -- Polar Grid Ring Distances (km) and Ray Angles (deg)
--------------

    Enter NRING lines identifying the radius (DISKM) of each ring in
    the polar grid, using a group terminator on each line.
    (Enter only if IGRID = 3)
    * DISKM =    1.5   * *END*
    * DISKM =    3.0   * *END*

    Enter NRAYS lines identifying the angle (ANGDEG) from North of
    each radial in the polar grid, using a group terminator on each line.
    (Enter only if IGRID = 3)
    * ANGDEG =    0.   * *END*
    * ANGDEG =   45.   * *END*
    * ANGDEG =   90.   * *END*

-------------------------------------------------------------------------------
    NIMA Datum-Regions  (Documentation Section)
-------------------------------------------------------------------------------
WGS-84    WGS-84 Reference Ellipsoid and Geoid, Global coverage (WGS84)
NAS-C     NORTH AMERICAN 1927 Clarke 1866 Spheroid, MEAN FOR CONUS (NAD27)
NAR-C     NORTH AMERICAN 1983 GRS 80 Spheroid, MEAN FOR CONUS (NAD83)
NWS-84    NWS 6370KM Radius, Sphere
ESR-S     ESRI REFERENCE 6371KM Radius, Sphere
'''.format(fdate,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF, YYYY0, MM0, DD0, HH0, YYYY1, MM1, DD1, HH1)
#           0       1         2     3     4    5    6    7    8      9      10      11     12     13     14     15      16    17   18  19    20    21    22   23

    return text


def namelist_ctgproc(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF):

    YYYY0 = str(fdate[0:4])
    MM0   = str(fdate[4:6])
    DD0   = str(fdate[6:8])
    HH0   = str(fdate[8:10])

    endTime = str(datetime.datetime.strptime(fdate, "%Y%m%d%H") + datetime.timedelta(hours=24))

    YYYY1 = str(endTime[0:4])
    MM1   = str(endTime[5:7])
    DD1   = str(endTime[8:10])
    HH1   = str(endTime[11:13])

    # This namelist is only for 00z initialization

    text='''-------------------------------------------------------------------------------

                 CTGPROC PROCESSOR CONTROL FILE
                 ------------------------------

  CTGPROC reads a Land Use and Land Cover (LULC) data file and determines
  fractional land use for each grid cell in a user-specified gridded
  domain.  If the domain requires multiple files, CTGPROC is applied
  iteratively (continuation option) to build the land use grid
  incrementally.  The LULC file must be either a compressed USGS 
  Composite Theme Grid (CTG) format (available for the U.S.), a 
  USGS Global format, or the New Zealand Generic format.

-------------------------------------------------------------------------------

INPUT GROUP: 0 -- Input and Output Files
--------------

-------------
Subgroup (0a)
-------------

     Number of Land Use Data Files provided in Subgroup 0b

     (NDBF)                     Default: 0      ! NDBF = 1 !


     Other Input and Output files:
     -----------------------------

     Default Name  Type          File Name
     ------------  ----          ---------
     PREV.DAT      input    * PREVDAT =             *
     LU.DAT        output   ! LUDAT   =lulc1km.dat  !
     CTGPROC.LST   output   ! RUNLST  =ctgproc.lst  !
    ---------------------------------------------------------
    (Coastline Data)
       USGS Global Self-consistent Hierarchical High-resolution
       Shoreline Database (GSHHS)
    GSHHS_F.B      input    ! GSHHSIN  = GSHHS_F.B   !
       Processed coastline polygons for
       CTGPROC grid (BLN)
    COAST.BLN      input or ! COASTBLN = coast.bln   !
                   output
    ---------------------------------------------------------

     All file names will be converted to lower case if LCFILES = T
     Otherwise, if LCFILES = F, file names will be converted to UPPER CASE
     (LCFILES)                  Default: T      ! LCFILES = T !
        T = lower case
        F = UPPER CASE

     NOTE: File/path names can be up to 70 characters in length;
           PREV.DAT is used only if LPREV=T (Input Group 1)

!END!


-------------
Subgroup (0b)
-------------

     The following NDBF Land Use Data Files are processed.
     Enter NDBF lines identifying the file name for each,
     followed by a group terminator.  The type of data base
     for each file is designated by the assignment name:

     (CTG)     designates USGS CTG (compressed)
     (NZGEN)   designates New Zealand Generic
     (GLAZNA)  designates USGS Global (Lambert Azimuthal) for North America
     (GLAZSA)  designates USGS Global (Lambert Azimuthal) for South America
     (GLAZEU)  designates USGS Global (Lambert Azimuthal) for Eurasia - Europe
     (GLAZAS)  designates USGS Global (Lambert Azimuthal) for Eurasia - Asia
     (GLAZAF)  designates USGS Global (Lambert Azimuthal) for Africa
     (GLAZAP)  designates USGS Global (Lambert Azimuthal) for Australia-Pacific
     (NLCD92)  designates USGS NLCD 1992

    ! GLAZNA = nausgs2_0l.img !  !END!
    

--------------------------------------------------------------------------------

INPUT GROUP: 1 -- Run control parameters
--------------

     When multiple applications of CTGPROC are needed, the gridded land
     use data file (LU.DAT) must be written in a continuation format rather
     than in the fractional land use format expected by MAKEGEO.  This 
     applies to all applications except the FINAL application, which must
     be in the fractional land use format.  Futhermore, if the application
     is not the first one in a series, then a PREVIOUS LU.DAT file must
     be identified.

     Is this the final run?
     (LFINAL)                   Default: T      ! LFINAL = T !
        T = LU.DAT file written in fractional land use format
        F = LU.DAT file written in continuation format

     Is a previous LU.DAT output file used to start this run?
     (LPREV)                    Default: F      ! LPREV = F !
        T = PREV.DAT file is used
        F = PREV.DAT file is NOT used

     Control for distributing input land use within its cell to improve
     the sampling density.  A mesh density greater than one is used to
     split each input cell into a finer grid of cells.  A density of 2
     creates 2 cells per side; 3 creates 3 cells per side.  The input
     land use is assigned to the center of each of the new cells.
     Specify a mesh density for CTG and USGS GLAZ file types:
     (MESHCTG)                  Default=1       ! MESHCTG  = 1 !
     (MESHGLAZ)                 Default=1       ! MESHGLAZ = 1 !

     The coordinates of the center of each input landuse "cell", both
     before and after applying the mesh density factor, can be written
     to QA plot files named QACTG.DAT, QAGLAZ.DAT, and QAMESH.DAT.
     These files can become very large for large domains.
     Create QA plot files of land use data points?
     (LQACELL)                  Default: F      ! LQACELL = F !
        T = QA files are created
        F = QA files are not created

       
     Marine Coastline Processing
     ---------------------------
     Land use data may be augmented with coastline information.  Coastline
     data are used to determine whether a particular point lies offshore,
     so that it may be given a marine (ocean) land use code.
   
     Process coastline data?
     (LCOAST)                   Default: F      ! LCOAST = T !
       T = Process coastline data
       F = Do not process coastline data
   
     Coastline processing method for points offshore may SWAP a land use
     type as it is read from an input data file with the type for ocean,
     and it may FILL empty marine cells at the end of a run with the
     type for ocean.

     (LMARSWAP)                 Default: F      ! LMARSWAP = T !
     (Used only if LCOAST=T)
       T = Replace land use type read from data file with type IOCEAN
       F = Use land use type read from data file

     (LMARFILL)                 Default: T      ! LMARFILL = T !
     (Used only if LCOAST=T and LFINAL=T)
       T = Fill empty marine grid cells with land use type IOCEAN
       F = Maintain empty grid cells
   
     Marine land use type:
     (Used only if LCOAST=T)
     (IOCEAN)                   Default: 55     ! IOCEAN = 55 !

     Read pre-processed coastline data (existing BLN file)?
     (Used only if LCOAST=T)
     (LBLNREAD)                 Default: F      ! LBLNREAD = F !
       T = Use pre-processed BLN coastline data
       F = Process GSHHS coastline data and create BLN

       
     Input Datum-Region
     ------------------
     The Datum-Region for coordinates in the input LULC Data File may be
     identified in the header records of the file.  Check the file documentation
     and change these defaults as needed.  The list of Datum-Regions with
     official transformation parameters is provided by the National Imagery and
     and Mapping Agency (NIMA).

     Datum-region for input LULC Data File coordinates

     (DCTG)                     Default: NAS-C     ! DCTG     = NAS-C  !
     for LULC = 1: USGS CTG (compressed)

     (DUSGSLA)                  Default: ESR-S     ! DUSGSLA  = ESR-S  !
     for LULC = 2: USGS Global (Lambert Azimuthal)

     (DNZGEN)                   Default: WGS-84    ! DNZGEN   = WGS-84 !
     for LULC = 3: New Zealand Generic

     (DNLCD)                    Default: NAR-C     ! DNLCD    = NAR-C  !
     for LULC = 4: USGS NLCD 1992


     QA threshold (% of average number of data points/grid cell)
     for reporting cells with poor data coverage
     (ITHRESH)                  Default: 75     ! ITHRESH = 75 !

!END!

--------------------------------------------------------------------------------

INPUT GROUP: 2 -- Map Projection and Grid Information for Output
--------------

     Projection
     ----------

     Map projection for all X,Y (km)
     (PMAP)                     Default: UTM    ! PMAP = UTM !

         UTM :  Universal Transverse Mercator
         TTM :  Tangential Transverse Mercator
         LCC :  Lambert Conformal Conic
         PS  :  Polar Stereographic
         EM  :  Equatorial Mercator
         LAZA:  Lambert Azimuthal Equal Area

     False Easting and Northing (km) at the projection origin
     (Used only if PMAP= TTM, LCC, or LAZA)
     (FEAST)                    Default=0.0     ! FEAST  = 0 !
     (FNORTH)                   Default=0.0     ! FNORTH = 0 !

     UTM zone (1 to 60)
     (Used only if PMAP=UTM)
     (IUTMZN)                   No Default      ! IUTMZN = 17 !

     Hemisphere for UTM projection?
     (Used only if PMAP=UTM)
     (UTMHEM)                   Default: N      ! UTMHEM = N !
         N   :  Northern hemisphere projection
         S   :  Southern hemisphere projection

     Latitude and Longitude (decimal degrees) of projection origin
     (Used only if PMAP= TTM, LCC, PS, EM, or LAZA)
     (RLAT0)                    No Default      ! RLAT0 = 0N !
     (RLON0)                    No Default      ! RLON0 = 0W !

         TTM :  RLON0 identifies central (true N/S) meridian of projection
                RLAT0 selected for convenience
         LCC :  RLON0 identifies central (true N/S) meridian of projection
                RLAT0 selected for convenience
         PS  :  RLON0 identifies central (grid N/S) meridian of projection
                RLAT0 selected for convenience
         EM  :  RLON0 identifies central meridian of projection
                RLAT0 is REPLACED by 0.0N (Equator)
         LAZA:  RLON0 identifies longitude of tangent-point of mapping plane
                RLAT0 identifies latitude of tangent-point of mapping plane

     Matching parallel(s) of latitude (decimal degrees) for projection
     (Used only if PMAP= LCC or PS)
     (RLAT1)                    No Default      ! RLAT1 = 0N !
     (RLAT2)                    No Default      ! RLAT2 = 0N !

         LCC :  Projection cone slices through Earth's surface at RLAT1 and RLAT2
         PS  :  Projection plane slices through Earth at RLAT1
                (RLAT2 is not used)

     ----------
     Note:  Latitudes and longitudes should be positive, and include a
            letter N,S,E, or W indicating north or south latitude, and
            east or west longitude.  For example,
            35.9  N Latitude  =  35.9N
            118.7 E Longitude = 118.7E


     Output Datum-Region
     -------------------

     The Datum-Region for the output coordinates is identified by a character
     string.  Many mapping products currently available use the model of the
     Earth known as the World Geodetic System 1984 (WGS-84).  Other local
     models may be in use, and their selection in TERREL will make its output
     consistent with local mapping products.  The list of Datum-Regions with
     official transformation parameters is provided by the National Imagery
     and Mapping Agency (NIMA).

     Datum-region for output coordinates
     (DATUM)                    Default: WGS-84    ! DATUM = WGS-84 !



     Grid
     ----

     Reference coordinates X,Y (km) assigned to the southwest corner 
     of grid cell (1,1)  (lower left corner of grid)
     (XREFKM)                   No Default      ! XREFKM = {8}  !
     (YREFKM)                   No Default      ! YREFKM = {10} !
     
     Cartesian grid definition
     (Used only if IGRID=1,2)
     No. X grid cells (NX)      No default      ! NX = 120 !
     No. Y grid cells (NY)      No default      ! NY = 90  !
     Grid Spacing (DGRIDKM)     No default      ! DGRIDKM = 1. !
     in kilometers


!END!


--------------------------------------------------------------------------------
NIMA Datum-Regions  (Documentation Section)
--------------------------------------------------------------------------------
     WGS-84    WGS-84 Reference Ellipsoid and Geoid, Global coverage (WGS84)
     NAS-C     NORTH AMERICAN 1927 Clarke 1866 Spheroid, MEAN FOR CONUS (NAD27)
     NAR-C     NORTH AMERICAN 1983 GRS 80 Spheroid, MEAN FOR CONUS (NAD83)
     NWS-84    NWS 6370KM Radius, Sphere
     ESR-S     ESRI REFERENCE 6371KM Radius, Sphere
'''.format(fdate,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF, YYYY0, MM0, DD0, HH0, YYYY1, MM1, DD1, HH1)
#           0       1         2     3     4    5    6    7    8      9      10      11     12     13     14     15      16    17   18  19    20    21    22   23

    return text


def namelist_makegeo(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF):

    YYYY0 = str(fdate[0:4])
    MM0   = str(fdate[4:6])
    DD0   = str(fdate[6:8])
    HH0   = str(fdate[8:10])

    endTime = str(datetime.datetime.strptime(fdate, "%Y%m%d%H") + datetime.timedelta(hours=24))

    YYYY1 = str(endTime[0:4])
    MM1   = str(endTime[5:7])
    DD1   = str(endTime[8:10])
    HH1   = str(endTime[11:13])

    # This namelist is only for 00z initialization

    text='''GEO.DAT file - System Demonstration - 99 X 99 grid
--------------------- Run title (1 line) --------------------------------------

                 MAKEGEO PROCESSOR CONTROL FILE
                 ------------------------------

  MAKEGEO creates the geophysical data file (GEO.DAT) for CALMET.  Using
  the fractional land use data from CTGPROC (LU.DAT), it calculates the
  dominant land use for each cell and computes weighted surface parameters.
  It may also remap land use categories if desired.  Terrain data can
  be obtained from TERREL, or provided in a file of similar format
  (TERR.DAT).

-------------------------------------------------------------------------------

INPUT GROUP: 0 -- Input and Output Files
--------------

     Default Name  Type          File Name
     ------------  ----          ---------
     LU.DAT        input    ! LUDAT   =lulc1km.dat   !
     LU2.DAT       input    * LU2DAT  =luglobe.dat *
     TERR.DAT      input    ! TERRDAT =terr1km.dat !
     GEO.DAT       output   ! GEODAT  =geo1km.dat  !
     MAKEGEO.LST   output   ! RUNLST  =geo1km.lst  !
     QALUSE.GRD    output   * LUGRD   =qaluse.grd  *
     QATERR.GRD    output   * TEGRD   =qaterr.grd  *

     All file names will be converted to lower case if LCFILES = T
     Otherwise, if LCFILES = F, file names will be converted to UPPER CASE
     (LCFILES)                  Default: T      ! LCFILES = T !
        T = lower case
        F = UPPER CASE

     NOTE: File/path names can be up to 70 characters in length

!END!

--------------------------------------------------------------------------------

INPUT GROUP: 1 -- Run control parameters
--------------

   Terrain Processing Control

     Read in a gridded terrain file?
     (LTERR)                    Default: T      ! LTERR = T !
        T = terrain elevations in GEO.DAT read from TERR.DAT
        F = terrain elevations in GEO.DAT are zero


   Land Use Processing Control

   A second file of fractional land use (LU2.DAT) may be provided for
   use when a cell in the primary land use file (LU.DAT) has no indicated
   land use.  This option allows a lower resolution dataset to supplement
   a higher resolution dataset where the higher resolution data are
   unavailable.

     Read in a second fractional land use file?
     (LLU2)                     Default: F      ! LLU2 = F !
        T = supplemental fractional land use read from LU2.DAT
        F = no supplemental fractional land use data are available


   QA information for 1 cell in the grid can be written to the list
   file.  Identify the cell by its grid location (IX,IY).
   No QA output is generated if either index is outside your grid.  For
   example, using 0 for either turns the QA output off.

     Location of grid cell for QA output
     (IXQA)                     Default: 0      ! IXQA = 1 !
     (IYQA)                     Default: 0      ! IYQA = 1 !

!END!

--------------------------------------------------------------------------------

INPUT GROUP: 2 -- Map Projection and Grid Information for Output
--------------

     Projection
     ----------

     Map projection for all X,Y (km)
     (PMAP)                     Default: UTM    ! PMAP = UTM !

         UTM :  Universal Transverse Mercator
         TTM :  Tangential Transverse Mercator
         LCC :  Lambert Conformal Conic
         PS  :  Polar Stereographic
         EM  :  Equatorial Mercator
         LAZA:  Lambert Azimuthal Equal Area

     False Easting and Northing (km) at the projection origin
     (Used only if PMAP= TTM, LCC, or LAZA)
     (FEAST)                    Default=0.0     ! FEAST  = 0 !
     (FNORTH)                   Default=0.0     ! FNORTH = 0 !

     UTM zone (1 to 60)
     (Used only if PMAP=UTM)
     (IUTMZN)                   No Default      ! IUTMZN = 17 !

     Hemisphere for UTM projection?
     (Used only if PMAP=UTM)
     (UTMHEM)                   Default: N      ! UTMHEM = N !
         N   :  Northern hemisphere projection
         S   :  Southern hemisphere projection

     Latitude and Longitude (decimal degrees) of projection origin
     (Used only if PMAP= TTM, LCC, PS, EM, or LAZA)
     (RLAT0)                    No Default      ! RLAT0 = 0N !
     (RLON0)                    No Default      ! RLON0 = 0W !

         TTM :  RLON0 identifies central (true N/S) meridian of projection
                RLAT0 selected for convenience
         LCC :  RLON0 identifies central (true N/S) meridian of projection
                RLAT0 selected for convenience
         PS  :  RLON0 identifies central (grid N/S) meridian of projection
                RLAT0 selected for convenience
         EM  :  RLON0 identifies central meridian of projection
                RLAT0 is REPLACED by 0.0N (Equator)
         LAZA:  RLON0 identifies longitude of tangent-point of mapping plane
                RLAT0 identifies latitude of tangent-point of mapping plane

     Matching parallel(s) of latitude (decimal degrees) for projection
     (Used only if PMAP= LCC or PS)
     (RLAT1)                    No Default      ! RLAT1 = 0N !
     (RLAT2)                    No Default      ! RLAT2 = 0N !

         LCC :  Projection cone slices through Earth's surface at RLAT1 and RLAT2
         PS  :  Projection plane slices through Earth at RLAT1
                (RLAT2 is not used)

     ----------
     Note:  Latitudes and longitudes should be positive, and include a
            letter N,S,E, or W indicating north or south latitude, and
            east or west longitude.  For example,
            35.9  N Latitude  =  35.9N
            118.7 E Longitude = 118.7E


     Output Datum-Region
     -------------------

     The Datum-Region for the output coordinates is identified by a character
     string.  Many mapping products currently available use the model of the
     Earth known as the World Geodetic System 1984 (WGS-84).  Other local
     models may be in use, and their selection in TERREL will make its output
     consistent with local mapping products.  The list of Datum-Regions with
     official transformation parameters is provided by the National Imagery and
     and Mapping Agency (NIMA).

     Datum-region for output coordinates
     (DATUM)                    Default: WGS-84    ! DATUM = WGS-84 !



     Grid
     ----

     Reference coordinates X,Y (km) assigned to the southwest corner 
     of grid cell (1,1)  (lower left corner of grid)
     (XREFKM)                   No Default      ! XREFKM = {8}  !
     (YREFKM)                   No Default      ! YREFKM = {10} !
     
     Cartesian grid definition
     (Used only if IGRID=1,2)
     No. X grid cells (NX)      No default      ! NX = 120 !
     No. Y grid cells (NY)      No default      ! NY = 90  !
     Grid Spacing (DGRIDKM)     No default      ! DGRIDKM = 1. !
     in kilometers


!END!

--------------------------------------------------------------------------------

INPUT GROUP: 3 -- Output Land Use
--------------

-------------
Subgroup (3a)
-------------

     Number of output land use categories
     (NOUTCAT)                  Default: 14     ! NOUTCAT = 14 !

     Output land use categories assigned to water
     range from IWAT1 to IWAT2 (inclusive)
     (IWAT1)                    Default: 50     ! IWAT1 = 50 !
     (IWAT2)                    Default: 55     ! IWAT2 = 55 !

!END!


-------------
Subgroup (3b)
-------------
                                                       a
           OUTPUT LAND USE CATEGORIES (NOUTCAT entries)
           --------------------------------------------

! OUTCAT =   10, 20, -20, 30, 40, 51, 54, 55    !  !END!
! OUTCAT =   60, 61,  62, 70, 80, 90            !  !END!

-------------
    a
     List categories in ascending order (absolute value), with up to 10
     per line.  Each line is treated as a separate input subgroup and
     therefore must end with an input group terminator.



--------------------------------------------------------------------------------

INPUT GROUP: 4 -- Input Land Use (Defaults are set for USGS categories)
--------------

-------------
Subgroup (4a)
-------------

     Number of input land use categories
     (NINCAT)                   Default: 38     ! NINCAT = 38 !

     Number of input water categories
     (NUMWAT)                   Default: 5      ! NUMWAT = 5  !

     Number of input categories that are split
     by apportioning area among the other land
     use categories
     (NSPLIT)                   Default: 0      ! NSPLIT = 0  !

     Minimum fraction of cell covered by water required
     to define the dominant land use as water
     (CFRACT)                   Default: 0.5    ! CFRACT = 0.5  !

     Land use category assigned to cell when
     no land use data are found
     (IMISS)                    Default: 55     ! IMISS = 84  !

     Minimum total fractional land use expected
     in a cell when land use data are available
     (FLUMIN)                   Default: 0.96   ! FLUMIN = 0.96 !

!END!


-------------
Subgroup (4b)
-------------
                                                              a
           LAND USE PROPERTIES AND OUTPUT MAP (NINCAT entries)
           ---------------------------------------------------

     Input                                 Soil   Anthropogenic Leaf   Output
    Category    z0     Albedo    Bowen   Heat Flux  Heat Flux   Area  Category
       ID       (m)   (0 to 1)   Ratio   Parameter  (W/m**2)   Index     ID
     ------   ------   ------    ------  ---------  --------   ------  ------

! X =   11,     0.5,    0.18,     1.0,     0.20,      0.0,      1.0,     10  !  !END!
! X =   12,     1.0,    0.18,     1.5,     0.25,      0.0,      0.2,     10  !  !END!
! X =   13,     1.0,    0.18,     1.5,     0.25,      0.0,      0.2,     10  !  !END!
! X =   14,     1.0,    0.18,     1.5,     0.25,      0.0,      0.2,     10  !  !END!
! X =   15,     1.0,    0.18,     1.5,     0.25,      0.0,      0.2,     10  !  !END!
! X =   16,     1.0,    0.18,     1.5,     0.25,      0.0,      0.2,     10  !  !END!
! X =   17,     1.0,    0.18,     1.5,     0.25,      0.0,      0.2,     10  !  !END!
! X =   21,    0.25,    0.15,     1.0,     0.15,      0.0,      3.0,     20  !  !END!
! X =   22,    0.25,    0.15,     1.0,     0.15,      0.0,      3.0,     20  !  !END!
! X =   23,    0.25,    0.15,     1.0,     0.15,      0.0,      3.0,     20  !  !END!
! X =   24,    0.25,    0.15,     1.0,     0.15,      0.0,      3.0,     20  !  !END!
! X =   31,    0.05,    0.25,     1.0,     0.15,      0.0,      0.5,     30  !  !END!
! X =   32,    0.05,    0.25,     1.0,     0.15,      0.0,      0.5,     30  !  !END!
! X =   33,    0.05,    0.25,     1.0,     0.15,      0.0,      0.5,     30  !  !END!
! X =   41,     1.0,     0.1,     1.0,     0.15,      0.0,      7.0,     40  !  !END!
! X =   42,     1.0,     0.1,     1.0,     0.15,      0.0,      7.0,     40  !  !END!
! X =   43,     1.0,     0.1,     1.0,     0.15,      0.0,      7.0,     40  !  !END!
! X =   51,   0.001,     0.1,     0.0,      1.0,      0.0,      0.0,     51  !  !END!
! X =   52,   0.001,     0.1,     0.0,      1.0,      0.0,      0.0,     51  !  !END!
! X =   53,   0.001,     0.1,     0.0,      1.0,      0.0,      0.0,     51  !  !END!
! X =   54,   0.001,     0.1,     0.0,      1.0,      0.0,      0.0,     54  !  !END!
! X =   55,   0.001,     0.1,     0.0,      1.0,      0.0,      0.0,     55  !  !END!
! X =   61,     1.0,     0.1,     0.5,     0.25,      0.0,      2.0,     61  !  !END!
! X =   62,     0.2,     0.1,     0.1,     0.25,      0.0,      1.0,     62  !  !END!
! X =   71,    0.05,     0.3,     1.0,     0.15,      0.0,     0.05,     70  !  !END!
! X =   72,    0.05,     0.3,     1.0,     0.15,      0.0,     0.05,     70  !  !END!
! X =   73,    0.05,     0.3,     1.0,     0.15,      0.0,     0.05,     70  !  !END!
! X =   74,    0.05,     0.3,     1.0,     0.15,      0.0,     0.05,     70  !  !END!
! X =   75,    0.05,     0.3,     1.0,     0.15,      0.0,     0.05,     70  !  !END!
! X =   76,    0.05,     0.3,     1.0,     0.15,      0.0,     0.05,     70  !  !END!
! X =   77,    0.05,     0.3,     1.0,     0.15,      0.0,     0.05,     70  !  !END!
! X =   81,     0.2,     0.3,     0.5,     0.15,      0.0,      0.0,     80  !  !END!
! X =   82,     0.2,     0.3,     0.5,     0.15,      0.0,      0.0,     80  !  !END!
! X =   83,     0.2,     0.3,     0.5,     0.15,      0.0,      0.0,     80  !  !END!
! X =   84,     0.2,     0.3,     0.5,     0.15,      0.0,      0.0,     80  !  !END!
! X =   85,     0.2,     0.3,     0.5,     0.15,      0.0,      0.0,     80  !  !END!
! X =   91,    0.05,     0.7,     0.5,     0.15,      0.0,      0.0,     90  !  !END!
! X =   92,    0.05,     0.7,     0.5,     0.15,      0.0,      0.0,     90  !  !END!

-------------
    a
     Data for each land use category are treated as a separate input
     subgroup and therefore must end with an input group terminator.



-------------
Subgroup (4c)
-------------
                                                             a
           INPUT CATEGORIES DEFINED AS WATER (NUMWAT entries)
           --------------------------------------------------

           ! IWAT =   51  !  !END!
           ! IWAT =   52  !  !END!
           ! IWAT =   53  !  !END!
           ! IWAT =   54  !  !END!
           ! IWAT =   55  !  !END!

-------------
    a
     Each water category ID is read as a separate input
     subgroup and therefore must end with an input group terminator.



-------------
Subgroup (4d)
-------------
                                                         a
           CATEGORY SPLIT INFORMATION (NSPLIT Categories)
           ----------------------------------------------

            Split        To       Amount
           Category   Category   of Split
              ID         ID        (%)
           --------   --------   --------

* XSPLIT =    14,        76,       15.8  *  *END*
* XSPLIT =    14,        77,       84.2  *  *END*

-------------
    a
     Each assignment is read as a separate input subgroup and therefore
     must end with an input group terminator.  A total of NSPLIT input
     land use categories must be listed, and the % split from each one
     of these to all receiving land use categories must sum to 100.0%



--------------------------------------------------------------------------------
NIMA Datum-Regions  (Documentation Section)
--------------------------------------------------------------------------------
     WGS-84    WGS-84 Reference Ellipsoid and Geoid, Global coverage (WGS84)
     NAS-C     NORTH AMERICAN 1927 Clarke 1866 Spheroid, MEAN FOR CONUS (NAD27)
     NAR-C     NORTH AMERICAN 1983 GRS 80 Spheroid, MEAN FOR CONUS (NAD83)
     NWS-84    NWS 6370KM Radius, Sphere
     ESR-S     ESRI REFERENCE 6371KM Radius, Sphere

'''.format(fdate,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF, YYYY0, MM0, DD0, HH0, YYYY1, MM1, DD1, HH1)
#           0       1         2     3     4    5    6    7    8      9      10      11     12     13     14     15      16    17   18  19    20    21    22   23

    return text


def namelist_calmet(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF):

    YYYY0 = str(fdate[0:4])
    MM0   = str(fdate[4:6])
    DD0   = str(fdate[6:8])
    HH0   = str(fdate[8:10])

    endTime = str(datetime.datetime.strptime(fdate, "%Y%m%d%H") + datetime.timedelta(hours=24))

    YYYY1 = str(endTime[0:4])
    MM1   = str(endTime[5:7])
    DD1   = str(endTime[8:10])
    HH1   = str(endTime[11:13])

    # This namelist is only for 00z initialization

    text='''CALMET.INP      2.1             Hour Start and End Times with Seconds
Example CALMET simulation (not intended as a guide to option selections)
MM4 data, 5 surface met stations, 1 overwater station, 
3 upper air met and 16 precip stations- 
---------------- Run title (3 lines) ------------------------------------------

                    CALMET MODEL CONTROL FILE
                    --------------------------

-------------------------------------------------------------------------------

INPUT GROUP: 0 -- Input and Output File Names


Subgroup (a)
------------
Default Name  Type          File Name
------------  ----          ---------
GEO.DAT       input    ! GEODAT=geo1km.dat       !
SURF.DAT      input    * SRFDAT=SURF.DAT    *
CLOUD.DAT     input    * CLDDAT=            *
PRECIP.DAT    input    * PRCDAT=PRECIP.DAT  *
WT.DAT        input    * WTDAT=             *

CALMET.LST    output   ! METLST=CALMET.LST     !
CALMET.DAT    output   ! METDAT=CALMET.DAT    !
PACOUT.DAT    output   * PACDAT=            *

All file names will be converted to lower case if LCFILES = T
Otherwise, if LCFILES = F, file names will be converted to UPPER CASE
         T = lower case      ! LCFILES = T !
         F = UPPER CASE

NUMBER OF UPPER AIR & OVERWATER STATIONS:

    Number of upper air stations (NUSTA)  No default     ! NUSTA =  0  !
    Number of overwater met stations
                                 (NOWSTA) No default     ! NOWSTA =  0  !

NUMBER OF PROGNOSTIC and IGF-CALMET FILEs:

    Number of MM4/MM5/3D.DAT files
                                 (NM3D) No default       ! NM3D =  1  !

    Number of IGF-CALMET.DAT files
                                 (NIGF)   No default     ! NIGF =  0  !

                       !END!
--------------------------------------------------------------------------------
Subgroup (b)
---------------------------------
Upper air files (one per station)
---------------------------------
Default Name  Type       File Name
------------  ----       ---------
UP1.DAT       input     1  * UPDAT=UPPWM.DAT  *    *END*
UP2.DAT       input     2  * UPDAT=UPALBR.DAT *    *END*
UP3.DAT       input     3  * UPDAT=UPCHH.DAT  *    *END*
--------------------------------------------------------------------------------
Subgroup (c)
-----------------------------------------
Overwater station files (one per station)
-----------------------------------------
Default Name  Type       File Name
------------  ----       ---------
SEA1.DAT       input     1  * SEADAT=4007.DAT *    *END*
--------------------------------------------------------------------------------
Subgroup (d)
------------------------------------------------
MM4/MM5/3D.DAT files (consecutive or overlapping)
------------------------------------------------
Default Name  Type       File Name
------------  ----       ---------
MM51.DAT       input     1  ! M3DDAT={1}.m3d !    !END!
--------------------------------------------------------------------------------
Subgroup (e)
-------------------------------------------------
IGF-CALMET.DAT files (consecutive or overlapping)
-------------------------------------------------
Default Name  Type       File Name
------------  ----       ---------
IGFn.DAT       input     1  * IGFDAT=CALMET0.DAT *    *END*
--------------------------------------------------------------------------------
Subgroup (f)
----------------
Other file names
----------------

Default Name  Type       File Name
------------  ----       ---------
DIAG.DAT      input      * DIADAT=                  *
PROG.DAT      input      * PRGDAT=                  *

TEST.PRT      output     * TSTPRT=                  *
TEST.OUT      output     * TSTOUT=                  *
TEST.KIN      output     * TSTKIN=                  *
TEST.FRD      output     * TSTFRD=                  *
TEST.SLP      output     * TSTSLP=                  *
DCST.GRD      output     * DCSTGD=                  *

--------------------------------------------------------------------------------
NOTES: (1) File/path names can be up to 70 characters in length
       (2) Subgroups (a) and (f) must have ONE 'END' (surrounded by
           delimiters) at the end of the group
       (3) Subgroups (b) through (e) are included ONLY if the corresponding
           number of files (NUSTA, NOWSTA, NM3D, NIGF) is not 0, and each must have
           an 'END' (surround by delimiters) at the end of EACH LINE

                         !END!


-------------------------------------------------------------------------------

INPUT GROUP: 1 -- General run control parameters
--------------

     Starting date:    Year   (IBYR)  --    No default   ! IBYR  =  {16}  !
                       Month  (IBMO)  --    No default   ! IBMO  =  {17}  !
                       Day    (IBDY)  --    No default   ! IBDY  =  {18}  !
     Starting time:    Hour   (IBHR)  --    No default   ! IBHR  =  0  !
                       Second (IBSEC) --    No default   ! IBSEC =  0  !

     Ending date:      Year   (IEYR)  --    No default   ! IEYR  =  {20}  !
                       Month  (IEMO)  --    No default   ! IEMO  =  {21}  !
                       Day    (IEDY)  --    No default   ! IEDY  =  {22}  !
     Ending time:      Hour   (IEHR)  --    No default   ! IEHR  =  19  !
                       Second (IESEC) --    No default   ! IESEC =  0  !

      UTC time zone         (ABTZ) -- No default       ! ABTZ= UTC-0500 !
         (character*8)
         PST = UTC-0800, MST = UTC-0700 , GMT = UTC-0000
         CST = UTC-0600, EST = UTC-0500

     Length of modeling time-step (seconds)
     Must divide evenly into 3600 (1 hour)
     (NSECDT)                        Default:3600     ! NSECDT =  3600  !
                                     Units: seconds

     Run type            (IRTYPE) -- Default: 1       ! IRTYPE=  1  !

        0 = Computes wind fields only
        1 = Computes wind fields and micrometeorological variables
            (u*, w*, L, zi, etc.)
        (IRTYPE must be 1 to run CALPUFF or CALGRID)

     Compute special data fields required
     by CALGRID (i.e., 3-D fields of W wind
     components and temperature)
     in additional to regular            Default: T    ! LCALGRD = T !
     fields ? (LCALGRD)
     (LCALGRD must be T to run CALGRID)

      Flag to stop run after
      SETUP phase (ITEST)             Default: 2       ! ITEST=  2   !
      (Used to allow checking
      of the model inputs, files, etc.)
      ITEST = 1 - STOPS program after SETUP phase
      ITEST = 2 - Continues with execution of
                  COMPUTATIONAL phase after SETUP


     Test options specified to see if
     they conform to regulatory
     values? (MREG)                   No Default       ! MREG =   0   !

        0 = NO checks are made
        1 = Technical options must conform to USEPA guidance
                  IMIXH    -1       Maul-Carson convective mixing height
                                    over land; OCD mixing height overwater
                  ICOARE   0        OCD deltaT method for overwater fluxes
                  THRESHL  0.0      Threshold buoyancy flux over land needed
                                    to sustain convective mixing height growth
                  ISURFT   > 0      Pick one representative station, OR
                           -2       in NOOBS mode (ITPROG=2) average all
                                    surface prognostic temperatures to get
                                    a single representative surface temp.
                  IUPT     > 0      Pick one representative station, OR
                           -2       in NOOBS mode (ITPROG>0) average all surface
                                    prognostic temperatures to get a single
                                    representative surface temp.

!END!

-------------------------------------------------------------------------------

INPUT GROUP: 2 -- Map Projection and Grid control parameters
--------------

     Projection for all (X,Y):
     -------------------------

     Map projection
     (PMAP)                     Default: UTM    ! PMAP = UTM  !

         UTM :  Universal Transverse Mercator
         TTM :  Tangential Transverse Mercator
         LCC :  Lambert Conformal Conic
          PS :  Polar Stereographic
          EM :  Equatorial Mercator
        LAZA :  Lambert Azimuthal Equal Area

     False Easting and Northing (km) at the projection origin
     (Used only if PMAP= TTM, LCC, or LAZA)
     (FEAST)                    Default=0.0     ! FEAST  = 0.000  !
     (FNORTH)                   Default=0.0     ! FNORTH = 0.000  !

     UTM zone (1 to 60)
     (Used only if PMAP=UTM)
     (IUTMZN)                   No Default      ! IUTMZN =  17   !

     Hemisphere for UTM projection?
     (Used only if PMAP=UTM)
     (UTMHEM)                   Default: N      ! UTMHEM = N  !
         N   :  Northern hemisphere projection
         S   :  Southern hemisphere projection

     Latitude and Longitude (decimal degrees) of projection origin
     (Used only if PMAP= TTM, LCC, PS, EM, or LAZA)
     (RLAT0)                    No Default      ! RLAT0 = 40N  !
     (RLON0)                    No Default      ! RLON0 = 90W  !

         TTM :  RLON0 identifies central (true N/S) meridian of projection
                RLAT0 selected for convenience
         LCC :  RLON0 identifies central (true N/S) meridian of projection
                RLAT0 selected for convenience
         PS  :  RLON0 identifies central (grid N/S) meridian of projection
                RLAT0 selected for convenience
         EM  :  RLON0 identifies central meridian of projection
                RLAT0 is REPLACED by 0.0N (Equator)
         LAZA:  RLON0 identifies longitude of tangent-point of mapping plane
                RLAT0 identifies latitude of tangent-point of mapping plane

     Matching parallel(s) of latitude (decimal degrees) for projection
     (Used only if PMAP= LCC or PS)
     (XLAT1)                    No Default      ! XLAT1 = 30N  !
     (XLAT2)                    No Default      ! XLAT2 = 60N  !

         LCC :  Projection cone slices through Earth's surface at XLAT1 and XLAT2
         PS  :  Projection plane slices through Earth at XLAT1
                (XLAT2 is not used)

     ----------
     Note:  Latitudes and longitudes should be positive, and include a
            letter N,S,E, or W indicating north or south latitude, and
            east or west longitude.  For example,
            35.9  N Latitude  =  35.9N
            118.7 E Longitude = 118.7E


     Datum-region
     ------------

     The Datum-Region for the coordinates is identified by a character
     string.  Many mapping products currently available use the model of the
     Earth known as the World Geodetic System 1984 (WGS-84).  Other local
     models may be in use, and their selection in CALMET will make its output
     consistent with local mapping products.  The list of Datum-Regions with
     official transformation parameters is provided by the National Imagery and
     Mapping Agency (NIMA).

     NIMA Datum - Regions(Examples)
     ------------------------------------------------------------------------------
     WGS-84    WGS-84 Reference Ellipsoid and Geoid, Global coverage (WGS84)
     NAS-C     NORTH AMERICAN 1927 Clarke 1866 Spheroid, MEAN FOR CONUS (NAD27)
     NAR-C     NORTH AMERICAN 1983 GRS 80 Spheroid, MEAN FOR CONUS (NAD83)
     NWS-84    NWS 6370KM Radius, Sphere
     ESR-S     ESRI REFERENCE 6371KM Radius, Sphere

     Datum-region for output coordinates
     (DATUM)                    Default: WGS-84    ! DATUM = WGS-84  !


     Horizontal grid definition:
     ---------------------------

     Rectangular grid defined for projection PMAP,
     with X the Easting and Y the Northing coordinate

            No. X grid cells (NX)      No default     ! NX =  120  !
            No. Y grid cells (NY)      No default     ! NY =   90  !

     Grid spacing (DGRIDKM)            No default     ! DGRIDKM = 1. !
                                       Units: km

     Reference grid coordinate of
     SOUTHWEST corner of grid cell (1,1)

        X coordinate (XORIGKM)         No default     ! XORIGKM = {8} !
        Y coordinate (YORIGKM)         No default     ! YORIGKM = {10} !
                                       Units: km


     Vertical grid definition:
     -------------------------

        No. of vertical layers (NZ)    No default     ! NZ =  10  !

        Cell face heights in arbitrary
        vertical grid (ZFACE(NZ+1))    No defaults
                                       Units: m
        ! ZFACE = 0.,20.,40.,80.,160.,300.,600.,1000.,1500.,2200.,3000. !

!END!


-------------------------------------------------------------------------------

INPUT GROUP: 3 -- Output Options
--------------


    DISK OUTPUT OPTION

       Save met. fields in an unformatted
       output file ?              (LSAVE)  Default: T     ! LSAVE = T !
       (F = Do not save, T = Save)

       Type of unformatted output file:
       (IFORMO)                            Default: 1    ! IFORMO =  1  !

            1 = CALPUFF/CALGRID type file (CALMET.DAT)
            2 = MESOPUFF-II type file     (PACOUT.DAT)


    LINE PRINTER OUTPUT OPTIONS:

       Print met. fields ?  (LPRINT)       Default: F     ! LPRINT = F !
       (F = Do not print, T = Print)
       (NOTE: parameters below control which
              met. variables are printed)

       Print interval
       (IPRINF) in hours                   Default: 1     ! IPRINF =  1  !
       (Meteorological fields are printed
        every  1  hours)


       Specify which layers of U, V wind component
       to print (IUVOUT(NZ)) -- NOTE: NZ values must be entered
       (0=Do not print, 1=Print)
       (used only if LPRINT=T)        Defaults: NZ*0 
       ! IUVOUT =  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0  !
       -----------------------


       Specify which levels of the W wind component to print
       (NOTE: W defined at TOP cell face --  10  values)
       (IWOUT(NZ)) -- NOTE: NZ values must be entered
       (0=Do not print, 1=Print)
       (used only if LPRINT=T & LCALGRD=T)
       -----------------------------------
                                            Defaults: NZ*0 
        ! IWOUT =  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0  !


       Specify which levels of the 3-D temperature field to print
       (ITOUT(NZ)) -- NOTE: NZ values must be entered
       (0=Do not print, 1=Print)
       (used only if LPRINT=T & LCALGRD=T)
       -----------------------------------
                                            Defaults: NZ*0 
        ! ITOUT =  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0  !

       Specify which meteorological fields
       to print
       (used only if LPRINT=T)             Defaults: 0 (all variables)
       -----------------------


         Variable            Print ?
                         (0 = do not print,
                          1 = print)
         --------        ------------------

      !  STABILITY  =           0           ! - PGT stability class
      !  USTAR      =           0           ! - Friction velocity
      !  MONIN      =           0           ! - Monin-Obukhov length
      !  MIXHT      =           0           ! - Mixing height
      !  WSTAR      =           0           ! - Convective velocity scale
      !  PRECIP     =           0           ! - Precipitation rate
      !  SENSHEAT   =           0           ! - Sensible heat flux
      !  CONVZI     =           0           ! - Convective mixing ht.


       Testing and debug print options for micrometeorological module

          Print input meteorological data and
          internal variables (LDB)         Default: F       ! LDB = F !
          (F = Do not print, T = print)
          (NOTE: this option produces large amounts of output)

          First time step for which debug data
          are printed (NN1)                Default: 1       ! NN1 =  1  !

          Last time step for which debug data
          are printed (NN2)                Default: 1       ! NN2 =  1  ! VER

          Print distance to land
          internal variables (LDBCST)      Default: F       ! LDBCST = F !
          (F = Do not print, T = print)
          (Output in .GRD file DCST.GRD, defined in input group 0)

       Testing and debug print options for wind field module
       (all of the following print options control output to
        wind field module's output files: TEST.PRT, TEST.OUT,
        TEST.KIN, TEST.FRD, and TEST.SLP)

          Control variable for writing the test/debug
          wind fields to disk files (IOUTD)
          (0=Do not write, 1=write)        Default: 0       ! IOUTD =  0  !

          Number of levels, starting at the surface,
          to print (NZPRN2)                Default: 1       ! NZPRN2 =  0  ! VER

          Print the INTERPOLATED wind components ?
          (IPR0) (0=no, 1=yes)             Default: 0       !  IPR0 =  0  !

          Print the TERRAIN ADJUSTED surface wind
          components ?
          (IPR1) (0=no, 1=yes)             Default: 0       !  IPR1 =  0  !

          Print the SMOOTHED wind components and
          the INITIAL DIVERGENCE fields ?
          (IPR2) (0=no, 1=yes)             Default: 0       !  IPR2 =  0  !

          Print the FINAL wind speed and direction
          fields ?
          (IPR3) (0=no, 1=yes)             Default: 0       !  IPR3 =  0  !

          Print the FINAL DIVERGENCE fields ?
          (IPR4) (0=no, 1=yes)             Default: 0       !  IPR4 =  0  !

          Print the winds after KINEMATIC effects
          are added ?
          (IPR5) (0=no, 1=yes)             Default: 0       !  IPR5 =  0  !

          Print the winds after the FROUDE NUMBER
          adjustment is made ?
          (IPR6) (0=no, 1=yes)             Default: 0       !  IPR6 =  0  !

          Print the winds after SLOPE FLOWS
          are added ?
          (IPR7) (0=no, 1=yes)             Default: 0       !  IPR7 =  0  !

          Print the FINAL wind field components ?
          (IPR8) (0=no, 1=yes)             Default: 0       !  IPR8 =  0  !

!END!


-------------------------------------------------------------------------------

INPUT GROUP: 4 -- Meteorological data options
--------------

    NO OBSERVATION MODE             (NOOBS)  Default: 0     ! NOOBS =  2   !
          0 = Use surface, overwater, and upper air stations
          1 = Use surface and overwater stations (no upper air observations)
              Use MM4/MM5/3D.DAT for upper air data
          2 = No surface, overwater, or upper air observations
              Use MM4/MM5/3D.DAT for surface, overwater, and upper air data

    NUMBER OF SURFACE & PRECIP. METEOROLOGICAL STATIONS

       Number of surface stations   (NSSTA)  No default     ! NSSTA =  0  !

       Number of precipitation stations
       (NPSTA=-1: flag for use of MM5/3D.DAT precip data)
                                    (NPSTA)  No default     ! NPSTA =  0  !

    CLOUD DATA OPTIONS
       Gridded cloud fields:
                                   (ICLOUD)  Default: 0     ! ICLOUD =  3  !
       ICLOUD = 0 - Gridded clouds not used
       ICLOUD = 1 - Gridded CLOUD.DAT generated as OUTPUT
       ICLOUD = 2 - Gridded CLOUD.DAT read as INPUT
       ICLOUD = 3 - Gridded cloud cover from Prognostic Rel. Humidity
                    at 850mb (Teixera)
       ICLOUD = 4 - Gridded cloud cover from Prognostic Rel. Humidity
                    at all levels (MM5toGrads algorithm)

    FILE FORMATS

       Surface meteorological data file format
                                   (IFORMS)  Default: 2     ! IFORMS =  2  !
       (1 = unformatted (e.g., SMERGE output))
       (2 = formatted   (free-formatted user input))

       Precipitation data file format
                                   (IFORMP)  Default: 2     ! IFORMP =  2  !
       (1 = unformatted (e.g., PMERGE output))
       (2 = formatted   (free-formatted user input))

       Cloud data file format
                                   (IFORMC)  Default: 2     ! IFORMC =  2  !
       (1 = unformatted - CALMET unformatted output)
       (2 = formatted   - free-formatted CALMET output or user input)

!END!


-------------------------------------------------------------------------------

INPUT GROUP: 5 -- Wind Field Options and Parameters
--------------


    WIND FIELD MODEL OPTIONS
       Model selection variable (IWFCOD)     Default: 1      ! IWFCOD =  1  !
          0 = Objective analysis only
          1 = Diagnostic wind module

       Compute Froude number adjustment
       effects ? (IFRADJ)                    Default: 1      ! IFRADJ =  1  !
       (0 = NO, 1 = YES)

       Compute kinematic effects ? (IKINE)   Default: 0      ! IKINE  =  0  !
       (0 = NO, 1 = YES)

       Use O'Brien procedure for adjustment
       of the vertical velocity ? (IOBR)     Default: 0      ! IOBR =  0  !
       (0 = NO, 1 = YES)

       Compute slope flow effects ? (ISLOPE) Default: 1      ! ISLOPE  =  1  !
       (0 = NO, 1 = YES)

       Extrapolate surface wind observations
       to upper layers ? (IEXTRP)            Default: -4     ! IEXTRP = -1  !
       (1 = no extrapolation is done,
        2 = power law extrapolation used,
        3 = user input multiplicative factors
            for layers 2 - NZ used (see FEXTRP array)
        4 = similarity theory used
        -1, -2, -3, -4 = same as above except layer 1 data
            at upper air stations are ignored

       Extrapolate surface winds even
       if calm? (ICALM)                      Default: 0      ! ICALM  =  0  !
       (0 = NO, 1 = YES)

       Layer-dependent biases modifying the weights of
       surface and upper air stations (BIAS(NZ))
         -1<=BIAS<=1
       Negative BIAS reduces the weight of upper air stations
         (e.g. BIAS=-0.1 reduces the weight of upper air stations
       by 10%; BIAS= -1, reduces their weight by 100 %)
       Positive BIAS reduces the weight of surface stations
         (e.g. BIAS= 0.2 reduces the weight of surface stations
       by 20%; BIAS=1 reduces their weight by 100%)
       Zero BIAS leaves weights unchanged (1/R**2 interpolation)
       Default: NZ*0
                               ! BIAS =  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0  !

       Minimum distance from nearest upper air station
       to surface station for which extrapolation
       of surface winds at surface station will be allowed
       (RMIN2: Set to -1 for IEXTRP = 4 or other situations
        where all surface stations should be extrapolated)
                                             Default: 4.     ! RMIN2 = -1.0 !

       Use gridded prognostic wind field model
       output fields as input to the diagnostic
       wind field model (IPROG)              Default: 0      ! IPROG =  14  !
       (0 = No, [IWFCOD = 0 or 1]
        1 = Yes, use CSUMM prog. winds as Step 1 field, [IWFCOD = 0]
        2 = Yes, use CSUMM prog. winds as initial guess field [IWFCOD = 1]
        3 = Yes, use winds from MM4.DAT file as Step 1 field [IWFCOD = 0]
        4 = Yes, use winds from MM4.DAT file as initial guess field [IWFCOD = 1]
        5 = Yes, use winds from MM4.DAT file as observations [IWFCOD = 1]
        13 = Yes, use winds from MM5/3D.DAT file as Step 1 field [IWFCOD = 0]
        14 = Yes, use winds from MM5/3D.DAT file as initial guess field [IWFCOD = 1]
        15 = Yes, use winds from MM5/3D.DAT file as observations [IWFCOD = 1]

       Timestep (seconds) of the prognostic
       model input data   (ISTEPPGS)         Default: 3600   ! ISTEPPGS =  3600   !

       Use coarse CALMET fields as initial guess fields (IGFMET)
       (overwrites IGF based on prognostic wind fields if any)
                                             Default: 0      ! IGFMET =  0  !

    RADIUS OF INFLUENCE PARAMETERS

       Use varying radius of influence       Default: F      ! LVARY =  T!  VER
       (if no stations are found within RMAX1,RMAX2,
        or RMAX3, then the closest station will be used)

       Maximum radius of influence over land
       in the surface layer (RMAX1)          No default      ! RMAX1 = 30. ! VER
                                             Units: km
       Maximum radius of influence over land
       aloft (RMAX2)                         No default      ! RMAX2 = 30. ! VER
                                             Units: km
       Maximum radius of influence over water
       (RMAX3)                               No default      ! RMAX3 = 50. ! VER
                                             Units: km


    OTHER WIND FIELD INPUT PARAMETERS

       Minimum radius of influence used in
       the wind field interpolation (RMIN)   Default: 0.1    ! RMIN = 0.1 !
                                             Units: km
       Radius of influence of terrain
       features (TERRAD)                     No default      ! TERRAD = 12. !

                                             Units: km
       Relative weighting of the first
       guess field and observations in the
       SURFACE layer (R1)                    No default      ! R1 = 1. !
       (R1 is the distance from an           Units: km
       observational station at which the
       observation and first guess field are
       equally weighted)

       Relative weighting of the first
       guess field and observations in the
       layers ALOFT (R2)                     No default      ! R2 = 1. !
       (R2 is applied in the upper layers    Units: km
       in the same manner as R1 is used in
       the surface layer).

       Relative weighting parameter of the
       prognostic wind field data (RPROG)    No default      ! RPROG = 0. !
       (Used only if IPROG = 1)              Units: km
       ------------------------

       Maximum acceptable divergence in the
       divergence minimization procedure
       (DIVLIM)                              Default: 5.E-6  ! DIVLIM= 5.0E-06 !

       Maximum number of iterations in the
       divergence min. procedure (NITER)     Default: 50     ! NITER =  50  !

       Number of passes in the smoothing
       procedure (NSMTH(NZ))
       NOTE: NZ values must be entered
            Default: 2,(mxnz-1)*4 ! NSMTH =  2 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4  !

       Maximum number of stations used in
       each layer for the interpolation of
       data to a grid point (NINTR2(NZ))
       NOTE: NZ values must be entered       Default: 99.    ! NINTR2 =  5 ,  5 ,  5 ,  5 ,  5 ,  5 ,  5 ,  5 ,  5 ,  5  ! VER

       Critical Froude number (CRITFN)       Default: 1.0    ! CRITFN = 1. !

       Empirical factor controlling the
       influence of kinematic effects
       (ALPHA)                               Default: 0.1    ! ALPHA = 0.1 !

       Multiplicative scaling factor for
       extrapolation of surface observations
       to upper layers (FEXTR2(NZ))          Default: NZ*0.0 
       ! FEXTR2 = 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. !
       (Used only if IEXTRP = 3 or -3)


    BARRIER INFORMATION

       Number of barriers to interpolation
       of the wind fields (NBAR)             Default: 0      ! NBAR =  0  !

       Level (1 to NZ) up to which barriers
       apply (KBAR)                          Default: NZ     ! KBAR =  10  ! VER

       THE FOLLOWING 4 VARIABLES ARE INCLUDED
       ONLY IF NBAR > 0
       NOTE: NBAR values must be entered     No defaults
             for each variable               Units: km

          X coordinate of BEGINNING
          of each barrier (XBBAR(NBAR))      ! XBBAR = 0. !
          Y coordinate of BEGINNING
          of each barrier (YBBAR(NBAR))      ! YBBAR = 0. !

          X coordinate of ENDING
          of each barrier (XEBAR(NBAR))      ! XEBAR = 0. !
          Y coordinate of ENDING
          of each barrier (YEBAR(NBAR))      ! YEBAR = 0. !


    DIAGNOSTIC MODULE DATA INPUT OPTIONS

       Surface temperature (IDIOPT1)         Default: 0      ! IDIOPT1 =  0  !
          0 = Compute internally from
              hourly surface observations or prognostic fields
          1 = Read preprocessed values from
              a data file (DIAG.DAT)

          Surface met. station to use for
          the surface temperature (ISURFT)   Default: -1    ! ISURFT =  -1  ! VER
          (Must be a value from 1 to NSSTA,
           or -1 to use 2-D spatially varying
              surface temperatures,
           or -2 to use a domain-average prognostic
              surface temperatures (only with ITPROG=2))
          (Used only if IDIOPT1 = 0)
          --------------------------

       Temperature lapse rate used in the    Default: 0     ! IDIOPT2 =  0  !
          computation of terrain-induced
          circulations (IDIOPT2)
          0 = Compute internally from (at least) twice-daily
              upper air observations or prognostic fields
          1 = Read hourly preprocessed values
              from a data file (DIAG.DAT)

          Upper air station to use for
          the domain-scale lapse rate (IUPT) Default: -1    ! IUPT   =  -1  ! VER 
          (Must be a value from 1 to NUSTA,
           or -1 to use 2-D spatially varying lapse rate,
           or -2 to use a domain-average prognostic
              lapse rate (only with ITPROG>0))
          (Used only if IDIOPT2 = 0)
          --------------------------

          Depth through which the domain-scale
          lapse rate is computed (ZUPT)      Default: 200.  ! ZUPT = 200. !
          (Used only if IDIOPT2 = 0)         Units: meters
          --------------------------

       Initial Guess Field Winds
       (IDIOPT3)                             Default: 0     ! IDIOPT3 =  0  !
          0 = Compute internally from
              observations or prognostic wind fields
          1 = Read hourly preprocessed domain-average wind values
              from a data file (DIAG.DAT)

          Upper air station to use for
          the initial guess winds (IUPWND)   Default: -1    ! IUPWND = -1  !
          (Must be a value from -1 to NUSTA, with
          -1 indicating 3-D initial guess fields,
          and IUPWND>1 domain-scaled (i.e. constant) IGF)
          (Used only if IDIOPT3 = 0 and noobs=0)
          --------------------------------------

          Bottom and top of layer through
          which the domain-scale winds
          are computed
          (ZUPWND(1), ZUPWND(2))        Defaults: 1., 1000. ! ZUPWND= 1., 1000. !
          (Used only if IDIOPT3 = 0, NOOBS>0 and IUPWND>0)    Units: meters
          --------------------------

       Observed surface wind components
       for wind field module (IDIOPT4)  Default: 0     ! IDIOPT4 =  0  !
          0 = Read WS, WD from a surface
              data file (SURF.DAT)
          1 = Read hourly preprocessed U, V from
              a data file (DIAG.DAT)

       Observed upper air wind components
       for wind field module (IDIOPT5)  Default: 0     ! IDIOPT5 =  0  !
          0 = Read WS, WD from an upper
              air data file (UP1.DAT, UP2.DAT, etc.)
          1 = Read hourly preprocessed U, V from
              a data file (DIAG.DAT)

       LAKE BREEZE INFORMATION

          Use Lake Breeze Module  (LLBREZE)
                                           Default: F      ! LLBREZE = F !

           Number of lake breeze regions (NBOX)            ! NBOX =  0  !

        X Grid line 1 defining the region of interest
                                                        ! XG1 = 0. !
        X Grid line 2 defining the region of interest
                                                        ! XG2 = 0. !
        Y Grid line 1 defining the region of interest
                                                        ! YG1 = 0. !
        Y Grid line 2 defining the region of interest
                                                        ! YG2 = 0. !

         X Point defining the coastline (Straight line)
                   (XBCST)  (KM)   Default: none    ! XBCST = 0. !

         Y Point defining the coastline (Straight line)
                   (YBCST)  (KM)   Default: none    ! YBCST = 0. !

         X Point defining the coastline (Straight line)
                   (XECST)  (KM)   Default: none    ! XECST = 0. !

         Y Point defining the coastline (Straight line)
                   (YECST)  (KM)   Default: none    ! YECST = 0. !


       Number of stations in the region     Default: none ! NLB =  0 ! 
       (Surface stations + upper air stations)

       Station ID's  in the region   (METBXID(NLB))
       (Surface stations first, then upper air stations)
         ! METBXID =  0 !

!END!


-------------------------------------------------------------------------------

INPUT GROUP: 6 -- Mixing Height, Temperature and Precipitation Parameters
--------------

    EMPIRICAL MIXING HEIGHT CONSTANTS

       Neutral, mechanical equation
       (CONSTB)                              Default: 1.41   ! CONSTB = 1.41 !
       Convective mixing ht. equation
       (CONSTE)                              Default: 0.15   ! CONSTE = 0.15 !
       Stable mixing ht. equation
       (CONSTN)                              Default: 2400.  ! CONSTN = 2400.!
       Overwater mixing ht. equation
       (CONSTW)                              Default: 0.16   ! CONSTW = 0.16 !
       Absolute value of Coriolis
       parameter (FCORIOL)                   Default: 1.E-4  ! FCORIOL = 1.0E-04!
                                             Units: (1/s)

    SPATIAL AVERAGING OF MIXING HEIGHTS

       Conduct spatial averaging
       (IAVEZI)  (0=no, 1=yes)               Default: 1      ! IAVEZI =  1  !

       Max. search radius in averaging
       process (MNMDAV)                      Default: 1      ! MNMDAV =  10  !  VER
                                             Units: Grid
                                                    cells
       Half-angle of upwind looking cone
       for averaging (HAFANG)                Default: 30.    ! HAFANG = 30. !
                                             Units: deg.
       Layer of winds used in upwind
       averaging (ILEVZI)                    Default: 1      ! ILEVZI =  1  !
       (must be between 1 and NZ)


    CONVECTIVE MIXING HEIGHT OPTIONS:
       Method to compute the convective
       mixing height(IMIHXH)                 Default: 1      ! IMIXH =  1  !
           1: Maul-Carson for land and water cells
          -1: Maul-Carson for land cells only -
              OCD mixing height overwater
           2: Batchvarova and Gryning for land and water cells
          -2: Batchvarova and Gryning for land cells only
              OCD mixing height overwater

       Threshold buoyancy flux required to
       sustain convective mixing height growth
       overland (THRESHL)                    Default: 0.0    ! THRESHL = 0. ! VER - no vienen
       (expressed as a heat flux             units: W/m3
        per meter of boundary layer)


       Threshold buoyancy flux required to
       sustain convective mixing height growth
       overwater (THRESHW)                   Default: 0.05   ! THRESHW = 0.05 ! VER - no vienen
       (expressed as a heat flux             units: W/m3
        per meter of boundary layer)


       Option for overwater lapse rates used
       in convective mixing height growth
       (ITWPROG)                             Default: 0      ! ITWPROG =  0  !
       0 : use SEA.DAT lapse rates and deltaT (or assume neutral
           conditions if missing)
       1 : use prognostic lapse rates (only if IPROG>2)
           and SEA.DAT deltaT (or neutral if missing)
       2 : use prognostic lapse rates and prognostic delta T
           (only if iprog>12 and 3D.DAT version# 2.0 or higher)

       Land Use category ocean in 3D.DAT datasets  
       (ILUOC3D)                             Default: 16     ! ILUOC3D =  16  !
       Note: if 3D.DAT from MM5 version 3.0, iluoc3d = 16
             if MM4.DAT,           typically iluoc3d = 7 


    OTHER MIXING HEIGHT VARIABLES

       Minimum potential temperature lapse
       rate in the stable layer above the
       current convective mixing ht.         Default: 0.001  ! DPTMIN = 0.001 !
       (DPTMIN)                              Units: deg. K/m
       Depth of layer above current conv.
       mixing height through which lapse     Default: 200.   ! DZZI = 200. !
       rate is computed (DZZI)               Units: meters

       Minimum overland mixing height        Default:  50.   ! ZIMIN = 50. !
       (ZIMIN)                               Units: meters
       Maximum overland mixing height        Default: 3000.  ! ZIMAX = 2500. ! VER 3000
       (ZIMAX)                               Units: meters
       Minimum overwater mixing height       Default:   50.  ! ZIMINW = 50. !
       (ZIMINW) -- (Not used if observed     Units: meters
       overwater mixing hts. are used)
       Maximum overwater mixing height       Default: 3000.  ! ZIMAXW = 2500. ! VER 3000
       (ZIMAXW) -- (Not used if observed     Units: meters
       overwater mixing hts. are used)


    OVERWATER SURFACE FLUXES METHOD and PARAMETERS
          (ICOARE)                           Default: 10      ! ICOARE =  10   !
           0: original deltaT method (OCD)
          10: COARE with no wave parameterization (jwave=0, Charnock)
          11: COARE with wave option jwave=1 (Oost et al.)
              and default wave properties
         -11: COARE with wave option jwave=1 (Oost et al.)
              and observed wave properties (must be in SEA.DAT files)
          12: COARE with wave option 2 (Taylor and Yelland)
               and default wave properties
         -12: COARE with wave option 2 (Taylor and Yelland)
              and observed wave properties (must be in SEA.DAT files)

          Note:  When ICOARE=0, similarity wind profile stability PSI functions
                 based on Van Ulden and Holtslag (1985) are substituted for
                 later formulations used with the COARE module, and temperatures
                 used for surface layer parameters are obtained from either the
                 nearest surface station temperature or prognostic model 2D
                 temperatures (if ITPROG=2).


          Coastal/Shallow water length scale (DSHELF)
          (for modified z0 in shallow water)
          ( COARE fluxes only)
                                          Default : 0.        ! DSHELF = 0. !
                                          units: km

           COARE warm layer computation (IWARM)               ! IWARM =  0   !
           1: on - 0: off (must be off if SST measured with
           IR radiometer)                 Default: 0

           COARE cool skin layer computation (ICOOL)          ! ICOOL =  0   !
           1: on - 0: off (must be off if SST measured with
           IR radiometer)                 Default: 0

    RELATIVE HUMIDITY PARAMETERS

       3D relative humidity from observations or
       from prognostic data? (IRHPROG)       Default:0        ! IRHPROG =  1   ! VER

          0 = Use RH from SURF.DAT file
              (only if NOOBS = 0,1)
          1 = Use prognostic RH
              (only if NOOBS = 0,1,2)

    TEMPERATURE PARAMETERS

       3D temperature from observations or
       from prognostic data? (ITPROG)        Default:0        ! ITPROG =  2   !

          0 = Use Surface and upper air stations
              (only if NOOBS = 0)
          1 = Use Surface stations (no upper air observations)
              Use MM5/3D.DAT for upper air data
              (only if NOOBS = 0,1)
          2 = No surface or upper air observations
              Use MM5/3D.DAT for surface and upper air data
              (only if NOOBS = 0,1,2)

       Interpolation type
       (1 = 1/R ; 2 = 1/R**2)                Default:1         ! IRAD =  1  !

       Radius of influence for temperature
       interpolation (TRADKM)                Default: 500.     ! TRADKM = 20. ! VER 500
                                             Units: km

       Maximum Number of stations to include
       in temperature interpolation (NUMTS)  Default: 5        ! NUMTS = 5  !

       Conduct spatial averaging of temp-
       eratures (IAVET)  (0=no, 1=yes)       Default: 1        ! IAVET =  1  !
       (will use mixing ht MNMDAV,HAFANG
        so make sure they are correct)

       Default temperature gradient          Default: -.0098   ! TGDEFB = -0.0098 !
       below the mixing height over          Units: K/m
       water (TGDEFB)

       Default temperature gradient          Default: -.0045   ! TGDEFA = -0.0045 !
       above the mixing height over          Units: K/m
       water (TGDEFA)

       Beginning (JWAT1) and ending (JWAT2)
       land use categories for temperature                    ! JWAT1 =  55  ! VER 999
       interpolation over water -- Make                       ! JWAT2 =  55  ! VER 999
       bigger than largest land use to disable

   PRECIP INTERPOLATION PARAMETERS

       Method of interpolation (NFLAGP)      Default: 2       ! NFLAGP =  2  !
        (1=1/R,2=1/R**2,3=EXP/R**2)
       Radius of Influence  (SIGMAP)         Default: 100.0   ! SIGMAP = 50. ! VER
        (0.0 => use half dist. btwn          Units: km
         nearest stns w & w/out
         precip when NFLAGP = 3)
       Minimum Precip. Rate Cutoff (CUTP)    Default: 0.01    ! CUTP = 0.01 !
        (values < CUTP = 0.0 mm/hr)          Units: mm/hr
!END!


-------------------------------------------------------------------------------

INPUT GROUP: 7 -- Surface meteorological station parameters
--------------

     SURFACE STATION VARIABLES
     (One record per station --  5  records in all)


             1     2
         Name   ID            X coord.   Y coord.   Time   Anem.
                               (km)       (km)      zone   Ht.(m)
       ----------------------------------------------------------
* SS1  ='BGR '    14606       514.500     4960.533    5    10  *
* SS2  ='NHZ '    14611       425.022     4859.123    5    10  *
* SS3  ='CON '    14745       296.881     4785.844    5    10  *
* SS4  ='BTV '    14742       169.881     4931.872    5    10  *
* SS5  ='PWM '    14764       393.788     4833.627    5    10  *
-------------------
      1
        Four character string for station name
        (MUST START IN COLUMN 9)

      2
        Six digit integer for station ID

!END!


-------------------------------------------------------------------------------

INPUT GROUP: 8 -- Upper air meteorological station parameters
--------------

     UPPER AIR STATION VARIABLES
     (One record per station --  3  records in all)

             1     2
         Name    ID      X coord.   Y coord.  Time zone
                           (km)       (km)    
        -----------------------------------------------
! US1  ='PWM '   14764    393.788   4833.627    5  !
! US2  ='ALB '   14735    107.130   4744.023    5  !
! US3  ='CHH '   14684    419.496   4613.379    5  !
-------------------
      1
        Four character string for station name
        (MUST START IN COLUMN 9)

      2
        Five digit integer for station ID

!END!


-------------------------------------------------------------------------------

INPUT GROUP: 9 -- Precipitation station parameters
--------------

     PRECIPITATION STATION VARIABLES
     (One record per station --  16  records in all)
     (NOT INCLUDED IF NPSTA = 0)

            1          2
         Name   Station    X coord.  Y coord.
                  Code       (km)      (km)
         ------------------------------------

! PS1  ='ME03'   170273    437.512   4905.276      !
! PS2  ='ME15'   176905    395.159   4833.606      !
! PS3  ='ME18'   177325    378.169   4932.039      !
! PS4  ='ME21'   178641    341.072   4877.271      !
! PS5  ='NH01'   270741    279.698   4799.350      !
! PS6  ='NH02'   270998    280.725   4830.818      !
! PS7  ='NH06'   272842    331.223   4960.879      !
! PS8  ='NH07'   273182    285.634   4815.835      !
! PS9  ='NH10'   274732    286.373   4880.668      !
! PS10 ='NH11'   274808    272.714   4910.780      !
! PS11 ='NH13'   275639    316.421   4903.848      !
! PS12 ='NH15'   275780    323.434   4816.590      !
! PS13 ='NH17'   276234    291.546   4958.336      !
! PS14 ='NH19'   276818    320.412   4903.737      !
! PS15 ='NH22'   278885    267.094   4864.639      !
! PS16 ='VT14'   437054    259.831   4922.363      !

-------------------
      1
        Four character string for station name
        (MUST START IN COLUMN 9)

      2
        Six digit station code composed of state
        code (first 2 digits) and station ID (last
        4 digits)

!END!
'''.format(fdate,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF, YYYY0, MM0, DD0, HH0, YYYY1, MM1, DD1, HH1)
#           0       1         2     3     4    5    6    7    8      9      10      11     12     13     14     15      16    17   18  19    20    21    22   23

    return text


def namelist_calpuff(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF,calsource):

    YYYY0 = str(fdate[0:4])
    MM0   = str(fdate[4:6])
    DD0   = str(fdate[6:8])
    HH0   = str(fdate[8:10])

    endTime = str(datetime.datetime.strptime(fdate, "%Y%m%d%H") + datetime.timedelta(hours=24))

    YYYY1 = str(endTime[0:4])
    MM1   = str(endTime[5:7])
    DD1   = str(endTime[8:10])
    HH1   = str(endTime[11:13])

    # extrayendo info de la fuente
    flon, flat, fprov, fpname, chimenea, base, diametro, veloc, tsalida, dwash, emis1, emis2, emis3, sname = calsource[:]

    # convirtiendo a UTM limites del dominio
    utmFx1,utmFx2,utmFy1,utmFy2 = grad_to_utm(flon, flon, flat, flat)


    # This namelist is only for 00z initialization

    text='''CALPUFF Demonstration Run
(Not intended as a guide for configuring options)

---------------- Run title (3 lines) ------------------------------------------

                    CALPUFF MODEL CONTROL FILE
                    --------------------------

-------------------------------------------------------------------------------

INPUT GROUP: 0 -- Input and Output File Names

--------------
Default Name  Type          File Name
------------  ----          ---------
CALMET.DAT    input    ! METDAT = calmet.dat   !
    or
ISCMET.DAT    input    * ISCDAT =             *
    or
PLMMET.DAT    input    * PLMDAT =             *
    or
PROFILE.DAT   input    * PRFDAT =             *
SURFACE.DAT   input    * SFCDAT =             *
RESTARTB.DAT  input    * RSTARTB=             *
--------------------------------------------------------------------------------
CALPUFF.LST   output   ! PUFLST =CALPUFF.LST  !
CONC.DAT      output   ! CONDAT =CONC.DAT     !
DFLX.DAT      output   * DFDAT  =CALPUFF.DRY     *
WFLX.DAT      output   * WFDAT  =CALPUFF.WET     *

VISB.DAT      output   * VISDAT =CALPUFF.VIS     *
TK2D.DAT      output   * T2DDAT =             *
RHO2D.DAT     output   * RHODAT =             *
RESTARTE.DAT  output   * RSTARTE=             *
--------------------------------------------------------------------------------
Emission Files
--------------
PTEMARB.DAT   input    * PTDAT  =             *
VOLEMARB.DAT  input    * VOLDAT =             *
BAEMARB.DAT   input    * ARDAT  =             *
LNEMARB.DAT   input    * LNDAT  =             *
--------------------------------------------------------------------------------
Other Files
-----------
OZONE.DAT     input    * OZDAT  =OZONE.DAT    *
VD.DAT        input    * VDDAT  =             *
CHEM.DAT      input    * CHEMDAT=             *
H2O2.DAT      input    * H2O2DAT=             *
HILL.DAT      input    * HILDAT=             *
HILLRCT.DAT   input    * RCTDAT=             *
COASTLN.DAT   input    * CSTDAT=             *
FLUXBDY.DAT   input    * BDYDAT=             *
BCON.DAT      input    * BCNDAT=             *
DEBUG.DAT     output   * DEBUG =             *
MASSFLX.DAT   output   * FLXDAT=             *
MASSBAL.DAT   output   * BALDAT=             *
FOG.DAT       output   * FOGDAT=             *
--------------------------------------------------------------------------------
All file names will be converted to lower case if LCFILES = T
Otherwise, if LCFILES = F, file names will be converted to UPPER CASE
         T = lower case      ! LCFILES = T !
         F = UPPER CASE
NOTE: (1) file/path names can be up to 70 characters in length


Provision for multiple input files
----------------------------------

     Number of CALMET.DAT files for run (NMETDAT)
                                     Default: 1       ! NMETDAT =   1   !

     Number of PTEMARB.DAT files for run (NPTDAT)
                                     Default: 0       ! NPTDAT =  0  !

     Number of BAEMARB.DAT files for run (NARDAT)
                                     Default: 0       ! NARDAT =  0  !

     Number of VOLEMARB.DAT files for run (NVOLDAT)
                                     Default: 0       ! NVOLDAT =  0  !

!END!

-------------
Subgroup (0a)
-------------

  The following CALMET.DAT filenames are processed in sequence if NMETDAT>1

Default Name  Type          File Name
------------  ----          ---------
 none         input    * METDAT=     *   *END*


--------------------------------------------------------------------------------

INPUT GROUP: 1 -- General run control parameters
--------------

    Option to run all periods found
    in the met. file     (METRUN)   Default: 0       ! METRUN =   0  !

         METRUN = 0 - Run period explicitly defined below
         METRUN = 1 - Run all periods in met. file

     Starting date:    Year   (IBYR)  --    No default   ! IBYR  =  {16}  !
                       Month  (IBMO)  --    No default   ! IBMO  =  {17}  !
                       Day    (IBDY)  --    No default   ! IBDY  =  {18}  !
     Starting time:    Hour   (IBHR)  --    No default   ! IBHR  =  0  !
                       Minute (IBMIN) --    No default   ! IBMIN =  0  !
                       Second (IBSEC) --    No default   ! IBSEC =  0  !

     Ending date:      Year   (IEYR)  --    No default   ! IEYR  =  {20}  !
                       Month  (IEMO)  --    No default   ! IEMO  =  {21}  !
                       Day    (IEDY)  --    No default   ! IEDY  =  {22}  !
     Ending time:      Hour   (IEHR)  --    No default   ! IEHR  =  19  !
                       Minute (IEMIN) --    No default   ! IEMIN =  0  !
                       Second (IESEC) --    No default   ! IESEC =  0  !

     (These are only used if METRUN = 0)

     Base time zone        (XBTZ) -- No default       ! XBTZ= 5.0  !
     The zone is the number of hours that must be
     ADDED to the time to obtain UTC (or GMT)
     Examples: PST = 8., MST = 7.
               CST = 6., EST = 5.

     Length of modeling time-step (seconds)
     Equal to update period in the primary
     meteorological data files, or an
     integer fraction of it (1/2, 1/3 ...)
     Must be no larger than 1 hour
     (NSECDT)                        Default:3600     ! NSECDT =  3600  !
                                     Units: seconds

     Number of chemical species (NSPEC)
                                     Default: 5       ! NSPEC =  3   !

     Number of chemical species
     to be emitted  (NSE)            Default: 3       ! NSE =  3   !

     Flag to stop run after
     SETUP phase (ITEST)             Default: 2       ! ITEST =  2   !
     (Used to allow checking
     of the model inputs, files, etc.)
           ITEST = 1 - STOPS program after SETUP phase
           ITEST = 2 - Continues with execution of program
                       after SETUP

     Restart Configuration:

        Control flag (MRESTART)      Default: 0       ! MRESTART =  0   !

           0 = Do not read or write a restart file
           1 = Read a restart file at the beginning of
               the run
           2 = Write a restart file during run
           3 = Read a restart file at beginning of run
               and write a restart file during run

        Number of periods in Restart
        output cycle (NRESPD)        Default: 0       ! NRESPD =  0   !

           0 = File written only at last period
          >0 = File updated every NRESPD periods

     Meteorological Data Format (METFM)
                                     Default: 1       ! METFM =  1   !

           METFM = 1 - CALMET binary file (CALMET.MET)
           METFM = 2 - ISC ASCII file (ISCMET.MET)
           METFM = 3 - AUSPLUME ASCII file (PLMMET.MET)
           METFM = 4 - CTDM plus tower file (PROFILE.DAT) and
                       surface parameters file (SURFACE.DAT)
           METFM = 5 - AERMET tower file (PROFILE.DAT) and
                       surface parameters file (SURFACE.DAT)

     Meteorological Profile Data Format (MPRFFM)
            (used only for METFM = 1, 2, 3)
                                     Default: 1       ! MPRFFM =  1   !

           MPRFFM = 1 - CTDM plus tower file (PROFILE.DAT)
           MPRFFM = 2 - AERMET tower file (PROFILE.DAT)

     PG sigma-y is adjusted by the factor (AVET/PGTIME)**0.2
     Averaging Time (minutes) (AVET)
                                     Default: 60.0    ! AVET = 60. !
     PG Averaging Time (minutes) (PGTIME)
                                     Default: 60.0    ! PGTIME = 60. !


!END!


-------------------------------------------------------------------------------

INPUT GROUP: 2 -- Technical options
--------------


     Vertical distribution used in the
     near field (MGAUSS)                   Default: 1     ! MGAUSS =  1   !
        0 = uniform
        1 = Gaussian

     Terrain adjustment method
     (MCTADJ)                              Default: 3     ! MCTADJ =  3   !
        0 = no adjustment
        1 = ISC-type of terrain adjustment
        2 = simple, CALPUFF-type of terrain
            adjustment 
        3 = partial plume path adjustment

     Subgrid-scale complex terrain
     flag (MCTSG)                          Default: 0     ! MCTSG =  0   !
        0 = not modeled
        1 = modeled

     Near-field puffs modeled as
     elongated slugs? (MSLUG)              Default: 0     ! MSLUG =  0   !
        0 = no
        1 = yes (slug model used)

     Transitional plume rise modeled?
     (MTRANS)                              Default: 1     ! MTRANS =  1   !
        0 = no  (i.e., final rise only)
        1 = yes (i.e., transitional rise computed)

     Stack tip downwash? (MTIP)            Default: 1     ! MTIP =  1  !
        0 = no  (i.e., no stack tip downwash)
        1 = yes (i.e., use stack tip downwash)

     Method used to simulate building
     downwash? (MBDW)                      Default: 1     ! MBDW =   1  !
        1 = ISC method
        2 = PRIME method

     Vertical wind shear modeled above
     stack top? (MSHEAR)                   Default: 0     ! MSHEAR =  0  !
        0 = no  (i.e., vertical wind shear not modeled)
        1 = yes (i.e., vertical wind shear modeled)

     Puff splitting allowed? (MSPLIT)      Default: 0     ! MSPLIT =  0  !
        0 = no (i.e., puffs not split)
        1 = yes (i.e., puffs are split)

     Chemical mechanism flag (MCHEM)       Default: 1     ! MCHEM =  0   !  VER OPT 3 como venia
        0 = chemical transformation not
            modeled
        1 = transformation rates computed
            internally (MESOPUFF II scheme)
        2 = user-specified transformation
            rates used
        3 = transformation rates computed
            internally (RIVAD/ARM3 scheme)
        4 = secondary organic aerosol formation
            computed (MESOPUFF II scheme for OH)

     Aqueous phase transformation flag (MAQCHEM)
     (Used only if MCHEM = 1, or 3)        Default: 0     ! MAQCHEM =  0   !
        0 = aqueous phase transformation
            not modeled
        1 = transformation rates adjusted
            for aqueous phase reactions

     Wet removal modeled ? (MWET)          Default: 1     ! MWET =  1   !
        0 = no
        1 = yes

     Dry deposition modeled ? (MDRY)       Default: 1     ! MDRY =  1   !
        0 = no
        1 = yes
        (dry deposition method specified
         for each species in Input Group 3)


     Gravitational settling (plume tilt)
     modeled ? (MTILT)                     Default: 0     ! MTILT =  0   !
        0 = no
        1 = yes
        (puff center falls at the gravitational
         settling velocity for 1 particle species)

     Restrictions:
         - MDRY  = 1
         - NSPEC = 1  (must be particle species as well)
         - sg    = 0  GEOMETRIC STANDARD DEVIATION in Group 8 is
                      set to zero for a single particle diameter

     Method used to compute dispersion
     coefficients (MDISP)                  Default: 3     ! MDISP =  3   !

        1 = dispersion coefficients computed from measured values
            of turbulence, sigma v, sigma w
        2 = dispersion coefficients from internally calculated 
            sigma v, sigma w using micrometeorological variables
            (u*, w*, L, etc.)
        3 = PG dispersion coefficients for RURAL areas (computed using
            the ISCST multi-segment approximation) and MP coefficients in
            urban areas
        4 = same as 3 except PG coefficients computed using
            the MESOPUFF II eqns.
        5 = CTDM sigmas used for stable and neutral conditions.
            For unstable conditions, sigmas are computed as in
            MDISP = 3, described above.  MDISP = 5 assumes that
            measured values are read

     Sigma-v/sigma-theta, sigma-w measurements used? (MTURBVW)
     (Used only if MDISP = 1 or 5)         Default: 3     ! MTURBVW =  3  !
        1 = use sigma-v or sigma-theta measurements
            from PROFILE.DAT to compute sigma-y
            (valid for METFM = 1, 2, 3, 4, 5)
        2 = use sigma-w measurements
            from PROFILE.DAT to compute sigma-z
            (valid for METFM = 1, 2, 3, 4, 5)
        3 = use both sigma-(v/theta) and sigma-w
            from PROFILE.DAT to compute sigma-y and sigma-z
            (valid for METFM = 1, 2, 3, 4, 5)
        4 = use sigma-theta measurements
            from PLMMET.DAT to compute sigma-y
            (valid only if METFM = 3)

     Back-up method used to compute dispersion
     when measured turbulence data are
     missing (MDISP2)                      Default: 3     ! MDISP2 =  3  !
     (used only if MDISP = 1 or 5)
        2 = dispersion coefficients from internally calculated 
            sigma v, sigma w using micrometeorological variables
            (u*, w*, L, etc.)
        3 = PG dispersion coefficients for RURAL areas (computed using
            the ISCST multi-segment approximation) and MP coefficients in
            urban areas
        4 = same as 3 except PG coefficients computed using
            the MESOPUFF II eqns.

     [DIAGNOSTIC FEATURE]
     Method used for Lagrangian timescale for Sigma-y
     (used only if MDISP=1,2 or MDISP2=1,2)
     (MTAULY)                              Default: 0     ! MTAULY =  0  !
        0 = Draxler default 617.284 (s)
        1 = Computed as Lag. Length / (.75 q) -- after SCIPUFF
       10 < Direct user input (s)             -- e.g., 306.9


     [DIAGNOSTIC FEATURE]
     Method used for Advective-Decay timescale for Turbulence
     (used only if MDISP=2 or MDISP2=2)
     (MTAUADV)                             Default: 0     ! MTAUADV =  0  !
        0 = No turbulence advection
        1 = Computed (OPTION NOT IMPLEMENTED)
       10 < Direct user input (s)   -- e.g., 800


     Method used to compute turbulence sigma-v &
     sigma-w using micrometeorological variables
     (Used only if MDISP = 2 or MDISP2 = 2)
     (MCTURB)                              Default: 1     ! MCTURB =  1  !
        1 = Standard CALPUFF subroutines
        2 = AERMOD subroutines

     PG sigma-y,z adj. for roughness?      Default: 0     ! MROUGH =  0  !
     (MROUGH)
        0 = no
        1 = yes

     Partial plume penetration of          Default: 1     ! MPARTL =  1  !
     elevated inversion modeled for
     point sources?
     (MPARTL)
        0 = no
        1 = yes

     Partial plume penetration of          Default: 1     ! MPARTLBA =  1  !  VER, DAYANA USA 0
     elevated inversion modeled for
     buoyant area sources?
     (MPARTLBA)
        0 = no
        1 = yes

     Strength of temperature inversion     Default: 0     ! MTINV =  0  !
     provided in PROFILE.DAT extended records?
     (MTINV)
        0 = no (computed from measured/default gradients)
        1 = yes

     PDF used for dispersion under convective conditions?
                                           Default: 0     ! MPDF =  0  !
     (MPDF)
        0 = no
        1 = yes

     Sub-Grid TIBL module used for shore line?
                                           Default: 0     ! MSGTIBL = 0  !
     (MSGTIBL)
        0 = no
        1 = yes

     Boundary conditions (concentration) modeled?
                                           Default: 0     ! MBCON = 0  !
     (MBCON)
        0 = no
        1 = yes, using formatted BCON.DAT file
        2 = yes, using unformatted CONC.DAT file

     Note:  MBCON > 0 requires that the last species modeled
            be 'BCON'.  Mass is placed in species BCON when
            generating boundary condition puffs so that clean
            air entering the modeling domain can be simulated
            in the same way as polluted air.  Specify zero
            emission of species BCON for all regular sources.

     Individual source contributions saved?
                                           Default: 0     ! MSOURCE = 0  !
     (MSOURCE)
        0 = no
        1 = yes


     Analyses of fogging and icing impacts due to emissions from
     arrays of mechanically-forced cooling towers can be performed
     using CALPUFF in conjunction with a cooling tower emissions
     processor (CTEMISS) and its associated postprocessors.  Hourly
     emissions of water vapor and temperature from each cooling tower
     cell are computed for the current cell configuration and ambient
     conditions by CTEMISS. CALPUFF models the dispersion of these
     emissions and provides cloud information in a specialized format
     for further analysis. Output to FOG.DAT is provided in either
     'plume mode' or 'receptor mode' format.

     Configure for FOG Model output?
                                           Default: 0     ! MFOG =  0   !
     (MFOG)
        0 = no
        1 = yes  - report results in PLUME Mode format
        2 = yes  - report results in RECEPTOR Mode format


     Test options specified to see if
     they conform to regulatory
     values? (MREG)                        Default: 1     ! MREG =  0   !

        0 = NO checks are made
        1 = Technical options must conform to USEPA
            Long Range Transport (LRT) guidance
                       METFM    1 or 2
                       AVET     60. (min)
                       PGTIME   60. (min)
                       MGAUSS   1
                       MCTADJ   3
                       MTRANS   1
                       MTIP     1
                       MCHEM    1 or 3 (if modeling SOx, NOx)
                       MWET     1
                       MDRY     1
                       MDISP    2 or 3
                       MPDF     0 if MDISP=3
                                1 if MDISP=2
                       MROUGH   0
                       MPARTL   1
                       MPARTLBA 0
                       SYTDEP   550. (m)
                       MHFTSZ   0
                       SVMIN    0.5 (m/s)


!END!


-------------------------------------------------------------------------------

INPUT GROUP: 3a, 3b -- Species list
-------------------

------------
Subgroup (3a)
------------

  The following species are modeled:

! CSPEC =          SO2 !         !END!
! CSPEC =          NOX !         !END!
! CSPEC =           CO !         !END!

* CSPEC =          SO4 *         *END*
* CSPEC =           NO *         *END* VER, VERSION DE DAYANA COMBINA en NOX
* CSPEC =         HNO3 *         *END*
* CSPEC =          NO3 *         *END*
* CSPEC =         PM10 *         *END*

                                                       Dry                OUTPUT GROUP
    SPECIES          MODELED          EMITTED       DEPOSITED                NUMBER
     NAME         (0=NO, 1=YES)    (0=NO, 1=YES)    (0=NO,                 (0=NONE,
   (Limit: 12                                        1=COMPUTED-GAS        1=1st CGRUP,
    Characters                                       2=COMPUTED-PARTICLE   2=2nd CGRUP,
    in length)                                       3=USER-SPECIFIED)     3= etc.)

*          SO2  =         1,               1,           1,                 0   *
*          SO4  =         1,               0,           2,                 0   *
*           NO  =         1,               1,           1,                 0   *
*          NO2  =         1,               1,           1,                 0   *
*         HNO3  =         1,               0,           1,                 0   *
*          NO3  =         1,               0,           2,                 0   *
*         PM10  =         1,               1,           2,                 0   *

!           CO  =         1,               1,           1,                 0   !
!          NOX  =         1,               1,           1,                 0   !
!          SO2  =         1,               1,           1,                 0   !

!END!

  Note:  The last species in (3a) must be 'BCON' when using the
         boundary condition option (MBCON > 0).  Species BCON should
         typically be modeled as inert (no chem transformation or
         removal).


-------------
Subgroup (3b)
-------------
  The following names are used for Species-Groups in which results
  for certain species are combined (added) prior to output.  The
  CGRUP name will be used as the species name in output files.
  Use this feature to model specific particle-size distributions
  by treating each size-range as a separate species.
  Order must be consistent with 3(a) above.



-------------------------------------------------------------------------------


INPUT GROUP: 4 -- Map Projection and Grid control parameters
--------------

     Projection for all (X,Y):
     -------------------------

     Map projection
     (PMAP)                     Default: UTM    ! PMAP = UTM  !

         UTM :  Universal Transverse Mercator
         TTM :  Tangential Transverse Mercator
         LCC :  Lambert Conformal Conic
          PS :  Polar Stereographic
          EM :  Equatorial Mercator
        LAZA :  Lambert Azimuthal Equal Area

     False Easting and Northing (km) at the projection origin
     (Used only if PMAP= TTM, LCC, or LAZA)
     (FEAST)                    Default=0.0     ! FEAST  = 0.000  !
     (FNORTH)                   Default=0.0     ! FNORTH = 0.000  !

     UTM zone (1 to 60)
     (Used only if PMAP=UTM)
     (IUTMZN)                   No Default      ! IUTMZN =  17   !

     Hemisphere for UTM projection?
     (Used only if PMAP=UTM)
     (UTMHEM)                   Default: N      ! UTMHEM = N  !
         N   :  Northern hemisphere projection
         S   :  Southern hemisphere projection

     Latitude and Longitude (decimal degrees) of projection origin
     (Used only if PMAP= TTM, LCC, PS, EM, or LAZA)
     (RLAT0)                    No Default      ! RLAT0 = 0N  !
     (RLON0)                    No Default      ! RLON0 = 0E  !

         TTM :  RLON0 identifies central (true N/S) meridian of projection
                RLAT0 selected for convenience
         LCC :  RLON0 identifies central (true N/S) meridian of projection
                RLAT0 selected for convenience
         PS  :  RLON0 identifies central (grid N/S) meridian of projection
                RLAT0 selected for convenience
         EM  :  RLON0 identifies central meridian of projection
                RLAT0 is REPLACED by 0.0N (Equator)
         LAZA:  RLON0 identifies longitude of tangent-point of mapping plane
                RLAT0 identifies latitude of tangent-point of mapping plane

     Matching parallel(s) of latitude (decimal degrees) for projection
     (Used only if PMAP= LCC or PS)
     (XLAT1)                    No Default      ! XLAT1 = 0N  !
     (XLAT2)                    No Default      ! XLAT2 = 0N  !

         LCC :  Projection cone slices through Earth's surface at XLAT1 and XLAT2
         PS  :  Projection plane slices through Earth at XLAT1
                (XLAT2 is not used)

     ----------
     Note:  Latitudes and longitudes should be positive, and include a
            letter N,S,E, or W indicating north or south latitude, and
            east or west longitude.  For example,
            35.9  N Latitude  =  35.9N
            118.7 E Longitude = 118.7E


     Datum-region
     ------------

     The Datum-Region for the coordinates is identified by a character
     string.  Many mapping products currently available use the model of the
     Earth known as the World Geodetic System 1984 (WGS-84).  Other local
     models may be in use, and their selection in CALMET will make its output
     consistent with local mapping products.  The list of Datum-Regions with
     official transformation parameters is provided by the National Imagery and
     Mapping Agency (NIMA).

     NIMA Datum - Regions(Examples)
     ------------------------------------------------------------------------------
     WGS-84    WGS-84 Reference Ellipsoid and Geoid, Global coverage (WGS84)
     NAS-C     NORTH AMERICAN 1927 Clarke 1866 Spheroid, MEAN FOR CONUS (NAD27)
     NAR-C     NORTH AMERICAN 1983 GRS 80 Spheroid, MEAN FOR CONUS (NAD83)
     NWS-84    NWS 6370KM Radius, Sphere
     ESR-S     ESRI REFERENCE 6371KM Radius, Sphere

     Datum-region for output coordinates
     (DATUM)                    Default: WGS-84    ! DATUM = WGS-84  !


METEOROLOGICAL Grid:

     Rectangular grid defined for projection PMAP,
     with X the Easting and Y the Northing coordinate

            No. X grid cells (NX)      No default     ! NX =  120   !
            No. Y grid cells (NY)      No default     ! NY =  90   !
         No. vertical layers (NZ)      No default     ! NZ =  10   !

           Grid spacing (DGRIDKM)      No default     ! DGRIDKM = 1.0 !
                                       Units: km

                Cell face heights
                    (ZFACE(nz+1))      No defaults
                                       Units: m
   ! ZFACE = .0, 20.0, 40.0, 80.0, 160.0, 300.0, 600.0, 1000.0, 1500.0, 2200.0, 3000.0 !

            Reference Coordinates
           of SOUTHWEST corner of
                 grid cell(1, 1):

            X coordinate (XORIGKM)     No default     ! XORIGKM = {8} !
            Y coordinate (YORIGKM)     No default     ! YORIGKM = {10} !
                                      Units: km


COMPUTATIONAL Grid:

     The computational grid is identical to or a subset of the MET. grid.
     The lower left (LL) corner of the computational grid is at grid point
     (IBCOMP, JBCOMP) of the MET. grid.  The upper right (UR) corner of the
     computational grid is at grid point (IECOMP, JECOMP) of the MET. grid.
     The grid spacing of the computational grid is the same as the MET. grid.

        X index of LL corner (IBCOMP)      No default     ! IBCOMP =  1   !
                  (1 <= IBCOMP <= NX)

        Y index of LL corner (JBCOMP)      No default     ! JBCOMP =  1   !
                  (1 <= JBCOMP <= NY)


        X index of UR corner (IECOMP)      No default     ! IECOMP =  120   !  VER
                  (1 <= IECOMP <= NX)

        Y index of UR corner (JECOMP)      No default     ! JECOMP =  90   !   VER
                  (1 <= JECOMP <= NY)



SAMPLING Grid (GRIDDED RECEPTORS):

     The lower left (LL) corner of the sampling grid is at grid point
     (IBSAMP, JBSAMP) of the MET. grid.  The upper right (UR) corner of the
     sampling grid is at grid point (IESAMP, JESAMP) of the MET. grid.
     The sampling grid must be identical to or a subset of the computational
     grid.  It may be a nested grid inside the computational grid.
     The grid spacing of the sampling grid is DGRIDKM/MESHDN.

        Logical flag indicating if gridded
        receptors are used (LSAMP)         Default: T     ! LSAMP = T !
        (T=yes, F=no)

        X index of LL corner (IBSAMP)      No default     ! IBSAMP =  1   ! VER
         (IBCOMP <= IBSAMP <= IECOMP)

        Y index of LL corner (JBSAMP)      No default     ! JBSAMP =  1   ! VER
         (JBCOMP <= JBSAMP <= JECOMP)


        X index of UR corner (IESAMP)      No default     ! IESAMP =  120   ! VER
         (IBCOMP <= IESAMP <= IECOMP)

        Y index of UR corner (JESAMP)      No default     ! JESAMP =  90   !  VER
         (JBCOMP <= JESAMP <= JECOMP)


       Nesting factor of the sampling
        grid (MESHDN)                      Default: 1     ! MESHDN =  1  !
        (MESHDN is an integer >= 1)

!END!


-------------------------------------------------------------------------------


INPUT GROUP: 5 -- Output Options
--------------
                                             *                          *
     FILE                       DEFAULT VALUE             VALUE THIS RUN
     ----                       -------------             --------------

   Concentrations (ICON)              1                   !  ICON =  1   !
   Dry Fluxes (IDRY)                  1                   !  IDRY =  1   !
   Wet Fluxes (IWET)                  1                   !  IWET =  1   !
   2D Temperature (IT2D)              0                   !  IT2D =  0   !
   2D Density (IRHO)                  0                   !  IRHO =  0   !
   Relative Humidity (IVIS)           1                   !  IVIS =  1   !
    (relative humidity file is
     required for visibility
     analysis)
   Use data compression option in output file?
   (LCOMPRS)                           Default: T         ! LCOMPRS = T !

   *
    0 = Do not create file, 1 = create file


    QA PLOT FILE OUTPUT OPTION:

       Create a standard series of output files (e.g.
       locations of sources, receptors, grids ...)
       suitable for plotting?
       (IQAPLOT)                       Default: 1         !  IQAPLOT =  1   !
         0 = no
         1 = yes

    DIAGNOSTIC MASS FLUX OUTPUT OPTIONS:

       Mass flux across specified boundaries
       for selected species reported?
       (IMFLX)                         Default: 0         ! IMFLX =  0  !
         0 = no
         1 = yes (FLUXBDY.DAT and MASSFLX.DAT filenames
                  are specified in Input Group 0)

       Mass balance for each species
       reported?
       (IMBAL)                         Default: 0         ! IMBAL =  0  !
         0 = no
         1 = yes (MASSBAL.DAT filename is
              specified in Input Group 0)


    LINE PRINTER OUTPUT OPTIONS:

       Print concentrations (ICPRT)    Default: 0         ! ICPRT =  1   !
       Print dry fluxes (IDPRT)        Default: 0         ! IDPRT =  0   !
       Print wet fluxes (IWPRT)        Default: 0         ! IWPRT =  0   !
       (0 = Do not print, 1 = Print)

       Concentration print interval
       (ICFRQ) in timesteps            Default: 1         ! ICFRQ =  1   !
       Dry flux print interval
       (IDFRQ) in timesteps            Default: 1         ! IDFRQ =  1   !
       Wet flux print interval
       (IWFRQ) in timesteps            Default: 1         ! IWFRQ =  1   !

       Units for Line Printer Output
       (IPRTU)                         Default: 1         ! IPRTU =  3   !  VER , DAYANA USA 2
                       for            for
                  Concentration    Deposition
           1 =       g/m**3         g/m**2/s
           2 =      mg/m**3        mg/m**2/s
           3 =      ug/m**3        ug/m**2/s
           4 =      ng/m**3        ng/m**2/s
           5 =     Odour Units

       Messages tracking progress of run
       written to the screen ?
       (IMESG)                         Default: 2         ! IMESG =  2   !
         0 = no
         1 = yes (advection step, puff ID)
         2 = yes (YYYYJJJHH, # old puffs, # emitted puffs)


     SPECIES (or GROUP for combined species) LIST FOR OUTPUT OPTIONS

                 ---- CONCENTRATIONS ----   ------ DRY FLUXES ------   ------ WET FLUXES ------   -- MASS FLUX --
   SPECIES
   /GROUP        PRINTED?  SAVED ON DISK?   PRINTED?  SAVED ON DISK?   PRINTED?  SAVED ON DISK?   SAVED ON DISK?
   -------       ------------------------   ------------------------   ------------------------   ---------------
*          SO2 =     1,           1,           0,           1,           0,           1,           0   *
*          SO4 =     0,           1,           0,           1,           0,           1,           0   *
*           NO =     0,           1,           0,           1,           0,           1,           0   *
*          NO2 =     0,           1,           0,           1,           0,           1,           0   *
*         HNO3 =     0,           1,           0,           1,           0,           1,           0   *
*          NO3 =     0,           1,           0,           1,           0,           1,           0   *
*         PM10 =     0,           1,           0,           1,           0,           1,           0   *

!           CO =     1,           1,           0,           1,           0,           1,           0   !
!          NOX =     1,           1,           0,           1,           0,           1,           0   !
!          SO2 =     1,           1,           0,           1,           0,           1,           0   !

  Note:  Species BCON (for MBCON > 0) does not need to be saved on disk.


     OPTIONS FOR PRINTING "DEBUG" QUANTITIES (much output)   

       Logical for debug output
       (LDEBUG)                                 Default: F     ! LDEBUG = F !

       First puff to track
       (IPFDEB)                                 Default: 1     ! IPFDEB =  1  !

       Number of puffs to track
       (NPFDEB)                                 Default: 1     ! NPFDEB =  1  !

       Met. period to start output
       (NN1)                                    Default: 1     ! NN1 =  1   !

       Met. period to end output
       (NN2)                                    Default: 10    ! NN2 =  10  !

!END!


-------------------------------------------------------------------------------


INPUT GROUP: 6a, 6b, & 6c -- Subgrid scale complex terrain inputs
-------------------------

---------------
Subgroup (6a)
---------------
       Number of terrain features (NHILL)       Default: 0     ! NHILL =  0   !

       Number of special complex terrain
       receptors  (NCTREC)                      Default: 0     ! NCTREC =  0   !

       Terrain and CTSG Receptor data for 
       CTSG hills input in CTDM format ?
       (MHILL)                                  No Default     ! MHILL =  2   !
       1 = Hill and Receptor data created
           by CTDM processors & read from
           HILL.DAT and HILLRCT.DAT files
       2 = Hill data created by OPTHILL &
           input below in Subgroup (6b);
           Receptor data in Subgroup (6c)

       Factor to convert horizontal dimensions  Default: 1.0   ! XHILL2M = 1.0 !
       to meters (MHILL=1)

       Factor to convert vertical dimensions    Default: 1.0   ! ZHILL2M = 1.0 !
       to meters (MHILL=1)

       X-origin of CTDM system relative to      No Default     ! XCTDMKM = 0 !
       CALPUFF coordinate system, in Kilometers (MHILL=1)

       Y-origin of CTDM system relative to      No Default     ! YCTDMKM = 0 !
       CALPUFF coordinate system, in Kilometers (MHILL=1)

! END !

---------------
Subgroup (6b)
---------------

                      1 **
     HILL information


HILL           XC        YC       THETAH  ZGRID  RELIEF    EXPO 1    EXPO 2   SCALE 1    SCALE 2    AMAX1     AMAX2
 NO.          (km)      (km)      (deg.)   (m)     (m)      (m)       (m)       (m)        (m)       (m)       (m)
----          ----      ----      ------  -----  ------    ------    ------   -------    -------    -----     -----

---------------
Subgroup (6c)
---------------

    COMPLEX TERRAIN RECEPTOR INFORMATION

                      XRCT         YRCT        ZRCT          XHH
                      (km)         (km)         (m)
                     ------        -----      ------         ----


-------------------
1
     Description of Complex Terrain Variables:
          XC, YC  = Coordinates of center of hill
          THETAH  = Orientation of major axis of hill (clockwise from
                    North)
          ZGRID   = Height of the  0  of the grid above mean sea
                    level
          RELIEF  = Height of the crest of the hill above the grid elevation
          EXPO 1  = Hill-shape exponent for the major axis
          EXPO 2  = Hill-shape exponent for the major axis
          SCALE 1 = Horizontal length scale along the major axis
          SCALE 2 = Horizontal length scale along the minor axis
          AMAX    = Maximum allowed axis length for the major axis
          BMAX    = Maximum allowed axis length for the major axis

          XRCT, YRCT = Coordinates of the complex terrain receptors
          ZRCT    = Height of the ground (MSL) at the complex terrain
                    Receptor
          XHH     = Hill number associated with each complex terrain receptor
                    (NOTE: MUST BE ENTERED AS A REAL NUMBER)

   **
     NOTE: DATA for each hill and CTSG receptor are treated as a separate
           input subgroup and therefore must end with an input group terminator.

-------------------------------------------------------------------------------


INPUT GROUP: 7 -- Chemical parameters for dry deposition of gases
--------------

      SPECIES     DIFFUSIVITY      ALPHA STAR      REACTIVITY    MESOPHYLL RESISTANCE     HENRY'S LAW COEFFICIENT
       NAME        (cm**2/s)                                            (s/cm)                (dimensionless)
      -------     -----------      ----------      ----------    --------------------     -----------------------

*          SO2 =      .1509,        1000.0,           8.0,                .0,                   .04 *
*           NO =      .1345,           1.0,           2.0,              25.0,                  18.0 *
*          NO2 =      .1656,           1.0,           8.0,               5.0,                   3.5 *
*         HNO3 =      .1628,           1.0,          18.0,                .0,              .0000001 *

!           CO =       .186,           1.0,           2.0,              61.0,                  44.0 !
!          NOX =      .1656,           1.0,           8.0,               5.0,                   3.5 !
!          SO2 =      .1509,        1000.0,           8.0,                .0,                   .04 !

!END!


-------------------------------------------------------------------------------


INPUT GROUP: 8 -- Size parameters for dry deposition of particles
--------------

     For SINGLE SPECIES, the mean and standard deviation are used to
     compute a deposition velocity for NINT (see group 9) size-ranges,
     and these are then averaged to obtain a mean deposition velocity.

     For GROUPED SPECIES, the size distribution should be explicitly
     specified (by the 'species' in the group), and the standard deviation
     for each should be entered as 0.  The model will then use the
     deposition velocity for the stated mean diameter.

      SPECIES      GEOMETRIC MASS MEAN        GEOMETRIC STANDARD
       NAME             DIAMETER                   DEVIATION
                        (microns)                  (microns)
      -------      -------------------        ------------------
*          SO4 =           .48,                     2.0   *
*          NO3 =           .48,                     2.0   *
*         PM10 =           .48,                     2.0   *

!          CO =           .48,                     2.0   !  Ver, esto no venia
!         NOX =           .48,                     2.0   !
!         SO2 =           .48,                     2.0   !

!END!


-------------------------------------------------------------------------------


INPUT GROUP: 9 -- Miscellaneous dry deposition parameters
--------------

     Reference cuticle resistance (s/cm)
     (RCUTR)                           Default: 30    !  RCUTR = 30.0 !
     Reference ground resistance  (s/cm)
     (RGR)                             Default: 10    !    RGR = 5.0 ! VER, DAYANA USA 10.0 
     Reference pollutant reactivity
     (REACTR)                          Default: 8     ! REACTR = 8.0 !

     Number of particle-size intervals used to 
     evaluate effective particle deposition velocity
     (NINT)                            Default: 9     !   NINT =  9  !

     Vegetation state in unirrigated areas
     (IVEG)                            Default: 1     !   IVEG =  1   !
        IVEG=1 for active and unstressed vegetation
        IVEG=2 for active and stressed vegetation
        IVEG=3 for inactive vegetation

!END!


-------------------------------------------------------------------------------


INPUT GROUP: 10 -- Wet Deposition Parameters
---------------

                                                          
                      Scavenging Coefficient -- Units: (sec)**(-1)

       Pollutant      Liquid Precip.       Frozen Precip.
       ---------      --------------       --------------
*          SO2 =         3.0E-05,              0.0E00 *
*          SO4 =         1.0E-04,             3.0E-05 *
*         HNO3 =         6.0E-05,              0.0E00 *
*          NO3 =         1.0E-04,             3.0E-05 *
*         PM10 =         1.0E-04,             3.0E-05 *

!           CO =          0.0E00,              0.0E00 !
!          NOX =          0.0E00,              0.0E00 !
!          SO2 =         3.0E-05,              0.0E00 !

!END!


-------------------------------------------------------------------------------


INPUT GROUP: 11 -- Chemistry Parameters
---------------

     Ozone data input option (MOZ)     Default: 1            ! MOZ =  1   !
     (Used only if MCHEM = 1, 3, or 4)
        0 = use a monthly background ozone value
        1 = read hourly ozone concentrations from
            the OZONE.DAT data file

     Monthly ozone concentrations
     (Used only if MCHEM = 1, 3, or 4 and 
      MOZ = 0 or MOZ = 1 and all hourly O3 data missing)
     (BCKO3) in ppb                    Default: 12*80.
     !  BCKO3 = 40.00, 40.00, 40.00, 40.00, 40.00, 40.00, 40.00, 40.00, 40.00, 40.00, 40.00, 40.00 ! VER

     Monthly ammonia concentrations
     (Used only if MCHEM = 1, or 3)
     (BCKNH3) in ppb                   Default: 12*10.       
     !  BCKNH3 = 10.00, 10.00, 10.00, 10.00, 10.00, 10.00, 10.00, 10.00, 10.00, 10.00, 10.00, 10.00 ! VER

     Nighttime SO2 loss rate (RNITE1)
     in percent/hour                   Default: 0.2          ! RNITE1 = .2 !

     Nighttime NOx loss rate (RNITE2)
     in percent/hour                   Default: 2.0          ! RNITE2 = 2.0 !

     Nighttime HNO3 formation rate (RNITE3)
     in percent/hour                   Default: 2.0          ! RNITE3 = 2.0 !

     H2O2 data input option (MH2O2)    Default: 1            ! MH2O2 =  1   !
     (Used only if MAQCHEM = 1)
        0 = use a monthly background H2O2 value
        1 = read hourly H2O2 concentrations from
            the H2O2.DAT data file

     Monthly H2O2 concentrations
     (Used only if MQACHEM = 1 and
      MH2O2 = 0 or MH2O2 = 1 and all hourly H2O2 data missing)
     (BCKH2O2) in ppb                  Default: 12*1.        
     !  BCKH2O2 = 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00 !


 --- Data for SECONDARY ORGANIC AEROSOL (SOA) Option
     (used only if MCHEM = 4)

     The SOA module uses monthly values of:
          Fine particulate concentration in ug/m^3 (BCKPMF)
          Organic fraction of fine particulate     (OFRAC)
          VOC / NOX ratio (after reaction)         (VCNX)
     to characterize the air mass when computing
     the formation of SOA from VOC emissions.
     Typical values for several distinct air mass types are:

        Month    1    2    3    4    5    6    7    8    9   10   11   12
                Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec

     Clean Continental
        BCKPMF   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.
        OFRAC  .15  .15  .20  .20  .20  .20  .20  .20  .20  .20  .20  .15
        VCNX    50.  50.  50.  50.  50.  50.  50.  50.  50.  50.  50.  50.

     Clean Marine (surface)
        BCKPMF  .5   .5   .5   .5   .5   .5   .5   .5   .5   .5   .5   .5
        OFRAC  .25  .25  .30  .30  .30  .30  .30  .30  .30  .30  .30  .25
        VCNX    50.  50.  50.  50.  50.  50.  50.  50.  50.  50.  50.  50.

     Urban - low biogenic (controls present)
        BCKPMF  30.  30.  30.  30.  30.  30.  30.  30.  30.  30.  30.  30.
        OFRAC  .20  .20  .25  .25  .25  .25  .25  .25  .20  .20  .20  .20
        VCNX     4.   4.   4.   4.   4.   4.   4.   4.   4.   4.   4.   4.

     Urban - high biogenic (controls present)
        BCKPMF  60.  60.  60.  60.  60.  60.  60.  60.  60.  60.  60.  60.
        OFRAC  .25  .25  .30  .30  .30  .55  .55  .55  .35  .35  .35  .25
        VCNX    15.  15.  15.  15.  15.  15.  15.  15.  15.  15.  15.  15.

     Regional Plume
        BCKPMF  20.  20.  20.  20.  20.  20.  20.  20.  20.  20.  20.  20.
        OFRAC  .20  .20  .25  .35  .25  .40  .40  .40  .30  .30  .30  .20
        VCNX    15.  15.  15.  15.  15.  15.  15.  15.  15.  15.  15.  15.

     Urban - no controls present
        BCKPMF 100. 100. 100. 100. 100. 100. 100. 100. 100. 100. 100. 100.
        OFRAC  .30  .30  .35  .35  .35  .55  .55  .55  .35  .35  .35  .30
        VCNX     2.   2.   2.   2.   2.   2.   2.   2.   2.   2.   2.   2.

     Default: Clean Continental
     !  BCKPMF = 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00 !
     !  OFRAC  = 0.15, 0.15, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.15 !
     !  VCNX   = 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00 !


!END!


-------------------------------------------------------------------------------


INPUT GROUP: 12 -- Misc. Dispersion and Computational Parameters
---------------

     Horizontal size of puff (m) beyond which
     time-dependent dispersion equations (Heffter)
     are used to determine sigma-y and
     sigma-z (SYTDEP)                           Default: 550.   ! SYTDEP = 5.5E02 !

     Switch for using Heffter equation for sigma z           
     as above (0 = Not use Heffter; 1 = use Heffter
     (MHFTSZ)                                   Default: 0      ! MHFTSZ =  0   !

     Stability class used to determine plume
     growth rates for puffs above the boundary
     layer (JSUP)                               Default: 5      ! JSUP =  5   !

     Vertical dispersion constant for stable
     conditions (k1 in Eqn. 2.7-3)  (CONK1)     Default: 0.01   ! CONK1 = .01 !

     Vertical dispersion constant for neutral/
     unstable conditions (k2 in Eqn. 2.7-4)
     (CONK2)                                    Default: 0.1    ! CONK2 = .1 !

     Factor for determining Transition-point from
     Schulman-Scire to Huber-Snyder Building Downwash
     scheme (SS used for Hs < Hb + TBD * HL)
     (TBD)                                      Default: 0.5    ! TBD = .5 !
        TBD < 0   ==> always use Huber-Snyder
        TBD = 1.5 ==> always use Schulman-Scire
        TBD = 0.5 ==> ISC Transition-point

     Range of land use categories for which
     urban dispersion is assumed
     (IURB1, IURB2)                             Default: 10     ! IURB1 =  10  !
                                                         19     ! IURB2 =  19  !

     Site characterization parameters for single-point Met data files ---------
     (needed for METFM = 2,3,4,5)

        Land use category for modeling domain
        (ILANDUIN)                              Default: 20     ! ILANDUIN =  20  !

        Roughness length (m) for modeling domain
        (Z0IN)                                  Default: 0.25   ! Z0IN = .25 !

        Leaf area index for modeling domain
        (XLAIIN)                                Default: 3.0    ! XLAIIN = 3.0 !

        Elevation above sea level (m)
        (ELEVIN)                                Default: 0.0    ! ELEVIN = .0 !

        Latitude (degrees) for met location
        (XLATIN)                                Default: -999.  ! XLATIN = .0 ! VER

        Longitude (degrees) for met location
        (XLONIN)                                Default: -999.  ! XLONIN = .0 ! VER

     Specialized information for interpreting single-point Met data files -----

        Anemometer height (m) (Used only if METFM = 2,3)
        (ANEMHT)                                Default: 10.    ! ANEMHT = 10.0 !

        Form of lateral turbulance data in PROFILE.DAT file
        (Used only if METFM = 4,5 or MTURBVW = 1 or 3)
        (ISIGMAV)                               Default: 1      ! ISIGMAV =  1  !
            0 = read sigma-theta
            1 = read sigma-v

        Choice of mixing heights (Used only if METFM = 4)
        (IMIXCTDM)                              Default: 0      ! IMIXCTDM =  0  !
            0 = read PREDICTED mixing heights
            1 = read OBSERVED mixing heights

     Maximum length of a slug (met. grid units)
     (XMXLEN)                                   Default: 1.0    ! XMXLEN = 1.0 !

     Maximum travel distance of a puff/slug (in
     grid units) during one sampling step
     (XSAMLEN)                                  Default: 1.0    ! XSAMLEN = 1.0 !

     Maximum Number of slugs/puffs release from
     one source during one time step            
     (MXNEW)                                    Default: 99     ! MXNEW =  99   !

     Maximum Number of sampling steps for    
     one puff/slug during one time step             
     (MXSAM)                                    Default: 99     ! MXSAM =  99   !

     Number of iterations used when computing
     the transport wind for a sampling step
     that includes gradual rise (for CALMET
     and PROFILE winds)
     (NCOUNT)                                   Default: 2      ! NCOUNT =  2   !

     Minimum sigma y for a new puff/slug (m)      
     (SYMIN)                                    Default: 1.0    ! SYMIN = 1.0  !

     Minimum sigma z for a new puff/slug (m)     
     (SZMIN)                                    Default: 1.0    ! SZMIN = 1.0  !

     Maximum sigma z (m) allowed to avoid
     numerical problem in calculating virtual
     time or distance.  Cap should be large
     enough to have no influence on normal events.
     Enter a negative cap to disable.
     (SZCAP_M)                                  Default: 5.0e06 ! SZCAP_M = 5.0E06 !

     Default minimum turbulence velocities sigma-v and sigma-w
     for each stability class over land and over water (m/s)
     (SVMIN(12) and SWMIN(12))

                     ----------  LAND  ----------       ---------  WATER  ----------
        Stab Class :  A    B    C    D    E    F         A    B    C    D    E    F
                     ---  ---  ---  ---  ---  ---       ---  ---  ---  ---  ---  ---
     Default SVMIN : .50, .50, .50, .50, .50, .50,      .37, .37, .37, .37, .37, .37
     Default SWMIN : .20, .12, .08, .06, .03, .016,     .20, .12, .08, .06, .03, .016

           ! SVMIN = 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.370, 0.370, 0.370, 0.370, 0.370, 0.370!
           ! SWMIN = 0.200, 0.120, 0.080, 0.060, 0.030, 0.016, 0.200, 0.120, 0.080, 0.060, 0.030, 0.016!

     Divergence criterion for dw/dz across puff
     used to initiate adjustment for horizontal
     convergence (1/s)
     Partial adjustment starts at CDIV(1), and
     full adjustment is reached at CDIV(2)
     (CDIV(2))                                  Default: 0.0,0.0  ! CDIV = .0, .0 !

     Search radius (number of cells) for nearest
     land and water cells used in the subgrid
     TIBL module
     (NLUTIBL)                                  Default: 4      ! NLUTIBL =  4  !

     Minimum wind speed (m/s) allowed for
     non-calm conditions. Also used as minimum
     speed returned when using power-law 
     extrapolation toward surface
     (WSCALM)                                   Default: 0.5    ! WSCALM = .5 !

     Maximum mixing height (m)                      
     (XMAXZI)                                   Default: 3000.  ! XMAXZI = 3000.0 !

     Minimum mixing height (m)                     
     (XMINZI)                                   Default: 50.    ! XMINZI = 20.0 ! VER, DAYANA USA 50.0

     Default wind speed classes --
     5 upper bounds (m/s) are entered;
     the 6th class has no upper limit
     (WSCAT(5))                      Default   : 
                                     ISC RURAL : 1.54, 3.09, 5.14, 8.23, 10.8 (10.8+)

                              Wind Speed Class :  1     2     3     4     5  
                                                 ---   ---   ---   ---   --- 
                                       ! WSCAT = 1.54, 3.09, 5.14, 8.23, 10.80 !

     Default wind speed profile power-law
     exponents for stabilities 1-6
     (PLX0(6))                       Default   : ISC RURAL values
                                     ISC RURAL : .07, .07, .10, .15, .35, .55
                                     ISC URBAN : .15, .15, .20, .25, .30, .30

                               Stability Class :  A     B     C     D     E     F
                                                 ---   ---   ---   ---   ---   ---
                                        ! PLX0 = 0.07, 0.07, 0.10, 0.15, 0.35, 0.55 !

     Default potential temperature gradient
     for stable classes E, F (degK/m)
     (PTG0(2))                       Default: 0.020, 0.035
                                        ! PTG0 = 0.020,   0.035 !

     Default plume path coefficients for
     each stability class (used when option
     for partial plume height terrain adjustment
     is selected -- MCTADJ=3)
     (PPC(6))                  Stability Class :  A     B     C     D     E     F
                                  Default  PPC : .50,  .50,  .50,  .50,  .35,  .35
                                                 ---   ---   ---   ---   ---   ---
                                        !  PPC = 0.50, 0.50, 0.50, 0.50, 0.35, 0.35 !

     Slug-to-puff transition criterion factor
     equal to sigma-y/length of slug
     (SL2PF)                               Default: 10.        ! SL2PF = 10.0 !

     Puff-splitting control variables ------------------------

       VERTICAL SPLIT
       --------------

       Number of puffs that result every time a puff
       is split - nsplit=2 means that 1 puff splits
       into 2
       (NSPLIT)                            Default:   3        ! NSPLIT =  3  !

       Time(s) of a day when split puffs are eligible to
       be split once again; this is typically set once
       per day, around sunset before nocturnal shear develops.
       24 values: 0 is midnight (00:00) and 23 is 11 PM (23:00)
       0=do not re-split    1=eligible for re-split
       (IRESPLIT(24))                      Default:  Hour 17 = 1
       !  IRESPLIT = 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0 !

       Split is allowed only if last hour's mixing
       height (m) exceeds a minimum value
       (ZISPLIT)                           Default: 100.       ! ZISPLIT = 100.0 !

       Split is allowed only if ratio of last hour's
       mixing ht to the maximum mixing ht experienced
       by the puff is less than a maximum value (this
       postpones a split until a nocturnal layer develops)
       (ROLDMAX)                           Default: 0.25       ! ROLDMAX = 0.25 !


       HORIZONTAL SPLIT
       ----------------

       Number of puffs that result every time a puff
       is split - nsplith=5 means that 1 puff splits
       into 5
       (NSPLITH)                           Default:   5        ! NSPLITH =  5  !

       Minimum sigma-y (Grid Cells Units) of puff
       before it may be split
       (SYSPLITH)                          Default:  1.0       ! SYSPLITH = 1.0 !

       Minimum puff elongation rate (SYSPLITH/hr) due to
       wind shear, before it may be split
       (SHSPLITH)                          Default:  2.        ! SHSPLITH = 2.0 !

       Minimum concentration (g/m^3) of each
       species in puff before it may be split
       Enter array of NSPEC values; if a single value is
       entered, it will be used for ALL species
       (CNSPLITH)                          Default:  1.0E-07   ! CNSPLITH = 1.0E-07 !

     Integration control variables ------------------------

       Fractional convergence criterion for numerical SLUG
       sampling integration
       (EPSSLUG)                           Default:   1.0e-04  ! EPSSLUG = 1.0E-04 !

       Fractional convergence criterion for numerical AREA
       source integration
       (EPSAREA)                           Default:   1.0e-06  ! EPSAREA = 1.0E-06 !

       Trajectory step-length (m) used for numerical rise
       integration
       (DSRISE)                            Default:   1.0      ! DSRISE = 1.0 !

       Boundary Condition (BC) Puff control variables ------------------------

       Minimum height (m) to which BC puffs are mixed as they are emitted
       (MBCON=2 ONLY).  Actual height is reset to the current mixing height
       at the release point if greater than this minimum.
       (HTMINBC)                           Default:   500.     ! HTMINBC = 500.0 !

       Search radius (km) about a receptor for sampling nearest BC puff.
       BC puffs are typically emitted with a spacing of one grid cell
       length, so the search radius should be greater than DGRIDKM.
       (RSAMPBC)                           Default:   10.      ! RSAMPBC = 10.0 !

       Near-Surface depletion adjustment to concentration profile used when
       sampling BC puffs?
       (MDEPBC)                            Default:   1        ! MDEPBC =  1  !
          0 = Concentration is NOT adjusted for depletion
          1 = Adjust Concentration for depletion

!END!


-------------------------------------------------------------------------------


INPUT GROUPS: 13a, 13b, 13c, 13d -- Point source parameters
--------------------------------

---------------
Subgroup (13a)
---------------

     Number of point sources with
     parameters provided below      (NPT1)  No default  !  NPT1 =  1  !

     Units used for point source
     emissions below                (IPTU)  Default: 1  !  IPTU =   1  !
           1 =        g/s
           2 =       kg/hr
           3 =       lb/hr
           4 =     tons/yr
           5 =     Odour Unit * m**3/s  (vol. flux of odour compound)
           6 =     Odour Unit * m**3/min
           7 =     metric tons/yr

     Number of source-species
     combinations with variable
     emissions scaling factors
     provided below in (13d)        (NSPT1) Default: 0  !  NSPT1 =  0  !

     Number of point sources with
     variable emission parameters
     provided in external file      (NPT2)  No default  !  NPT2 =  0  !

     (If NPT2 > 0, these point
     source emissions are read from
     the file: PTEMARB.DAT)

!END!

---------------
Subgroup (13b)
---------------
                                      a
          POINT SOURCE: CONSTANT DATA
          -----------------------------
                                                                              b          c
  Source       X         Y       Stack    Base     Stack    Exit  Exit    Bldg.  Emission
   No.     Coordinate Coordinate Height Elevation Diameter  Vel.  Temp.   Dwash   Rates
              (km)      (km)       (m)      (m)       (m)  (m/s) (deg. K)         
  ------   ---------- ---------- ------  ------   -------- ----- -------- ----- --------
   1 * SRCNAM = STK1 *
   1 * X =    340.5,   4870.5,    40.0, 160.0,       3.0,   5.0,  355.0,   .0, 1.0E01,  0.0E00,  4.0E00,  1.0E00,  0.0E00,  0.0E00,  1.0E01 * 
   1 * ZPLTFM  =       .0 *
   1 * FMFAC  =      1.0 *   *END*

   1 ! SRCNAM = {36} !
   1 ! X ={24}, {25}, {27}, {28}, {29}, {30}, {31}, {32}, {33}, {34}, {35} ! VER
   1 ! ZPLTFM  =       .0 !
   1 ! FMFAC  =      1.0 !   !END!

--------

    a
     Data for each source are treated as a separate input subgroup
     and therefore must end with an input group terminator.

     SRCNAM  is a 12-character name for a source
             (No default)
     X       is an array holding the source data listed by the column headings
             (No default)
     SIGYZI  is an array holding the initial sigma-y and sigma-z (m)
             (Default: 0.,0.)
     FMFAC   is a vertical momentum flux factor (0. or 1.0) used to represent
             the effect of rain-caps or other physical configurations that
             reduce momentum rise associated with the actual exit velocity.
             (Default: 1.0  -- full momentum used)
     ZPLTFM  is the platform height (m) for sources influenced by an isolated
             structure that has a significant open area between the surface
             and the bulk of the structure, such as an offshore oil platform.
             The Base Elevation is that of the surface (ground or ocean),
             and the Stack Height is the release height above the Base (not
             above the platform).  Building heights entered in Subgroup 13c
             must be those of the buildings on the platform, measured from
             the platform deck.  ZPLTFM is used only with MBDW=1 (ISC
             downwash method) for sources with building downwash.
             (Default: 0.0)

    b
     0. = No building downwash modeled
     1. = Downwash modeled for buildings resting on the surface
     2. = Downwash modeled for buildings raised above the surface (ZPLTFM > 0.)
     NOTE: must be entered as a REAL number (i.e., with decimal point)

    c
     An emission rate must be entered for every pollutant modeled.
     Enter emission rate of zero for secondary pollutants that are
     modeled, but not emitted.  Units are specified by IPTU
     (e.g. 1 for g/s).

---------------
Subgroup (13c)
---------------

           BUILDING DIMENSION DATA FOR SOURCES SUBJECT TO DOWNWASH
           -------------------------------------------------------
Source                                                                     a
 No.       Effective building height, width, length and X/Y offset (in meters)
           every 10 degrees.  LENGTH, XBADJ, and YBADJ are only needed for
           MBDW=2 (PRIME downwash option)
------     --------------------------------------------------------------------


--------

    a
     Building height, width, length, and X/Y offset from the source are treated
     as a separate input subgroup for each source and therefore must end with
     an input group terminator.  The X/Y offset is the position, relative to the
     stack, of the center of the upwind face of the projected building, with the
     x-axis pointing along the flow direction.

---------------
Subgroup (13d)
---------------
                                                a
          POINT SOURCE: VARIABLE EMISSIONS DATA
          ---------------------------------------

     Use this subgroup to describe temporal variations in the emission
     rates given in 13b.  Factors entered multiply the rates in 13b.
     Skip sources here that have constant emissions.  For more elaborate
     variation in source parameters, use PTEMARB.DAT and NPT2 > 0.

     IVARY determines the type of variation, and is source-specific:
     (IVARY)                                Default: 0
           0 =       Constant
           1 =       Diurnal cycle (24 scaling factors: hours 1-24)
           2 =       Monthly cycle (12 scaling factors: months 1-12)
           3 =       Hour & Season (4 groups of 24 hourly scaling factors,
                                    where first group is DEC-JAN-FEB)
           4 =       Speed & Stab. (6 groups of 6 scaling factors, where
                                    first group is Stability Class A,
                                    and the speed classes have upper
                                    bounds (m/s) defined in Group 12
           5 =       Temperature   (12 scaling factors, where temperature
                                    classes have upper bounds (C) of:
                                    0, 5, 10, 15, 20, 25, 30, 35, 40,
                                    45, 50, 50+)



--------
    a
     Data for each species are treated as a separate input subgroup
     and therefore must end with an input group terminator.


-------------------------------------------------------------------------------


INPUT GROUPS: 14a, 14b, 14c, 14d -- Area source parameters
--------------------------------

---------------
Subgroup (14a)
---------------

     Number of polygon area sources with
     parameters specified below (NAR1)       No default  !  NAR1 =  0   !

     Units used for area source
     emissions below            (IARU)       Default: 1  !  IARU =   1  !
           1 =        g/m**2/s
           2 =       kg/m**2/hr
           3 =       lb/m**2/hr
           4 =     tons/m**2/yr
           5 =     Odour Unit * m/s  (vol. flux/m**2 of odour compound)
           6 =     Odour Unit * m/min
           7 =     metric tons/m**2/yr

     Number of source-species
     combinations with variable
     emissions scaling factors
     provided below in (14d)        (NSAR1) Default: 0  !  NSAR1 =  0  !

     Number of buoyant polygon area sources
     with variable location and emission
     parameters (NAR2)                      No default  !  NAR2 =  0   !
     (If NAR2 > 0, ALL parameter data for
     these sources are read from the file: BAEMARB.DAT)

!END!

---------------
Subgroup (14b)
---------------
                                     a
          AREA SOURCE: CONSTANT DATA
          ----------------------------
                                                         b
Source           Effect.    Base      Initial    Emission
 No.             Height   Elevation   Sigma z     Rates
                   (m)       (m)        (m)      
-------          ------    ------     --------   ---------


--------
    a
     Data for each source are treated as a separate input subgroup
     and therefore must end with an input group terminator.
    b
     An emission rate must be entered for every pollutant modeled.
     Enter emission rate of zero for secondary pollutants that are
     modeled, but not emitted.  Units are specified by IARU 
     (e.g. 1 for g/m**2/s).

---------------
Subgroup (14c)
---------------

           COORDINATES (km) FOR EACH VERTEX(4) OF EACH POLYGON
           --------------------------------------------------------
Source                                                               a
 No.       Ordered list of X followed by list of Y, grouped by source
------     ------------------------------------------------------------


--------
    a
     Data for each source are treated as a separate input subgroup
     and therefore must end with an input group terminator.


---------------
Subgroup (14d)
---------------
                                               a
          AREA SOURCE: VARIABLE EMISSIONS DATA
          --------------------------------------

     Use this subgroup to describe temporal variations in the emission
     rates given in 14b.  Factors entered multiply the rates in 14b.
     Skip sources here that have constant emissions.  For more elaborate
     variation in source parameters, use BAEMARB.DAT and NAR2 > 0.

     IVARY determines the type of variation, and is source-specific:
     (IVARY)                                Default: 0
           0 =       Constant
           1 =       Diurnal cycle (24 scaling factors: hours 1-24)
           2 =       Monthly cycle (12 scaling factors: months 1-12)
           3 =       Hour & Season (4 groups of 24 hourly scaling factors,
                                    where first group is DEC-JAN-FEB)
           4 =       Speed & Stab. (6 groups of 6 scaling factors, where
                                    first group is Stability Class A,
                                    and the speed classes have upper
                                    bounds (m/s) defined in Group 12
           5 =       Temperature   (12 scaling factors, where temperature
                                    classes have upper bounds (C) of:
                                    0, 5, 10, 15, 20, 25, 30, 35, 40,
                                    45, 50, 50+)



--------
    a
     Data for each species are treated as a separate input subgroup
     and therefore must end with an input group terminator.


-------------------------------------------------------------------------------

INPUT GROUPS: 15a, 15b, 15c -- Line source parameters
---------------------------

---------------
Subgroup (15a)
---------------

     Number of buoyant line sources
     with variable location and emission
     parameters (NLN2)                              No default  !  NLN2 =  0   !

     (If NLN2 > 0, ALL parameter data for
      these sources are read from the file: LNEMARB.DAT)

     Number of buoyant line sources (NLINES)        No default   ! NLINES =  0  !

     Units used for line source
     emissions below                (ILNU)          Default: 1  !  ILNU =   1  !
           1 =        g/s
           2 =       kg/hr
           3 =       lb/hr
           4 =     tons/yr
           5 =     Odour Unit * m**3/s  (vol. flux of odour compound)
           6 =     Odour Unit * m**3/min
           7 =     metric tons/yr

     Number of source-species
     combinations with variable
     emissions scaling factors
     provided below in (15c)        (NSLN1) Default: 0  !  NSLN1 =  0  !

     Maximum number of segments used to model
     each line (MXNSEG)                             Default: 7   ! MXNSEG =  7  !

     The following variables are required only if NLINES > 0.  They are
     used in the buoyant line source plume rise calculations.

        Number of distances at which                Default: 6   ! NLRISE =  6  !
        transitional rise is computed

        Average building length (XL)                No default   ! XL = .0 !
                                                    (in meters)

        Average building height (HBL)               No default   ! HBL = .0 !
                                                    (in meters)

        Average building width (WBL)                No default   ! WBL = .0 !
                                                    (in meters)

        Average line source width (WML)             No default   ! WML = .0 !
                                                    (in meters)

        Average separation between buildings (DXL)  No default   ! DXL = .0 !
                                                    (in meters)

        Average buoyancy parameter (FPRIMEL)        No default   ! FPRIMEL = .0 !
                                                    (in m**4/s**3)

!END!

---------------
Subgroup (15b)
---------------

          BUOYANT LINE SOURCE: CONSTANT DATA
          ----------------------------------
                                                                                          a
Source     Beg. X      Beg. Y      End. X    End. Y     Release    Base        Emission
 No.     Coordinate  Coordinate  Coordinate Coordinate  Height    Elevation      Rates
            (km)        (km)        (km)       (km)       (m)       (m)          
------   ----------  ----------  ---------  ----------  -------   ---------    ---------

--------

    a
     Data for each source are treated as a separate input subgroup
     and therefore must end with an input group terminator.

    b
     An emission rate must be entered for every pollutant modeled.
     Enter emission rate of zero for secondary pollutants that are
     modeled, but not emitted.  Units are specified by ILNTU 
     (e.g. 1 for g/s).

---------------
Subgroup (15c)
---------------
                                                       a
          BUOYANT LINE SOURCE: VARIABLE EMISSIONS DATA
          ----------------------------------------------

     Use this subgroup to describe temporal variations in the emission
     rates given in 15b.  Factors entered multiply the rates in 15b.
     Skip sources here that have constant emissions.

     IVARY determines the type of variation, and is source-specific:
     (IVARY)                                Default: 0
           0 =       Constant
           1 =       Diurnal cycle (24 scaling factors: hours 1-24)
           2 =       Monthly cycle (12 scaling factors: months 1-12)
           3 =       Hour & Season (4 groups of 24 hourly scaling factors,
                                    where first group is DEC-JAN-FEB)
           4 =       Speed & Stab. (6 groups of 6 scaling factors, where
                                    first group is Stability Class A,
                                    and the speed classes have upper
                                    bounds (m/s) defined in Group 12
           5 =       Temperature   (12 scaling factors, where temperature
                                    classes have upper bounds (C) of:
                                    0, 5, 10, 15, 20, 25, 30, 35, 40,
                                    45, 50, 50+)



--------
    a
     Data for each species are treated as a separate input subgroup
     and therefore must end with an input group terminator.


-------------------------------------------------------------------------------


INPUT GROUPS: 16a, 16b, 16c -- Volume source parameters
---------------------------

---------------
Subgroup (16a)
---------------

     Number of volume sources with
     parameters provided in 16b,c (NVL1)     No default  !  NVL1 =  0   !

     Units used for volume source
     emissions below in 16b       (IVLU)     Default: 1  !  IVLU =   1  !
           1 =        g/s
           2 =       kg/hr
           3 =       lb/hr
           4 =     tons/yr
           5 =     Odour Unit * m**3/s  (vol. flux of odour compound)
           6 =     Odour Unit * m**3/min
           7 =     metric tons/yr

     Number of source-species
     combinations with variable
     emissions scaling factors
     provided below in (16c)      (NSVL1)    Default: 0  !  NSVL1 =  0  !

     Number of volume sources with
     variable location and emission
     parameters                   (NVL2)     No default  !  NVL2 =   0   !

     (If NVL2 > 0, ALL parameter data for
      these sources are read from the VOLEMARB.DAT file(s) )

!END!

---------------
Subgroup (16b)
---------------
                                        a
           VOLUME SOURCE: CONSTANT DATA
           ------------------------------
                                                                               b
         X           Y        Effect.    Base     Initial    Initial    Emission
     Coordinate  Coordinate   Height   Elevation  Sigma y    Sigma z     Rates
        (km)       (km)         (m)       (m)        (m)       (m)      
     ----------  ----------   ------    ------    --------   --------   --------


--------
    a
     Data for each source are treated as a separate input subgroup
     and therefore must end with an input group terminator.

    b
     An emission rate must be entered for every pollutant modeled.
     Enter emission rate of zero for secondary pollutants that are
     modeled, but not emitted.  Units are specified by IVLU 
     (e.g. 1 for g/s).

---------------
Subgroup (16c)
---------------
                                                 a
          VOLUME SOURCE: VARIABLE EMISSIONS DATA
          ----------------------------------------

     Use this subgroup to describe temporal variations in the emission
     rates given in 16b.  Factors entered multiply the rates in 16b.
     Skip sources here that have constant emissions.  For more elaborate
     variation in source parameters, use VOLEMARB.DAT and NVL2 > 0.

     IVARY determines the type of variation, and is source-specific:
     (IVARY)                                Default: 0
           0 =       Constant
           1 =       Diurnal cycle (24 scaling factors: hours 1-24)
           2 =       Monthly cycle (12 scaling factors: months 1-12)
           3 =       Hour & Season (4 groups of 24 hourly scaling factors,
                                    where first group is DEC-JAN-FEB)
           4 =       Speed & Stab. (6 groups of 6 scaling factors, where
                                    first group is Stability Class A,
                                    and the speed classes have upper
                                    bounds (m/s) defined in Group 12
           5 =       Temperature   (12 scaling factors, where temperature
                                    classes have upper bounds (C) of:
                                    0, 5, 10, 15, 20, 25, 30, 35, 40,
                                    45, 50, 50+)



--------
    a
     Data for each species are treated as a separate input subgroup
     and therefore must end with an input group terminator.


-------------------------------------------------------------------------------

INPUT GROUPS: 17a & 17b -- Non-gridded (discrete) receptor information
-----------------------

---------------
Subgroup (17a)
---------------

     Number of non-gridded receptors (NREC)  No default  !  NREC =  0   !

!END!

---------------
Subgroup (17b)
---------------
                                               a
           NON-GRIDDED (DISCRETE) RECEPTOR DATA
           ------------------------------------

                   X            Y          Ground        Height   b
Receptor       Coordinate   Coordinate    Elevation   Above Ground
  No.             (km)         (km)          (m)           (m)
--------       ----------   ----------    ---------   ------------



-------------
    a
     Data for each receptor are treated as a separate input subgroup
     and therefore must end with an input group terminator.

    b
     Receptor height above ground is optional.  If no value is entered,
     the receptor is placed on the ground.

'''.format(fdate,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF, YYYY0, MM0, DD0, HH0, YYYY1, MM1, DD1, HH1, str(utmFx1), str(utmFy1), fpname, chimenea, base, diametro, veloc, tsalida, dwash, emis1, emis2, emis3, sname)
#           0       1         2     3     4    5    6    7    8      9      10      11     12     13     14     15      16    17   18  19    20    21    22   23      24          25          26       27      28      29       30      31      32     33    34     35      36


    return text


def namelist_calpost(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF,spec):

    YYYY0 = str(fdate[0:4])
    MM0   = str(fdate[4:6])
    DD0   = str(fdate[6:8])
    HH0   = str(fdate[8:10])

    endTime = str(datetime.datetime.strptime(fdate, "%Y%m%d%H") + datetime.timedelta(hours=24))

    YYYY1 = str(endTime[0:4])
    MM1   = str(endTime[5:7])
    DD1   = str(endTime[8:10])
    HH1   = str(endTime[11:13])

    # This namelist is only for 00z initialization


    text='''CALPUFF Demonstration


---------------- Run title (3 lines) ------------------------------------------

                    CALPOST MODEL CONTROL FILE
                    --------------------------

-------------------------------------------------------------------------------

INPUT GROUP: 0 -- Input and Output File Names
--------------

Input Files
-----------

File                      Default File Name
----                      -----------------
Conc/Dep Flux File        MODEL.DAT          ! MODDAT =conc.dat   !
Relative Humidity File    VISB.DAT           * VISDAT =visb.dat   *
Background Data File      BACK.DAT           * BACKDAT =   *
Transmissometer or        VSRN.DAT           * VSRDAT =   *
Nephelometer Data File     or
DATSAV Weather Data File   or
Prognostic Weather  File     

Single-point Met File     SURFACE.DAT        * MET1DAT =   *
 (Used ONLY to identify CALM hours for plume model
  output averaging when MCALMPRO option is used)


Output Files
------------

File                      Default File Name
----                      -----------------
List File                 CALPOST.LST        ! PSTLST =CALPOST.LST   !

Pathname for Timeseries Files   (blank)      ! TSPATH =./  !
(activate with exclamation points only if
providing NON-BLANK character string)

Pathname for Plot Files   (blank)            ! PLPATH =./ !
(activate with exclamation points only if
 providing NON-BLANK character string)

User Character String (U) to augment default filenames
(activate with exclamation points only if
 providing NON-BLANK character string)

Timeseries          TSERIES_ASPEC_ttHR_CONC_TSUNAM.DAT
Peak Value          PEAKVAL_ASPEC_ttHR_CONC_TSUNAM.DAT

                                             * TSUNAM = {16}{17}{18}Z0000  *

Top Nth Rank Plot   RANK(ALL)_ASPEC_ttHR_CONC_TUNAM.DAT
                or  RANK(ii)_ASPEC_ttHR_CONC_TUNAM.GRD 

                                             * TUNAM =   *

Exceedance Plot      EXCEED_ASPEC_ttHR_CONC_XUNAM.DAT
                 or  EXCEED_ASPEC_ttHR_CONC_XUNAM.GRD

                                             * XUNAM =   *

Echo Plot
(Specific Days) 
           yyyy_Mmm_Ddd_hhmm(UTCszzzz)_L00_ASPEC_ttHR_CONC.DAT
     or    yyyy_Mmm_Ddd_hhmm(UTCszzzz)_L00_ASPEC_ttHR_CONC.GRD


Visibility Plot      DAILY_VISIB_VUNAM.DAT   ! VUNAM =VTEST   !
(Daily Peak Summary)    


Auxiliary Output Files
----------------------

File                      Default File Name
----                      -----------------
Visibility Change         DELVIS.DAT         * DVISDAT =   *

--------------------------------------------------------------------------------
All file names will be converted to lower case if LCFILES = T
Otherwise, if LCFILES = F, file names will be converted to UPPER CASE
         T = lower case               ! LCFILES = T !
         F = UPPER CASE
NOTE: (1) file/path names can be up to 132 characters in length
NOTE: (2) Filenames for ALL PLOT and TIMESERIES FILES are constructed
          using a template that includes a pathname, user-supplied 
          character(s), and context-specific strings, where
             ASPEC = Species Name
              CONC = CONC Or WFLX Or DFLX Or TFLX
                tt = Averaging Period (e.g. 03)
                ii = Rank (e.g. 02)
                hh = Hour(ending) in LST
             szzzz = LST time zone shift (EST is -0500)
              yyyy = Year(LST)
                mm = Month(LST)
                dd = day of month (LST)
          are determined internally based on selections made below.
          If a path or user-supplied character(s) are supplied, each
          must contain at least 1 non-blank character.

!END!
--------------------------------------------------------------------------------

INPUT GROUP: 1 -- General run control parameters
--------------

     Option to run all periods found
     in the met. file(s)  (METRUN)        Default: 0   ! METRUN =   1  !

         METRUN = 0 - Run period explicitly defined below
         METRUN = 1 - Run all periods in CALPUFF data file(s)

     Starting date:    Year   (ISYR)  --    No default   ! ISYR  =  {16}  !
                       Month  (ISMO)  --    No default   ! ISMO  =  {17}  !
                       Day    (ISDY)  --    No default   ! ISDY  =  {18}  !
     Starting time:    Hour   (ISHR)  --    No default   ! ISHR  =  0  !
                       Minute (ISMIN) --    No default   ! ISMIN =  0  !
                       Second (ISSEC) --    No default   ! ISSEC =  0  !

     Ending date:      Year   (IEYR)  --    No default   ! IEYR  =  {20}  !
                       Month  (IEMO)  --    No default   ! IEMO  =  {21}  !
                       Day    (IEDY)  --    No default   ! IEDY  =  {22}  !
     Ending time:      Hour   (IEHR)  --    No default   ! IEHR  =  18  !
                       Minute (IEMIN) --    No default   ! IEMIN =  0  !
                       Second (IESEC) --    No default   ! IESEC =  0  !

     (These are only used if METRUN = 0)

     All times are in the base time zone of the CALPUFF simulation.
     CALPUFF Dataset Version 2.1 contains the zone, but earlier versions
     do not, and the zone must be specified here.  The zone is the
     number of hours that must be ADDED to the time to obtain UTC (or GMT).
     Identify the Base Time Zone for the CALPUFF simulation
                              (BTZONE) -- No default   ! BTZONE = 5.0 !

     Process every period of data?
                                (NREP) -- Default: 1   ! NREP  =  1  !
      (1 = every period processed,
       2 = every 2nd period processed,
       5 = every 5th period processed, etc.)

Species & Concentration/Deposition Information
----------------------------------------------

      Species to process (ASPEC)       -- No default   ! ASPEC = {24}  !
      (ASPEC = VISIB for visibility processing)

      Layer/deposition code (ILAYER)   -- Default: 1   ! ILAYER =  1  !
        '1'  for CALPUFF concentrations,
        '-1' for dry deposition fluxes,
        '-2' for wet deposition fluxes,
        '-3' for wet+dry deposition fluxes.

      Scaling factors of the form:     -- Defaults:    ! A =  0.0    !
            X(new) = X(old) * A + B         A = 0.0    ! B =  0.0    !
        (NOT applied if A = B = 0.0)        B = 0.0

      Add Hourly Background Concentrations/Fluxes?
                              (LBACK)  -- Default: F   ! LBACK =  F !


      Source of NO2 when ASPEC=NO2 (above) or LVNO2=T (Group 2) may be
      from CALPUFF NO2 concentrations OR from a fraction of CALPUFF NOx
      concentrations.  Specify the fraction of NOx that is treated as NO2
      either as a constant or as a table of fractions that depend on the
      magnitude of the NOx concentration:
                             (NO2CALC) -- Default: 1   ! NO2CALC =   1  !
         0 =  Use NO2 directly (NO2 must be in file)
         1 =  Specify a single NO2/NOx ratio (RNO2NOX)
         2 =  Specify a table NO2/NOx ratios (TNO2NOX)
              (NOTE: Scaling Factors must NOT be used with NO2CALC=2)

      Single NO2/NOx ratio (0.0 to 1.0) for treating some
      or all NOx as NO2, where [NO2] = [NOX] * RNO2NOX
      (used only if NO2CALC = 1)
                             (RNO2NOX) -- Default: 1.0 ! RNO2NOX = 0.75 ! VER 1.0

      Table of NO2/NOx ratios that vary with NOx concentration.
      Provide 14 NOx concentrations (ug/m**3) and the corresponding
      NO2/NOx ratio, with NOx increasing in magnitude.  The ratio used
      for a particular NOx concentration is interpolated from the values
      provided in the table.  The ratio for the smallest tabulated NOx
      concentration (the first) is used for all NOx concentrations less
      than the smallest tabulated value, and the ratio for the largest
      tabulated NOx concentration (the last) is used for all NOx
      concentrations greater than the largest tabulated value.
      (used only if NO2CALC = 2)

       NOx concentration(ug / m3)
                             (CNOX)    -- No default
         * CNOX = 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0 *

       NO2/NOx ratio for each NOx concentration:
                             (TNO2NOX) -- No default
         * TNO2NOX = 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 *


Source information
------------------

  Option to process source contributions:
         0 =  Process only total reported contributions
         1 =  Sum all individual source contributions and process
         2 =  Run in TRACEBACK mode to identify source
              contributions at a SINGLE receptor
                             (MSOURCE) -- Default: 0   ! MSOURCE = 0  !


Plume Model Output Processing Options
-------------------------------------

  Output from models other than CALPUFF and CALGRID can be written in
  the CONC.DAT format and processed by CALPOST.  Plume models such as
  AERMOD typically do not treat CALM hours, and do not include such hours
  in multiple-hour averages, with specific rules about how many calm hours
  can be removed from an average.  This treatment is known as CALM
  PROCESSING.  Calm periods are identified from wind speeds in the
  meteorological data file for the application, which must be identified
  in Input Group 0 as the single-point meteorological data file MET1DAT.
         0 =  Option is not used for CALPUFF/CALGRID output files
         1 =  Apply CALM processing procedures to multiple-hour averages
                          (MCALMPRO) -- Default: 0   ! MCALMPRO =  0  !

  Format of Single-point Met File
         1 =  AERMOD/AERMET SURFACE file
                           (MET1FMT) -- Default: 1   ! MET1FMT =  1  !
    

Receptor information
--------------------

  Gridded receptors processed?    (LG) -- Default: F   ! LG  = T  ! VER T
  Discrete receptors processed?   (LD) -- Default: F   ! LD  = F  ! VER F
  CTSG Complex terrain receptors processed?
                                 (LCT) -- Default: F   ! LCT = F  !

--Report results by DISCRETE receptor RING?
  (only used when LD = T)     (LDRING) -- Default: F   ! LDRING = F  !

--Select range of DISCRETE receptors (only used when LD = T):

  Select ALL DISCRETE receptors by setting NDRECP flag to -1;
                               OR
  Select SPECIFIC DISCRETE receptors by entering a flag (0,1) for each
     0 = discrete receptor not processed
     1 = discrete receptor processed
  using repeated value notation to select blocks of receptors:
     23*1, 15*0, 12*1
  Flag for all receptors after the last one assigned is set to 0
  (NDRECP) -- Default: -1
                                               ! NDRECP =  -1  !


--Select range of GRIDDED receptors (only used when LG = T):

       X index of LL corner (IBGRID) -- Default: -1     ! IBGRID = -1  !
           (-1 OR 1 <= IBGRID <= NX)

       Y index of LL corner (JBGRID) -- Default: -1     ! JBGRID = -1  !
           (-1 OR 1 <= JBGRID <= NY)

       X index of UR corner (IEGRID) -- Default: -1     ! IEGRID = -1  !
           (-1 OR 1 <= IEGRID <= NX)

       Y index of UR corner (JEGRID) -- Default: -1     ! JEGRID = -1  !
           (-1 OR 1 <= JEGRID <= NY)

  Note: Entire grid is processed if IBGRID=JBGRID=IEGRID=JEGRID=-1


--Specific gridded receptors can also be excluded from CALPOST
  processing by filling a processing grid array with 0s and 1s.  If the
  processing flag for receptor index (i,j) is 1 (ON), that receptor
  will be processed if it lies within the range delineated by IBGRID,
  JBGRID,IEGRID,JEGRID and if LG=T. If it is 0 (OFF), it will not be
  processed in the run.  By default, all array values are set to 1 (ON).

  Number of gridded receptor rows provided in Subgroup (1a) to
  identify specific gridded receptors to process
                           (NGONOFF) -- Default: 0      ! NGONOFF =  0  !

!END!


--------------
Subgroup (1a) -- Specific gridded receptors included/excluded
--------------

    Specific gridded receptors are excluded from CALPOST processing
    by filling a processing grid array with 0s and 1s.  A total of
    NGONOFF lines are read here.  Each line corresponds to one 'row'
    in the sampling grid, starting with the NORTHERNMOST row that
    contains receptors that you wish to exclude, and finishing with
    row 1 to the SOUTH (no intervening rows may be skipped).  Within
    a row, each receptor position is assigned either a 0 or 1,
    starting with the westernmost receptor.
       0 = gridded receptor not processed
       1 = gridded receptor processed

    Repeated value notation may be used to select blocks of receptors:
       23*1, 15*0, 12*1

    Because all values are initially set to 1, any receptors north of
    the first row entered, or east of the last value provided in a row,
    remain ON.

    (NGXRECP) -- Default: 1


-------------------------------------------------------------------------------

INPUT GROUP: 2 -- Visibility Parameters (ASPEC = VISIB)
--------------

    Test visibility options specified to see
    if they conform to FLAG 2008 configuration?
                           (MVISCHECK) -- Default: 1   ! MVISCHECK =   1  !
         0 =  NO checks are made
         1 =  Technical options must conform to FLAG 2008 visibility guidance
                ASPEC = VISIB
                LVNO2 = T
                NO2CALC = 1
                RNO2NOX = 1.0
                MVISBK = 8
                M8_MODE = 5

    Some of the data entered for use with the FLAG 2008 configuration
    are specific to the Class I area being evaluated. These values can
    be checked within the CALPOST user interface when the name of the
    Class I area is provided.

    Name of Class I Area (used for QA purposes only)
                            (AREANAME) -- Default: User  ! AREANAME =  USER !

    Particle growth curve f(RH) for hygroscopic species
                                (MFRH) -- Default: 4   ! MFRH   =  2  !  VER 4

         1 =  IWAQM (1998) f(RH) curve (originally used with MVISBK=1)
         2 =  FLAG (2000) f(RH) tabulation
         3 =  EPA (2003) f(RH) tabulation
         4 =  IMPROVE (2006) f(RH) tabulations for sea salt, and for small and
              large SULFATE and NITRATE particles;
              Used in Visibility Method 8 (MVISBK = 8 with M8_MODE = 1, 2, or 3)

    Maximum relative humidity (%) used in particle growth curve
                               (RHMAX) -- Default: 98  ! RHMAX  = 98 !

    Modeled species to be included in computing the light extinction
     Include SULFATE?          (LVSO4) -- Default: T   ! LVSO4  = T  !
     Include NITRATE?          (LVNO3) -- Default: T   ! LVNO3  = T  !
     Include ORGANIC CARBON?   (LVOC)  -- Default: T   ! LVOC   = F  ! VER
     Include COARSE PARTICLES? (LVPMC) -- Default: T   ! LVPMC  = F  ! VER
     Include FINE PARTICLES?   (LVPMF) -- Default: T   ! LVPMF  = F  ! VER
     Include ELEMENTAL CARBON? (LVEC)  -- Default: T   ! LVEC   = T  ! 
     Include NO2 absorption?   (LVNO2) -- Default: F   ! LVNO2  = T  !
              With Visibility Method 8 -- Default: T
                                          FLAG (2008)

    And, when ranking for TOP-N, TOP-50, and Exceedance tables,
     Include BACKGROUND?       (LVBK)  -- Default: T   ! LVBK   = T  !

    Species name used for particulates in MODEL.DAT file
                   COARSE    (SPECPMC) -- Default: PMC ! SPECPMC = PMC !
                   FINE      (SPECPMF) -- Default: PMF ! SPECPMF = PMF !

Extinction Efficiency (1/Mm per ug/m**3)
----------------------------------------
    MODELED particulate species:
               PM  COARSE      (EEPMC) -- Default: 0.6   ! EEPMC  = 0.6 !
               PM  FINE        (EEPMF) -- Default: 1.0   ! EEPMF  = 1 !
    BACKGROUND particulate species:
               PM  COARSE    (EEPMCBK) -- Default: 0.6   ! EEPMCBK = 0.6 !
    Other species:
              AMMONIUM SULFATE (EESO4) -- Default: 3.0   ! EESO4  = 3 !
              AMMONIUM NITRATE (EENO3) -- Default: 3.0   ! EENO3  = 3 !
              ORGANIC CARBON   (EEOC)  -- Default: 4.0   ! EEOC   = 4 !
              SOIL             (EESOIL)-- Default: 1.0   ! EESOIL = 1 !
              ELEMENTAL CARBON (EEEC)  -- Default: 10.   ! EEEC   = 10 !
              NO2 GAS          (EENO2) -- Default: .1755 ! EENO2  = 0.17 !
    Visibility Method 8:
              AMMONIUM SULFATE (EESO4S)   Set Internally (small)
              AMMONIUM SULFATE (EESO4L)   Set Internally (large)
              AMMONIUM NITRATE (EENO3S)   Set Internally (small)
              AMMONIUM NITRATE (EENO3L)   Set Internally (large)
              ORGANIC CARBON   (EEOCS)    Set Internally (small)
              ORGANIC CARBON   (EEOCL)    Set Internally (large)
              SEA SALT         (EESALT)   Set Internally

Background Extinction Computation
---------------------------------

    Method used for the 24h-average of percent change of light extinction:
    Hourly ratio of source light extinction / background light extinction
    is averaged?               (LAVER) -- Default: F   ! LAVER = F  !


    Method used for background light extinction
                              (MVISBK) -- Default: 8   ! MVISBK =  2  !  # VER 8
                                          FLAG (2008)

         1 =  Supply single light extinction and hygroscopic fraction
              - Hourly F(RH) adjustment applied to hygroscopic background
                and modeled sulfate and nitrate
         2 =  Background extinction from speciated PM concentrations (A)
              - Hourly F(RH) adjustment applied to observed and modeled sulfate
                and nitrate
              - F(RH) factor is capped at F(RHMAX)
         3 =  Background extinction from speciated PM concentrations (B)
              - Hourly F(RH) adjustment applied to observed and modeled sulfate
                and nitrate
              - Receptor-hour excluded if RH>RHMAX
              - Receptor-day excluded if fewer than 6 valid receptor-hours
         4 =  Read hourly transmissometer background extinction measurements
              - Hourly F(RH) adjustment applied to modeled sulfate and nitrate
              - Hour excluded if measurement invalid (missing, interference,
                or large RH)
              - Receptor-hour excluded if RH>RHMAX
              - Receptor-day excluded if fewer than 6 valid receptor-hours
         5 =  Read hourly nephelometer background extinction measurements
              - Rayleigh extinction value (BEXTRAY) added to measurement
              - Hourly F(RH) adjustment applied to modeled sulfate and nitrate
              - Hour excluded if measurement invalid (missing, interference,
                or large RH)
              - Receptor-hour excluded if RH>RHMAX
              - Receptor-day excluded if fewer than 6 valid receptor-hours
         6 =  Background extinction from speciated PM concentrations
              - FLAG (2000) monthly RH adjustment factor applied to observed and
                and modeled sulfate and nitrate
         7 =  Use observed weather or prognostic weather information for
              background extinction during weather events; otherwise, use Method 2
              - Hourly F(RH) adjustment applied to modeled sulfate and nitrate
              - F(RH) factor is capped at F(RHMAX)
              - During observed weather events, compute Bext from visual range
                if using an observed weather data file, or
              - During prognostic weather events, use Bext from the prognostic
                weather file
              - Use Method 2 for hours without a weather event
         8 =  Background extinction from speciated PM concentrations using
              the IMPROVE (2006) variable extinction efficiency formulation
              (MFRH must be set to 4)
              - Split between small and large particle concentrations of
                SULFATES, NITRATES, and ORGANICS is a function of concentration
                and different extinction efficiencies are used for each
              - Source-induced change in visibility includes the increase in
                extinction of the background aerosol due to the change in the
                extinction efficiency that now depends on total concentration.
              - Fsmall(RH) and Flarge(RH) adjustments for small and large
                particles are applied to observed and modeled sulfate and
                nitrate concentrations
              - Fsalt(RH) adjustment for sea salt is applied to background
                sea salt concentrations
              - F(RH) factors are capped at F(RHMAX)
              - RH for Fsmall(RH), Flarge(RH), and Fsalt(RH) may be obtained
                from hourly data as in Method 2 or from the FLAG monthly RH
                adjustment factor used for Method 6 where EPA F(RH) tabulation
                is used to infer RH, or monthly Fsmall, Flarge, and Fsalt RH
                adjustment factors can be directly entered.
                Furthermore, a monthly RH factor may be applied to either hourly
                concentrations or daily concentrations to obtain the 24-hour
                extinction.
                These choices are made using the M8_MODE selection.

    Additional inputs used for MVISBK = 1:
    --------------------------------------
     Background light extinction (1/Mm)
                              (BEXTBK) -- No default   ! BEXTBK = 12 !  VER 0 
     Percentage of particles affected by relative humidity
                              (RHFRAC) -- No default   ! RHFRAC = 10 !  VER 0

    Additional inputs used for MVISBK = 6,8:
    ----------------------------------------
     Extinction coefficients for hygroscopic species (modeled and
     background) are computed using a monthly RH adjustment factor
     in place of an hourly RH factor (VISB.DAT file is NOT needed).
     Enter the 12 monthly factors here (RHFAC).  Month 1 is January.

     (RHFAC)  -- No default     ! RHFAC = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !

    Additional inputs used for MVISBK = 7:
    --------------------------------------
     The weather data file (DATSAV abbreviated space-delimited) that
     is identified as VSRN.DAT may contain data for more than one
     station.  Identify the stations that are needed in the order in
     which they will be used to obtain valid weather and visual range.
     The first station that contains valid data for an hour will be
     used.  Enter up to MXWSTA (set in PARAMS file) integer station IDs
     of up to 6 digits each as variable IDWSTA, and enter the corresponding
     time zone for each, as variable TZONE (= UTC-LST).

     A prognostic weather data file with Bext for weather events may be used
     in place of the observed weather file.  Identify this as the VSRN.DAT
     file and use a station ID of IDWSTA = 999999, and TZONE = 0.

     NOTE:  TZONE identifies the time zone used in the dataset.  The
            DATSAV abbreviated space-delimited data usually are prepared
            with UTC time rather than local time, so TZONE is typically
            set to zero.

     (IDWSTA)   -- No default   * IDWSTA = 000000 *
     (TZONE)    -- No default   * TZONE =      0. *

    Additional inputs used for MVISBK = 2,3,6,7,8:
    ----------------------------------------------
     Background extinction coefficients are computed from monthly
     CONCENTRATIONS of ammonium sulfate (BKSO4), ammonium nitrate (BKNO3),
     coarse particulates (BKPMC), organic carbon (BKOC), soil (BKSOIL), and
     elemental carbon (BKEC).  Month 1 is January.
     (ug/m**3)

     (BKSO4)  -- No default     ! BKSO4 = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !
     (BKNO3)  -- No default     ! BKNO3 = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !
     (BKPMC)  -- No default     ! BKPMC = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !
     (BKOC)   -- No default     ! BKOC  = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !
     (BKSOIL) -- No default     ! BKSOIL= 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !
     (BKEC)   -- No default     ! BKEC  = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !

    Additional inputs used for MVISBK = 8:
    --------------------------------------
     Extinction coefficients for hygroscopic species (modeled and
     background) may be computed using hourly RH values and hourly
     modeled concentrations, or using monthly RH values inferred from
     the RHFAC adjustment factors and either hourly or daily modeled
     concentrations, or using monthly RHFSML, RHFLRG, and RHFSEA adjustment
     factors and either hourly or daily modeled concentrations.
     
     (M8_MODE) -- Default: 5     ! M8_MODE=  3   !  VER 5
                  FLAG (2008)
 
          1 = Use hourly RH values from VISB.DAT file with hourly
              modeled and monthly background concentrations.
          2 = Use monthly RH from monthly RHFAC and EPA (2003) f(RH) tabulation
              with hourly modeled and monthly background concentrations.
              (VISB.DAT file is NOT needed).
          3 = Use monthly RH from monthly RHFAC with EPA (2003) f(RH) tabulation
              with daily modeled and monthly background concentrations.
              (VISB.DAT file is NOT needed).
          4 = Use monthly RHFSML, RHFLRG, and RHFSEA with hourly modeled
              and monthly background concentrations.
              (VISB.DAT file is NOT needed).
          5 = Use monthly RHFSML, RHFLRG, and RHFSEA with daily modeled
              and monthly background concentrations.
              (VISB.DAT file is NOT needed).

     Background extinction coefficients are computed from monthly
     CONCENTRATIONS of sea salt (BKSALT).  Month 1 is January.
     (ug/m**3)

     (BKSALT) -- No default     ! BKSALT= 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !

     Extinction coefficients for hygroscopic species (modeled and
     background) can be computed using monthly RH adjustment factors
     in place of an hourly RH factor (VISB.DAT file is NOT needed).
     Enter the 12 monthly factors here (RHFSML,RHFLRG,RHFSEA).
     Month 1 is January.  (Used if M8_MODE = 4 or 5)

     Small ammonium sulfate and ammonium nitrate particle sizes
     (RHFSML) -- No default     ! RHFSML= 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !

     Large ammonium sulfate and ammonium nitrate particle sizes
     (RHFLRG) -- No default     ! RHFLRG= 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !

     Sea salt particles
     (RHFSEA) -- No default     ! RHFSEA= 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 !

    Additional inputs used for MVISBK = 2,3,5,6,7,8:
    ------------------------------------------------
     Extinction due to Rayleigh scattering is added (1/Mm)
                             (BEXTRAY) -- Default: 10.0 ! BEXTRAY = 10 !
 
!END!
-------------------------------------------------------------------------------

INPUT GROUP: 3 -- Output options
--------------

Documentation
-------------

    Documentation records contained in the header of the
    CALPUFF output file may be written to the list file.
    Print documentation image?
                                (LDOC) -- Default: F   !  LDOC = F !

Output Units
------------
    Units for All Output       (IPRTU) -- Default: 1   ! IPRTU =  3   !
                     for            for
                Concentration    Deposition
       1 =         g/m**3         g/m**2/s
       2 =        mg/m**3        mg/m**2/s
       3 =        ug/m**3        ug/m**2/s
       4 =        ng/m**3        ng/m**2/s
       5 =      Odour Units

    Visibility: extinction expressed in 1/Mega-meters (IPRTU is ignored)


Averaging time(s) reported
--------------------------

    1-pd averages           (L1PD) -- Default: T   !   L1PD = F  !
    (pd = averaging period of model output)

    1-hr averages           (L1HR) -- Default: T   !   L1HR = T  !

    3-hr averages           (L3HR) -- Default: T   !   L3HR = F  !

    24-hr averages         (L24HR) -- Default: T   !  L24HR = F  !

    Run-length averages    (LRUNL) -- Default: T   !  LRUNL = F  !

    User-specified averaging time in hours, minutes, seconds
    - results for this averaging time are reported if it is not zero

                           (NAVGH) -- Default: 0   !   NAVGH =  0  !
                           (NAVGM) -- Default: 0   !   NAVGM =  0  !
                           (NAVGS) -- Default: 0   !   NAVGS =  0  !


Types of tabulations reported
------------------------------

   1) Visibility: daily visibility tabulations are always reported
                  for the selected receptors when ASPEC = VISIB.
                  In addition, any of the other tabulations listed
                  below may be chosen to characterize the light
                  extinction coefficients.
                  [List file or Plot/Analysis File]


   2) Top 50 table for each averaging time selected
      [List file only]
                            (LT50) -- Default: T   !   LT50 = F  !

   3) Top 'N' table for each averaging time selected
      [List file or Plot file]
                           (LTOPN) -- Default: F   !  LTOPN = T ! VER F

        -- Number of 'Top-N' values at each receptor
           selected (NTOP must be <= 4)
                            (NTOP) -- Default: 4   ! NTOP =  1   ! VER 4

        -- Specific ranks of 'Top-N' values reported
           (NTOP values must be entered)
                   (ITOP(4) array) -- Default:     ! ITOP =  1  ! ver  1 , 2 , 3 , 4 
                                      1,2,3,4


   4) Threshold exceedance counts for each receptor and each averaging
      time selected
      [List file or Plot file]
                           (LEXCD) -- Default: F   !  LEXCD = T  !  VER F

        -- Identify the threshold for each averaging time by assigning a
           non-negative value (output units).

                                   -- Default: -1.0
           Threshold for  1-hr averages   (THRESH1) !  THRESH1 = 10 !
           Threshold for  3-hr averages   (THRESH3) !  THRESH3 = -1.0  !
           Threshold for 24-hr averages  (THRESH24) ! THRESH24 = -1.0  !
           Threshold for NAVG-hr averages (THRESHN) !  THRESHN = -1.0  !


        -- Counts for the shortest averaging period selected can be
           tallied daily, and receptors that experience more than NCOUNT
           counts over any NDAY period will be reported.  This type of
           exceedance violation output is triggered only if NDAY > 0.

           Accumulation period(Days)
                            (NDAY) -- Default: 0   !    NDAY =  0  !
           Number of exceedances allowed
                          (NCOUNT) -- Default: 1   !  NCOUNT =  3  !


   5) Selected day table(s)

      Echo Option -- Many records are written each averaging period
      selected and output is grouped by day
      [List file or Plot file]
                           (LECHO) -- Default: F   !  LECHO = T  ! VER T

      Timeseries Option -- Averages at all selected receptors for
      each selected averaging period are written to timeseries files.
      Each file contains one averaging period, and all receptors are
      written to a single record each averaging time.
      [TSERIES_ASPEC_ttHR_CONC_TSUNAM.DAT files]
                           (LTIME) -- Default: F   !  LTIME = T  ! VER F

      Peak Value Option -- Averages at all selected receptors for
      each selected averaging period are screened and the peak value
      each period is written to timeseries files.
      Each file contains one averaging period.
      [PEAKVAL_ASPEC_ttHR_CONC_TSUNAM.DAT files]
                           (LPEAK) -- Default: F   !  LPEAK = F  !

        -- Days selected for output
                      (IECHO(366)) -- Default: 366*0
           ! IECHO  = 366*0 !    IECHO  = 193*0,1,1,171*0  
           (366 values must be entered)

Plot output options
-------------------

     Plot files can be created for the Top-N, Exceedance, and Echo
     tables selected above.  Two formats for these files are available,
     DATA and GRID.  In the DATA format, results at all receptors are
     listed along with the receptor location [x,y,val1,val2,...].
     In the GRID format, results at only gridded receptors are written,
     using a compact representation.  The gridded values are written in
     rows (x varies), starting with the most southern row of the grid.
     The GRID format is given the .GRD extension, and includes headers
     compatible with the SURFER(R) plotting software.

     A plotting and analysis file can also be created for the daily
     peak visibility summary output, in DATA format only.

     Generate Plot file output in addition to writing tables
     to List file?
                                 (LPLT) -- Default: F   ! LPLT  = T !  ver T

     Use GRID format rather than DATA format,
     when available?
                                 (LGRD) -- Default: F   ! LGRD  = F ! VER


Auxiliary Output Files (for subsequent analyses)
------------------------------------------------

      Visibility

      A separate output file may be requested that contains the change
      in visibility at each selected receptor when ASPEC = VISIB.  This
      file can be processed to construct visibility measures that are
      not available in CALPOST.

      Output file with the visibility change at each receptor?
                                (MDVIS) -- Default: 0   ! MDVIS  =  0  !

           0 =  Do Not create file
           1 =  Create file of DAILY (24 hour) Delta-Deciview
           2 =  Create file of DAILY (24 hour) Extinction Change (%)
           3 =  Create file of HOURLY Delta-Deciview
           4 =  Create file of HOURLY Extinction Change (%)


Additional Debug Output
-----------------------

   Output selected information to List file
    for debugging?
                               (LDEBUG) -- Default: F  ! LDEBUG  = F !

   Output hourly extinction information to REPORT.HRV?
    (Visibility Method 7)
                              (LVEXTHR) -- Default: F  ! LVEXTHR = F !

!END!
'''.format(fdate,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF, YYYY0, MM0, DD0, HH0, YYYY1, MM1, DD1, HH1, spec)
#           0       1         2     3     4    5    6    7    8      9      10      11     12     13     14     15      16    17   18  19    20    21    22   23   24

    return text


def namelist_prtmet(fdate,data_dir,runs_dir,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF):

    YYYY0 = str(fdate[0:4])
    MM0   = str(fdate[4:6])
    DD0   = str(fdate[6:8])
    HH0   = str(fdate[8:10])

    endTime = str(datetime.datetime.strptime(fdate, "%Y%m%d%H") + datetime.timedelta(hours=24))

    YYYY1 = str(endTime[0:4])
    MM1   = str(endTime[5:7])
    DD1   = str(endTime[8:10])
    HH1   = str(endTime[11:13])

    # This namelist is only for 00z initialization

    text='''PRTMET.INP      2.1             Hour Start and End Times with Seconds
-------------------------------------------------------------------------------

                 PRTMET PROCESSOR CONTROL FILE
                 -----------------------------

  PRTMET reads the binary meteorological data file produced by CALMET
  (CALMET.DAT), and reports selected information in formats amenable to
  quantitative analysis, QA review, or visualization.

-------------------------------------------------------------------------------
                                        a
INPUT GROUP: 0 -- Input and Output Files
--------------

     Default Name  Type          File Name
     ------------  ----          ---------
     CALMET.DAT    input     ! METDAT  = calmet.dat  !
     PRTMET.LST    output    ! RUNLST  = prtmet.lst      !
     PRTTIME.DAT   output    * PRTTIME  = prttime.dat      *

     Note: PRTTIME is a time-series file created only if a single point is
     selected for processing/printing in Input Group 1.  2D and 3D variables
     specified in Input Group 2 are written each timestep for this point.

     All file names will be converted to lower case if LCFILES = T
     Otherwise, if LCFILES = F, file names will be converted to UPPER CASE
     (LCFILES)                  Default: T      ! LCFILES = T     !
        T = lower case
        F = UPPER CASE
   
     NOTE: File/path names can be up to 132 characters in length

-------------
   a
     Additional output files may be defined in Input Groups 3 and 4 when 
     specific snapshot plots or average field plots are requested.

!END!

--------------------------------------------------------------------------------

INPUT GROUP: 1 -- Run control parameters
--------------

--- Processing Period ---

     Starting date:   Year (IBYR) -- No default       ! IBYR  = {16}  !
                     Month (IBMO) -- No default       ! IBMO  = {17}  !
                       Day (IBDY) -- No default       ! IBDY  = {18} !
                      Hour (IBHR) -- No default       ! IBHR  = 0  !
                   Second (IBSEC) -- No default       ! IBSEC = 0  !

     Ending date:     Year (IEYR) -- No default       ! IEYR  = {20}  !
                     Month (IEMO) -- No default       ! IEMO  = {21}  !
                       Day (IEDY) -- No default       ! IEDY  = {22} !
                      Hour (IEHR) -- No default       ! IEHR  = 19 !
                   Second (IESEC) -- No default       ! IESEC = 0  !

 ---------------
     NOTE:  The date/time is in the base time zone of the CALMET run.


--- Processing Options ---

     Time interval between printed/plotted fields:
     (number of CALMET output timesteps)
     Enter 1 to print every timestep, enter 2 to
     print every second timestep, etc.
     (ICHR)                     Default: 1      ! ICHR = 1  !

     Portion of meteorological grid to print/plot
     Enter beginning (NBX, NBY) and ending (NEX, NEY)
     cell indices (enter 0 to indicate entire grid).
     (NBX)                      Default: 0      ! NBX = 1  !
     (NBY)                      Default: 0      ! NBY = 1  !
     (NEX)                      Default: 0      ! NEX = 120 !
     (NEY)                      Default: 0      ! NEY = 90  !

     Note: If only one gridpoint is specified, variables selected in
     Input Group 2 are written to a separate time-series output file
     defined in Input Group 0.

!END!

--------------------------------------------------------------------------------

INPUT GROUP: 2 -- Listfile Output Options
--------------

-------------
Subgroup (2a)
-------------

     Print CALMET header run variables
     (e.g., grid definition, ...)?
     (LHDV)                     Default: T      ! LHDV = F  !

     Print full CALMET control file image?
     (LMETCF)                   Default: F      ! LMETCF = F  !

     Print meteorological station (X, Y)
     coordinates?
     (LSFC)  surface            Default: F      ! LSFC = F  !
     (LUPC)  upper air          Default: F      ! LUPC = F  !
     (LPRC)  precipitation      Default: F      ! LPRC = F  !

     Print nearest surface station ID for
     each grid point?
     (LNEARS)                   Default: F      ! LNEARS = F  !

     Print surface meteorological data?
     (temp, rho, SW rad, rh, precip code)
     (LSURF)                    Default: F      ! LSURF = F  !

     Print 2-D gridded domain characteristics?
     (LLI)  Leaf Area Index     Default: F      ! LLI = F  !
     (LLU)  Landuse             Default: F      ! LLU = F  !
     (LTE)  Terrain             Default: F      ! LTE = F  !
     (LZ0)  Roughness           Default: F      ! LZ0 = F  !

     Format used when printing gridded domain characteristics
     (used only if corresponding LLI,LLU,LTE,LZ0 is true)
        0 = use self-scaling exponential format
        1 = use fixed decimal format
     (FLI)  Leaf Area Index     Default: 0      ! FLI = 0  !
     (FLU)  Landuse             Default: 0      ! FLU = 0  !
     (FTE)  Terrain             Default: 0      ! FTE = 0  !
     (FZ0)  Roughness           Default: 0      ! FZ0 = 0  !

     Print 2-D gridded meteorological data?
     (LSTAB)  PG stability      Default: F      ! LSTAB = F  !
     (LUSTR)  u-star            Default: F      ! LUSTR = F  !
     (LMOLN)  Monin-Obukhov L   Default: F      ! LMOLN = F  !
     (LWSTR)  w-star            Default: F      ! LWSTR = F  !
     (LMXHT)  mixing ht         Default: F      ! LMXHT = F  !
     (LPRAT)  precip. rate      Default: F      ! LPRAT = F  !

     Format used when printing 2-D gridded meteorological data
     (used only if corresponding LSTAB,LUSTR,LMOLN,LWSTR,LMXHT,LPRAT
      is true)
        0 = use self-scaling exponential format
        1 = use fixed decimal format
     (FSTAB)  PG stability      Default: 0      ! FSTAB = 0  !
     (FUSTR)  u-star            Default: 0      ! FUSTR = 0  !
     (FMOLN)  Monin-Obukhov L   Default: 0      ! FMOLN = 0  !
     (FWSTR)  w-star            Default: 0      ! FWSTR = 0  !
     (FMXHT)  mixing ht         Default: 0      ! FMXHT = 0  !
     (FPRAT)  precip. rate      Default: 0      ! FPRAT = 0  !

     Present wind data as speed and direction?
     (IPWS)                     Default: 1      ! IPWS = 1  !
        0 = present as U,V components
        1 = present as wind speed, direction

     Scale factor to convert wind speed from m/s to other units
     (WSFAC)                    Default: 1.0    ! WSFAC = 1.0  !
        1.0   = m/s
        1.944 = to knots
        2.237 = to mph

     Format used when printing wind speeds
        0 = use self-scaling exponential format
        1 = use fixed decimal format
     (FWS)                      Default: 0      ! FWS = 0  !

     Number of layers of 3-D meteorological data printed
     (Identify data for each layer in Subgroup 2b)
     (N3D)                      Default: 0      ! N3D = 10  !

!END!

-------------
Subgroup (2b)
-------------
                                                    a,b
           DATA FOR EACH LAYER PRINTED (N3D entries)
           -----------------------------------------
                     c
                  U,V
                   or
       LAYER     WS,WD       W    TEMPERATURE
       -----     -----     -----     -----
* X =    3 ,        1 ,        0 ,        0       *  *END*

-------------
    a
     0 = do not print this variable for this layer
     1 = print this variable for this level
    b
     Each line is treated as a separate input subgroup and therefore
     must end with an input group terminator.
    c
     U,V or WS,WD format is selected by variable IPWS

--------------------------------------------------------------------------------

INPUT GROUP: 3 -- Snapshot Output Plotfiles
--------------

-------------
Subgroup (3a)
-------------

     Automatically generated snapshot plotfiles
     ------------------------------------------

     Snapshot plotfiles can be created automatically for each CALMET layer, and
     each timestep in the processing period identified in Group 1.  The plotfiles
     are compatible with the SURFER graphics system and are given names that
     include the date-time and model layer.  Filenames are of the form

     yyyy_Mmm_Ddd_hhmm(UTC+hhmm)_Lzz_tMIN.* or
     yyyy_Mmm_Ddd_hhmm(UTC+hhmm)_Lzz_tHR.*  where

     yyyy       = Year (Base Time Zone)
     mm         = Month (Base Time Zone)
     dd         = Day (Base Time Zone)
     hhmm       = Start of Timestep: Hour & Minute (Base Time Zone)
     (UTC+hhmm) = Base Time Zone definition (e.g. EST = UTC-0500)
     zz         = CALMET layer (00 for 2D variables)
     t          = Length of timestep (e.g., 1HR or 5MIN or 30MIN etc.)


     Create automatic plotfiles for each timestep?
        (LVECT)  Wind Vectors  (*.wsp,*.wdr)    Default: F      * LVECT = T  *
                     -- or --  (*.usp,*.vsp)
                     -- or --  (*.vec)
        (LTEMP)  Temperature   (*.deg)          Default: F      * LTEMP = F  *
        (LPREC)  Precipitation (*.prc)          Default: F      * LPREC = F  *
        (LMIXH)  Mixing Height (*.mix)          Default: F      * LMIXH = F  *
        (LIPGT)  PGT Stability (*.pgt)          Default: F      * LIPGT = F  *

     Force snapshot files to be ASCII (text), otherwise files
     containing non-integer data will be written as BINARY
     to reduce file size.
     (LSNAPTXT)                                 Default: T      * LSNAPTXT = T  *

     Type of file created for the Wind Vector option
     (MVECT)                                    Default: 0      * MVECT = 0  *
        0   = *.vec        (SURFER POST file)
        1   = *.wsp, *.wdr (speed and direction SURFER GRD files)
        2   = *.usp, *.vsp (U and V speed SURFER GRD files)

     Number of layers of 3-D meteorological data written to plot files.
     (Identify layers in Subgroup 3b)
     If set to 0, only layer 1 is provided.
     (NZPLOT)                                   Default: 0      * NZPLOT = 10 *

     Explicitly defined snapshot plotfiles
     -------------------------------------

     Specific snapshot plotfiles can also be created for selected CALMET layers
     and timesteps in the processing period identified in Group 1.  Plotfiles
     are compatible with the SURFER graphics system and are given names by the
     user.

     Number of snapshot plotfiles explicitly defined in Subgroup 3c
     (NSNAP)                                    Default: 0      * NSNAP = 0  *


!END!

-------------
Subgroup (3b)
-------------
                                                        a,b
           LAYERS AUTOMATICALLY PLOTTED (NZPLOT entries)
           ---------------------------------------------

       LAYER     WIND     TEMPERATURE
       -----     ----     -----------
* X =    1,        1,          0      *  *END*
* X =    2,        1,          0      *  *END*
* X =    3,        1,          0      *  *END*
* X =    4,        1,          0      *  *END*
* X =    5,        1,          0      *  *END*
* X =    6,        1,          0      *  *END*
* X =    7,        1,          0      *  *END*
* X =    8,        1,          0      *  *END*
* X =    9,        1,          0      *  *END*
* X =    10,        1,          0      *  *END*

-------------
    a
     0 = do not print this variable for this layer
     1 = print this variable for this level
    b
     Each line is treated as a separate input subgroup and therefore
     must end with an input group terminator.


-------------
Subgroup (3c)
-------------
                                                              a,b
           EXPLICIT SNAPSHOT DEFINITION (NSNAP 2-line entries)
           ---------------------------------------------------

             Layer  Timestep (position in processing period)
             -----  --------

* FILESNAP =   PREC1.grd  *
* PREC     =   1,     18    *  *END*
* FILESNAP =   PREC8.grd  *
* PREC     =   1,     20    *  *END*
* FILESNAP =   PREC11.grd  *
* PREC     =   1,     21    *  *END*



-------------
    a
     Enter information for each of the NSNAP plotfiles in 2-line groups.
     One line identifies the filename (FILESNAP = outfile), and the
     other line defines the type of snapshot and the layer & timestep.
     The type (e.g., MIXH =) must be one of the following:
         VECT = wind field (vector plot)
         UVEL = u-component of the wind (contour plot)
         VVEL = v-component of the wind (contour plot)
         WVEL = w-component of the wind (contour plot)
         TEMP = temperature (contour plot)
         WDIR = wind direction (contour plot)
         WSPE = wind speed (contour plot)
         IPGT = PG stability class (contour plot)
         USTA = friction velocity u-star (contour plot)
         MONL = Monin-Obukhov length (contour plot)
         WSTA = convective velocity w-star (contour plot)
         MIXH = mixing height (contour plot)
         PREC = precipitation rate (contour plot)
    b
     Each pair of lines is treated as a separate input subgroup and
     therefore must end with an input group terminator.

--------------------------------------------------------------------------------


INPUT GROUP: 4 -- Average Field Output Plotfiles
--------------

-------------
Subgroup (4a)
-------------

     Number of average field plotfiles
     (NMEAN)                    Default: 0      * NMEAN = 0 *

     Time period to begin averaging
     (timestep within processing period)
     (IBEGAV)                   Default: 1      * IBEGAV = 1 *

    Time period to end averaging
     (timestep within processing period)
     (IENDAV)                   Default: 1      * IENDAV = 1 *


!END!

-------------
Subgroup (4b)
-------------
                                                        a,b
          AVERAGE PLOT DEFINITION (NMEAN 2-line entries)
          ----------------------------------------------

             Layer
             -----

* FILEMEAN =   t1_20.grd    *
* TEMP     =   1            *  *END*

-------------
    a
     Enter information for each of the NMEAN plotfiles in 2-line groups.
     One line identifies the filename (FILEMEAN = outfile), and the
     other line defines the type of average and the layer.
     The type (e.g., MIXH =) must be one of the following:
         VECT = wind field (vector plot)
         UVEL = u-component of the wind (contour plot)
         VVEL = v-component of the wind (contour plot)
         WVEL = w-component of the wind (contour plot)
         TEMP = temperature (contour plot)
         WDIR = wind direction (contour plot)
         WSPE = wind speed (contour plot)
         IPGT = PG stability class (contour plot)
         USTA = friction velocity u-star (contour plot)
         MONL = Monin-Obukhov length (contour plot)
         WSTA = convective velocity w-star (contour plot)
         MIXH = mixing height (contour plot)
         PREC = precipitation rate (contour plot)
    b
     Each pair of lines is treated as a separate input subgroup and
     therefore must end with an input group terminator.

'''.format(fdate,PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF, YYYY0, MM0, DD0, HH0, YYYY1, MM1, DD1, HH1)
#           0       1         2     3     4    5    6    7    8      9      10      11     12     13     14     15      16    17   18  19    20    21    22   23

    return text


#--------------------------------------------------------------------------------------------------------
# Domains generator
#--------------------------------------------------------------------------------------------------------

def domains(sources,base_dir):

    # WRF-SisPI Domain limits

    xlong = np.loadtxt(base_dir+'/wxlon_d03.txt')
    xlat  = np.loadtxt(base_dir+'/wxlat_d03.txt')
    landmask = np.loadtxt(base_dir+'/landmask_d03.txt')

    # coordenadas sacado dinamico del netcdf

    XLAT1 = np.max(xlat[0,:])
    XLAT2 = np.min(xlat[-1,:])
    XLON1 = np.max(xlong[:,0])
    XLON2 = np.min(xlong[:,-1])

#    XLAT1 = 19.340546
#    XLAT2 = 24.264412
#    XLON1 = -85.71704
#    XLON2 = -73.765396


    # cargando fichero de fuentes

    sources = np.loadtxt(base_dir+'/'+sources,delimiter=',',dtype='str')
    lons, lats, fuente, provs = (sources[:,0],sources[:,1],sources[:,2],sources[:,3])
    calsources = sources[:,:]

    # Ejemplo:
    # -75.827722,20.036417,Santiago_de_Cuba,Torrefactora_Santiago_de_Cuba,6.0,2.0,0.5,5.4,524.87,0.0,0.1266,0.01055,0.0042201
    #      0         1             2                   3                    4  5    6  7     8    9   10      11      12

    # creando diccionario vacio para ordenar puntos por provincias

    arch = {} # diccionario para todos los puntos agrupados 
    cenDoms = {} # diccionario para los centroides de cada dominio provincial

    for prov in provs:
        arch[prov] = {}
        cenDoms[prov] = {}

        arch[prov]['lons'] = []
        arch[prov]['lats'] = []

        cenDoms[prov]['lons'] = 0
        cenDoms[prov]['lats'] = 0

    # rellenando diccionario que agrupa por provincias

    for i in range(len(provs)):
        arch[provs[i]]['lons'].append(float(lons[i]))
        arch[provs[i]]['lats'].append(float(lats[i]))


    # rellenando diccionario que calcula los centros de las fuentes de cada provincias

    for prov in arch.keys():
        if len(arch[prov]['lons']) > 1:
            xx, yy = centroide(arch[prov]['lons'], arch[prov]['lats'])
            cenDoms[prov]['lons'] = xx
            cenDoms[prov]['lats'] = yy
        else:
            cenDoms[prov]['lons'] = arch[prov]['lons'][0]
            cenDoms[prov]['lats'] = arch[prov]['lats'][0]


    # Definiendo dominio en funcion de la lista de fuentes o centroides

    sizeX, sizeY = (120/111.1,90/111.1) # tamaño de referencia del dominio llevado de km a grados

    outdoms = np.zeros(shape=(len(cenDoms.keys()),14))
    oname = []

    c=0
    for prov in cenDoms.keys():
        x1 = cenDoms[prov]['lons'] - sizeX/2
        x2 = cenDoms[prov]['lons'] + sizeX/2
        y1 = cenDoms[prov]['lats'] - sizeY/2
        y2 = cenDoms[prov]['lats'] + sizeY/2

        # Verificando distancias de fuentes con respecto al borde del dominio

        # ordenando puntos para determinar los extremos
        sortedLons = sorted(arch[prov]['lons'])
        sortedLats = sorted(arch[prov]['lats'])

        # obteniendo limites del dominio
        diffX1 = np.abs(sortedLons[0]-x1)
        diffX2 = np.abs(sortedLons[-1]-x2)
        diffY1 = np.abs(sortedLons[0]-y1)
        diffY2 = np.abs(sortedLons[-1]-y1)

        # corrigiendo longitudes de los limites del dominio

        if diffX1 < sizeX/2:
            newX1 = x1 - np.abs(sizeX/2 - diffX1)
        else:
            newX1 = x1

        if diffX2 < sizeX/2:
            newX2 = x2 + np.abs(sizeX/2 - diffX2)
        else:
            newX2 = x2

        # corrigiendo latitudes de los limites del dominio

        if diffY1 < sizeY/2:
            newY1 = y1 - np.abs(sizeY/2 - diffY1)
        else:
            newY1 = y1

        if diffY2 < sizeY/2:
            newY2 = y2 + np.abs(sizeY/2 - diffY2)
        else:
            newY2 = y2


        # Verificando que el dominio no sea mas grande que el de WRF

        # corrigiendo longitudes de los limites del dominio

        if newX1 < XLON1:
            fixX1 = XLON1
        else:
            fixX1 = newX1

        if newX2 > XLON2:
            fixX2 = XLON2
        else:
            fixX2 = newX2

        # corrigiendo latitudes de los limites del dominio

        if newY1 < XLAT1:
            fixY1 = XLAT1
        else:
            fixY1 = newY1

        if newY2 > XLAT2:
            fixY2 = XLAT2
        else:
            fixY2 = newY2

        # convirtiendo a UTM limites del dominio
        print(fixX1, fixX2, fixY1, fixY2)
        utmX1,utmX2,utmY1,utmY2 = grad_to_utm(fixX1, fixX2, fixY1, fixY2)

        # Buscando en rejilla del WRF la i,j correspondiente con la coordenada la esquina SW del dominio de calmet-calpuff
        iwrf,jwrf,iwrf2,jwrf2 = wrfcheck_ij(fixX1, fixX2, fixY1, fixY2, xlong, xlat)

        # Archivando en variable que se escribira a un txt todos los dominios
        outdoms[c,:] = (cenDoms[prov]['lons'],cenDoms[prov]['lats'],fixX1,fixX2,fixY1,fixY2,utmX1,utmX2,utmY1,utmY2,iwrf,jwrf,iwrf2,jwrf2)
        oname.append(prov.lower())

        c+=1


    # Salvando fichero con coordenadas de los bordes de los dominios y creando mapa representativo

    if not os.path.exists(base_dir+"/doms_map.png"):

        fig = plt.figure(1,figsize=(14,9.31),frameon=True,dpi=100)

        plt.title("Mapa de dominios a correr con CALMET/CALPUFF", fontsize=14, fontweight='bold')

        m = Basemap(projection='cyl', resolution='h',llcrnrlon=XLON1, llcrnrlat=XLAT1, urcrnrlon=XLON2, urcrnrlat=XLAT2)
        #m = Basemap(projection='tmerc', resolution='i',llcrnrlon=XLON1, llcrnrlat=XLAT1, urcrnrlon=XLON2, urcrnrlat=XLAT2, lat_0 = XLAT1+np.abs(XLAT1-XLAT2)/2, lon_0 = XLON1+np.abs(XLON1-XLON2)/2)

        m.drawmeridians(range(0, 360, 2),labels=[1,0,0,1],fontsize=8, linewidth=0)
        m.drawparallels(range(-180, 180, 1),labels=[1,0,0,1],fontsize=8, linewidth=0)

        m.drawmapboundary(fill_color='#98acc0')
        m.fillcontinents(color='#dfdcd8',lake_color='#98acc0', alpha=0.9)
        m.drawcoastlines(linewidth = 0.35)

        # Salvando txt

        ofile = open(base_dir+'/calx_doms.txt', 'w')
        ofile.write('PROVINCIA,CENLON,CENLAT,LON1,LON2,LAT1,LAT2,UTM_X1,UTM_X2,UTM_Y1,UTM_Y2,I1_WRF,J1_WRF,I2_WRF,J2_WRF\n')

        for l in range(len(outdoms[:,0])):
            ofile.write(oname[l]+','+str(outdoms[l,0])+','+str(outdoms[l,1])+','+str(outdoms[l,2])+','+str(outdoms[l,3])+','+str(outdoms[l,4])+','+str(outdoms[l,5])+','+str(outdoms[l,6])+','+str(outdoms[l,7])+','+str(outdoms[l,8])+','+str(outdoms[l,9])+','+str(int(outdoms[l,10]))+','+str(int(outdoms[l,11]))+','+str(int(outdoms[l,12]))+','+str(int(outdoms[l,13]))+'\n')

            elons = [outdoms[l,2],outdoms[l,2],outdoms[l,3],outdoms[l,3],outdoms[l,2]]
            elats = [outdoms[l,4],outdoms[l,5],outdoms[l,5],outdoms[l,4],outdoms[l,4]]
            xs, ys = m(elons, elats)
            m.plot(xs, ys, marker=None, color='r', linewidth=2.0, latlon=True)

        ofile.close()

        m.scatter(outdoms[:,0],outdoms[:,1], marker='D',color='b', zorder=3, s=0.8, label='Centro Doms.')
        m.scatter(np.array(lons,dtype=np.float32), np.array(lats,dtype=np.float32), marker='D',color='g', zorder=3, s=0.8, label='Fuentes Emis.')

        plt.legend(fontsize=8)

        plt.savefig(base_dir+"/doms_map.png", dpi=100, bbox_inches='tight', pad_inches=0)
        plt.close()


    if not os.path.exists(base_dir+"/doms_map2.png"):

        fig = plt.figure(1,figsize=(14,9.31),frameon=True,dpi=100)

        plt.title("Mapa de dominios verificando celdas de WRF", fontsize=14, fontweight='bold')

        m = Basemap(projection='cyl', resolution='h',llcrnrlon=XLON1, llcrnrlat=XLAT1, urcrnrlon=XLON2, urcrnrlat=XLAT2)
        #m = Basemap(projection='tmerc', resolution='i',llcrnrlon=XLON1, llcrnrlat=XLAT1, urcrnrlon=XLON2, urcrnrlat=XLAT2, lat_0 = XLAT1+np.abs(XLAT1-XLAT2)/2, lon_0 = XLON1+np.abs(XLON1-XLON2)/2)

        m.drawmeridians(range(0, 360, 2),labels=[1,0,0,1],fontsize=8, linewidth=0)
        m.drawparallels(range(-180, 180, 1),labels=[1,0,0,1],fontsize=8, linewidth=0)

        m.drawmapboundary(fill_color='#98acc0')
        m.fillcontinents(color='#dfdcd8',lake_color='#98acc0', alpha=0.9)
        m.drawcoastlines(linewidth = 0.35)

        # Buscando mini-dominios

        for l in range(len(outdoms[:,0])):

            m.contourf(xlong[int(outdoms[l,10]):int(outdoms[l,12]),int(outdoms[l,11]):int(outdoms[l,13])],xlat[int(outdoms[l,10]):int(outdoms[l,12]),int(outdoms[l,11]):int(outdoms[l,13])],landmask[int(outdoms[l,10]):int(outdoms[l,12]),int(outdoms[l,11]):int(outdoms[l,13])],zorder=3)

            elons = [outdoms[l,2],outdoms[l,2],outdoms[l,3],outdoms[l,3],outdoms[l,2]]
            elats = [outdoms[l,4],outdoms[l,5],outdoms[l,5],outdoms[l,4],outdoms[l,4]]
            xs, ys = m(elons, elats)
            m.plot(xs, ys, marker=None, color='r', linewidth=2.0, latlon=True, zorder=4)

        m.scatter(outdoms[:,0],outdoms[:,1], marker='D',color='b', zorder=4, s=0.8, label='Centro Doms.')
        m.scatter(np.array(lons,dtype=np.float32), np.array(lats,dtype=np.float32), marker='D',color='g', zorder=3, s=0.8, label='Fuentes Emis.')

        plt.legend(fontsize=8)

        plt.savefig(base_dir+"/doms_map2.png", dpi=100, bbox_inches='tight', pad_inches=0)
        plt.close()

    return outdoms,oname,calsources

#--------------------------------------------------------------------------------------------------------
# Tool funtions for domains generator
#--------------------------------------------------------------------------------------------------------

def utmToLatLng(zone, easting, northing, northernHemisphere=True):
    if not northernHemisphere:
        northing = 10000000 - northing

    a = 6378137
    e = 0.081819191
    e1sq = 0.006739497
    k0 = 0.9996

    arc = northing / k0
    mu = arc / (a * (1 - np.power(e, 2) / 4.0 - 3 * np.power(e, 4) / 64.0 - 5 * np.power(e, 6) / 256.0))

    ei = (1 - np.power((1 - e * e), (1 / 2.0))) / (1 + np.power((1 - e * e), (1 / 2.0)))

    ca = 3 * ei / 2 - 27 * np.power(ei, 3) / 32.0

    cb = 21 * np.power(ei, 2) / 16 - 55 * np.power(ei, 4) / 32
    cc = 151 * np.power(ei, 3) / 96
    cd = 1097 * np.power(ei, 4) / 512
    phi1 = mu + ca * np.sin(2 * mu) + cb * np.sin(4 * mu) + cc * np.sin(6 * mu) + cd * np.sin(8 * mu)

    n0 = a / np.power((1 - np.power((e * np.sin(phi1)), 2)), (1 / 2.0))

    r0 = a * (1 - e * e) / np.power((1 - np.power((e * np.sin(phi1)), 2)), (3 / 2.0))
    fact1 = n0 * np.tan(phi1) / r0

    _a1 = 500000 - easting
    dd0 = _a1 / (n0 * k0)
    fact2 = dd0 * dd0 / 2

    t0 = np.power(np.tan(phi1), 2)
    Q0 = e1sq * np.power(np.cos(phi1), 2)
    fact3 = (5 + 3 * t0 + 10 * Q0 - 4 * Q0 * Q0 - 9 * e1sq) * np.power(dd0, 4) / 24

    fact4 = (61 + 90 * t0 + 298 * Q0 + 45 * t0 * t0 - 252 * e1sq - 3 * Q0 * Q0) * np.power(dd0, 6) / 720

    lof1 = _a1 / (n0 * k0)
    lof2 = (1 + 2 * t0 + Q0) * np.power(dd0, 3) / 6.0
    lof3 = (5 - 2 * Q0 + 28 * t0 - 3 * np.power(Q0, 2) + 8 * e1sq + 24 * np.power(t0, 2)) * np.power(dd0, 5) / 120
    _a2 = (lof1 - lof2 + lof3) / np.cos(phi1)
    _a3 = _a2 * 180 / np.pi

    latitude = 180 * (phi1 - fact1 * (fact2 + fact3 + fact4)) / np.pi

    if not northernHemisphere:
        latitude = -latitude

    longitude = ((zone > 0) and (6 * zone - 183.0) or 3.0) - _a3

    return (latitude, longitude)


def grad_to_utm(fixX1, fixX2, fixY1, fixY2):

    ZoneNo = 17
#    myProj = Proj("+proj=utm +zone="+ZoneNo+",+north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

    myProj = pyproj.Proj(proj='utm', zone=ZoneNo, ellps='WGS84', preserve_units=True)

    utmX1,utmY1 = myProj(fixX1, fixY1)
    utmX2,utmY2 = myProj(fixX2, fixY2)


#    ZoneNo = "17"
#    myProj = Proj("+proj=utm +zone="+ZoneNo+"T,+north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#    utmX1,utmY1 = myProj(fixX1, fixY1)
#    utmX2,utmY2 = myProj(fixX2, fixY2)

#    n1 = utm.latlon_to_zone_number(fixX1, fixY1)
#    l1 = utm.latitude_to_zone_letter(fixY1)
#    proj1 = '{:02d}{}'.format(n1, l1)
#    x1, y1 = pyproj.transform(pyproj.Proj('+proj=latlong'),
#                            pyproj.Proj('+proj=utm +zone={}'.format(proj1)),
#                            fixX1, fixY1)
##    return x, y, n, l

#    n2 = utm.latlon_to_zone_number(fixX2, fixY2)
#    l2 = utm.latitude_to_zone_letter(fixY2)
#    proj2 = '{:02d}{}'.format(n, l)
#    x2, y2 = pyproj.transform(pyproj.Proj('+proj=latlong'),
#                            pyproj.Proj('+proj=utm +zone={}'.format(proj2)),
#                            fixX2, fixY2)

#    x1, y1, z1, emis1 = utm.from_latlon(*utm.to_latlon(fixX1, fixY1, 17, 'Q'))
#    x2, y2, z2, emis2 = utm.from_latlon(*utm.to_latlon(fixX2, fixY2, 17, 'Q'))

#    utmX1,utmX2,utmY1,utmY2 = (x1,x2,y1,y2)

    return utmX1/1000,utmX2/1000,utmY1/1000,utmY2/1000



def wrfcheck_ij(fixX1, fixX2, fixY1, fixY2, xlong, xlat):

    zcoords = np.zeros(shape=(len(xlong[:,:].flatten()),2))
    zcoords[:,0] = xlong[:,:].flatten()
    zcoords[:,1] = xlat[:,:].flatten()

    pt = np.array([[fixX1, fixY1]])
    d = cdist(pt,zcoords)
    xyindex = np.argmin(d)

    reflon, reflat = (xlong.flatten()[xyindex], xlat.flatten()[xyindex])

    for i in range(xlong.shape[0]):
        for j in range(xlong.shape[1]):
            if xlong[i,j] == reflon and xlat[i,j] == reflat:
                iwrf,jwrf = (i,j)
                
    pt = np.array([[fixX2, fixY2]])
    d = cdist(pt,zcoords)
    xyindex = np.argmin(d)

    reflon, reflat = (xlong.flatten()[xyindex], xlat.flatten()[xyindex])

    for i in range(xlong.shape[0]):
        for j in range(xlong.shape[1]):
            if xlong[i,j] == reflon and xlat[i,j] == reflat:
                iwrf2,jwrf2 = (i,j)


    return iwrf-5,jwrf-5,iwrf2+5,jwrf2+5


def centroide(lons, lats):

     _x_list = [lon for lon in lons]
     _y_list = [lat for lat in lats]
     _len = len(lats)
     _x = sum(_x_list) / _len
     _y = sum(_y_list) / _len

     return(_x, _y)



if __name__ == '__main__':
    main()



