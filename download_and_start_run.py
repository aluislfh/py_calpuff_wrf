import os, sys, glob
import requests
import datetime as dt


def main():

    base_dir = '/home/dayana/Desktop/new_calpuff'

    url1 = 'http://modelos.insmet.cu/static/models/ascii_data/gamma/'+yyyy+mm+dd+'00/d03.tar.gz'
    url2 = 'http://modelos.insmet.cu/static/models/ascii_data/gamma/'+yyyy+mm+dd+'06/d03.tar.gz'

    sdate = ''

    if uri_exists(url1) == True:
        print('Descargando fichero: '+url1)
        sdate = yyyy+mm+dd+'00'

        os.system('mkdir -p '+base_dir+'/data/'+sdate)
        os.chdir(base_dir+'/data'+sdate)
        os.system('wget -c --tries=20 '+url1)
        os.system('tar -xf d03.tar.gz')


    elif uri_exists(url2) == True:
        print('Descargando fichero: '+url2)
        sdate = yyyy+mm+dd+'06'

        os.system('mkdir -p '+base_dir+'/data/'+sdate)
        os.chdir(base_dir+'/data'+sdate)
        os.system('wget -c --tries=20 '+url1)
        os.system('tar -xf d03.tar.gz')

    else:
        print('No hay datos de 00z o 06z del WRF para dia '+yyyy+mm+dd)


    os.system('python run_calpuff.py '+sdate)




def uri_exists(url):
    r = requests.get(url, stream=True)
    if r.status_code == 200:
        return True
    else:
        return False


if __name__ == "__main__":
    main()


