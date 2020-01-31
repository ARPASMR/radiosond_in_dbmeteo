#!/bin/bash
#
# 2020-01-31 MR dockerizzazione codice 
#               per alimentare tabelle
#               METEO.RADIO_elaborazioni
# 
########################################

S3CMD='s3cmd --config=config_minio.txt'

numsec=7200 # 2 ore 

   # leggo il file col radiosondaggio da Minio
     s3cmd --config=config_minio.txt --force get s3://rete-monitoraggio/radiosondaggi/*.00S ./
   #eseguo lo script 
     ./driver_elabora_radiosond.sh
   # verifico se è andato a buon fine
     STATO=$?
     echo "STATO USCITA SCRIPT ====> "$STATO

     if [ "$STATO" -eq 1 ] # se si sono verificate anomalie esci 
     then
         exit 1
     fi
    ################# pulizia cartella di minio
    periodo="3 days"
    $S3CMD --config=config_minio.txt ls s3://rete-monitoraggio/radiosondaggi/ | while read -r line;
    do
      createDate=`echo $line|awk {'print $1'}`
      createDate=`date -d"$createDate" +%s`
      olderThan=`date -d"-$periodo" +%s`
      if [[ $createDate -lt $olderThan ]]
        then
          fileName=`echo $line|awk {'print $4'}`
          if [[ $fileName != "" ]]
            then
            $S3CMD del "$fileName"
          fi
      fi
    done;
    sleep $numsec



while [ 1 ]
do
  if [ $SECONDS -ge $numsec ]
  then
    SECONDS=0
      # leggo il file col radiosondaggio da Minio
       s3cmd --config=config_minio.txt --force get s3://rete-monitoraggio/radiosondaggi/*.00S ./
      #eseguo lo script 
       ./driver_elabora_radiosond.sh
      # verifico se è andato a buon fine
      STATO=$?
      echo "STATO USCITA SCRIPT ====> "$STATO

      if [ "$STATO" -eq 1 ] # se si sono verificate anomalie esci 
      then
         exit 1
      fi
      ################# pulizia cartella di minio
      periodo="3 days"
      $S3CMD --config=config_minio.txt ls s3://rete-monitoraggio/radiosondaggi/ | while read -r line;
      do
        createDate=`echo $line|awk {'print $1'}`
        createDate=`date -d"$createDate" +%s`
        olderThan=`date -d"-$periodo" +%s`
        if [[ $createDate -lt $olderThan ]]
          then
            fileName=`echo $line|awk {'print $4'}`
            if [[ $fileName != "" ]]
              then
              $S3CMD del "$fileName"
            fi
        fi
      done;
      sleep $numsec
   fi
done




