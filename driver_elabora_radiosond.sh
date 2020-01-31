# MR 2020-01-31
# nel dockerizzare semplificato codice perchè
# eliminata parte di archiviazione file
#############################################


for FILE in *.00S; do
  echo $FILE
  /usr/bin/R --vanilla $FILE elaborazioni_radiosond_milano.txt < elabora_radiosond.R
  /usr/bin/R --vanilla $FILE elaborazioni_radiosond_milano.txt < importa_elabora_radiosond_DBmeteo.R
  rm -vf $MAIN/elaborazioni_radiosond_milano.txt
done
#
