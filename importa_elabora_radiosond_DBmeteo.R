library(DBI)
library(RMySQL)

#definisco driver
drv<-dbDriver("MySQL")

#apro connessione con il db 
conn<-try(dbConnect(drv, user=as.character(Sys.getenv("MYSQL_USR")), password=as.character(Sys.getenv("MYSQL_PWD")), dbname=as.character(Sys.getenv("MYSQL_DBNAME")), host=as.character(Sys.getenv("MYSQL_HOST"))))

t<-read.table(file="elaborazioni_radiosond_milano.txt",header=F,sep=",")
#data<-as.POSIXlt(strptime(t$V1,"%Y/%m/%d %H"),"UTC" )
data<-t$V1
zmin<-t$V2
zmax<-t$V3
psurf<-t$V4
zt500_850<-t$V5
zt700_850<-t$V6
hablmin<-t$V7
hablmax<-t$V8
N<-t$V9
#
zmin[zmin>7000 | zmax>7000]<-NA
zmax[zmin>7000 | zmax>7000]<-NA
#
valori <-vector(length=length(zmin))
valori[] <- paste ( "('",
                    data[]     , "',"  ,
                    zmin[]     , ","  ,
                    zmax[]     , ","  ,
                    psurf[]    , ","  ,
                    zt500_850[], ","  ,
                    zt700_850[], ","  ,
                    hablmin[],   ","  ,
                    hablmax[],   ","  ,
                    N[], ")" ,sep="")

stringa<-NULL
stringa <- toString(valori[])
#sostituisco eventuali NA
stringa <- gsub("NA","NULL",stringa)
#print(stringa)
query_inserimento_riga<-paste("insert into RADIO_elaborazioni values",
                              stringa," on duplicate key update data=values(data), ",
                              "zmin=values(zmin), zmax=values(zmax), ",
                              "psurf=values(psurf), z500_850=values(z500_850),",
                              "z700_850=values(z700_850), hablmin=values(hablmin),",
                              "hablmax=values(hablmax),N=values(N)", sep="")
print( paste(" query inserisci > ", query_inserimento_riga," \n",sep=""))
#cat(query_inserimento_riga,"\n", file=file_log, append=T)
inserimento_riga <- try(dbGetQuery(conn,query_inserimento_riga),silent=TRUE)
if (inherits(inserimento_riga,"try-error")) {
  print(paste(inserimento_riga,"\n",sep=""))
  quit(status=1)
}
#
print( "chiudo DB \n" )
dbDisconnect(conn)
rm(conn)
dbUnloadDriver(drv)

print ( paste("PROGRAMMA ESEGUITO CON SUCCESSO alle ", date()," \n",sep="" ))
quit(status=0)
