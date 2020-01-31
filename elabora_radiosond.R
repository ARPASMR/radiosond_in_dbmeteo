source("lib.R")
# Formato radiosondaggio fine
#   616    80  4546   928   104  1902   304  1641 818406507 00900 ///// ////
#     0   104 10130   128    65    64    90    10      1
#     2   113 10119   120    63    51    91    12      1
#     4   121 10109   117    63    49    93    14      1
#     6   129 10100   116    64    51    94    16      1
#     8   136 10091   115    65    52    95    18      1
#    10   144 10082   114    66    53    96    20      1
#    12   152 10072   113    66    53    96    22      1
#    14   160 10062   113    66    53    97    23      1
#    16   169 10051   112    67    53    97    24      1
#    18   179 10039   111    67    53    97    25      1
#    20   190 10027   110    68    53    97    25      1
#    22   199 10015   109    68    53    97    26      1
#    24   209 10004   108    69    53    96    26      1
#    26   218  9993   107    69    52    95    26      1
#    28   226  9983   106    69    52    94    26      1
#    30   235  9972   106    69    51    94    26      1
#    32   245  9960   105    69    51    93    26      1
#    34   255  9948   104    70    51    92    26      1
#    36   266  9936   103    70    51    92    26      1
#    38   276  9924   102    71    51    91    26      1
#
arguments<-commandArgs()
print(arguments)
file_input<-arguments[3]
file_output<-arguments[4]
#
# data
file_input_1<-strsplit(file_input,"/")[[1]][length(strsplit(file_input,"/")[[1]])]
anno<-as.numeric(substr(file_input_1,1,2))
mese<-as.numeric(substr(file_input_1,3,4))
giorno<-as.numeric(substr(file_input_1,5,6))
ora<-as.numeric(substr(file_input_1,7,8))
if (anno<50){
  anno<-anno+2000
} else {
  anno<-anno+1900
}
if ( (mese>12) | (mese<1) | (giorno>31) | (giorno<1) | (ora>24) | (ora<0) ) {
  quit(status=1)
}
#------------------------------------------------------------------------------
# leggi dati di input
tab <- scan(file=file_input,what=list( "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       ""),skip=1)
#tab<-scan(file="09112022.00S",skip=1,what=list("","","","","","","","",""))
sec<-as.numeric(tab[[1]][])
el<-as.numeric(tab[[2]][])
z<-el-el[1]
p<-as.numeric(tab[[3]][])/10.
t<-as.numeric(tab[[4]][])/10.
rh<-as.numeric(tab[[5]][])
td<-as.numeric(tab[[6]][])/10.
dv<-as.numeric(tab[[7]][])
vv<-as.numeric(tab[[8]][])/10.
flagval<-as.numeric(tab[[9]][])
tk<-t+273.15
tdk<-td+273.15
# potential temperature
theta<-GetTheta(t,p)+273.15
# virtual potential temperature
thetav<-virtual2(theta,tdk,p)
#------------------------------------------------------------------------------
# Rib:bulk Richardson number
# Jerevic A. et al, 2006. The critical bulk Richardson number in urban areas: verification and application in a numerical weather prediction model. Tellus 58A. (and reference therein)
Rib<-array()
i=1
vv_aux<-vv
vv_aux[vv==0]<-0.001
g<-9.81
while(i<=length(thetav)){
  thetav_avg<-mean(thetav[1:i])
  Rib[i] <- (g * z[i]) / thetav_avg * (thetav[i]-thetav[1]) / vv_aux[i]**2
  i<-i+1
}
#------------------------------------------------------------------------------
# Ri: Richardson number
Ri<-array()
Ri[1]<-NA
Ri[length(thetav)]<-NA

Ri1<-array()
Ri1[1]<-NA
Ri1[length(thetav)]<-NA

Ri2<-array()
Ri2[1]<-NA
Ri2[length(thetav)]<-NA

Ri3<-array()
Ri3[1]<-NA
Ri3[length(thetav)]<-NA

dthetavdz<-array()
dthetavdz[1]<-NA
dthetavdz[length(thetav)]<-NA

dthetadz<-array()
dthetadz[1]<-NA
dthetadz[length(theta)]<-NA

dtkdz<-array()
dtkdz[1]<-NA
dtkdz[length(t)]<-NA

dudz<-array()
dudz[1]<-NA
dudz[length(thetav)]<-NA

i<-2
while(i<length(thetav)){
  if (z[i+1]==z[i-1]) {
    dthetavdz[i]<-0.
    dthetadz[i]<-0.
    dtkdz[i]<-0.
    dudz[i]<-0.
  } else {
    dthetavdz[i]<-(thetav[i+1]-thetav[i-1])/(z[i+1]-z[i-1])
    dthetadz[i]<-(theta[i+1]-theta[i-1])/(z[i+1]-z[i-1])
    dtkdz[i]<-(tk[i+1]-tk[i-1])/(z[i+1]-z[i-1])
    dudz[i]<-(vv[i+1]-vv[i-1])/(z[i+1]-z[i-1])
  }
#  tk_avg<-mean(tk[1:i])
  tk_avg<-mean(tk[i-2:i+2])
# Per il calcolo di Ri e' necessario the buoyancy parameter "beta=g/Tref" dove:
#  + g e' l'accelerazione di gravita'
#  + Tref e' un valore di riferimento per la temperatura assoluta.
#    si sceglie Tref=273K riferimento:
#    Esau I.N., Zilitnkevich S. 2006: Universal Dependences between Turbulent 
#     and Mean Flow Parameters in Stably and Neutrally Stratified Planetary
#     Boundary Layers,  Nonlinear Processes in Geophysics.
#  Tref<-mean(thetav[(i-1):(i+1)])
  Tref<-293.
  if (dudz[i]==0) {
    dudz[i]<-0.05
    Ri[i]<- g/Tref * dthetavdz[i]/(dudz[i])**2
#    Ri1[i]<- g/mean(theta[i-1],theta[i],theta[i+1]) * dthetadz[i]/(dudz[i])**2
#    Ri2[i]<- g/tk[i] * dtkdz[i]/(dudz[i])**2
#    Ri3[i]<- g/tk_avg * dthetadz[i]/(dudz[i])**2
  } else {
    Ri[i]<- g/Tref * dthetavdz[i]/(dudz[i])**2
#    Ri1[i]<- g/mean(theta[i-1],theta[i],theta[i+1]) * dthetadz[i]/(dudz[i])**2
#    Ri2[i]<- g/tk[i] * dtkdz[i]/(dudz[i])**2
#    Ri3[i]<- g/tk_avg * dthetadz[i]/(dudz[i])**2
  }
#  print(paste(dthetavdz[i],dudz[i],Ri[i]))
  i<-i+1
}
#------------------------------------------------------------------------------
# tentativo di stima di L a partire da Ri. riferimento (Eq (41)):
# Monin A.S, Obukhov A.M., 1954. Basic Laws of turbulent mixing in the ground layer of the atmosphere. Geofizicheskii Institut, Trudy.
#
#Ri[1]<-g/thetav[1] * ((thetav[2]-thetav[1])/(z[2]-z[1])) / ( (vv[2]-vv[1])/(z[2]-z[1]) )**2
Ri[1]<-g/Tref * ((thetav[2]-thetav[1])/(z[2]-z[1])) / ( (vv[2]-vv[1])/(z[2]-z[1]) )**2
Linv<-(Ri[3]-Ri[2])/(z[3]-z[2])
L<-1./Linv
print(L)
Linv<-(Ri[2]-Ri[1])/(z[2]-z[1])
L<-1./Linv
print(L)
# debug
#cbind(sec[1:50],z[1:50],p[1:50],t[1:50],rh[1:50],td[1:50],dv[1:50],vv[1:50],flagval[1:50],theta[1:50],thetav[1:50],Rib[1:50],round(Ri[1:50],4))
#
#------------------------------------------------------------------------------
# Stima della quota dell'Atmospheric Boundary Layer
# Zilitinkevich S., Baklanov A. 2002. Calculation of the height of the stable boundary layer in practical applications. Boundary Layer Meteorology.
# "taking RiBc in the interval 0.2 < RiBc < 0.5 gives reasonable order-of-magnitude estimates of atmosperic boundary layer heigth"
Rib_min<-0.2
Rib_max<-0.5
habl<-mean(z[(Rib>0.2) & (Rib<0.5)])
z[(Rib>0.2) & (Rib<0.5)]
#
i<-1
while(Rib[i]<=Rib_min){
  i<-i+1
}
i<-i-1
habl_min<-z[i] + (Rib_min-Rib[i]) *  (z[i+1]-z[i]) / (Rib[i+1]-Rib[i])
habl_min
#
i<-1
while(Rib[i]<=Rib_max){
  i<-i+1
}
i<-i-1
habl_max<-z[i] + (Rib_max-Rib[i]) *  (z[i+1]-z[i]) / (Rib[i+1]-Rib[i])
habl_max
habl_med<-(habl_max+habl_min)/2
#
#cbind(z[1:15],Rib[1:15])
#
#------------------------------------------------------------------------------
# Stima della Brunt-Vaisala frequency N
# vedi Eq(18) di 
# Zilitinkevich S.S., Esau I.N. 2007. Similarity theory and calculation of turbulent fluxes 
#  at the surface for the stably stratified atmospheric boundary layer. Boundary-Layer Meteorology
beta<-g/Tref
i<-1
j<-0
N_aux<-0
lim<-2*habl_max
while(z[i]<=lim){
  if (z[i]>habl_min) {
    N_aux <- N_aux + (beta*dthetavdz[i])**2.*(z[i+1]-z[i])
    j<-j+1
  }
  i<-i+1
}
N<-( 1. / habl_med * N_aux )**0.25
N
#------------------------------------------------------------------------------
# Stima della quota dello zero termico
dim<-length(t)
zmax<-0
zmin<-0
for (i in seq(dim,2,by=-1) ) {
  if ( t[i]==0 | (t[i]<0 & t[i-1]>=0) ) {
    print( paste("i z t_i t_i-1",i,el[i],t[i],t[i-1] ) )
    zmax<-el[i]
    break
  }
}

#
for (i in seq(2,dim,by=1) ) {
  if ( t[i]==0 | (t[i]<0 & t[i-1]>=0) ) {
    print( paste("i z t_i t_i-1",i,el[i],t[i],t[i-1] ) )
    zmin<-el[i]
    break
  }
}
#------------------------------------------------------------------------------
# Stima dello spessore atmosferico fra punti notevoli
for (i in seq(2,dim,by=1) ) {
  if ( p[i]<=500 ) {
    el500=el[i-1]+(500-p[i-1])*(el[i]-el[i-1])/(p[i]-p[i-1])
    break
  }
  if ( p[i]<=850 ) {
    el850=el[i-1]+(850-p[i-1])*(el[i]-el[i-1])/(p[i]-p[i-1])
  }
  if ( p[i]<=700 ) {
    el700=el[i-1]+(700-p[i-1])*(el[i]-el[i-1])/(p[i]-p[i-1])
  }
}
#print(t[1:400])
#print(el850)
#print(el500)
#------------------------------------------------------------------------------
cat(file=file_output,paste(anno,"/",mese,"/",giorno," ",ora,",",
                           zmin,",",zmax,",",p[1],",",
                           round(el500-el850,2),",",round(el700-el850,2),",",
                           round(habl_min,2),",",round(habl_max,2),",",round(N,6),
                           sep=""),append=T)
cat(file=file_output,"\n",append=T)
quit()
