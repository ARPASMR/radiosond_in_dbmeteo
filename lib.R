#************************************************************************
#* FUNZIONI DI VARIA NATURA
#*************************************************************************
Templcl<-function(temp,dewp){

#*------------------------------------------------------
#* Calculate the temp at the LCL given temp & dewp in C
#*------------------------------------------------------

tempk=temp+273.15
dewpk=dewp+273.15
Parta=1./(dewpk-56.)
Partb=log(tempk/dewpk)/800.
Tlcl=1./(Parta+Partb)+56.
return(Tlcl-273.15)
}
#**************************************************************************

Preslcl<-function(temp,dewp,pres){

#*-------------------------------------------------------
#* Calculate press of LCL given temp & dewp in C and pressure
#*-------------------------------------------------------

Tlclk=Templcl(temp,dewp)+273.15
tempk=temp+273.15
theta=tempk*(1000/pres)**0.286
plcl=1000*(Tlclk/theta)**3.48
return(plcl)
}
#**************************************************************************
#LiftWet<-function(startt,startp,endp,display,Pmin,Pmax){
#
##*--------------------------------------------------------------------
##* Lift a parcel moist adiabatically from startp to endp.
##* Init temp is startt in C.  If you wish to see the parcel's
##* path plotted, display should be 1.  Returns temp of parcel at endp.
##*--------------------------------------------------------------------
#
#temp=startt
#pres=startp
#cont = 1
#delp=10
#While (pres >= endp & cont = 1)
#    If (display = 1)
#       xtemp=GetXLoc(temp,pres)
#       "q w2xy "xtemp" "pres
#       xloc=subwrd(result,3)
#       yloc=subwrd(result,6)
#       If (xtemp < 0 | xtemp > 100)
#          cont=0
#       Else
#          If (pres >= Pmin & pres < Pmax & pres < startp)  
#             "draw line "xold" "yold" "xloc" "yloc 
#          Endif
#       Endif
#    Endif
#    xold=xloc
#    yold=yloc
#    temp=temp-100*delp*gammaw(temp,pres-delp/2,100)
#    pres=pres-delp
#EndWhile
#return(temp)
#}

#**************************************************************************
#function LiftDry(startt,startp,endp,display,Pmin,Pmax){
#
##*--------------------------------------------------------------------
##* Lift a parcel dry adiabatically from startp to endp.
##* Init temp is startt in C.  If you wish to see the parcel's
##* path plotted, display should be 1.  Returns temp of parcel at endp.
##*--------------------------------------------------------------------
#
#starttk=startt+273.15
#cont = 1
#delp=10
#round=int(startp/10)*10
#subscr=0.1*round
#powstart=pow(startp,-0.286)
#temp=starttk*_powpres.subscr*powstart-273.15
#pres=round-10
#While (pres >= endp & cont = 1) 
#    subscr=0.1*pres
#    temp=starttk*_powpres.subscr*powstart-273.15
#    If (display = 1) 
#       xtemp=GetXLoc(temp,pres)
#       "q w2xy "xtemp" "pres
#       xloc=subwrd(result,3)
#       yloc=subwrd(result,6)
#       If (xtempold > 0 & xtempold < 100 & xtemp > 0 & xtemp < 100) 
#          If (pres >= Pmin & pres < Pmax & pres < startp)  
#             "draw line "xold" "yold" "xloc" "yloc
#          Endif
#       Endif
#    Endif
#    xold=xloc
#    xtempold=xtemp
#    yold=yloc
#    pres=pres-delp
#EndWhile
#return(temp)
#}
#**************************************************************************
#function CAPE(startt,startp,endp,sndtemp,snddewp){
#
##*---------------------------------------------------------------------
##* Returns all postive area and convective inhibition above LCL.
##* Parcel is lifted from LCL at startt,startp and is halted
##* at endp.   Integration method used is trapezoid method.
##*---------------------------------------------------------------------
#
#pres=startp
#PclTemp=startt
#PclTempV=virtual2(PclTemp+273.15,PclTemp+273.15,pres)-273.15
#delp=10
#Pos=0
#Neg=0
#Neg2=0
#
#count=0
#while (pres >= endp){
#   EnvTemp=interp(sndtemp,pres)
#   EnvDewp=interp(snddewp,pres)
#   EnvTempV=virtual2(EnvTemp+273.15,EnvDewp+273.15,pres)-273.15
#   DelT=PclTempV-EnvTempV
#   If (abs(EnvTempV) < 130 & abs(PclTempV) < 130)
#     count=count+1
#     If (count > 1) 
#       Val=DelT/pres+Prev 
#       If (Val > 0)
#          Pos=Pos+Val
#          Neg2=0
#       Else
#          Neg=Neg+abs(Val)
#          Neg2=Neg2+abs(Val)
#       Endif
#     Endif
#     Prev=DelT/pres
#   Endif
#   pres=pres-delp
#   PclTemp=PclTemp-100*delp*gammaw(PclTemp,pres,100)
#   PclTempV=virtual2(PclTemp+273.15,PclTemp+273.15,pres)-273.15
#}
#
#Pos=0.5*Pos*287*delp
#CIN=0.5*(Neg-Neg2)*287*delp
#
#return(Pos" "CIN)
#}
#***************************************************************************
gammaw<-function(tempc,pres,rh){

#*-----------------------------------------------------------------------
#* Function to calculate the moist adiabatic lapse rate (deg C/Pa) based
#* on the temperature, pressure, and rh of the environment.
#*----------------------------------------------------------------------

tempk=tempc+273.15
es=satvap2(tempc)
ws=mixratio(es,pres)
w=rh*ws/100
tempv=virtual(tempk,w)
latent=latentc(tempc)

A=1.0+latent*ws/(287*tempk)
B=1.0+0.622*latent*latent*ws/(1005*287*tempk*tempk)
Density=100*pres/(287*tempv)
lapse=(A/B)/(1005*Density)
return(lapse)
}
#*************************************************************************
latentc<-function(tempc){

#*-----------------------------------------------------------------------
#* Function to return the latent heat of condensation in J/kg given
#* temperature in degrees Celsius.
#*-----------------------------------------------------------------------

val=2502.2-2.43089*tempc

return(val*1000)
}
#*************************************************************************
#function precipw(sndtemp,snddewp,startp,endp){
#
##*-----------------------------------------------------------------------
##* Function to calculate the precipitable water (cm) in a sounding
##* starting at pressure level startp and ending at pressure level endp.
##*-----------------------------------------------------------------------
#
#ppold=-999
#ttold=-999
#ddold=-999
#delp=10
#Int=0
#mix=0 
#pres=startp
#logpp=log(pres)
#logppm=log(pres-delp)
#while (pres >= endp)
#   tt=interp(sndtemp,pres)
#   dd=interp(snddewp,pres)
#   if (tt>-900 & ttold>-900 & dd>-900 & ddold>-900)
#      e=satvap2(dd)
#      mix=mixratio(e,pres)
#      mixavg=(logpp*mix+logppm*mixold)/(logpp+logppm)
#      Int=Int+1.020408*mixavg*delp
#   endif
#   ttold=tt
#   ddold=dd
#   ppold=pp
#   mixold=mix
#   pres=pres-delp
#   logpp=logppm
#   logppm=log(pres-delp)
#endwhile
#
#return(Int)
#}
#*************************************************************************

virtual<-function(temp,mix){

#*------------------------------------------------------------
#* Function to return virtual temperature given temperature in
#* kelvin and mixing ratio in g/g.
#*-------------------------------------------------------------

tempv=temp*(1.0+0.6*mix)

return (tempv)
}
#************************************************************************

virtual2<-function(temp,dewp,pres){

#*------------------------------------------------------------
#* Function to return virtual temperature in kelvin given temperature in
#* kelvin and dewpoint in kelvin and pressure in mb
#*-------------------------------------------------------------

#if ( !(any(temp<0) || any(dewp>0)) ){
  vap=satvap2(dewp-273.15)
  mix=mixratio(vap,pres)
  tempv=virtual(temp,mix)
#}else{
#  tempv=-9999
#}

return (tempv)
}
#************************************************************************

#function satvapor(temp){
#
##*---------------------------------------------------------------
##* Given temp in Celsius, returns saturation vapor pressure in mb
##*---------------------------------------------------------------
#
#pol=_C0+temp*(_C1+temp*(_C2+temp*(_C3+temp*(_C4+temp*(_C5+temp*(_C6+temp*(_C7+temp*(_C8+temp*(_C9)))))))))
#
#return(6.1078/pol**8)
#}
#************************************************************************

satvap2<-function(temp){

#*---------------------------------------------------------------
#* Given temp in Celsius, returns saturation vapor pressure in mb
#*---------------------------------------------------------------

es=6.112*exp(17.67*temp/(temp+243.5))

return(es)
}
#*************************************************************************

mixratio<-function(e,p){

#*------------------------------------------------------
#* Given vapor pressure and pressure, return mixing ratio
#*-------------------------------------------------------

mix=0.622*e/(p-e)

return(mix)
}
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#FINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUIFINOQUI
#************************************************************************

getrh<-function(temp,dewp,pres){

tempk=temp+273.15
dewpk=dewp+273.15

es=satvap2(temp)

if (temp > 0){
   A=2.53*10**9
   B=5420
}else{
   A=3.41*10**10
   B=6130
}

w=A*0.622*exp(-B/dewpk)/pres
ws=mixratio(es,pres)

return(100*w/ws)
}
#************************************************************************
#function interp(array,pres){
#
##*------------------------------------------------------------------------
##* Interpolate inside array for pressure level pres.
##* Returns estimated value of array at pressure pres.
##*------------------------------------------------------------------------
#
#"set gxout stat"
#"set lev "pres
#altpres=subwrd(result,4)
#"q dims"
#rec=sublin(result,4)
#zlev=subwrd(rec,9)
#
#If (zlev < 2 | zlev > _zmaxfile)
#  Vest = -9999.0
#Else
#  If (altpres > pres)
#    zlev=zlev+1
#  Endif
#  "set z "zlev
#  PAbove=subwrd(result,4)
#  "d "array"(lev="PAbove")"
#  rec=sublin(result,1)
#  check=substr(rec,1,6)
#  If (check = "Notice") 
#      rec=sublin(result,9)
#  Else
#      rec=sublin(result,8)
#  Endif
#  VAbove=subwrd(rec,4)
#  "set z "zlev-1
#  PBelow=subwrd(result,4)
#  "d "array"(lev="PBelow")"
#  rec=sublin(result,1)
#  check=substr(rec,1,6)
#  If (check = "Notice") 
#      rec=sublin(result,9)
#  Else
#      rec=sublin(result,8)
#  Endif
#  VBelow=subwrd(rec,4)
#
##* Now if we are in a region of missing data, find next good level.
#
#  If (abs(VAbove) > 130 & zlev > 1 & zlev < _zmaxfile)
#     zz=zlev
#     While (abs(VAbove) > 130 & zz < _zmaxfile)
#       zz=zz+1
#       "set z "zz
#       PAbove=subwrd(result,4)
#       "d "array"(lev="PAbove")"
#       rec=sublin(result,1)
#       check=substr(rec,1,6)
#       If (check = "Notice") 
#           rec=sublin(result,9)
#       Else
#           rec=sublin(result,8)
#       Endif
#       VAbove=subwrd(rec,4)
#     EndWhile
#  Endif
#
#  If (abs(VBelow) > 130 & zlev > 1 & zlev < _zmaxfile)
#      zz=zlev-1
#      While (abs(VBelow) > 130 & zz > 1)
#        zz=zz-1
#        "set z "zz
#        PBelow=subwrd(result,4)
#        "d "array"(lev="PBelow")"
#        rec=sublin(result,1)
#        check=substr(rec,1,6)
#        If (check = "Notice") 
#            rec=sublin(result,9)
#        Else
#            rec=sublin(result,8)
#        Endif
#        VBelow=subwrd(rec,4)
#      EndWhile
#  Endif
#
#  If (abs(VAbove) < 130 & abs(VBelow) < 130)
#     Vest=VBelow+log(PBelow/pres)*(VAbove-VBelow)/log(PBelow/PAbove)
#  Else
#     Vest=-9999.0
#  Endif
#
#Endif
#
#Return(Vest)
#}
#****************************************************************************

GetUWnd<-function(wspd,wdir){

#*------------------------
#* Get x-component of wind. 
#*------------------------

If (wspd >= 0) 
   xwind=wspd*cos((270-wdir)*pi/180.)
Else
   xwind = -9999.0
Endif
return(xwind)
}
#**************************************************************************

GetVWnd<-function(wspd,wdir){

#*-----------------------
#* Get y-component of wind
#*------------------------

If (wspd >= 0) 
   ywind=wspd*sin((270-wdir)*pi/180.)
Else
   ywind = -9999.0
Endif
return(ywind)
}

#*************************************************************************

#function GetWSpd(xwind,ywind){
#
#
#"set gxout stat"
#"d mag("xwind","ywind")"
#rec=sublin(result,1)
#check=substr(rec,1,6)
#If (check = "Notice") 
#    rec=sublin(result,9)
#Else
#    rec=sublin(result,8)
#Endif
#val=subwrd(rec,4)
#
#return (val)
#}
#*************************************************************************

#function GetWDir(xwind,ywind){
#
##* Return wind direction given x and y components
#
#"set gxout stat"
#"define theta=270-"_rtd"*atan2("ywind","xwind")"
#"d theta"
#rec=sublin(result,1)
#check=substr(rec,1,6)
#If (check = "Notice") 
#    rec=sublin(result,9)
#Else
#    rec=sublin(result,8)
#Endif
#Dir=subwrd(rec,4)
#
#If (Dir > 360)
#   Dir=Dir-360
#Endif
#
#If (Dir < 0)
#   Dir=360+Dir
#Endif
#
#return(Dir)
#}
##*************************************************************************
#
#function GetXLoc(temp,pres){
#
##*-------------------------------------------------
##* Get x-location on skew-t based on temp, pressure
##*-------------------------------------------------
#
#xloc=(temp-_m1*log10(pres)-_m3)/_m2
#return(xloc)
#}
##*************************************************************************
# 
#function GetTemp(xloc,pres){
#
##*------------------------------------------------- 
##* Return temperature at location given by xloc,pres
##*-------------------------------------------------
#
#tempval=_m1*log10(pres)+_m2*xloc+_m3
#return(tempval)
#}
#**************************************************************************

GetTheta<-function(temp,pres){

#*---------------------------------------------------
#* Calculate theta for a given temperature and pressure
#*---------------------------------------------------

theta=(temp+273.15) * (1000/pres)**0.286 - 273.15
return(theta)
}

#*************************************************************************

GetThet2<-function(temp,dewp,pres){

#*---------------------------------------------------
#* Calculate theta for a given temperature,dewp, and pressure
#*---------------------------------------------------

tempk=273.15+temp
dewpk=273.15+dewp

es=satvap2(temp)
ws=mixratio(es,pres)

mix=10*getrh(temp,dewp,pres)*ws

exponent=0.2854*(1.0-0.00028*mix)
theta= (temp+273.15) * (1000/pres)**exponent - 273.15
return(theta)
}
#*************************************************************************
#
#Thetae<-function(temp,dewp,pres){
#
##*--------------------------------------------------------------
##* Return equiv. pot. temp in Kelvin given temp, dewp in celsius
##*--------------------------------------------------------------
#
#es=satvap2(temp)
#ws=mixratio(es,pres)
#mix=10*getrh(temp,dewp,pres)*ws
#theta=GetThet2(temp,dewp,pres)+273.15
#TLcl=Templcl(temp,dewp)+273.15
#thetae=theta*exp((3.376/TLcl-0.00254)*mix*1.0+0.00081*mix)
#
#return(thetae)
#}
#**************************************************************************
