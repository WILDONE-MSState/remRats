#SCRIPT DESARROLLADO PARA EVALUAR TECNICAS DE MUESTREO EN LARVAS DE ANFIBIOS#


## Tengo que andar con tuneos para permitir que la capturabilidad varie temporalmente, d emanera aleatoria (via rnorm) o que sea densodependiente.....


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#                        funcion REMOVE.SIM                                 #
#                                                                           #
#                                                                           #
# funcion: simula los resultados de procesos de remocion de organismos      #
# parametros:                                                               #
#     N: Population size                                                    #
#     p: proability of being catched for any individual                     #
#     j: sampling effort (number of sampling events if standarized          #
#                                                                           #
#                                                                           #
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param N 
##' @param p 
##' @param j 
##' @return 
##' @author Fernando Arce
remove.sim<-function(N,p,j){                                                #
    Capt<-numeric(j)                                                        #
    for (i in 1:j){                                                         #
        Capt[i]<-sum(rbinom(N-sum(Capt),1,prob=p))                          #
    }                                                                       #
    return(Capt)                                                            #
}                                                                           #
#                                                                           #
## tengo qu extender la funcion para que pare cuando se muestreen todos, el resultado
## sea cero en las demas visitas
# resultado: un vector de longitud "j" con las capturas de cada evento      #
#                                                                           #
#                                                                           #
# ejemplo: pob1<-remove.sim(N=100,p=.3,j=5)                                 #
#                                                                           #
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##






##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#                             funcion  ESTIMA                               #
#                                                                           #
#                                                                           #
# funcion: analiza datos de remocion de ejemplares segun el estimador de    #
#          MORAN-ZIPPIN                                                     #
# parametros:                                                               #
#     n1: capturas del primer evento                                        #
#     n2: capturas del segundo evento                                       #
#                                                                           #
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param n1 
##' @param n2 
##' @return 
##' @author Fernando Arce
estima<-function(n1,n2){                                                    #
  N.pob<- (n1^2) /(n1-n2)                                                   #
  Sd.pob<- (n1*n2*sqrt(n1+n2)) / (n1-n2)^2                                  #
  lcl<-N.pob-1.96*Sd.pob                                                    #
  ucl<-N.pob+1.96*Sd.pob                                                    #
  rango.estima<-ucl-lcl                                                     #
  a<-data.frame(N.pob,Sd.pob,lcl,ucl,rango.estima)                          #
  return(a)                                                                 #
}                                                                           #
#                                                                           #
#resultado: un data.frame conteniendo los siguientes valores:               #
#    N.pob: Estima puntual del tamaño poblacional                          #
#    Sd.pob: Desviacion estandar de la estima poblacional                   #
#    lcl: limite inferior del intervalo de confianza al 95%                 #
#    ucl: limite superior del intervalo de confianza al 95%                 #
#    rango.estima: rango del intervalo de confianza estimado                #
#                                                                           #
#                                                                           #
# ejemplo: est.pop1<-estima(35,21)                                          #
# ejemplo enlazado: est.pop1<-estima(pob1[1],pob1[2])                       #
#                                                                           #
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##






##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#                            funcion  HIST.GEN                              #
#                                                                           #
#                                                                           #
# funcion: generar un historial analizable por el RMark a partir de los     #
#          resultados de la funcion REMOVE.SIM                              #
#                                                                           #
# parametros:                                                               #
#     j: numero de jornadas que se desean tener en cuenta                   #
#     b: vector de capturas por jornada                                     #
#                                                                           #
# j tiene que ser menor que la longitud de b                                #
# b puede generarse mediante la funcion REMOVE.SIM o a mano                 #
#                                                                           #
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param j 
##' @param b 
##' @return 
##' @author Fernando Arce
hist.gen<-function(j,b){                                                    #
indi<-numeric(1)                                                            #
for (i in 1:j){                                                             #
  indi=indi+b[i]                                                            #
}                                                                           #
ali<-matrix(nrow=indi,ncol=j)                                               #
ba<-cumsum(b)                                                               #
ali[1:b[1],1]=rep(1,b[1]) #la primera jornada va fuera del lazo             #
for (i in 2:j){                                                             #
  if (b[i]>0){                                                              #
    ali[(ba[i-1]+1):ba[i],i]=rep(1,b[i])                                    #
}                                                                           #
}                                                                           #
ali[is.na(ali)]<-0                                                          #
ch<-apply(ali,1,paste,collapse="")                                          #
ind<-rep(1,length(ch))                                                      #
gen.sim<-data.frame(ch,ind)                                                 #
gen.sim$ch<-as.character(gen.sim$ch)                                        #
return(gen.sim)                                                             #
}                                                                           #
#                                                                           #
# Resultado: un data.frame compuesto por:                                   #
#     ch: historial individul                                               #
#     ind: numero de individuos por cada historial                          #
#                                                                           #
# ejemplo enlazado: simudata<-hist.gen(5,pob1)                              #
#                                                                           #
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##



MarkPath="~/HOME-INST"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          loading  RMark                                   #
library(RMark)                                                              #
#                                                                           #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#                       funcion REMOVE.MLE                                  #
#                                                                           #
#                                                                           #
# funcion: analizar los datos con Rmark y extraer los resultados            #
#                                                                           #
# parametros:                                                               #
#     gen.sim: resultado obtenido con la funcion HIST.GEN                   #
#                                                                           #
remove.mle<-function(gen.sim){                                              #
c.1=list(formula=~1,fixed=0)                                                #
ren<-mark(gen.sim,model="Closed",model.parameters=list(c=c.1),delete=TRUE)  #
N.est<-ren$results$real[2,1]                                                #
Sd.est<-ren$results$real[2,2]                                               #
lcl.est<-ren$results$real[2,3]                                              #
ucl.est<-ren$results$real[2,4]                                              #
rango.est<-ucl.est-lcl.est                                                  #
resumen<-data.frame(N.est,Sd.est,lcl.est,ucl.est,rango.est)                 #
return(resumen)                                                             #
}                                                                           #
#                                                                           #
# ejemplo enlazado: pop.res1<-remove.mle(simudata)                          #
#                                                                           #
#                                                                           #
#                                                                           #
#                                                                           #
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##



pob1<-remove.sim(N=100,p=.3,j=5)
simudata<-hist.gen(5,pob1) 
pop.res1<-remove.mle(simudata)

mark(simudata)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#                             funcion COMP.EST                              #
#                                                                           #
#                   parametros para el analisis                             #
#                                                                           #
N<-400         #poblacion real                                              #
p<-.23         #capturabilidad                                              #
j<-5           #numero de jornadas                                          #
nsim=100       #numero de simulaciones                                      #
#                                                                           #
#                                                                           #
comp.est<-function(N=N,p=p,j=j,nsim=nsim){                                  #
#                                                                           #
#                   Estimas del tamaño poblacional                          #
N.z<-numeric(nsim)                                                          #
N.m1<-numeric(nsim)                                                         #
N.m2<-numeric(nsim)                                                         #
N.m3<-numeric(nsim)                                                         #
N.m4<-numeric(nsim)                                                         #
#                      estimas del limite inferior del IC                   #
lcl.z<-numeric(nsim)                                                        #
lcl.m1<-numeric(nsim)                                                       #
lcl.m2<-numeric(nsim)                                                       #
lcl.m3<-numeric(nsim)                                                       #
lcl.m4<-numeric(nsim)                                                       #
#                      estimas del limite superior del IC                   #
ucl.z<-numeric(nsim)                                                        #
ucl.m1<-numeric(nsim)                                                       #
ucl.m2<-numeric(nsim)                                                       #
ucl.m3<-numeric(nsim)                                                       #
ucl.m4<-numeric(nsim)                                                       #
#                                                                           #
#                                LAZO                                       #
#                                                                           #
for (i in 1:nsim){                                                          #
b<-remove.sim(N=N,p=p,j=j)                                                  #
#                                                                           #
gen.sim2<-hist.gen(j=2,b=b)                                                 #
gen.sim3<-hist.gen(j=3,b=b)                                                 #
gen.sim4<-hist.gen(j=4,b=b)                                                 #
gen.sim5<-hist.gen(j=5,b=b)                                                 #
estmark5<-est.mle<-remove.mle(gen.sim5)                                     #
estmark4<-est.mle<-remove.mle(gen.sim4)                                     #
estmark3<-est.mle<-remove.mle(gen.sim3)                                     #
estmark2<-est.mle<-remove.mle(gen.sim2)                                     #
zippi<-estima(b[1],b[2])                                                    #
#                                                                           #
N.z[i]=as.numeric(floor(zippi[1]))                                          #
N.m1[i]=as.numeric(floor(estmark2[1]))                                      #
N.m2[i]=as.numeric(floor(estmark3[1]))                                      #
N.m3[i]=as.numeric(floor(estmark4[1]))                                      #
N.m4[i]=as.numeric(floor(estmark5[1]))                                      #
#                                                                           #
#                                                                           #
lcl.z[i]=    as.numeric(floor(zippi[3]))                                    #
lcl.m1[i]=as.numeric(floor(estmark2[3]))                                    #
lcl.m2[i]=as.numeric(floor(estmark3[3]))                                    #
lcl.m3[i]=as.numeric(floor(estmark4[3]))                                    #
lcl.m4[i]=as.numeric(floor(estmark5[3]))                                    #
#                                                                           #
ucl.z[i]=    as.numeric(floor(zippi[4]))                                    #
ucl.m1[i]=as.numeric(floor(estmark2[4]))                                    #
ucl.m2[i]=as.numeric(floor(estmark3[4]))                                    #
ucl.m3[i]=as.numeric(floor(estmark4[4]))                                    #
ucl.m4[i]=as.numeric(floor(estmark5[4]))                                    #
#                                                                           #
}                                                                           #
#                                                                           #
#                                                                           #
#                                                                           #
esso<-data.frame(                                                           #
                 N.z,                                                       #
                 N.m2,                                                      #
                 N.m3,                                                      #
                 N.m4,                                                      #
                 lcl.z,                                                     #
                 lcl.m2,                                                    #
                 lcl.m3,                                                    #
                 lcl.m4,                                                    #
                 ucl.z,                                                     #
                 ucl.m2,                                                    #
                 ucl.m3,                                                    #
                 ucl.m4                                                     #
                 )                                                          #
return(subset(esso,esso$N.z>0 & esso$N.z!=Inf))                             #
}                                                                           #
#                                                                           #
#                                                                           #
#                                                                           #
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#                   lazo comparacion esfuerzo                               #
pep<-seq(.1,.9,.01)                                                         #
n.z<-   numeric(length(pep))                                                #
n.m2<-  numeric(length(pep))                                                #
n.m3<-  numeric(length(pep))                                                #
n.m4<-  numeric(length(pep))                                                #
lcl.z<- numeric(length(pep))                                                #
lcl.m2<-numeric(length(pep))                                                #
lcl.m3<-numeric(length(pep))                                                #
lcl.m4<-numeric(length(pep))                                                #
ucl.z<- numeric(length(pep))                                                #
ucl.m2<-numeric(length(pep))                                                #
ucl.m3<-numeric(length(pep))                                                #
ucl.m4<-numeric(length(pep))                                                #
#                                                                           #
for(s in 1:length(pep)){                                                    #
  pinn=comp.est(N=N,p=pep[s],j=10,nsim=100)                                 #
  n.z[s]=    mean(pinn[,1])                                                 #
  n.m2[s]=   mean(pinn[,2])                                                 #
  n.m3[s]=   mean(pinn[,3])                                                 #
  n.m4[s]=   mean(pinn[,4])                                                 #
  lcl.z[s]=  mean(pinn[,5])                                                 #
  lcl.m2[s]= mean(pinn[,6])                                                 #
  lcl.m3[s]= mean(pinn[,7])                                                 #
  lcl.m4[s]= mean(pinn[,8])                                                 #
  ucl.z[s]=  mean(pinn[,9])                                                 #
  ucl.m2[s]=mean(pinn[,10])                                                 #
  ucl.m3[s]=mean(pinn[,11])                                                 #
  ucl.m4[s]=mean(pinn[,12])                                                 #
}                                                                           #
#                                                                           #
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#                                                                           #
##       grafico para mostrar la evolucion del intervalo de confianza      ##
#                                                                           #
pdf("cuatro.pdf")                                                           #
par(mfrow=c(2,2))                                                           #
plot(n.z,                                                                   #
     ylim=c(0,1000),                                                        #
     type="l",                                                              #
     bty="n",                                                               #
     xlab="Capture probability (p)",                                        #
     ylab="Pop estimate (N-hat)",                                           #
     main="2 catching events")                                              #
pos<-seq(1,length(n.z),1)                                                   #
segments(pos,n.z,pos,lcl.z,col="grey",lwd=4.5)                              #
segments(pos,n.z,pos,ucl.z,col="grey",lwd=4.5)                              #
#points(n.z,type="l")                                                       #
abline(h=N,col="green")                                                     #
abline(h=(N+N/4),col="red")                                                 #
abline(h=(N-N/4),col="red")                                                 #
#                                                                           #
plot(n.m2,                                                                  #
     ylim=c(0,1000),                                                        #
     type="l",                                                              #
     bty="n",                                                               #
     xlab="Capture probability (p)",                                        #
     ylab="Pop size estimate (N-hat)",                                      #
     main="3 catching events")                                              #
pos<-seq(1,length(n.m2),1)                                                  #
segments(pos,n.m2,pos,lcl.m2,col="grey",lwd=4.5)                            #
segments(pos,n.m2,pos,ucl.m2,col="grey",lwd=4.5)                            #
#points(n.m2,type="l")                                                      #
abline(h=N,col="green")                                                     #
abline(h=(N+N/4),col="red")                                                 #
abline(h=(N-N/4),col="red")                                                 #
#                                                                           #
plot(n.m3,                                                                  #
     ylim=c(0,1000),                                                        #
     type="l",                                                              #
     bty="n",                                                               #
     xlab="Capture probability (p)",                                        #
     ylab="Pop size estimate (N-hat)",                                      #
     main="4 catching events")                                              #
pos<-seq(1,length(n.m3),1)                                                  #
segments(pos,n.m3,pos,lcl.m3,col="grey",lwd=4.5)                            #
segments(pos,n.m3,pos,ucl.m3,col="grey",lwd=4.5)                            #
#points(n.m3,type="l")                                                      #
abline(h=N,col="green")                                                     #
abline(h=(N+N/4),col="red")                                                 #
abline(h=(N-N/4),col="red")                                                 #
#                                                                           #
plot(n.m4,                                                                  #
     ylim=c(0,1000),                                                        #
     type="l",                                                              #
     bty="n",                                                               #
     xlab="Capture probability (p)",                                        #
     ylab="Pop size estimate (N-hat)",                                      #
     main="5 catching events")                                              #
pos<-seq(1,length(n.m4),1)                                                  #
segments(pos,n.m4,pos,lcl.m4,col="grey",lwd=4.5)                            #
segments(pos,n.m4,pos,ucl.m4,col="grey",lwd=4.5)                            #
#points(n.m4,type="l")                                                      #
abline(h=N,col="green")                                                     #
abline(h=(N+N/4),col="red")                                                 #
abline(h=(N-N/4),col="red")                                                 #
dev.off()                                                                   #
par(mfrow=c(1,1))                                                           #
#                                                                           #
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#                                                                           #
#                GRAFICA DE EXITOS ZIPPIN                                   #
#
N=100
p=.45
j=2
nsim=100
zippi.3<-function(N=N,p=p,j=j,nsim=nsim){
N.zip<-numeric(nsim)
Sd.zip<-numeric(nsim)
lcl.zip<-numeric(nsim)
ucl.zip<-numeric(nsim)
C.1<-numeric(nsim)
C.2<-numeric(nsim)
for (i in 1:nsim){
  b<-remove.sim(N=N,p=p,j=j)## poblacion simulada
  zippi<-estima(b[1],b[2]) #estima zippin
  N.zip[i]=as.numeric(floor(zippi[1]))
  Sd.zip[i]=as.numeric(round(zippi[2],2))
  lcl.zip[i]=as.numeric(floor(zippi[3]))
  ucl.zip[i]=as.numeric(floor(zippi[4]))
  C.1[i]=as.numeric(b[1])
  C.2[i]=as.numeric(b[2])
}
resti<-data.frame(N.zip,Sd.zip,lcl.zip,ucl.zip,C.1,C.2,N=rep(N,nsim),P=rep(p,nsim))
#Con el primero lo voy desarrollando ya, para la extraccion de datos
return(length(resti$N.zip[resti$N.zip>0 & resti$N.zip!=Inf & resti$ucl.zip>=N & resti$lcl.zip<=N]))
}
zippi.3(N=N,p=p,j=j,nsim=nsim)


length(cap)

sim.Nc<-function(N,p,j,nsim){  #funcion para generar en cada punto todo lo necesario para p=cte
  cap<-seq(10,5000,10)
ress<-numeric(length(cap))
for (i in 1:length(cap)){
  zi=zippi.3(N=cap[i],p=p,j=j,nsim=nsim)
  ress[i]=as.numeric(zi)
}
  return(ress)
}

pp<-seq(0.10,.9,.01)

py<-numeric()
Ny<-numeric()
by<-numeric()
for (i in 1:length(pp)){
  a<-sim.Nc(N=N,p=pp[i],j=j,nsim=nsim)
  py<-c(py,rep(pp[i],length(cap)))
  Ny<-c(Ny,cap)
  by<-c(by,a)
}

sst<-cbind(Ny,by,py)
sst<-data.frame(sst)

##nueva escala de puntos
st1<-     subset(sst,by>=0 & by<5,select=c(py,Ny))  #
st2<-    subset(sst,by>=5 & by<10,select=c(py,Ny))  #
st3<-   subset(sst,by>=10 & by<15,select=c(py,Ny))  #
st4<-   subset(sst,by>=15 & by<20,select=c(py,Ny))  #
st5<-   subset(sst,by>=20 & by<25,select=c(py,Ny))  #
st6<-   subset(sst,by>=25 & by<30,select=c(py,Ny))  #
st7<-   subset(sst,by>=30 & by<35,select=c(py,Ny))  #
st8<-   subset(sst,by>=35 & by<40,select=c(py,Ny))  #
st9<-   subset(sst,by>=40 & by<45,select=c(py,Ny))  #
st10<-  subset(sst,by>=45 & by<50,select=c(py,Ny))  #
st11<-  subset(sst,by>=50 & by<55,select=c(py,Ny))  #
st12<-  subset(sst,by>=55 & by<60,select=c(py,Ny))  #
st13<-  subset(sst,by>=60 & by<65,select=c(py,Ny))  #
st14<-  subset(sst,by>=65 & by<70,select=c(py,Ny))  #
st15<-  subset(sst,by>=70 & by<75,select=c(py,Ny))  #
st16<-  subset(sst,by>=75 & by<80,select=c(py,Ny))  #
st17<-  subset(sst,by>=80 & by<85,select=c(py,Ny))  #
st18<-  subset(sst,by>=85 & by<90,select=c(py,Ny))  #
st19<-  subset(sst,by>=90 & by<95,select=c(py,Ny))  #
st20<-subset(sst,by>=95 & by<=100,select=c(py,Ny))

pdf()
plot(st20,
     xlim=c(.1,.5),
     ylim=c(0,1500),
     col="gray0",
     pch=15,bty="n",
     xlab="Capturabilidad (p)",
     ylab="Tamaño poblacional (N)",
     cex=1.78,cexlab=2)
points(st19,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray5",pch=15,bty="n",cex=1.78)
points(st18,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray10",pch=15,bty="n",cex=1.78)
points(st17,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray15",pch=15,bty="n",cex=1.78)
points(st16,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray20",pch=15,bty="n",cex=1.78)
points(st15,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray25",pch=15,bty="n",cex=1.78)
points(st14,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray30",pch=15,bty="n",cex=1.78)
points(st13,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray35",pch=15,bty="n",cex=1.78)
points(st12,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray40",pch=15,bty="n",cex=1.78)
points(st11,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray45",pch=15,bty="n",cex=1.78)
points(st10,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray50",pch=15,bty="n",cex=1.78)
points(st9,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray55",pch=15,bty="n",cex=1.78)
points(st8,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray60",pch=15,bty="n",cex=1.78)
points(st7,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray65",pch=15,bty="n",cex=1.78)
points(st6,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray70",pch=15,bty="n",cex=1.78)
points(st5,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray75",pch=15,bty="n",cex=1.78)
points(st4,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray80",pch=15,bty="n",cex=1.78)
points(st3,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray85",pch=15,bty="n",cex=1.78)
points(st2,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray90",pch=15,bty="n",cex=1.78)
points(st1,xlim=c(.1,.5),ylim=c(0,1500),
       col="gray95",pch=15,bty="n",cex=1.78)
dev.off()






















q()


















## condiciones logicas
# resti$N.zip>0  el valor N es positivo
# resti$N.zip!=Inf el valor N es real
# resti$ucl.zip>N el intervalo de confianza cubre el valor real de la poblacion



#ahora la simulacion

a<-numeric(1000)  #diferencia entre la primera y segunda jornada (si es cero se capturan los mismos, si es neg se capturan mas en la segunda)
B<-numeric(1000)

for (i in 1:length(a)) {
  b=remove.sim(N=N,c=c,j=j) #simulacion de un experimento de captura sin reemplazamiento
  a[i]=b[1]-b[2]            #diferencia entrelas capturas de la primera y segunda jornada
  es=estima(b[1],b[2])      #analisis segun el metodo propuesto en el boletin
  B[i]=es$lci<N & es$uci>N  #probabilidad de que el intervalo de conf calculado cubra el tamaño poblacional real
  ###hay que incluir el analisis con RMark con 2, 3 y 4 muestreos 1º paso generar historiales
}
mean(a<=0)  #probabilidad de que los datos no permitan el analisis
table(B)
mean(B, na.rm=TRUE) #probabilidad de que el intervalo estimado cubra el valor real de la poblacion
#rm(list=ls())


##vamos a ir empaquetando las funciones que se van creando:
##parametros necesarios de la funcion:
#n1 en la funcion simulador ya esta incluida, va implicto
#n2

#generar historiales para MARK de momento con 2 vamos generalizando hacia 3 o 4

#dos vectores clave:
#b es el numero de capturas de cada jornada
#ba es el numero acumulado de capturas
j=4


##GENERECION DE LA POBLACION

N<-100  #poblacion real
p<-.1   #capturabilidad
j<-15    #numero de jornadas
nsim=1000 #numero de simulaciones



###esto hay que ordenarlo bastante mejor
#ahora la simulacion

a<-numeric(1000)  #diferencia entre la primera y segunda jornada (si es cero se capturan los mismos, si es neg se capturan mas en la segunda)
B<-numeric(1000)

for (i in 1:length(a)) {
  b=remove.sim(N=N,p=p,j=j) #simulacion de un experimento de captura sin reemplazamiento
  a[i]=b[1]-b[2]            #diferencia entrelas capturas de la primera y segunda jornada
  es=estima(b[1],b[2])      #analisis segun el metodo propuesto en el boletin
  B[i]=es$lci<N & es$uci>N  #probabilidad de que el intervalo de conf calculado cubra el tamaño poblacional real
}
mean(a<=0)  #probabilidad de que los datos no permitan el analisis
table(B)
mean(B, na.rm=TRUE) #probabilidad de que el intervalo estimado cubra el valor real de la poblacion
#rm(list=ls())
