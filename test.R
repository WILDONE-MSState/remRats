source('R/remSim.R')
source('R/genHistRMark.R')
source('R/removeMLE.R')
pob1<-remSim(N=100,p=.3,j=5)
simudata<-genHistRMark(5,pob1)
pop.res1<-removeMLE(simudata)

