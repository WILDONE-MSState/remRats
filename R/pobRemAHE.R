


pob1<-remove.sim(N=100,p=.3,j=5)
simudata<-hist.gen(5,pob1) 
pop.res1<-remove.mle(simudata)
