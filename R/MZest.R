##' Estimation of population size after two consecutive renmoval
##' sessions using Moran-Zippin estimator
##'
##' Function to estimate the size of a population subject of two consecutive removal sessions
##' @title Moran-Zippin estimator for two consecutive trapping experiments
##' @param n1 numeric. First session catches
##' @param n2 numeric. Second session catches
##' @return data.frame
##' @author Fer Arce
MZest<-function(n1,n2){                                                    #
  N.pob<- (n1^2) /(n1-n2)                                                   #
  Sd.pob<- (n1*n2*sqrt(n1+n2)) / (n1-n2)^2                                  #
  lcl<-N.pob-1.96*Sd.pob                                                    #
  ucl<-N.pob+1.96*Sd.pob                                                    #
  rango.estima<-ucl-lcl                                                     #
  a<-data.frame(N.pob,Sd.pob,lcl,ucl,rango.estima)                          #
  return(a)                                                                 #
} 
