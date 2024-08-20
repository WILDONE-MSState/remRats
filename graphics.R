oad all funs as if it were a package

source('R/remSim.R')
source('R/genHistRMark.R')
source('R/removeMLE.R')
source('R/recRemMLE.R')
source('R/evalPerf.R')



## figure making
N = 500
p = .1
j = 15

##

########################################################################################

## THIS IS THE SIMULATIUON PART ## quizas tenga que hacer con 4
## decimales?  ##I, tengo que repetir lsa simulacion con mas decimales
## para lograr mejor aspecto visual de los plots p.vect <-
## round(runif(5000, .1,.9), 3)

## rec1 <- list()
## outs1 <- list()

## for(i in seq_along(p.vect)){
##     pob1<-remSim(N=N,p=p.vect[i],j=j, max.capt = N)
##     ## si maxcapt esta ausente, que sea igual al total de la pob
##     simudata<-genHistRMark(pob1)
##     rec <- recRemMLE(simudata, events = 15)
##     res <- rec$Nest
##     res$a <- res$NCatch/N -.9
##     res$c <- res$se/res$estimate
##     res$p <- p.vect[i]
##     res$sim <- i
##     ev <- evalPerf(rec,10, .9)
##     res$b <- res$n.occ/ev[[1]][1]
##     outs1[[i]] <- ev
##     rec1[[i]] <- res
## }

## u <- do.call(rbind, rec1)

## saveRDS(list(rec1,outs1,u, p.vect), 'randomP5000.RDS')

############################################################################################



rec <- readRDS('randomP5000.RDS')

ev <- rec[[2]]
ev <- do.call(cbind, ev)
ev <- t(ev)




u <- rec[[3]]

vect <- rec[[4]]

keep <- which(u$se > 0)

u <- u[keep, ]

u <- u[u$p > .1, ]

library(MASS)

dens1 <- kde2d(u$a, u$b, n = 50)
dens2 <- kde2d(u$c, u$n.occ, n = 50)

dev.new()
plot(u$a + .4, u$b, col = adjustcolor('dodgerblue', .5), pch = 19)

points(c(-3,0), c(1,1), type = 'l')
points(0.3, 1)

## cortar e;l eje y por un valor de 5, y tirar varias lineas de diferentes targets.tres paneles.

abline(h = 1)
abline(v = 1)

plot(u$a, u$b, col = adjustcolor('dodgerblue', .5), pch = 19)


plot(jitter(u$c,2), jitter(u$n.occ, .5))

par(mfrow = c(1,2))

filled.contour(dens1, color.palette = terrain.colors,
               main = "2D Density Contour Plot",
               xlab = "X-axis", ylab = "Y-axis", xlim = c(0,.1))


filled.contour(dens2, color.palette = terrain.colors,
               main = "2D Density Contour Plot",
               xlab = "X-axis", ylab = "Y-axis")



uC <- u[u$estimate < 2000, ]


densN <- kde2d(uC$n.occ, uC$estimate, n = 200)

filled.contour(densN, color.palette = terrain.colors,
               main = "2D Density Contour Plot",
               xlab = "X-axis", ylab = "Y-axis")


plot(u$n.occ, u$estimate, ylim = c(0, 1800), pch = 19, col = adjustcolor('black', .7), cex = 2, xlim = c(0,20))
points(u$n.occ[u$p == .1], u$estimate[u$p == .1],type = 'b', pch = 19, col = 'firebrick', lwd = 2)
points(u$n.occ[u$p == .16], u$estimate[u$p == .16],type = 'b', pch = 19, col = 'green', lwd = 2)
points(u$n.occ[u$p == .20], u$estimate[u$p == .20],type = 'b', pch = 19, col = 'dodgerblue', lwd = 2)


maxN <- 0
minN <- 0
for (i in 2:max(u$n.occ)){
    maxN[i] <- max(u$estimate[u$n.occ == i])
    minN[i] <- min(u$estimate[u$n.occ == i])
}
maxN <- maxN[-1]
minN <- minN[-1]
    
plot(0, 0, col = 'white', xlim = c(0,35), xlab = 'ocassions', ylab = 'N est', ylim = c(0, 2000))
polygon(c(2:max(u$n.occ), rev(2:max(u$n.occ))), c(minN,rev(maxN)), col = 'coral')
## tirar al  eje X dividirlo entre el optimo de remrats para cada punto y luego agrupar en colores por capt baja media y media alta.

ev <- as.data.frame(ev)



u <- u[order(u$p), ]

for (i in 1:nrow(u)){
    if (i == 1)
        plot(u$n.occ[i] / ev$daysToAchieve[u$sim[i]], u$estimate[i]/500, ylim = c(0, 5), xlim = c(0,15))
    points(u$n.occ[i] / ev$daysToAchieve[u$sim[i]], u$estimate[i]/500, col = ifelse(u$p[i] < .3, 'red', ifelse(u$p[i] < .6, 'green', 'dodgerblue')))
}
points(c(1,1), c(.5, 1.5), type = 'l')

points(c(1,1), c(0,1000), type = 'l', lwd = 5, col = adjustcolor('blue', .4))


ev2 <- ev[keep, ]

points(c(3,35), c(500,500), type = 'l', lty = 2, lwd = 3, col = 'dodgerblue')

points(u$n.occ[u$p == .1], u$estimate[u$p == .1],type = 'b', pch = 19, col = 'firebrick', lwd = 2)
## points(u$n.occ[u$p == .16], u$estimate[u$p == .16],type = 'b', pch = 19, col = 'green', lwd = 2)
## points(u$n.occ[u$p == .20], u$estimate[u$p == .20],type = 'b', pch = 19, col = 'dodgerblue', lwd = 2)



plot(u$n.occ, u$se, ylim = c(0, 1800), pch = 19, col = adjustcolor('black', .7), cex = 2, xlim = c(0,20))
points(u$n.occ[u$p == .1], u$se[u$p == .1],type = 'b', pch = 19, col = 'firebrick', lwd = 2)
points(u$n.occ[u$p == .16], u$se[u$p == .16],type = 'b', pch = 19, col = 'green', lwd = 2)
points(u$n.occ[u$p == .20], u$se[u$p == .20],type = 'b', pch = 19, col = 'dodgerblue', lwd = 2)


maxSE <- 0
minSE <- 0
for (i in 2:max(u$n.occ)){
    maxSE[i] <- max(u$se[u$n.occ == i])
    minSE[i] <- min(u$se[u$n.occ == i])
}
maxSE <- maxSE[-1]
minSE <- minSE[-1]

plot(0, 0, col = 'white', ylim = c(0,2000), xlim = c(0,35))
polygon(c(2:max(u$n.occ), rev(2:max(u$n.occ))), c(minSE,rev(maxSE)), col = 'blue')





plot(u$n.occ, u$ucl - u$lcl, ylim = c(0, 1800), pch = 19, col = adjustcolor('black', .7), cex = 2)
points(u$n.occ[u$p == .1], u$ucl[u$p == .1] - u$lcl[u$p == .1],type = 'b', pch = 19, col = 'firebrick', lwd = 2)
points(u$n.occ[u$p == .16], u$ucl[u$p == .16] - u$lcl[u$p == .16],type = 'b', pch = 19, col = 'green', lwd = 2)
points(u$n.occ[u$p == .20], u$ucl[u$p == .16] - u$lcl[u$p == .20],type = 'b', pch = 19, col = 'dodgerblue', lwd = 2)



plot(rec1[[1]]$ucl, ylim = c(0, 2000), type = 'l', xlim = c(1,10))
points(rec1[[1]]$lcl, ylim = c(0, 1000), type = 'l')
points(rec1[[11]]$ucl, ylim = c(100, 2000), type = 'l')
points(rec1[[11]]$lcl, ylim = c(100, 2000), type = 'l')
points(rec1[[21]]$ucl, ylim = c(100, 2000), type = 'l')
points(rec1[[21]]$lcl, ylim = c(100, 2000), type = 'l')
abline(h = 500, col = 'blue', lwd = 3)

## rec2 <- list()
## outs2 <- list()
## for(i in seq_along(vect)){
##     pob1<-remSim(N=N,p=vect[i],j=j, max.capt = N)
##     ## si maxcapt esta ausente, que sea igual al total de la pob
##     simudata<-genHistRMark(pob1)
##     rec <- recRemMLE(simudata, events = 35)
##     res <- rec$Nest
##     rec2[[i]] <- res
##     ev <- evalPerf(rec,10, .9)
##     outs2[[i]] <- ev
##     if (i == 1)
##         plot( res$se/res$estimate,ev[[1]][1]/res$n.occ, type = 'b', pch = 19, xlim = c(0,5))
##     points( res$se/res$estimate,ev[[1]][1]/res$n.occ, type = 'b', pch = 19, col = 'dodgerblue')
## }


