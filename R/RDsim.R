## let's simulate

RDsim <- function(inds, primary, secondary){
    ## state process
inds.each <- 100
prim.occ <- 10
prim.mat <- matrix(0, nrow = inds.each*prim.occ, ncol = prim.occ)
prim.mat[,1] <- 1

for(i in 1:nrow(prim.mat)){
    for( j in 2:ncol(prim.mat)){
        if (prim.mat[i, (j-1)] == 0)
            next
        prim.mat[i,j] <- rbinom(1,1,.7)       
    }
}


head(prim.mat)

## now the presence at each primry occasion

inds.each <- 100
prim.occ <- 10
prim.pres <- matrix(0, nrow = inds.each*prim.occ, ncol = prim.occ)

prim.pres[,1] <- rbinom(1000,1, .4)


for(i in 1:nrow(prim.pres)){
    for( j in 2:ncol(prim.pres)){
        if(prim.mat[i,(j-1)] == 0) next
        if (prim.pres[i, (j-1)] == 0){
            prim.pres[i,j] <- rbinom(1,1,.3)
        } else {
            prim.pres[i,j] <- rbinom(1,1,.7)
        }
    }
}


prim.obs <- prim.mat * prim.pres

prim.obs <- prim.obs[rowSums(prim.obs) > 0, ]


## secondary matrix

## observational process

sec.ses <- 4

mat.end <- matrix(0, nrow = nrow(prim.obs), ncol = sec.ses*ncol(prim.obs))
