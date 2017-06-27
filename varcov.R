#HIger order effects?

int <- read.csv("data/interactions.csv", h = T)
head(int)

#try to get a var/cov network.

#1) per site using round as replicate

head(int)
dat <- subset(int, Site_ID == "Aznalcazar", Out == "transect")
#dat <- subset(int, Out == "transect")

library(reshape2)
head(dat)
dat[which(is.na(dat$Frequency)),"Frequency"] <- 1 #ASK CURRO!
dat2 <- dcast(dat, Pollinator_gen_sp ~ Round, value.var = "Frequency", fun.aggregate = sum)
head(dat2)

dim(mat)
mat <- var(t(dat2[,-1]))
mat[27,] #Apis

#questions
#nest site and split per plant
#add time as continous
#standardize/zero inflate?


#multiplicative question... better integrated in statistical framework like join SDM or HOE's?
# p(int_ij) * Abi * Abj * cov/var of pollinators on plant i 
#[this impli as many multiplying factors as pollinators (but most set to 1?), 
    #PLUS, ignoring the rest of the plant community]
#to solve this you can calculate covariance of i vs all others?

#HOE's:
# Visits ij ~ TMcoef*Abi*Abj + beta*Ai*Aj*Ax + ... #scale up is hard bc beta params are infinite

#JOINT?
# Vij ~ Abi + Abj + Trait matching (con o sin detect) | cov (Ab...) modelando la estructura de error
#models cada celda de la matriz, la estructura de error te modela de distribuciones multivariantes
# que son las HOE's Pollock MEE

#In plants
# fitness i ~ sum(Ab i) + Other environ var's | cov(Abi) 

#En resumen. emergent properties can be measured by:
#- Mechanistic model (Galeano, etc... mecanistic)
#- Mayfield & Stouffer (data hungry)
#- Joint's 
















