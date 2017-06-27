#test link prediction

library(devtools)
install_github("traitmatch", "ibartomeus")
require(traitmatch)


#data

int <- read.csv("data/interactions.csv", h = T)
head(int)
pol_tr <- read.csv("data/traits_pollinators.csv", h = T)
head(pol_tr)
plant_tr <- read.csv("data/traits_plants.csv", h = T)
head(plant_tr)
flw <- read.csv("data/flowers.csv", h = T)
head(flw)

head(int)
unique(int[,3:4])
unique(int$Plant_gen_sp[which(!int$Plant_gen_sp %in% plant_tr$Plant_gen_sp)]) #ok
unique(int$Pollinator_gen_sp[which(!int$Pollinator_gen_sp %in% pol_tr$Pollinator_gen_sp)])
temp <- merge(int, pol_tr, by.x = "Pollinator_gen_sp", by.y = "Pollinator_gen_sp", all.x = TRUE)
temp2 <- merge(temp, plant_tr, by.x = "Plant_gen_sp", by.y = "Plant_gen_sp", all.x = TRUE)
pooled_data <- temp2
head(pooled_data, 20)

mt <- 100 #Define max.time to 60 sec to run things fast. Set to minimum 900 for a decent estimation of parameters.

dat_no_bt <- pooled_data[-which(pooled_data$tongue_length > 20),]
scatter.smooth(pooled_data$nectar_tube_depth_mm ~ pooled_data$tongue_length)
scatter.smooth(dat_no_bt$nectar_tube_depth_mm ~ dat_no_bt$tongue_length)

pars_pre <- fit_it(integrated_model, 
                   Tlevel1 = pooled_data$nectar_tube_depth_mm,  
                   Tlevel2 = pooled_data$tongue_length,
                   mean_Tlevel1 = mean(pooled_data$nectar_tube_depth_mm),
                   sd_Tlevel1 = sd(pooled_data$nectar_tube_depth_mm),
                   pars = c(a0 = 0, a1 = 0, b0 = 0, b1 = 0),
                   par_lo = c(a0 = -20, a1 = 0, b0 = -10, b1 = -10),
                   par_hi = c(a0 = 10, a1 = 10, b0 = 20, b1 = 10),
                   max.time = mt)
pars_pre 
# With this model, the first estimate is the intercept and second number the slope.
plot_pred(pars = pars_pre, Tlevel1 = pooled_data$nectar_tube_depth_mm, 
          Tlevel2 = pooled_data$tongue_length, xlab = "tongue length", 
          ylab = "nectar depth", pch = ".")

hist(pooled_data$nectar_tube_depth_mm)
hist(pooled_data$tongue_length)

pooled_data[which(pooled_data$tongue_length > 15),] #butterflies

#try log?
pars_pre <- fit_it(integrated_model, 
                   Tlevel1 = pooled_data$nectar_tube_depth_mm,  
                   Tlevel2 = log(pooled_data$tongue_length+0.01),
                   mean_Tlevel1 = mean(pooled_data$nectar_tube_depth_mm),
                   sd_Tlevel1 = sd(pooled_data$nectar_tube_depth_mm),
                   pars = c(a0 = 0, a1 = 0, b0 = 0, b1 = 0),
                   par_lo = c(a0 = -20, a1 = 0, b0 = -10, b1 = -10),
                   par_hi = c(a0 = 10, a1 = 10, b0 = 20, b1 = 10),
                   max.time = mt)
plot_pred(pars = pars_pre, Tlevel1 = pooled_data$nectar_tube_depth_mm, 
          Tlevel2 = log(pooled_data$tongue_length+0.01), xlab = "tongue length", 
          ylab = "nectar depth", pch = ".")

#-bt

pars_pre <- fit_it(integrated_model, 
                   Tlevel1 = dat_no_bt$nectar_tube_depth_mm,  
                   Tlevel2 = dat_no_bt$tongue_length,
                   mean_Tlevel1 = mean(dat_no_bt$nectar_tube_depth_mm),
                   sd_Tlevel1 = sd(dat_no_bt$nectar_tube_depth_mm),
                   pars = c(a0 = 0, a1 = 0, b0 = 0, b1 = 0),
                   par_lo = c(a0 = -20, a1 = 0, b0 = -10, b1 = -10),
                   par_hi = c(a0 = 10, a1 = 10, b0 = 20, b1 = 10),
                   max.time = mt)
pars_pre 
# With this model, the first estimate is the intercept and second number the slope.
plot_pred(pars = pars_pre, Tlevel1 = dat_no_bt$nectar_tube_depth_mm, 
          Tlevel2 = dat_no_bt$tongue_length, xlab = "tongue length", 
          ylab = "nectar depth", pch = ".")

#load predict functions.
predict.niche.prob <- function(pars,Tlevel1, Tlevel2, replicates = 100){
    out <-  list()
    # Optimum and range
    o = pars[1] + pars[2]*Tlevel2
    r = pars[3] + pars[4]*Tlevel2
    
    # Compute the conditional
    pLM = exp(-(o-Tlevel1)^2/2/r^2)
    
    # save outout
    out[[1]] <- pLM
    #out[[2]] <- data.frame(Tlevel1, Tlevel2, pLM)
    for(i in 1:replicates+1){
        out[[i]] = rbinom(length(Tlevel1), size = 1, prob = pLM)
    }
    out
}


residuals.niche.prob <- function(pars,Tlevel1, Tlevel2){
    pred <- predict.niche.prob(pars, Tlevel1, Tlevel2, replicates = 1)[[1]]
    1-pred
}



predict.niche.prob(pars = pars_pre, Tlevel1 = pooled_data$nectar_tube_depth_mm, 
Tlevel2 = pooled_data$tongue_length)

round(residuals.niche.prob(pars = pars_pre, Tlevel1 = pooled_data$nectar_tube_depth_mm, 
                     Tlevel2 = pooled_data$tongue_length),2)


#loop throug sites
sites <- unique(dat_no_bt$Site_ID)
resid <- list()
for(i in 1:16){
    temp <- subset(dat_no_bt, Site_ID == sites[i])
    pred <- predict.niche.prob(pars = pars_pre, Tlevel1 = temp$nectar_tube_depth_mm, 
                       Tlevel2 = temp$tongue_length)
    pred_sum <- pred[[2]]+pred[[3]]+pred[[4]]+pred[[5]] #need to do this sum properly
    #this is interactions ij
    #calculate residuals...
    resid[[i]] <- residuals.niche.prob(pars = pars_pre, 
                                       Tlevel1 = temp$nectar_tube_depth_mm, 
                         Tlevel2 = temp$tongue_length)
}

pred[[1]]

resid

hist(resid[[10]])


