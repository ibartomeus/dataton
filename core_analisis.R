

#Data collected in 2015 across 16 sites. Each site was visited 7 times. 
#Each time we recorded three data types:
    
#- All interactions in 100 m transects (`transects`)
#- Focal interactions to selected plants (`focal`)
#- Oportunistic new interactions not detected by the last two methods (`out`)

#there are three datasets so far:
    
#__Interactions:__ This include `transect`, `visitation` and `out` data. 
#Data can be used pooled (1469 interactions ; 5706 visits) for 
#general question abouts trait matching or splited by site (`Site_ID`) and `Round`.

int <- read.csv("data/interactions.csv", h = T)
head(int)
nrow(int)
sum(int$Frequency, na.rm = TRUE)

#ojo Genus_sp

#As for now:
#. sp means we only know the genus
#. morpho1,2,3 Unidentified species, but we know is a unique species diferent from anything else.
#. Family NA- means we only know the family. 


#Analysis----

library(bipartite)
library(reshape2)

head(int)

sites <- unique(int$Site_ID)
ntw_m <- data.frame(site = sites, connectance = NA, links_per_species = NA, nestedness = NA, 
                    H2 = NA, weightedNODF = NA, interaction_evenness = NA)
for(i in 1:length(sites)){
    temp <- subset(int, Site_ID == sites[i])
    temp <- droplevels(temp)
    web <- dcast(temp, Plant_gen_sp ~ Pollinator_gen_sp, fun.aggregate = sum, value.var = "Frequency")
    rownames(web) <- web$Plant_gen_sp
    web <- web[,-1]
    ntw_m[i,2:7] <- networklevel(web = web, index = c("connectance", "links per species", "nestedness", 
                                "H2", "weighted NODF", "interaction evenness"))
}

ranks <- data.frame(site = sites, treatment = "all", resolution = "species", connectance = NA, links_per_species = NA, nestedness = NA, 
                    H2 = NA, weightedNODF = NA, interaction_evenness = NA)

for(i in 1:ncol(ntw_m)){
    ranks[,4] <- rank(ntw_m$connectance)
    ranks[,5] <- rank(ntw_m$links_per_species)
    ranks[,6] <- rank(ntw_m$nestedness)
    ranks[,7] <- rank(ntw_m$H2)
    ranks[,8] <- rank(ntw_m$weightedNODF)
    ranks[,9] <- rank(ntw_m$interaction_evenness)
}    


ranks

unique(int$Pollinator_gen_sp)
unique(int$Plant_gen_sp)


#Analysis with transects----

library(bipartite)
library(reshape2)

head(int)

#Subset only with transects
int_trans<-subset(int, subset=(int$Out == "transect"))
#Checking that we subset only transects
unique(int_trans$Out)

sites <- unique(int_trans$Site_ID)
ntw_m <- data.frame(site = sites, connectance = NA, links_per_species = NA, nestedness = NA, 
                    H2 = NA, weightedNODF = NA, interaction_evenness = NA)
for(i in 1:length(sites)){
    temp <- subset(int_trans, Site_ID == sites[i])
    temp <- droplevels(temp)
    web <- dcast(temp, Plant_gen_sp ~ Pollinator_gen_sp, fun.aggregate = sum, value.var = "Frequency")
    rownames(web) <- web$Plant_gen_sp
    web <- web[,-1]
    ntw_m[i,2:7] <- networklevel(web = web, index = c("connectance", "links per species", "nestedness", 
                                                      "H2", "weighted NODF", "interaction evenness"))
}

ranks_trans <- data.frame(site = sites, treatment = "transect", resolution = "species", connectance = NA, links_per_species = NA, nestedness = NA, 
                    H2 = NA, weightedNODF = NA, interaction_evenness = NA)

for(i in 1:ncol(ntw_m)){
    ranks_trans[,4] <- rank(ntw_m$connectance)
    ranks_trans[,5] <- rank(ntw_m$links_per_species)
    ranks_trans[,6] <- rank(ntw_m$nestedness)
    ranks_trans[,7] <- rank(ntw_m$H2)
    ranks_trans[,8] <- rank(ntw_m$weightedNODF)
    ranks_trans[,9] <- rank(ntw_m$interaction_evenness)
}    

ranks_trans

#Analysis with transects + focal----


#Subset with transects and focal
int_transfocal<-subset(int, subset=((int$Out == "transect")|(int$Out == "focal")))
#Checking that we subset only transects
unique(int_transfocal$Out)

sites <- unique(int_transfocal$Site_ID)
ntw_m <- data.frame(site = sites, connectance = NA, links_per_species = NA, nestedness = NA, 
                    H2 = NA, weightedNODF = NA, interaction_evenness = NA)
for(i in 1:length(sites)){
    temp <- subset(int_transfocal, Site_ID == sites[i])
    temp <- droplevels(temp)
    web <- dcast(temp, Plant_gen_sp ~ Pollinator_gen_sp, fun.aggregate = sum, value.var = "Frequency")
    rownames(web) <- web$Plant_gen_sp
    web <- web[,-1]
    ntw_m[i,2:7] <- networklevel(web = web, index = c("connectance", "links per species", "nestedness", 
                                                      "H2", "weighted NODF", "interaction evenness"))
}

ranks_transfocal <- data.frame(site = sites, treatment = "transect & focal", resolution = "species", connectance = NA, links_per_species = NA, nestedness = NA, 
                          H2 = NA, weightedNODF = NA, interaction_evenness = NA)

for(i in 1:ncol(ntw_m)){
    ranks_transfocal[,4] <- rank(ntw_m$connectance)
    ranks_transfocal[,5] <- rank(ntw_m$links_per_species)
    ranks_transfocal[,6] <- rank(ntw_m$nestedness)
    ranks_transfocal[,7] <- rank(ntw_m$H2)
    ranks_transfocal[,8] <- rank(ntw_m$weightedNODF)
    ranks_transfocal[,9] <- rank(ntw_m$interaction_evenness)
}    

ranks_transfocal

#bind the three treatments
ranking_species<-rbind(ranks, ranks_trans, ranks_transfocal)

#plot

ranking_species

connectance <- matrix(data=NA, nrow=(length(levels(int$Site_ID))), ncol = length(levels(int$Out)))
connectance[,1]<- ranks_trans$connectance
connectance[,2]<- ranks_transfocal$connectance
connectance[,3]<- ranks$connectance

colnames(connectance) <- c("transect","transfocal","all")
row.names(connectance) <-levels(int$Site_ID)

dotplot(connectance)







unique(int$Pollinator_gen_sp)
unique(int$Plant_gen_sp)

#assumptions to test:

###pollinators
#species: species level excluding 
#species + morphospecies: 
#species + morphospecies + genus sp: 
#species + morphospecies + genus sp +  family: 
#morphospecies level (need Curro translation)
#genus level:
#excluding males:



#transects
#trasects + focal
#transects + focal + out


