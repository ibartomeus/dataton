#null models test----
int <- read.csv("data/interactions_clean.csv", h = T)

#creo modelo nulo
head(int)
sites <- unique(int$Site_ID)
ntw_m <- data.frame(site = sites, connectance = NA, links_per_species = NA, nestedness = NA, 
                    H2 = NA, weighted_NODF = NA, interaction_evenness = NA)
for(i in 1:length(sites)){
    temp <- subset(int, Site_ID == sites[i])
    temp <- droplevels(temp)
    webs <- dcast(temp, Plant_gen_sp ~ Pollinator_gen_sp, fun.aggregate = sum, value.var = "Frequency")
    rownames(webs) <- web$Plant_gen_sp
    webs <- webs[,-1]
    n = 100
    webnull<-nullmodel(webs, N=n, method = "vaznull")
    ntw_null <- data.frame(connectance = NA, links_per_species = NA, nestedness = NA, 
                           H2 = NA, weighted_NODF = NA, interaction_evenness = NA)
    for(j in 1:n){
        ntw_null[j,] <- networklevel(web = webnull[[j]], 
                                     index = c("connectance", "links per species", "nestedness", 
                                               "H2", "weighted NODF", "interaction evenness"))    
    }
    ntw_lev <- networklevel(web = webs, 
                            index = c("connectance", "links per species", "nestedness", 
                                      "H2", "weighted NODF", "interaction evenness")) 
    sd_ <- apply(ntw_null, MARGIN = 2, FUN = sd)
    #deviance in 5% of randomizations, which fill an extra cell. Why? No idea
    #hist(ntw_null$connectance)
    #webnull[[49]]
    #webs
    #length(which(webs > 0))
    #length(which(webnull[[1]] > 0))
    #length(which(webnull[[49]] > 0))
    ############
    sd_ <- sd_[gsub(pattern = " ", replacement = "_", names(ntw_lev))]
    sig <- ( ntw_lev - colMeans(ntw_null) ) / sd_
    ntw_m[i,2:(length(sig)+1)] <- sig
    #saavedra method
    N <- networklevel(web = webs, index = "nestedness")
    M <- dim(web)[1]+dim[2]
    C <- networklevel(web = webs, index = "connectance")
    # Nmax <- function... serguei.
    Nmax <- N
    N_ <- N/Nmax
    N_cor <- N_/C*log(M, base = 10) 
    #need to add... 
}
ntw_m