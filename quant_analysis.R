

#Data collected in 2015 across 16 sites. Each site was visited 7 times.
#Each time we recorded three data types:

#- All interactions in 100 m transects (`transects`)
#- Focal interactions to selected plants (`focal`)
#- Oportunistic new interactions not detected by the last two methods (`out`)

#there are three datasets so far:

#__Interactions:__ This include `transect`, `visitation` and `out` data.
#Data can be used pooled (1469 interactions ; 5706 visits) for
#general question abouts trait matching or splited by site (`Site_ID`) and `Round`.

int <- read.csv("data/interactions_clean.csv", header = TRUE)

#ojo Genus_sp

#As for now:
#. sp means we only know the genus
#. morpho1,2,3 Unidentified species, but we know is a unique species diferent from anything else.
#. Family NA- means we only know the family.




calculate_metrics <- function(df = int, treatment) {

  if (treatment == "transects") df <- subset(df, df$Out == "transect")
  if (treatment == "transfocal") df <- subset(df, df$Out == "transect" | df$Out == "focal")
  if (treatment == "all") df = df

  sites <- unique(df$Site_ID)
  ntw_m <- data.frame(site = sites, connectance = NA, links_per_species = NA,
                      nestedness = NA, H2 = NA, weightedNODF = NA,
                      interaction_evenness = NA)
  for (i in 1:length(sites)) {
    temp <- subset(df, Site_ID == sites[i])
    temp <- droplevels(temp)
    web <- reshape2::dcast(temp, Plant_gen_sp ~ Pollinator_gen_sp, fun.aggregate = sum, value.var = "Frequency")
    rownames(web) <- web$Plant_gen_sp
    web <- web[,-1]
    ntw_m[i,2:7] <- bipartite::networklevel(web = web, index = c("connectance", "links per species", "nestedness",
                                                      "H2", "weighted NODF", "interaction evenness"))
  }

  ntw_m$treatment <- treatment

  ntw_m

}


transects <- calculate_metrics(int, treatment = "transects")
transects.focal <- calculate_metrics(int, treatment = "transfocal")
all <- calculate_metrics(int, treatment = "all")

comparison <- dplyr::bind_rows(transects, transects.focal, all)



#####################################

library(ggplot2)

comparison$treatment <- factor(comparison$treatment,
                               levels = c("transects", "transfocal", "all"))

ggplot(comparison, aes(treatment, connectance, group = site, colour = site)) +
  geom_line() +
  geom_point()

 

plot_comparison <- function(metric, df) {
    
    p <- ggplot(df, aes(x = treatment, group = site, colour = site)) +
        aes_string(y = metric) +
        geom_line() +
        geom_point()
    
    print(p)
    
}

plots <- lapply(c("connectance", "links_per_species", "nestedness", "H2", 
         "weightedNODF", "interaction_evenness"), plot_comparison, df = comparison)


###################################################


