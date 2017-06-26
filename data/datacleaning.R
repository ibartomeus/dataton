library(readr)
library(dplyr)
source("https://raw.githubusercontent.com/Pakillo/pacotools/master/R/move_columns.R")

datos <- readr::read_csv("data/interactions.csv")

dataclean <- datos

## Fix plant species names
dataclean$Plant_gen_sp <- recode(dataclean$Plant_gen_sp,
                                 "Acacia adealbata" = "Acacia dealbata",
                                 "Mirtus communis" = "Myrtus communis",
                                 "Retama sp" = "Retama monosperma",
                                 "Erodium sp" = "Erodium primulaceum",
                                 "Leontodon sp" = "Leontodon taraxacoides",
                                 "Ranunculus sp" = "Ranunculus tribbus")


## Split species names into genus and species names columns
pos <- regexpr(" ", dataclean$Pollinator_gen_sp, fixed = TRUE)
dataclean$Pollinator_genus <- substr(dataclean$Pollinator_gen_sp, start = 1, stop = pos - 1)
dataclean$Pollinator_species <- substr(dataclean$Pollinator_gen_sp, start = pos + 1, stop = nchar(dataclean$Pollinator_gen_sp))

dataclean <- move_columns(dataclean,
                          who = c("Pollinator_genus", "Pollinator_species"),
                          after = "Pollinator_gen_sp")

#Add morphospecies

morfo <- read.csv("data/morfos.csv")
head(morfo)

sort(table(morfo$Morphospecie))
morfo$Morphospecie2 <- morfo$Morphospecie
for(i in 1:length(levels(morfo$Morphospecie))){
    levels(morfo$Morphospecie2)[i] <- paste("morpho", i, sep = "_") 
}
head(morfo)
sort(table(morfo$Morphospecie2))

#merge
head(dataclean)

dataclean2 <- merge(as.data.frame(dataclean), morfo[,c(1,4)], 
                    by.x = "Pollinator_gen_sp", by.y = "Species", all.x = TRUE)
head(dataclean2)

#maybe recode column order?
write_csv(dataclean, "data/interactions_clean.csv")
