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



write_csv(dataclean, "data/interactions_clean.csv")
