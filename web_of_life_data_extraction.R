#ANALYSES FOR METHODOLOGICAL PAPER ON NETWORKS.
#HOW MANY INDS AND SPS ARE IDENTIFIED FULLY, ONLY TO GENUS LEVEL OR TO MORPHOSPECIES??


####WEB OF LIFE DATA
#___________________________________________________________________________________________________________


#___________________________________________________________________________________________________________
TMP=c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014","015", "016", "017",
      "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029", "030", "031", "032", "033",
    "034", "035", "036", "037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048", "049", 
     "050", "051", "052", "053", "054", "055", "056", "057", "058","059", "060_24", "061_30", "062", "063", "064", 
    "065", "066", "067", "068", "069_01", "069_02", "069_03", "070", "071")
#CREATE TABLE TO POPULATE
out <- data.frame(weboflife.ref=NA, Reference=NA, Journal=NA, Title=NA, network_type=NA, level=NA, inds_fully_resolved=NA, 
                  inds_to_genus=NA, inds_to_morphosps=NA, sps_fully_resolved=NA, sps_to_genus=NA, sps_to_morphosps=NA)


  for(j in 1:length(TMP)){ 
      setwd("/Volumes/New Volume/bartomeus lab paper/dataton/data/web_of_life_data")
   

  d<-read.csv(file=(paste("M_PL_",TMP[j],"-species.csv", sep="")))
 

  d.poll<-subset(d, d$Role=="Pollinator")
  sum(d.poll$Degree) 
  nrow(d.poll) 
  #select only not fully identified species
  selectedRows <- d[grep(paste("M_PL_"), d$Specie), ]
  nrow(selectedRows) 
  
  #pollinators identified to morphospecies only
  d.poll2<-subset(selectedRows, selectedRows$Role=="Pollinator")
  d.poll.gen<-d.poll2[grep("Unidentified", d.poll2$Specie), ]
  
  sum(d.poll.gen$Degree) 
  nrow(d.poll.gen) 
  
  sum(d.poll2$Degree)-sum(d.poll.gen$Degree)
  
  out[j,1]<-TMP[j]
  out[j,8]<-(sum(d.poll2$Degree)-sum(d.poll.gen$Degree))/sum(d.poll$Degree)
  out[j,9]<-sum(d.poll.gen$Degree)/sum(d.poll$Degree)
  out[j,7]<-1-(out[j,8]+out[j,9])
  
  
  out[j,11]<-(nrow(selectedRows)-nrow(d.poll.gen))/nrow(d.poll)
  out[j,12]<-nrow(d.poll.gen)/nrow(d.poll)
  out[j,10]<-1-(out[j,11]+out[j,12])
  
  
  
  
  }
  
out
out[1:nrow(out),5]<-"pollination"
out[1:nrow(out),6]<-"pollinators"
out


out[1,2]<-"Arroyo et al 1982"
out[2,2]<-"Arroyo et al 1982"
out[3,2]<-"Arroyo et al 1982"
out[4,2]<-"Barret & Helenum 1987"
out[5,2]<-"Clements & Long 1923"
out[6,2]<- "Dicks et al 2002"
out[7,2]<-"Dicks et al 2002"
out[8,2]<-"Dupont et al 2003"
out[9,2]<-"Eiberling and Olesen 1999"
out[10,2]<-"Eiberling and Olesen unpublished"
out[11,2]<-"Olesen et al 2002"
out[12,2]<- "Olesen unpublished"
out[13,2]<- "Ollerton et al 2003"
out[14,2]<- "Hocking 1968"
out[15,2]<- "Petanidou 1991"
out[16,2]<- "Herrera 1988"
out[17,2]<- "Memmot 1999"
out[18,2]<- "Olesen unpublished"
out[19,2]<- "Inouye & Pyke 1988"
out[20,2]<- "Kevan 1970"
out[21,2]<- "Kato et al 1990"
out[22,2]<- "Medan et al 2002"
out[23,2]<- "Medan et al 2002"
out[24,2]<- "Mosquin & Martin 1967"
out[25,2]<- "Motten 1982"
out[26,2]<- "MacMullen 1993"
out[27,2]<- "Primack 1983"
out[28,2]<- "Primack 1983"
out[29,2]<- "Primack 1983"
out[30,2]<- "Ramirez & Brito 1990"
out[31,2]<- "Ramirez 1989"
out[32,2]<- "Schemske et al 1978"
out[33,2]<- "Small 1976"
out[34,2]<- "Smith-Ramirez et al 2005"
out[35,2]<- "Percival 1974"
out[36,2]<- "Olesen unpublished"
out[37,2]<- "Montero 2005"
out[38,2]<- "Montero 2005"
out[39,2]<- "Stald 2003"
out[40,2]<- "Ingversen 2006"
out[41,2]<- "Ingversen 2006"
out[42,2]<- "Phillip et al 2006"
out[43,2]<- "Montero 2005"
out[44,2]<- "Kato 2000"
out[45,2]<- "Lundgren & Olesen 2005"
out[46,2]<- "Bundgaard 2003"
out[47,2]<- "Dupont & Olesen 2009"
out[48,2]<- "Dupont & Olesen 2009"
out[49,2]<- "Beck 2006"
out[50,2]<- "Stald et al 2003"
out[51,2]<- "Vazquez 2002"
out[52,2]<- "Witt 1998"
out[53,2]<- "Yamazaki & Kato 2003"
out[54,2]<- "Kakutani et al 1990"
out[55,2]<- "Kato & Miura 1996"
out[56,2]<- "Kato et al 1993"
out[57,2]<- "Inouye et al 1990"
out[58,2]<- "Bartomeus et al 2008"
out[59,2]<- "Bezerra et al 2009"
out[60,2]<- "Kaiser-Bunbury et al 2010" 
out[61,2]<- "Kaiser-Bunbury et al 2014"
out[62,2]<- "Robertson 1929"
out[63,2]<- "Vizentin-Bugoni et al 2016"
out[64,2]<- "Abreu & Vieira 2004"
out[65,2]<- "Arizmendi & Ornelas 1999"
out[66,2]<- "Canela 2006"
out[67,2]<- "Las Casas et al 2012"
out[68,2]<- "Gutierrez & Rojas 2011"
out[69,2]<- "Kohler 2011"
out[70,2]<- "Kohler 2011"
out[71,2]<- "Kohler 2011"
out[72,2]<- "Lara 2006"
out[73,2]<- "Rosero 2003"


out
write.csv(out, "results.csv", row.names = FALSE)
  
  
  
