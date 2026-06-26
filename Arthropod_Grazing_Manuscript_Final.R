#### Grazing x Arthropod Data - 2020 - 2022 #
#### Code created by: Kathryn Bloodworth
# Original file in "Arthropod_Grazing_Manuscript.R" - this file has been reduced to only analyses and graphs used in the manuscript

#### Set working directory and load libraries ####

library(scales)
library(vegan)
library(lmerTest)
library(grid)
library(multcomp)
library(tidyverse)
library(olsrr)
library(patchwork)
library(codyn)
library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
#install.packages("pairwiseAdonis")
library(pairwiseAdonis)
library(ggpattern)


#set working directory - UMD mac
setwd("/Users/kjbloodw/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/Insect_Data")

#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme
theme_update(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank())
theme_pub <- theme_minimal(base_size = 55) +
  theme(
    legend.text  = element_text(size = 45),
    legend.title = element_text(size = 55),
    axis.title   = element_text(size = 55),
    axis.text    = element_text(size = 45),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

#### Load Data ####
###### Arthropod ID Data ####
#make sure column names are consistent 

#2020
ID_Data_20<-read.csv("2020_Sweep_Net_Dvac_Data_FK.csv",header=T) %>% 
  #make all collection methods the same across years
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method))) %>% 
  #rename sample column so that it's the same across years
  rename(Sample_Number="Sample") %>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Order,Family,Genus,Species,Notes) %>% 
  filter(Collection_Method=="dvac")

#2021
ID_Data_21<-read.csv("2021_Sweep_Net_Dvac_Data_FK.csv",header=T) %>% 
  #make all collection methods the same across years
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method))) %>% 
  #rename sample column so that it's the same across years
  rename(Sample_Number="Sample")%>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Order,Family,Genus,Species,Notes) %>% 
  #remove blanks from dataframe
  filter(Collection_Method!="") %>% 
  #fix "LG " to "LG"
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="LG ","LG",Grazing_Treatment))%>%
  filter(Collection_Method=="dvac")

#2022
ID_Data_22<-read.csv("2022_Sweep_Net_D-Vac_Data_FK.csv",header=T) %>% 
  #make all collection methods the same across years
  mutate(Collection_Method=ifelse(Collection_Method=="Dvac","dvac",ifelse(Collection_Method=="Sweep_Net","sweep",Collection_Method))) %>% 
  #rename sample column so that it's the same across years
  rename(Sample_Number="Sample")%>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Order,Family,Genus,Species,Notes)%>% 
  filter(Collection_Method=="dvac")

###### Arthropod Weight Data ####

#2020
Weight_Data_20<-read.csv("2020_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  rename(Sample_Number=Sample_num) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))%>% 
  filter(Collection_Method=="dvac")

#2021
Weight_Data_21<-read.csv("2021_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))%>% 
  filter(Collection_Method=="dvac")

#2022
Weight_Data_22<-read.csv("2022_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))%>% 
  filter(Collection_Method=="dvac")

###### Feeding Guild Data ####
Feeding<-read.csv("Arthropod_ID_Data_Guilds.csv", header=T)

#### Formatting and Cleaning ####

###### ID Data ####

#ID Data 2020
ID_20<-ID_Data_20 %>% 
  #Change block and grazing treatment to be consistent
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Order=ifelse(Order=="orthoptera","Orthoptera",ifelse(Order=="hemiptera","Hemiptera",ifelse(Order=="coleoptera","Coleoptera",ifelse(Order=="hymenoptera","Hymenoptera",ifelse(Order=="diptera","Diptera",ifelse(Order=="araneae","Araneae",Order))))))) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Family=ifelse(Family=="acrididae", "Acrididae",ifelse(Family=="cicadellidae", "Cicadellidae", ifelse(Family=="geocoridae", "Geocordidae", ifelse(Family=="carabidae", "Carabidae", ifelse(Family=="chrysomelidae","Chrysomelidae", ifelse(Family=="formicidae", "Formicidae", ifelse(Family=="halictidae", "Halictidae", ifelse(Family=="agromyzidae", "Agromyzidae", ifelse(Family=="lycosidae", "Lycosidae", ifelse(Family=="platygastridae", "Platygastridae", ifelse(Family=="tettigoniidae", "Tettigoniidae", ifelse(Family=="salticidae", "Salticidae", ifelse(Family=="thomisidae", "Thomisidae", ifelse(Family=="pentatomidae", "Pentatomidae", ifelse(Family=="lygaeidae", "Lygaeidae", ifelse(Family=="scutelleridae", "Scutelleridae", ifelse(Family=="gryllidae", "Gryllidae", ifelse(Family=="asilidae", "Asilidae", ifelse(Family=="chrysididae", "Chrysididae", ifelse(Family=="curculionidae", "Curculionidae", ifelse(Family=="latridiidae","Latridiidae", ifelse(Family=="muscidae", "Muscidae", ifelse(Family=="tenebrionidae", "Tenebrionidae",ifelse(Family=="Lygacidae","Lygaeidae",ifelse(Family=="Salticide","Salticidae", Family)))))))))))))))))))))))))) %>% 
  mutate(Correct_Genus=ifelse(Genus=="Melanoplus","Melanoplus",ifelse(Genus=="arphia","Arphia",ifelse(Genus=="melanoplus","Melanoplus",ifelse(Genus=="opeia","Opeia",ifelse(Genus=="nenconocephalus","Neoconocephalus",ifelse(Genus=="pachybrachis","Pachybrachis",ifelse(Genus=="ageneotettix ","Ageneotettix", ifelse(Genus=="phoetaliotes","Phoetaliotes",ifelse(Genus=="Ageneotettix ","Ageneotettix",ifelse(Genus=="amphiturnus","Amphiturnus",ifelse(Genus=="Ageneotettox","Ageneotettix",ifelse(Genus=="Agneotettix","Ageneotettix",ifelse(Genus=="ageneotettix","Ageneotettix",Genus)))))))))))))) %>% 
  mutate(Correct_Species=ifelse(Species=="differentalis","differentialis",ifelse(Species=="sanguinipes","sanguinipes",ifelse(Species=="packardi","packardii",ifelse(Species=="unknown","sp",ifelse(Species=="pachardii","packardii",ifelse(Species=="sanguinpes","sanguinipes",Species))))))) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Correct_Family,Correct_Genus,Correct_Species,Notes) %>% 
  #remove all body part entries
  filter(Notes!="Body Parts" & Notes!="Body Parts/Legs" & Notes!="Body parts" & Notes!="too smooshed to tell, put into body parts jar") %>% 
  #make sample # numeric instead of character 
  mutate(Sample_Number=as.numeric(Sample_Number)) 

#ID Data 2021
ID_21<-ID_Data_21 %>% 
  #Change block and grazing treatment to be consistent and match plot numbers
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  filter(!is.na(Year)) %>% 
  #fix block numbers
  mutate(Block=ifelse(Plot<=15,1,ifelse(Plot==16,2,ifelse(Plot==17,2,ifelse(Plot==18,2,ifelse(Plot==19,2,ifelse(Plot==20,2,ifelse(Plot==21,2,ifelse(Plot==22,2,ifelse(Plot==23,2,ifelse(Plot==24,2,ifelse(Plot==25,2,ifelse(Plot==26,2,ifelse(Plot==27,2,ifelse(Plot==28,2,ifelse(Plot==29,2,ifelse(Plot==30,2,ifelse(Plot==31,3,ifelse(Plot==32,3,ifelse(Plot==33,3,ifelse(Plot==34,3,ifelse(Plot==35,3,ifelse(Plot==36,3,ifelse(Plot==37,3,ifelse(Plot==38,3,ifelse(Plot==39,3,ifelse(Plot==40,3,ifelse(Plot==41,3,ifelse(Plot==43,3,ifelse(Plot==43,3,ifelse(Plot==44,3,ifelse(Plot==45,3,Block))))))))))))))))))))))))))))))))%>% 
  #change grazing treatments to be consistent
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="LG ","LG",Grazing_Treatment)) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Order=ifelse(Order=="Aranea ","Araneae",ifelse(Order=="Hemiptera ","Hemiptera",ifelse(Order=="Araneae ","Araneae",ifelse(Order=="Coleopetra","Coleoptera",ifelse(Order=="Coleoptera ","Coleoptera",ifelse(Order=="Hymenoptera ","Hymenoptera",ifelse(Order=="Hymeonptera","Hymenoptera",ifelse(Order=="Orthoptera ","Orthoptera",Order))))))))) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Family=ifelse(Family=="Acridiae", "Acrididae",ifelse(Family=="Agramyzidae", "Agromyzidae", ifelse(Family=="Coleoptera ", "Coleoptera", ifelse(Family=="Currulianidae", "Curculionidae", ifelse(Family=="Ligidae","Lygaeidae", ifelse(Family=="Scuttelleridae", "Scutelleridae", ifelse(Family=="Scutelleridae ", "Scutelleridae", ifelse(Family=="staphylinidae", "Staphylinidae", ifelse(Family=="Thamisidae", "Thomisidae", ifelse(Family=="Thomsidae", "Thomisidae", ifelse(Family=="Formicide", "Formicidae", Family))))))))))))%>% 
  mutate(Correct_Genus=ifelse(Genus=="longipennis","Longipennis",ifelse(Genus=="Opcia","Opeia",ifelse(Genus=="melanoplus","Melanoplus",ifelse(Genus=="opeia","Opeia",ifelse(Genus=="Phoetaliotes ","Phoetaliotes",ifelse(Genus=="Erittix","Eritettix",Genus))))))) %>% 
  mutate(Correct_Species=ifelse(Species=="bru","bruneri",ifelse(Species=="Bruneri","bruneri",ifelse(Species=="Bruneri ","bruneri",ifelse(Species=="confuscus","confusus",ifelse(Species=="Confusus","confusus",ifelse(Species=="Curtipennis","curtipennis",ifelse(Species=="Deorum","deorum",ifelse(Species=="differntialis","differentialis",ifelse(Species=="Gladstoni","gladstoni",ifelse(Species=="Hebrascensis","nebrascensis",ifelse(Species=="Infantilis","infantilis",ifelse(Species=="Keeleri","keeleri",ifelse(Species=="Nebrascensis","nebrascensis",ifelse(Species=="Obscrua","obscura",ifelse(Species=="Obscura ","obscura",ifelse(Species=="Obscuria","obscura",ifelse(Species=="Pseudonietara","pseudonietana",ifelse(Species=="Pseudonietena","pseudonietana",ifelse(Species=="Sanguinipes","sanguinipes",ifelse(Species=="Simplex","simplex",ifelse(Species=="Angustipennis","angustipennis",Species)))))))))))))))))))))) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Correct_Family,Correct_Genus,Correct_Species,Notes) %>% 
  mutate(Sample_Number=as.numeric(Sample_Number))

#ID Data 2022
ID_22<-ID_Data_22 %>% 
  #Change block and grazing treatment to be consistent and match plot numbers
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>%
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Order=ifelse(Order=="araneae","Araneae",ifelse(Order=="coleoptera","Coleoptera",ifelse(Order=="diptera","Diptera",ifelse(Order=="hemiptera","Hemiptera",ifelse(Order=="hymenoptera","Hymenoptera",ifelse(Order=="lepidoptera","Lepidoptera",ifelse(Order=="neuroptera","Neuroptera",ifelse(Order=="orthoptera","Orthoptera", ifelse(Order=="thysanoptera","Thysanoptera",ifelse(Order=="unknown","Unknown",Order))))))))))) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Family=ifelse(Family=="aphididae", "Aphididae",ifelse(Family=="asilidae", "Asilidae",ifelse(Family=="Ceraphionidae","Ceraphronidae",ifelse(Family=="chloropidae","Chloropidae",ifelse(Family=="Chrionomidae","Chironomidae",ifelse(Family=="chrysididae","Chrysididae",ifelse(Family=="Cicadellidea","Cicadellidae",ifelse(Family=="coccinellidae","Coccinellidae",ifelse(Family=="Coccinelliadae","Coccinellidae",ifelse(Family=="culicidae","Culicidae",ifelse(Family=="curculionidae","Curculionidae",ifelse(Family=="Diapriidea","Diapriidae",ifelse(Family=="Euiophidae","Eulophidae",ifelse(Family=="eupelmidae","Eupelmidae",ifelse(Family=="ichneumonidae","Ichneumonidae",ifelse(Family=="latridiidae","Latridiidae",ifelse(Family=="lycosidae","Lycosidae",ifelse(Family=="muscidae","Muscidae",ifelse(Family=="myrmeleontidae","Myrmeleontidae",ifelse(Family=="nabidae","Nabidae",ifelse(Family=="pentatomidae","Pentatomidae",ifelse(Family=="perilampidae","Perilampidae",ifelse(Family=="platygastridae","Platygastridae",ifelse(Family=="scarabaeidae","Scarabaeidae",ifelse(Family=="Scarabacidae","Scarabaeidae",ifelse(Family=="sepsidae","Sepsidae",ifelse(Family=="tomisidae","Thomisidae",ifelse(Family=="Thripinae","Thripidae",ifelse(Family=="Thrips","Thripidae",ifelse(Family=="Tiombiculidae","Trombiculidae",ifelse(Family=="tingidae","Tingidae",ifelse(Family=="trichoceridae","Trichoceridae",ifelse(Family=="Trichoceridea","Trichoceridae",ifelse(Family=="unknown","Unknown",ifelse(Family=="",NA,ifelse(Family=="N/A",NA,ifelse(Family=="n/a",NA,Family)))))))))))))))))))))))))))))))))))))) %>% 
  mutate(Correct_Genus=ifelse(Genus=="ageneotettix","Ageneotettix",ifelse(Genus=="arphia","Arphia",ifelse(Genus=="melanoplus","Melanoplus",ifelse(Genus=="opeia","Opeia",ifelse(Genus=="dissosteira","Dissosteira",ifelse(Genus=="Dissosteria","Dissosteira" ,ifelse(Genus=="Eritcttix","Eritettix",ifelse(Genus=="eritettix","Eritettix",ifelse(Genus=="Erotettix","Eritettix",ifelse(Genus=="phoetaliotes","Phoetaliotes",ifelse(Genus=="unknown","Unknown",ifelse(Genus=="",NA,ifelse(Genus=="N/A",NA,ifelse(Genus=="n/a",NA,Genus))))))))))))))) %>% 
  mutate(Correct_Species=ifelse(Species=="os","obscura",ifelse(Species=="pseudomietana","pseudonietana",ifelse(Species=="unknown","Unknown",ifelse(Species=="",NA,ifelse(Species=="N/A",NA,ifelse(Species=="n/a",NA,Species))))))) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Correct_Family,Correct_Genus,Correct_Species,Notes) %>% 
  mutate(Sample_Number=as.numeric(Sample_Number))

# Merge together ID data frames
ID_Data_Official<-ID_20 %>% 
  rbind(ID_21) %>% 
  rbind(ID_22) %>% 
  mutate(Coll_Year_Bl_Trt=paste(Collection_Method,Year,Block,Grazing_Treatment,sep = "_")) %>% 
  mutate(Coll_Year_Bl_Trt_Pl=paste(Coll_Year_Bl_Trt,Plot,sep = "-")) %>% 
  mutate(Order_Family=paste(Correct_Order,Correct_Family,sep="_")) %>% 
  #fix remaining issues
  mutate(Correct_Family=ifelse(Correct_Family=="thomisidae","Thomisidae",ifelse(Correct_Family=="curculionidae","Curculionidae",ifelse(Correct_Family=="Staphylindae","Staphylinidae",ifelse(Correct_Family=="unknown","Unknown",ifelse(Order_Family=="Araneae_Lygaeidae","Lycosidae",Correct_Family)))))) %>% 
  mutate(Correct_Order=ifelse(Order_Family=="Coleoptera_Scutelleridae","Hemiptera",ifelse(Order_Family=="Hemiptera_Latridiidae","Coleoptera",ifelse(Order_Family=="Diptera_Platygastridae","Hymenoptera",Correct_Order)))) %>% 
  dplyr::select(-Order_Family)

###### Weight Data ####

#2020
Weight_20<-Weight_Data_20 %>%
  #change blocks to be numeric
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  #Correct order spellings
  mutate(Correct_Order=ifelse(Order=="Aranaea","Araneae",ifelse(Order=="Aranea","Araneae",ifelse(Order=="Hempitera","Hemiptera",ifelse(Order=="Cicadellidae","Hemiptera",ifelse(Order=="Lyaceidae","Hemiptera",ifelse(Order=="","Orthoptera",Order))))))) %>%
  #fix NA issue related to body parts
  mutate(Correct_Order=ifelse(Notes=="Body Parts","Body_Parts",ifelse(Notes=="Body parts","Body_Parts",ifelse(Notes=="unknown","unknown",Correct_Order)))) %>%
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Dry_Weight_g,Notes)

#2021
Weight_21<-Weight_Data_21 %>%  
  #change grazing treatments to be correct
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="LG ","LG",ifelse(Grazing_Treatment=="LH","LG",Grazing_Treatment))) %>% 
  #change blocks to be numeric
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  #Remove extra rows
  filter(!is.na(Year)) %>% 
  #correct order spellings
  mutate(Correct_Order=ifelse(Order=="aranea","Araneae",ifelse(Order=="body_parts","Body_Parts",ifelse(Order=="Body Parts","Body_Parts",ifelse(Order=="Body_Parts ","Body_Parts",ifelse(Order=="coleoptera","Coleoptera",ifelse(Order=="Coleoptera ","Coleoptera",ifelse(Order=="diptera","Diptera",ifelse(Order=="hemiptera","Hemiptera",ifelse(Order=="hymenoptera","Hymenoptera",ifelse(Order=="Orthoptera ","Orthoptera",ifelse(Order=="body parts","Body_Parts",ifelse(Order=="Cicadellidae","Hemiptera",Order))))))))))))) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Dry_Weight_g,Notes)

#2022
Weight_22<-Weight_Data_22 %>%  
  #change blocks to be numeric
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>%
  mutate(Correct_Order=ifelse(Order=="Trombicvlidae","Trombiculidae",Order)) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Dry_Weight_g,Notes)

# Merge together data frames
Weight_Data_Official<-Weight_20 %>% 
  rbind(Weight_21) %>% 
  rbind(Weight_22) %>% 
  #wrong grazing treatment fixed
  mutate(Grazing_Treatment=ifelse(Plot==35,"NG",Grazing_Treatment)) %>% 
  #wrong block numbers fixed
  mutate(Block=ifelse(Plot=="40",3,Block)) %>% 
  #replace any weight that is <0.0001 with 0.00001 %>% 
  mutate(Dry_Weight_g=as.numeric(ifelse(Dry_Weight_g=="<0.0001","0.00001",Dry_Weight_g))) %>% 
  #Create a column that merges together treatment data and year
  mutate(Coll_Year_Bl_Trt=paste(Collection_Method,Year,Block,Grazing_Treatment,sep = "_")) %>% 
  mutate(Coll_Year_Bl_Trt_Pl=paste(Coll_Year_Bl_Trt,Plot,sep = "-")) %>% 
  mutate(Coll_Year_Bl_Trt_Pl=ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2021_1_NG_33","dvac_2021_3_NG_33",Coll_Year_Bl_Trt_Pl)) %>% 
  #fix plot numbers to be correct numbers
  mutate(Coll_Year_Bl_Trt_Pl=ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-1","dvac_2020_1_NG-1",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-2","dvac_2020_1_NG-2",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-3","dvac_2020_1_NG-3",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-4","dvac_2020_1_NG-4",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-5","dvac_2020_1_NG-5",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-1","dvac_2020_1_LG-6",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-2","dvac_2020_1_LG-7",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-3","dvac_2020_1_LG-8",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-4","dvac_2020_1_LG-9",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-5","dvac_2020_1_LG-10",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-1","dvac_2020_1_HG-11",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-2","dvac_2020_1_HG-12",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-3","dvac_2020_1_HG-13",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-4","dvac_2020_1_HG-14",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-5","dvac_2020_1_HG-15",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-1","dvac_2020_2_NG-16",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-2","dvac_2020_2_NG-17",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-3","dvac_2020_2_NG-18",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-4","dvac_2020_2_NG-19",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-5","dvac_2020_2_NG-20",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-1","dvac_2020_2_LG-21",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-2","dvac_2020_2_LG-22",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-3","dvac_2020_2_LG-23",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-4","dvac_2020_2_LG-24",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-5","dvac_2020_2_LG-25",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-1","dvac_2020_2_HG-26",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-2","dvac_2020_2_HG-27",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-3","dvac_2020_2_HG-28",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-4","dvac_2020_2_HG-29",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-5","dvac_2020_2_HG-30",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-1","dvac_2020_3_NG-31",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-2","dvac_2020_3_NG-32",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-3","dvac_2020_3_NG-33",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-4","dvac_2020_3_NG-34",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-5","dvac_2020_3_NG-35",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-1","dvac_2020_3_LG-36",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-2","dvac_2020_3_LG-37",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-3","dvac_2020_3_LG-38",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-4","dvac_2020_3_LG-39",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-5","dvac_2020_3_LG-40",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-1","dvac_2020_3_HG-41",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-2","dvac_2020_3_HG-42",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-3","dvac_2020_3_HG-43",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-4","dvac_2020_3_HG-44",ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-5","dvac_2020_3_HG-45",Coll_Year_Bl_Trt_Pl)))))))))))))))))))))))))))))))))))))))))))))) %>% 
  dplyr::select(Coll_Year_Bl_Trt_Pl,Sample_Number,Correct_Order,Dry_Weight_g,Notes) %>% 
  #RemovNAs from Dry weight
  filter(!is.na(Dry_Weight_g)) %>% 
  separate(Coll_Year_Bl_Trt_Pl, c("Coll_Year_Bl_Trt","Plot"), "-") 


#### Calculate Abundance #### 

###### Order with in a Plot ####
Abundance<-ID_Data_Official %>% 
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order) %>% 
  summarise(Abundance=length(Sample_Number)) %>% 
  ungroup() 

###### Absolute Weight ####
#Summing all weights by order within dataset, grazing treatment, block, and plot so that we can look at differences in order across plots
Weight_Data_Summed<-aggregate(Dry_Weight_g~Coll_Year_Bl_Trt+Plot+Correct_Order, data=Weight_Data_Official, FUN=sum, na.rm=FALSE) 

#Separating out Treatment_Plot into all distinctions again so that we can group based on different things
Weight_Data_Summed<-Weight_Data_Summed %>% 
  separate(Coll_Year_Bl_Trt, c("Collection_Method","Year","Block","Grazing_Treatment"), "_") %>%  
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Light Grazing",ifelse(Grazing_Treatment=="NG","Rest from Grazing",Grazing_Treatment))))

#create dataframe that just has dvac samples in it
Weight_Data_Summed_dvac<-Weight_Data_Summed %>% 
  filter(Plot!="NA") %>% 
  #sum by plot 
  group_by(Year,Block,Grazing_Treatment,Plot) %>% 
  summarise(Plot_Weight=sum(Dry_Weight_g)) %>% 
  ungroup() %>%
  #average across plots (n=3)
  group_by(Year,Block,Grazing_Treatment) %>% 
  summarise(GrTrt_Weight=mean(Plot_Weight)) %>% 
  ungroup()

Weight_by_Grazing_dvac<-Weight_Data_Summed_dvac %>% 
  #mean and SD for each treatment & year
  group_by(Year,Grazing_Treatment) %>% 
  summarise(Average_Weight=mean(GrTrt_Weight),Weight_SD=sd(GrTrt_Weight),Weight_n=length(GrTrt_Weight)) %>% 
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()

###### Relative Weight ####
Relative_Weight<-Weight_Data_Summed %>% 
  filter(Plot!="NA") %>% 
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts") %>% 
  #add together all data of each orders across grazing treatments 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  mutate(Order_Weight=sum(Dry_Weight_g)) %>%
  ungroup() %>% 
  #add together all data within each grazing treatment for total "plot" weight
  group_by(Year,Grazing_Treatment) %>% 
  mutate(Total_Weight=sum(Dry_Weight_g)) %>%
  ungroup() %>% 
  select(Year,Grazing_Treatment,Correct_Order,Order_Weight,Total_Weight) %>% 
  unique() %>% 
  mutate(RelativeWeight=Order_Weight/Total_Weight) %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_RelativeWeight=mean(RelativeWeight)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Light Grazing",ifelse(Grazing_Treatment=="NG","Rest from Grazing",Grazing_Treatment))))

###### Relative Count ####
Relative_Count<-Abundance %>% 
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts") %>% 
  select(Year,Block,Grazing_Treatment,Plot,Correct_Order,Abundance) %>% 
  unique() %>% 
  #add together all data of each orders across grazing treatments 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  mutate(Order_Abundance=sum(Abundance)) %>%
  ungroup() %>% 
  #add together all data within each grazing treatment for total "plot"count
  group_by(Year,Grazing_Treatment) %>% 
  mutate(Total_Abundance=sum(Abundance)) %>%
  ungroup() %>% 
  select(Year,Grazing_Treatment,Correct_Order,Order_Abundance,Total_Abundance) %>% 
  unique() %>% 
  mutate(RelativeCount=Order_Abundance/Total_Abundance) %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_RelativeCount=mean(RelativeCount)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Light Grazing",ifelse(Grazing_Treatment=="NG","Rest from Grazing",Grazing_Treatment))))

#### Calculate Community Metrics: Biomass ####
# uses codyn package and finds shannon's diversity 
Weight_Data_Summed_2<-Weight_Data_Summed %>% 
  filter(Plot!="NA")
Diversity_Weight <- community_diversity(df = Weight_Data_Summed_2,
                                        time.var = "Year",
                                        replicate.var = c("Collection_Method","Plot","Block","Grazing_Treatment"),
                                        abundance.var = "Dry_Weight_g")
#Community Structure
Structure_Weight <- community_structure(df = Weight_Data_Summed_2,time.var = "Year",replicate.var = c("Collection_Method","Plot","Block","Grazing_Treatment"),abundance.var = "Dry_Weight_g",metric = "Evar")

#Make a new data frame from "Extra_Species_Identity" to generate richness values for each research area
Order_Richness_Weight<-ID_Data_Official %>%  
  select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order) %>% 
  unique() %>% 
  #group data frame by Watershed and exclosure
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot) %>%
  #Make a new column named "Richness" and add the unique number of rows in the column "taxa" according to the groupings
  summarise(richness=length(Correct_Order)) %>%
  #stop grouping by watershed and exclosure
  ungroup()  %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Light Grazing",ifelse(Grazing_Treatment=="NG","Rest from Grazing",Grazing_Treatment))))

Order_Richness_Weight$Year=as.character(Order_Richness_Weight$Year)
Order_Richness_Weight$Plot=as.character(Order_Richness_Weight$Plot)

#join the datasets
CommunityMetrics_Weight <- Diversity_Weight %>%
  full_join(Structure_Weight) %>% 
  select(-richness) %>% 
  full_join(Order_Richness_Weight) %>% 
  #average across plots (n=3)
  group_by(Year,Block,Grazing_Treatment) %>% 
  summarise(Shannon=mean(Shannon,na.rm=T),
            Evar=mean(Evar,na.rm=T),
            richness=mean(richness,na.rm=T)) %>% 
  ungroup()

#make dataframe with averages
CommunityMetrics_Weight_Avg<-CommunityMetrics_Weight  %>% 
  group_by(Year,Grazing_Treatment) %>%
  summarize(richness_Std=sd(richness),richness_Mean=mean(richness),richness_n=length(richness),Shannon_Std=sd(Shannon),Shannon_Mean=mean(Shannon),Shannon_n=length(Shannon),Evar_Std=sd(Evar,na.rm=T),Evar_Mean=mean(Evar,na.rm=T),Evar_n=length(Evar))%>%
  mutate(richness_St_Error=richness_Std/sqrt(richness_n),Shannon_St_Error=Shannon_Std/sqrt(Shannon_n),Evar_St_Error=Evar_Std/sqrt(Evar_n)) %>% 
  ungroup() 

#### Figure 1 & Stats: Shannon Diversity ####

###### Figure ####

##reorder bar graphs##
CommunityMetrics_Weight_Avg$Grazing_Treatment <- factor(CommunityMetrics_Weight_Avg$Grazing_Treatment, levels = c("Rest from Grazing", "Light Grazing", "High Impact Grazing"))


# 2020 
Shannon_2020_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2020),aes(x=Grazing_Treatment,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon Diversity")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"), labels=c("Rest from Grazing","Light Grazing","High Impact Grazing"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.88, y=1, label="a) 2020",size=20)

# 2021 - Dvac
#Graph of Weights from dvac by Grazing treatment- 2021
Shannon_2021_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2021),aes(x=Grazing_Treatment,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon Diversity")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"), labels=c("Rest from Grazing","Light Grazing","High Impact Grazing"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.88, y=1, label="b) 2021",size=20)+
  #no grazing is different than high grazing, low grazing is not different than high grazing, no and low grazing not different
  annotate("text",x=1,y=0.39,label="a",size=20)+ #no grazing
  annotate("text",x=2,y=0.61,label="b",size=20)+ #low grazing
  annotate("text",x=3,y=0.73,label="c",size=20) #high grazing


# 2022 - Dvac
Shannon_2022_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2022),aes(x=Grazing_Treatment,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon Diversity")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"),labels=c("Rest From Grazing","Light Grazing","High Impact Grazing"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.88, y=1, label="c) 2022",size=20)

Shannon_2020_Weight+  
  Shannon_2021_Weight+
  Shannon_2022_Weight+
  plot_layout(ncol = 3,nrow = 1)
#Save at 3000x1500


###### Normality ####

# Weight 2020
Weight_2020_OrderShannon <- lm(data = subset(CommunityMetrics_Weight, Year == 2020),(Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2020_OrderShannon) 
ols_test_normality(Weight_2020_OrderShannon) #normal

# Weight 2021
Weight_2021_OrderShannon <- lm(data = subset(CommunityMetrics_Weight, Year == 2021),(Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2021_OrderShannon) 
ols_test_normality(Weight_2021_OrderShannon) #normal

# Weight 2020
Weight_2022_OrderShannon <- lm(data = subset(CommunityMetrics_Weight, Year == 2022),(Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2022_OrderShannon) 
ols_test_normality(Weight_2022_OrderShannon) #normal

###### Stats ####

# 2020 Weight
OrderShannon_2020_Glmm_Weight_Pad <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2020))
anova(OrderShannon_2020_Glmm_Weight_Pad) #not significant

# 2021 Weight
OrderShannon_2021_Glmm_Weight_Pad <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2021))
anova(OrderShannon_2021_Glmm_Weight_Pad) #0.005528
summary(glht(OrderShannon_2021_Glmm_Weight_Pad, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.09455), #LG-HG (0.09455), NG-HG (0.00178)

# 2022 Weight
OrderShannon_2022_Glmm_Weight_Pad <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2022))
anova(OrderShannon_2022_Glmm_Weight_Pad) #not significant


#### Figure S1 & Stats: Richness & Evenness ####

###### Figure S1a-c ####

# 2020 
richness_2020_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2020),aes(x=Grazing_Treatment,y=richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=richness_Mean-richness_St_Error,ymax=richness_Mean+richness_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species richness"
  ylab("Richness")+
  ggtitle("2020")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"), labels=c("Rest from Grazing","Light Grazing","High Impact Grazing"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.text.x = element_blank(), axis.title.x = element_blank(),plot.title=element_text(hjust=0.5))+
  geom_text(x=0.7, y=7.8, label="a)",size=20)

# 2021 - Dvac
#Graph of Weights from dvac by Grazing treatment- 2021
richness_2021_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2021),aes(x=Grazing_Treatment,y=richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=richness_Mean-richness_St_Error,ymax=richness_Mean+richness_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species richness"
  ylab("Richness")+
  ggtitle("2021")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"), labels=c("Rest from Grazing","Light Grazing","High Impact Grazing"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.text.x = element_blank(), axis.title.x = element_blank(),plot.title=element_text(hjust=0.5))+
  geom_text(x=0.7, y=7.8, label="b)",size=20)


# 2022 - Dvac
richness_2022_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2022),aes(x=Grazing_Treatment,y=richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=richness_Mean-richness_St_Error,ymax=richness_Mean+richness_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species richness"
  ylab("Richness")+
  ggtitle("2022")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"),labels=c("Rest From Grazing","Light Grazing","High Impact Grazing"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.text.x = element_blank(), axis.title.x = element_blank(),plot.title=element_text(hjust=0.5))+
  geom_text(x=0.7, y=7.8, label="c)",size=20)

###### Normality S1a-c ####

# Weight 2020
Weight_2020_Orderrichness <- lm(data = subset(CommunityMetrics_Weight, Year == 2020),(richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2020_Orderrichness) 
ols_test_normality(Weight_2020_Orderrichness) #normal

# Weight 2021
Weight_2021_Orderrichness <- lm(data = subset(CommunityMetrics_Weight, Year == 2021),(richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2021_Orderrichness) 
ols_test_normality(Weight_2021_Orderrichness) #normal

# Weight 2020
Weight_2022_Orderrichness <- lm(data = subset(CommunityMetrics_Weight, Year == 2022),(richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2022_Orderrichness) 
ols_test_normality(Weight_2022_Orderrichness) #normal

###### Stats S1a-c ####

# 2020 Weight
Orderrichness_2020_Glmm_Weight_Pad <- lmer((richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2020))
anova(Orderrichness_2020_Glmm_Weight_Pad) #not significant

# 2021 Weight
Orderrichness_2021_Glmm_Weight_Pad <- lmer((richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2021))
anova(Orderrichness_2021_Glmm_Weight_Pad) #not significant
# 2022 Weight
Orderrichness_2022_Glmm_Weight_Pad <- lmer((richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2022))
anova(Orderrichness_2022_Glmm_Weight_Pad) #not significant

###### Figure S1d-f ####

# 2020 
Evar_2020_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2020),aes(x=Grazing_Treatment,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"), labels=c("Rest from Grazing","Light Grazing","High Impact Grazing"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.7, y=0.39, label="d)",size=20)

# 2021 - Dvac
#Graph of Weights from dvac by Grazing treatment- 2021
Evar_2021_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2021),aes(x=Grazing_Treatment,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"), labels=c("Rest from Grazing","Light Grazing","High Impact Grazing"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.7, y=0.39, label="e)",size=20)


# 2022 - Dvac
Evar_2022_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2022),aes(x=Grazing_Treatment,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Evanness")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"),labels=c("Rest From Grazing","Light Grazing","High Impact Grazing"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.7, y=0.39, label="f)",size=20)

###### Normality S1d-f ####

# Weight 2020
Weight_2020_OrderEvar <- lm(data = subset(CommunityMetrics_Weight, Year == 2020),(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2020_OrderEvar) 
ols_test_normality(Weight_2020_OrderEvar) #normal

# Weight 2021
Weight_2021_OrderEvar <- lm(data = subset(CommunityMetrics_Weight, Year == 2021),(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2021_OrderEvar) 
ols_test_normality(Weight_2021_OrderEvar) #normal

# Weight 2020
Weight_2022_OrderEvar <- lm(data = subset(CommunityMetrics_Weight, Year == 2022),(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2022_OrderEvar) 
ols_test_normality(Weight_2022_OrderEvar) #normal

###### Stats S1d-f ####

# 2020 Weight
OrderEvar_2020_Glmm_Weight_Pad <- lmer((Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2020))
anova(OrderEvar_2020_Glmm_Weight_Pad) #not significant

# 2021 Weight
OrderEvar_2021_Glmm_Weight_Pad <- lmer((Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2021))
anova(OrderEvar_2021_Glmm_Weight_Pad) #not significant

# 2022 Weight
OrderEvar_2022_Glmm_Weight_Pad <- lmer((Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2022))
anova(OrderEvar_2022_Glmm_Weight_Pad) #not significant

###### Create Figure S1 ####
richness_2020_Weight+  
  richness_2021_Weight+
  richness_2022_Weight+
  Evar_2020_Weight+  
  Evar_2021_Weight+
  Evar_2022_Weight+
  plot_layout(ncol = 3,nrow = 2)
#Save at 2500x2000

#### Figure 2: Plot Weight & Proportion Figure #### 

###### Figure 2a-c ####

##reorder bar graphs##
Weight_by_Grazing_dvac$Grazing_Treatment <- factor(Weight_by_Grazing_dvac$Grazing_Treatment, levels = c("Rest from Grazing", "Light Grazing", "High Impact Grazing"))

# 2020 Average Plot Weight
Dvac_2020_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2020),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  xlab("Grazing Regime")+
  ylab("Average Plot Biomass (g)")+
  ggtitle("2020") +
  theme(legend.background=element_blank(),legend.position = "none")+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"), labels = c("Rest from Grazing", "Light Grazing", "High Impact Grazing")) +
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none",plot.title=element_text(hjust=0.5))+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55))+
  geom_text(x=0.6, y=0.39, label="a)",size=20)

# 2021 Average Plot Weight
Dvac_2021_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2021),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  xlab("Grazing Regime")+
  ylab("Plot Biomass (g)")+
  ggtitle("2021") +
  theme(legend.background=element_blank())+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"), labels = c("Rest from Grazing", "Light Grazing", "High Impact Grazing"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none",plot.title=element_text(hjust=0.5))+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55))+
  geom_text(x=0.6, y=0.39, label="b)",size=20)+
  #no grazing is different than high grazing, low grazing is different than high grazing, no and low grazing are the same
  annotate("text",x=1,y=0.13,label="a",size=20)+ #no grazing
  annotate("text",x=2,y=0.09,label="a",size=20)+ #low grazing
  annotate("text",x=3,y=0.05,label="b",size=20) #high grazing

# 2022 Average Plot Weight
Dvac_2022_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2022),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  xlab("Grazing Regime")+
  ylab("Plot Biomass(g)")+
  ggtitle("2022") +
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"), labels = c("Rest from Grazing", "Light Grazing", "High Impact Grazing"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none",plot.title=element_text(hjust=0.5))+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55))+
  geom_text(x=0.6, y=0.39, label="c)",size=20)

####### Normality: Figure 2a-c ####
#2020
dvac_2020_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2020), sqrt(GrTrt_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_Weight) 
ols_test_normality(dvac_2020_Weight) #normal

#2021
dvac_2021_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2021), log(GrTrt_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_Weight) 
ols_test_normality(dvac_2021_Weight) #normal

#2022
dvac_2022_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2022), log(GrTrt_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_Weight) 
ols_test_normality(dvac_2022_Weight) #normal

###### Stats: Figure 2a-c ####
#2020
Plot_Weight_D_2020_Glmm_Pad <- lmer(sqrt(GrTrt_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2020))
anova(Plot_Weight_D_2020_Glmm_Pad) #not significant

#2021
Plot_Weight_D_2021_Glmm_Pad <- lmer(log(GrTrt_Weight) ~ (Grazing_Treatment) + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2021))
summary(Plot_Weight_D_2021_Glmm_Pad)
anova(Plot_Weight_D_2021_Glmm_Pad) # p=0.05189
###post hoc test for lmer test ##
summary(glht(Plot_Weight_D_2021_Glmm_Pad, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (0.6109), #LG-HG (p=0.02081), NG-HG (p=0.02081)

#2022
Plot_Weight_D_2022_Glmm_Pad <- lmer(log(GrTrt_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2022))
anova(Plot_Weight_D_2022_Glmm_Pad) #not significant


###### Figure 2d-f ####


##reorder bar graphs##
Relative_Weight$Grazing_Treatment <- factor(Relative_Weight$Grazing_Treatment, levels = c("Rest from Grazing", "Light Grazing", "High Impact Grazing"))


#Proportion of Orders by Weight
Order_Weight_2020<-ggplot(subset(Relative_Weight,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  xlab("Grazing  Regime")+
  ylab("Proportion by \n Biomass (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_text(size=55),axis.text.y=element_text(size=55))+
  geom_text(x=0.6, y=1.2, label="d)",size=20)

Order_Weight_2021<-ggplot(subset(Relative_Weight,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  xlab("Grazing  Regime")+
  ylab("Proportion by Biomass (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.6, y=1.2,label="e)",size=20)

Order_Weight_2022<-ggplot(subset(Relative_Weight,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  ylab("Proportion by Biomass (g)")+ 
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#66676C","#403025","#B89984","#CABEB9","#72544D"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.6, y=1.2, label="f)",size=20)

###### Figure 2g-j ####


##reorder bar graphs##
Relative_Count$Grazing_Treatment <- factor(Relative_Count$Grazing_Treatment, levels = c("Rest from Grazing", "Light Grazing", "High Impact Grazing"))

#proportion by abundance
Order_Count_2020<-ggplot(subset(Relative_Count,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  ylab("Proportion by \n Abundance (count)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_text(size=55),axis.text.y=element_text(size=55))+
  geom_text(x=0.6, y=1.2,label="g)",size=20)

Order_Count_2021<-ggplot(subset(Relative_Count,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  ylab("Proportion by Abundance (count)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.6, y=1.2, label="h)",size=20)

Order_Count_2022<-ggplot(subset(Relative_Count,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  ylab("Proportion by Abundance (count)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#66676C","#403025","#B89984","#CABEB9","#72544D"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.6, y=1.2, label="i)",size=20)

###### Create Figure 2 ####
Dvac_2020_Plot+
  Dvac_2021_Plot+
  Dvac_2022_Plot+
  Order_Weight_2020 +  
  Order_Weight_2021+
  Order_Weight_2022 +
  Order_Count_2020 +  
  Order_Count_2021+
  Order_Count_2022 +
  plot_layout(ncol = 3,nrow = 3)
#save at 3000 x 3000


#### Figure 3: RDA ####

###### Create Dataframes for RDA ####

Abundance_Wide_Weight<-Weight_Data_Summed %>%
  filter(!Correct_Order %in% c("Unknown","unknown", "Unknown_1","Body_Parts","Body Parts")) %>% 
  filter(Plot!="NA") %>% 
  group_by(Year,Block,Grazing_Treatment,Correct_Order) %>% 
  summarise(GrTrt_Order_Weight=sum(Dry_Weight_g)) %>% 
  ungroup() %>%
  spread(key=Correct_Order,value=GrTrt_Order_Weight, fill=0) 

RDA_Data <- Abundance_Wide_Weight[,4:13]

#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
RDA_Meta_Data <- Abundance_Wide_Weight[,1:3]

###### RDA year*grazing ####

RDA_Year_Grazing_Avg <- rda(RDA_Data ~ Year*Grazing_Treatment, data=RDA_Meta_Data)

summary(RDA_Year_Grazing_Avg)


capture.output(summary(RDA_Year_Grazing_Avg), file = "RDA_SummaryOutput.txt")

#pull scores to use for subsequent univariate analyses
scores(RDA_Year_Grazing_Avg, c(1:4), scaling=3)

capture.output(scores(RDA_Year_Grazing_Avg, c(1:4), scaling=3), file = "RDA_ScoresOutput.txt")

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova.cca(RDA_Year_Grazing_Avg)   
capture.output(anova(RDA_Year_Grazing_Avg), file = "RDA_ModelAnova.txt")
#test significance by terms (= PerMANOVA)
anova.cca(RDA_Year_Grazing_Avg, by = "terms")  
capture.output(anova(RDA_Year_Grazing_Avg, by = "terms"), file = "RDA_ModelAnova_Terms.txt")
#justifies subsequent univariate tests for axes that are significant
anova.cca(RDA_Year_Grazing_Avg, by = "axis")  
capture.output(anova(RDA_Year_Grazing_Avg, by = "axis"), file = "RDA_ModelAnova_Axis.txt")

###### RDA Graph ####
RDA_Meta_Data <- RDA_Meta_Data %>%
  mutate(Year_Grazing = interaction(Year, Grazing_Treatment, sep = "_", drop = TRUE))

scl <- 3

site_sc <- scores(RDA_Year_Grazing_Avg, display = "sites", scaling = scl)
sp_sc   <- scores(RDA_Year_Grazing_Avg, display = "species", scaling = scl)

sites_df <- RDA_Meta_Data %>%
  mutate(RDA1 = site_sc[,1],
         RDA2 = site_sc[,2])

sp_df <- data.frame(
  RDA1 = sp_sc[,1],
  RDA2 = sp_sc[,2],
  label = rownames(sp_sc)
)


sites_df$Grazing_Treatment <- factor(sites_df$Grazing_Treatment, levels = c("Rest from Grazing", "Light Grazing", "High Impact Grazing"))

ggplot(sites_df, aes(RDA1, RDA2)) +
  geom_point(aes(color = Year, shape = Grazing_Treatment),
             alpha = 0.5, size = 4, show.legend = TRUE) +
  
  # Fill (no linetype here)
  stat_ellipse(aes(color = Year, fill = Year),
               type = "t", level = 0.95, geom = "polygon",
               alpha = 0.15, linewidth = 0, show.legend = TRUE) +
  
  # Outline (this creates the linetype legend)
  stat_ellipse(aes(color = Year),
               type = "t", level = 0.95, geom = "path",
               linewidth = 0.9, show.legend = F) +
  
  geom_text_repel(data = sp_df, aes(RDA1, RDA2, label = label),
                  inherit.aes = FALSE, size = 5, color = "black",
                  max.overlaps = Inf) +
  
  theme_minimal() + 
  scale_fill_manual(values=c("#4c956c","#e3b23c","#08415c"), labels = c("2020", "2021", "2022"),name="Year")+
  scale_color_manual(values=c("#4c956c","#e3b23c","#08415c"), labels = c("2020", "2021", "2022"),name="Year")+
  scale_shape_manual(values=c(15,16,17),name="Grazing Regime")+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "grey", linewidth = 0.6),
    axis.title = element_text(size=15),
    axis.text = element_text(size=15),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    legend.key = element_blank(),             
    legend.background = element_blank() ,
    legend.position = "inside",
    legend.position.inside = c(0.12,0.18)
  ) +
  labs(color = "Year", fill = "Year", shape = "Regime")
#save as 1100 x 800


#### Figure S2: Feeding Guild ####

Abundance_Family_Guild<-ID_Data_Official %>% 
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order,Correct_Family) %>% 
  summarise(Abundance=length(Sample_Number)) %>% 
  ungroup() %>% 
  left_join(Feeding) %>% 
  dplyr::select(Collection_Method,Year,Block,Plot, Grazing_Treatment,Correct_Order,Correct_Family,Guild,Abundance)

###### Graph: Feeding Guild ####

Relative_Count_Family<-Abundance_Family_Guild %>% 
  filter(Plot!="NA" & Correct_Family!="NA") %>% 
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts" & Correct_Family!="Unknown") %>% 
  select(Year,Block,Grazing_Treatment,Plot,Correct_Order,Correct_Family,Guild,Abundance) %>% 
  unique() %>% 
  #add together all data of each orders across grazing treatments 
  group_by(Year,Grazing_Treatment,Guild) %>% 
  mutate(FeedingGuild_Abundance=sum(Abundance)) %>%
  ungroup() %>% 
  #add together all data within each grazing treatment for total "plot"count
  group_by(Year,Grazing_Treatment) %>% 
  mutate(Total_Abundance=sum(Abundance)) %>%
  ungroup() %>% 
  select(Year,Grazing_Treatment,Guild,FeedingGuild_Abundance,Total_Abundance) %>% 
  unique() %>% 
  mutate(RelativeCount=FeedingGuild_Abundance/Total_Abundance) %>% 
  group_by(Year,Grazing_Treatment,Guild) %>% 
  summarise(Average_RelativeCount=mean(RelativeCount)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment)))) 


##reorder bar graphs##
Relative_Count_Family$Grazing_Treatment <- factor(Relative_Count_Family$Grazing_Treatment, levels = c("Cattle Removal", "Destock Grazing", "High Impact Grazing"))
Relative_Count_Family$Guild <- factor(Relative_Count_Family$Guild, levels = c("Detritivore","Parasitoid","Predator","Leaf-Chewing Herbivore","Leaf-Mining Herbivore","Pollen/Nectar-Eating Herbivore","Sap-Sucking Herbivore","Wood-Eating Herbivore","Other Herbivore"))

Feeding_Guild_2020<-ggplot(subset(Relative_Count_Family,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Guild, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion by Abundance")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#714c42","#a0897b", "#9CA497","#c9d0c5","#798671","#1E3907"), name = "Feeding Guild")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=1.2,label="a) 2020",size=20)

Feeding_Guild_2021<-ggplot(subset(Relative_Count_Family,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Guild, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion by Abundance")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#503930","#714c42","#a0897b", "#9CA497","#c9d0c5","#798671","#4e6b5d","#1E3907"), name = "Feeding Guild")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1, y=1.2,label="b) 2021",size=20)

Feeding_Guild_2022<-ggplot(subset(Relative_Count_Family,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Guild, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion by Abundance")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#503930","#714c42","#a0897b", "#9CA497","#c9d0c5","#9CA497","#798671","#1E3907"), name = "Feeding Guild")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="right")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1, y=1.2,label="c) 2022",size=20)

Feeding_Guild_2020+
  Feeding_Guild_2021+
  Feeding_Guild_2022+
  plot_layout(ncol = 3,nrow = 1)
#Save at 3000x2000
