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
setwd("/Users/kathryn/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/Insect_Data")

#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme
theme_update(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank())

#### Load in Arthropod ID Data ####
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

#### Load in Arthropod Weight Data ####

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

#### Read in Arthropod Feeding Guild Data ####
Feeding<-read.csv("Arthropod_ID_Data_Guilds.csv", header=T)

#### Read in Plant Species Comp Data ####
PlantComp<-read.csv("Plant_Species_Comp_2022.csv",header=T) 
Functional_Groups<-read.csv("FunctionalGroups.csv")

#### Formatting and Cleaning ID Data ####

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

#### Merge together ID data frames ####
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

#### Abundance of Order with in a Plot ####
Abundance<-ID_Data_Official %>% 
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order) %>% 
  mutate(Abundance=length(Sample_Number)) %>% 
  ungroup()
Abundance_Order<-Abundance %>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Correct_Order,Plot,Abundance) %>% 
  unique() 

#### Abundance of All Arthropods Plot ####
Abundance_Plot<-ID_Data_Official %>% 
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot) %>% 
  mutate(Plot_Abundance=length(Sample_Number)) %>% 
  ungroup() %>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Plot_Abundance) %>% 
  unique()

#### Abundance of Family within a Plot ####
Abundance_Family_Guild<-ID_Data_Official %>% 
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order,Correct_Family) %>% 
  mutate(Abundance=length(Sample_Number)) %>% 
  ungroup() %>% 
  left_join(Feeding) %>% 
  dplyr::select(Collection_Method,Year,Block,Plot, Grazing_Treatment,Correct_Order,Correct_Family,Guild,Abundance) %>% 
  unique() 

Abundance_Family<-Abundance %>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Correct_Order,Correct_Family, Plot,Abundance) %>% 
  unique() 

#### Formatting and Cleaning Weight Data ####

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

#Merge together data frames
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


#### Plot Level Arthropod Abundance by Grazing Treatment ####

## Weight
#Summing all weights by order within dataset, grazing treatment, block, and plot so that we can look at differences in order across plots
Weight_Data_Summed<-aggregate(Dry_Weight_g~Coll_Year_Bl_Trt+Plot+Correct_Order, data=Weight_Data_Official, FUN=sum, na.rm=FALSE) 

#Separating out Treatment_Plot into all distinctions again so that we can group based on different things
Weight_Data_Summed<-Weight_Data_Summed %>% 
  separate(Coll_Year_Bl_Trt, c("Collection_Method","Year","Block","Grazing_Treatment"), "_") %>%  
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

#create dataframe that just has dvac samples in it
Weight_Data_Summed_dvac<-Weight_Data_Summed %>% 
  filter(Collection_Method=="dvac") %>% 
  filter(Plot!="NA") %>% 
  #sum by plot 
  group_by(Year,Block,Grazing_Treatment,Plot) %>% 
  summarise(Plot_Weight=sum(Dry_Weight_g)) %>% 
  ungroup() 

Weight_by_Grazing_dvac<-Weight_Data_Summed_dvac %>% 
  group_by(Year,Grazing_Treatment) %>% 
  summarise(Average_Weight=mean(Plot_Weight),Weight_SD=sd(Plot_Weight),Weight_n=length(Plot_Weight)) %>% 
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()%>% 
  mutate(Correct_Order="Plot") %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))


## Count
Abundance_Count<-Abundance_Order %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_Count=mean(Abundance),Count_SD=sd(Abundance),Count_n=length(Abundance)) %>% 
  mutate(Count_St_Error=Count_SD/sqrt(Count_n)) %>% 
  ungroup()%>% 
  mutate(Correct_Order="Plot")

### Plot Level Abundance by Order by Grazing Treatment ###
Weight_by_Order_Dvac<-Weight_Data_Summed %>%  
  filter(Correct_Order!="Unknown_1") %>% 
  filter(Correct_Order!="Unknown") %>% 
  filter(Correct_Order!="unknown") %>% 
  filter(Correct_Order!="Snail") %>% 
  filter(Correct_Order!="Body_Parts") %>% 
  filter(Correct_Order!="Body Parts") %>% 
  filter(Plot!="NA") %>% 
  spread(key=Correct_Order,value=Dry_Weight_g, fill=0) %>% 
  gather(key="Correct_Order","Dry_Weight_g",6:15) %>% 
  group_by(Collection_Method,Year, Grazing_Treatment, Correct_Order) %>% 
  summarise(Average_Weight=mean(Dry_Weight_g),Weight_SD=sd(Dry_Weight_g),Weight_n=length(Dry_Weight_g)) %>%
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

#### Order Relative Weight ####
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
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

#### Order Relative Count ####
Relative_Count<-Abundance %>% 
  filter(Plot!="NA") %>% 
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
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

#### Plot Weight Figure #### 

##reorder bar graphs##
Weight_by_Grazing_dvac$Grazing_Treatment <- factor(Weight_by_Grazing_dvac$Grazing_Treatment, levels = c("Cattle Removal", "Destock Grazing", "High Impact Grazing"))

# 2020 Average Plot Weight
Dvac_2020_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2020),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Plot Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = "none")+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55))+
  geom_text(x=0.95, y=0.49, label="a) 2020",size=20)

# 2021 Average Plot Weight
Dvac_2021_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2021),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Plot Biomass (g)")+
  theme(legend.background=element_blank())+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55))+
  geom_text(x=0.95, y=0.49, label="b) 2021",size=20)+
  #no grazing is different than high grazing, low grazing is different than high grazing, no and low grazing are the same
  annotate("text",x=1,y=0.14,label="a",size=20)+ #no grazing
  annotate("text",x=2,y=0.12,label="a",size=20)+ #low grazing
  annotate("text",x=3,y=0.07,label="b",size=20) #high grazing

# 2022 Average Plot Weight
Dvac_2022_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2022),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Plot Biomass(g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55))+
  geom_text(x=0.95, y=0.49, label="c) 2022",size=20)

Dvac_2020_Plot+
  Dvac_2021_Plot+
  Dvac_2022_Plot+
  plot_layout(ncol = 3,nrow = 1)
#save at 3000 x 1000

#### Normality: Plot Weights####
#2020
dvac_2020_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2020), sqrt(Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_Weight) 
ols_test_normality(dvac_2020_Weight) #normal

#2021
dvac_2021_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2021), log(Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_Weight) 
ols_test_normality(dvac_2021_Weight) #normal

#2022
dvac_2022_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2022), log(Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_Weight) 
ols_test_normality(dvac_2022_Weight) #normal

#### Stats: Plot Weights by Grazing Treatment####
#2020
Plot_Weight_D_2020_Glmm_Pad <- lmer(sqrt(Plot_Weight) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed_dvac,Year==2020))
anova(Plot_Weight_D_2020_Glmm_Pad) #not significant

#2021
Plot_Weight_D_2021_Glmm_Pad <- lmer(log(Plot_Weight) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed_dvac,Year==2021))
summary(Plot_Weight_D_2021_Glmm_Pad)
anova(Plot_Weight_D_2021_Glmm_Pad) # p=0.05189
###post hoc test for lmer test ##
summary(glht(Plot_Weight_D_2021_Glmm_Pad, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (0.6109), #LG-HG (p=0.02081), NG-HG (p=0.02081)

#2022
Plot_Weight_D_2022_Glmm_Pad <- lmer(log(Plot_Weight) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed_dvac,Year==2022))
anova(Plot_Weight_D_2022_Glmm_Pad) #not significant

#### Calculate Community Metrics: Biomass ####
# uses codyn package and finds shannon's diversity 
Weight_Data_Summed_2<-Weight_Data_Summed %>% 
  filter(Plot!="NA")
Diversity_Weight <- community_diversity(df = Weight_Data_Summed_2,
                                        time.var = "Year",
                                        replicate.var = c("Collection_Method","Plot","Block","Grazing_Treatment"),
                                        abundance.var = "Dry_Weight_g")
#Sweep Net Community Structure
Structure_Weight <- community_structure(df = Weight_Data_Summed_2,
                                        time.var = "Year",
                                        replicate.var = c("Collection_Method","Plot","Block","Grazing_Treatment"),
                                        abundance.var = "Dry_Weight_g",
                                        metric = "Evar")

#Make a new data frame from "Extra_Species_Identity" to generate richness values for each research area
Order_Richness_Weight<-ID_Data_Official %>%  
  select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order) %>% 
  unique() %>% 
  #group data frame by Watershed and exclosure
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot) %>%
  #Make a new column named "Richness" and add the unique number of rows in the column "taxa" according to the groupings
  summarise(richness=length(Correct_Order)) %>%
  #stop grouping by watershed and exclosure
  ungroup()

Order_Richness_Weight$Year=as.character(Order_Richness_Weight$Year)
Order_Richness_Weight$Plot=as.character(Order_Richness_Weight$Plot)

#join the datasets
CommunityMetrics_Weight <- Diversity_Weight %>%
  full_join(Structure_Weight) %>% 
  select(-richness) %>% 
  full_join(Order_Richness_Weight)

#make dataframe with averages
CommunityMetrics_Weight_Avg<-CommunityMetrics_Weight  %>% 
  group_by(Year,Grazing_Treatment) %>%
  summarize(Richness_Std=sd(richness),Richness_Mean=mean(richness),Richness_n=length(richness),Shannon_Std=sd(Shannon),Shannon_Mean=mean(Shannon),Shannon_n=length(Shannon),Evar_Std=sd(Evar,na.rm=T),Evar_Mean=mean(Evar,na.rm=T),Evar_n=length(Evar))%>%
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n),Shannon_St_Error=Shannon_Std/sqrt(Shannon_n),Evar_St_Error=Evar_Std/sqrt(Evar_n)) %>% 
  ungroup %>% 
  mutate(Grazing_Treatment_Fig=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

#### Plot Diversity Figure ####
##reorder bar graphs##
CommunityMetrics_Weight_Avg$Grazing_Treatment <- factor(CommunityMetrics_Weight_Avg$Grazing_Treatment, levels = c("NG", "LG", "HG"))

# 2020 
Shannon_2020_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2020),aes(x=Grazing_Treatment_Fig,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon Diversity")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="a) 2020",size=20)

# 2021 - Dvac
#Graph of Weights from dvac by Grazing treatment- 2021
Shannon_2021_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2021),aes(x=Grazing_Treatment_Fig,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon Diversity")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=1, label="b) 2021",size=20)+
  #no grazing is different than high grazing, low grazing is not different than high grazing, no and low grazing not different
  annotate("text",x=1,y=0.40,label="a",size=20)+ #no grazing
  annotate("text",x=2,y=0.61,label="ab",size=20)+ #low grazing
  annotate("text",x=3,y=0.81,label="b",size=20) #high grazing


# 2022 - Dvac
Shannon_2022_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2022),aes(x=Grazing_Treatment_Fig,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon Diversity")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=1, label="c) 2022",size=20)

Shannon_2020_Weight+  
  Shannon_2021_Weight+
  Shannon_2022_Weight+
  plot_layout(ncol = 3,nrow = 1)
#Save at 3000x1000


#### Normality: Shannon Diversity ####

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

#### Stats: Shannon's Diversity####

# 2020 Weight
OrderShannon_2020_Glmm_Weight_Pad <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(CommunityMetrics_Weight,Year==2020))
anova(OrderShannon_2020_Glmm_Weight_Pad) #not significant

# 2021 Weight
OrderShannon_2021_Glmm_Weight_Pad <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(CommunityMetrics_Weight,Year==2021))
anova(OrderShannon_2021_Glmm_Weight_Pad) #0.005528
summary(glht(OrderShannon_2021_Glmm_Weight_Pad, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.09455), #LG-HG (0.09455), NG-HG (0.00178)

# 2022 Weight
OrderShannon_2022_Glmm_Weight_Pad <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(CommunityMetrics_Weight,Year==2022))
anova(OrderShannon_2022_Glmm_Weight_Pad) #not significant

#### Absolute Arthropod Weight by Order Graphs ####

Order_Weight_1<-Weight_Data_Summed %>% 
  filter(Correct_Order!="Body Parts" & Correct_Order!="Body_Parts" & Correct_Order!="Unknown" & Correct_Order!="unknown" & Correct_Order!="Unknown_1")

Order_Weight_1$Grazing_Treatment <- factor(Order_Weight_1$Grazing_Treatment, levels = c("Cattle Removal", "Destock Grazing", "High Impact Grazing")) 


Order_Weight<-Order_Weight_1%>%  
  filter(Plot!="NA") %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_Weight=mean(Dry_Weight_g),Weight_SD=sd(Dry_Weight_g),Weight_n=length(Dry_Weight_g)) %>%
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()

#### Araneae Biomass ####
Araneae_Weight<-ggplot(subset(Order_Weight, Correct_Order=="Araneae"),aes(x=Year,y=Average_Weight,fill=Grazing_Treatment))+
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position = position_dodge(0.85), width = 0.5,size=1)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = c(0.75,0.75),legend.title=element_blank(),legend.text = element_text(size=40))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55))+
 # scale_y_continuous(labels = label_number(accuracy = 0.01))+
  #theme(text = element_text(size = 55))+
  expand_limits(y=0.008)+
  geom_text(x=0.95, y=0.008, label="a) Araneae",size=20)

#### Normality: Araneae Biomass ####
#2020
Araneae_Biomass_2020 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Araneae"))
ols_plot_resid_hist(Araneae_Biomass_2020) 
ols_test_normality(Araneae_Biomass_2020) #normal

#2021
Araneae_Biomass_2021 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Araneae"))
ols_plot_resid_hist(Araneae_Biomass_2021) 
ols_test_normality(Araneae_Biomass_2021) #normal

#2022
Araneae_Biomass_2022 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Araneae"))
ols_plot_resid_hist(Araneae_Biomass_2022) 
ols_test_normality(Araneae_Biomass_2022) #normalish

#### Stats: Plot Weights by Grazing Treatment####
#2020
Aranaea_Biomass_2020_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Araneae"))
anova(Aranaea_Biomass_2020_Glmm) #not significant

#2021
Aranaea_Biomass_2021_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Araneae"))
anova(Aranaea_Biomass_2021_Glmm) #not significant

#2022
Aranaea_Biomass_2022_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Araneae"))
anova(Aranaea_Biomass_2022_Glmm) #not significant


#### Coleoptera Biomass ####
Coleoptera_Weight<-ggplot(subset(Order_Weight, Correct_Order=="Coleoptera"),aes(x=Year,y=Average_Weight,fill=Grazing_Treatment))+
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position = position_dodge(0.85), width = 0.5,size=1)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = "NONE")+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55))+
  # scale_y_continuous(labels = label_number(accuracy = 0.01))+
  #theme(text = element_text(size = 55))+
  expand_limits(y=0.008)+
  geom_text(x=0.95, y=0.008, label="b) Coleoptera",size=20)

#### Normality: Coleoptera Biomass ####
#2020
Coleoptera_Biomass_2020 <- lm(sqrt(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Coleoptera"))
ols_plot_resid_hist(Coleoptera_Biomass_2020) 
ols_test_normality(Coleoptera_Biomass_2020) #normal

#2021
Coleoptera_Biomass_2021 <- lm(sqrt(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Coleoptera"))
ols_plot_resid_hist(Coleoptera_Biomass_2021) 
ols_test_normality(Coleoptera_Biomass_2021) #normal

#2022
Coleoptera_Biomass_2022 <- lm(sqrt(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Coleoptera"))
ols_plot_resid_hist(Coleoptera_Biomass_2022) 
ols_test_normality(Coleoptera_Biomass_2022) #normalish

#### Stats: Coleoptera Biomass ####
#2020
Coleoptera_Biomass_2020_Glmm <- lmer(sqrt(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Coleoptera"))
anova(Coleoptera_Biomass_2020_Glmm) #not significant

#2021
Coleoptera_Biomass_2021_Glmm <- lmer(sqrt(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Coleoptera"))
anova(Coleoptera_Biomass_2021_Glmm) #not significant

#2022
Coleoptera_Biomass_2022_Glmm <- lmer(sqrt(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Coleoptera"))
anova(Coleoptera_Biomass_2022_Glmm) #not significant

# Diptera
Diptera_Weight<-ggplot(subset(Order_Weight, Correct_Order=="Diptera"),aes(x=Year,y=Average_Weight,fill=Grazing_Treatment))+
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position = position_dodge(0.85), width = 0.5,size=1)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = c(0.75,0.75),legend.title=element_blank(),legend.text = element_text(size=40))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55))+
  # scale_y_continuous(labels = label_number(accuracy = 0.01))+
  #theme(text = element_text(size = 55))+
  expand_limits(y=0.008)+
  geom_text(x=0.95, y=0.008, label="b) Diptera",size=20)

#### START HERE Add in stats here ####

# Hemiptera
Hemiptera_Weight<-ggplot(subset(Order_Weight, Correct_Order=="Hemiptera"),aes(x=Year,y=Average_Weight,fill=Grazing_Treatment))+
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position = position_dodge(0.85), width = 0.5,size=1)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = c(0.75,0.75),legend.title=element_blank(),legend.text = element_text(size=40))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55))+
  # scale_y_continuous(labels = label_number(accuracy = 0.01))+
  #theme(text = element_text(size = 55))+
  expand_limits(y=0.008)+
  geom_text(x=0.95, y=0.008, label="b) Hemiptera",size=20)

# Hymenoptera
Hymenoptera_Weight<-ggplot(subset(Order_Weight, Correct_Order=="Hymenoptera"),aes(x=Year,y=Average_Weight,fill=Grazing_Treatment))+
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position = position_dodge(0.85), width = 0.5,size=1)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = c(0.75,0.75),legend.title=element_blank(),legend.text = element_text(size=40))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55))+
  # scale_y_continuous(labels = label_number(accuracy = 0.01))+
  #theme(text = element_text(size = 55))+
  expand_limits(y=0.008)+
  geom_text(x=0.95, y=0.008, label="b) Hymenoptera",size=20)

# Orthoptera
Orthoptera_Weight<-ggplot(subset(Order_Weight, Correct_Order=="Orthoptera"),aes(x=Year,y=Average_Weight,fill=Grazing_Treatment))+
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position = position_dodge(0.85), width = 0.5,size=1)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = c(0.75,0.75),legend.title=element_blank(),legend.text = element_text(size=40))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55))+
  # scale_y_continuous(labels = label_number(accuracy = 0.01))+
  #theme(text = element_text(size = 55))+
  expand_limits(y=0.008)+
  geom_text(x=0.95, y=0.008, label="b) Orthoptera",size=20)

# Lepidoptera
Lepidoptera_Weight<-ggplot(subset(Order_Weight, Correct_Order=="Lepidoptera"),aes(x=Year,y=Average_Weight,fill=Grazing_Treatment))+
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position = position_dodge(0.85), width = 0.5,size=1)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = c(0.75,0.75),legend.title=element_blank(),legend.text = element_text(size=40))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55))+
  # scale_y_continuous(labels = label_number(accuracy = 0.01))+
  #theme(text = element_text(size = 55))+
  expand_limits(y=0.008)+
  geom_text(x=0.95, y=0.008, label="b) Lepidoptera",size=20)

# Thysanoptera
Thysanoptera_Weight<-ggplot(subset(Order_Weight, Correct_Order=="Thysanoptera"),aes(x=Year,y=Average_Weight,fill=Grazing_Treatment))+
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position = position_dodge(0.85), width = 0.5,size=1)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = c(0.75,0.75),legend.title=element_blank(),legend.text = element_text(size=40))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55))+
  # scale_y_continuous(labels = label_number(accuracy = 0.01))+
  #theme(text = element_text(size = 55))+
  expand_limits(y=0.008)+
  geom_text(x=0.95, y=0.008, label="b) Thysanoptera",size=20)

# Trombiculidae
Trombiculidae_Weight<-ggplot(subset(Order_Weight, Correct_Order=="Trombiculidae"),aes(x=Year,y=Average_Weight,fill=Grazing_Treatment))+
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position = position_dodge(0.85), width = 0.5,size=1)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = c(0.75,0.75),legend.title=element_blank(),legend.text = element_text(size=40))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55))+
  # scale_y_continuous(labels = label_number(accuracy = 0.01))+
  #theme(text = element_text(size = 55))+
  expand_limits(y=0.008)+
  geom_text(x=0.95, y=0.008, label="b) Trombiculidae",size=20)

# Neuroptera
Neuroptera_Weight<-ggplot(subset(Order_Weight, Correct_Order=="Neuroptera"),aes(x=Year,y=Average_Weight,fill=Grazing_Treatment))+
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position = position_dodge(0.85), width = 0.5,size=1)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Biomass (g)")+
  theme(legend.background=element_blank(),legend.position = c(0.75,0.75),legend.title=element_blank(),legend.text = element_text(size=40))+
  scale_fill_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Cattle Removal","Destock Grazing","High Impact Grazing"),labels=c("Cattle Removal","Destock Grazing","High Impact Grazing"))+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(axis.title.y=element_text(size=55),axis.text.y=element_text(size=55),axis.title.x=element_text(size=55),axis.text.x=element_text(size=55))+
  # scale_y_continuous(labels = label_number(accuracy = 0.01))+
  #theme(text = element_text(size = 55))+
  expand_limits(y=0.008)+
  geom_text(x=0.95, y=0.008, label="b) Neuroptera",size=20)

