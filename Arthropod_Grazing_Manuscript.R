#### Grazing x Arthropod Data - 2020 - 2022#
#### Code created by: Kathryn Bloodworth #

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
#install.packages('devtools')
library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
#install.packages("pairwiseAdonis")
library(pairwiseAdonis)
library(ggpattern)

#set working directory - UMD mac
setwd("/Users/kjbloodw/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/Insect_Data")

# Set Working Directory - Mac
setwd("~/Box-Box/Projects/Dissertation/Data/Insect_Data")

# Set Working Directory - PC
setwd("C:/Users/kjbloodw/Box/Projects/Dissertation/Data/Insect_Data")


#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank())

#### Load in data ####
#make sure column names are consistent 

#ID Data
ID_Data_20<-read.csv("2020_Sweep_Net_Dvac_Data_FK.csv",header=T) %>% 
  #make all collection methods the same across years
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method))) %>% 
  #rename sample column so that it's the same across years
  rename(Sample_Number="Sample") %>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Order,Family,Genus,Species,Notes) %>% 
  filter(Collection_Method=="dvac")

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

ID_Data_22<-read.csv("2022_Sweep_Net_D-Vac_Data_FK.csv",header=T) %>% 
  #make all collection methods the same across years
  mutate(Collection_Method=ifelse(Collection_Method=="Dvac","dvac",ifelse(Collection_Method=="Sweep_Net","sweep",Collection_Method))) %>% 
  #rename sample column so that it's the same across years
  rename(Sample_Number="Sample")%>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Order,Family,Genus,Species,Notes)%>% 
  filter(Collection_Method=="dvac")


#Weight Data
Weight_Data_20<-read.csv("2020_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  rename(Sample_Number=Sample_num) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))%>% 
  filter(Collection_Method=="dvac")

Weight_Data_21<-read.csv("2021_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))%>% 
  filter(Collection_Method=="dvac")

Weight_Data_22<-read.csv("2022_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))%>% 
  filter(Collection_Method=="dvac")

#Arthropod Feeding Guild Data
Feeding<-read.csv("Arthropod_ID_Data_Guilds.csv", header=T)

#Plant Species Comp Data 
PlantComp<-read.csv("Plant_Species_Comp_2022.csv",header=T) 


Functional_Groups<-read.csv("FunctionalGroups.csv")


#### Formatting and Cleaning ID Data ####

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


ID_22<-ID_Data_22 %>% 
  #Change block and grazing treatment to be consistent and match plot numbers
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>%
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Order=ifelse(Order=="araneae","Araneae",
                              ifelse(Order=="coleoptera","Coleoptera",
                                     ifelse(Order=="diptera","Diptera",
                                            ifelse(Order=="hemiptera","Hemiptera",
                                                   ifelse(Order=="hymenoptera","Hymenoptera",
                                                          ifelse(Order=="lepidoptera","Lepidoptera",
                                                                 ifelse(Order=="neuroptera","Neuroptera",
                                                                        ifelse(Order=="orthoptera","Orthoptera",
                                                                               ifelse(Order=="thysanoptera","Thysanoptera",
                                                                                      ifelse(Order=="unknown","Unknown",Order))))))))))) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Family=ifelse(Family=="aphididae", "Aphididae",ifelse(Family=="asilidae", "Asilidae",ifelse(Family=="Ceraphionidae","Ceraphronidae",ifelse(Family=="chloropidae","Chloropidae",ifelse(Family=="Chrionomidae","Chironomidae",ifelse(Family=="chrysididae","Chrysididae",ifelse(Family=="Cicadellidea","Cicadellidae",ifelse(Family=="coccinellidae","Coccinellidae",ifelse(Family=="Coccinelliadae","Coccinellidae",ifelse(Family=="culicidae","Culicidae",ifelse(Family=="curculionidae","Curculionidae",ifelse(Family=="Diapriidea","Diapriidae",ifelse(Family=="Euiophidae","Eulophidae",ifelse(Family=="eupelmidae","Eupelmidae",ifelse(Family=="ichneumonidae","Ichneumonidae",ifelse(Family=="latridiidae","Latridiidae",ifelse(Family=="lycosidae","Lycosidae",ifelse(Family=="muscidae","Muscidae",ifelse(Family=="myrmeleontidae","Myrmeleontidae",ifelse(Family=="nabidae","Nabidae",ifelse(Family=="pentatomidae","Pentatomidae",ifelse(Family=="perilampidae","Perilampidae",ifelse(Family=="platygastridae","Platygastridae",ifelse(Family=="scarabaeidae","Scarabaeidae",ifelse(Family=="Scarabacidae","Scarabaeidae",ifelse(Family=="sepsidae","Sepsidae",ifelse(Family=="tomisidae","Thomisidae",ifelse(Family=="Thripinae","Thripidae",ifelse(Family=="Thrips","Thripidae",ifelse(Family=="Tiombiculidae","Trombiculidae",ifelse(Family=="tingidae","Tingidae",ifelse(Family=="trichoceridae","Trichoceridae",ifelse(Family=="Trichoceridea","Trichoceridae",ifelse(Family=="unknown","Unknown",ifelse(Family=="",NA,ifelse(Family=="N/A",NA,ifelse(Family=="n/a",NA,Family)))))))))))))))))))))))))))))))))))))) %>% 
  mutate(Correct_Genus=ifelse(Genus=="ageneotettix","Ageneotettix",ifelse(Genus=="arphia","Arphia",ifelse(Genus=="melanoplus","Melanoplus",ifelse(Genus=="opeia","Opeia",ifelse(Genus=="dissosteira","Dissosteira",ifelse(Genus=="Dissosteria","Dissosteira" ,ifelse(Genus=="Eritcttix","Eritettix",ifelse(Genus=="eritettix","Eritettix",ifelse(Genus=="Erotettix","Eritettix",ifelse(Genus=="phoetaliotes","Phoetaliotes",ifelse(Genus=="unknown","Unknown",ifelse(Genus=="",NA,ifelse(Genus=="N/A",NA,ifelse(Genus=="n/a",NA,Genus))))))))))))))) %>% 
  mutate(Correct_Species=ifelse(Species=="os","obscura",ifelse(Species=="pseudomietana","pseudonietana",ifelse(Species=="unknown","Unknown",ifelse(Species=="",NA,ifelse(Species=="N/A",NA,ifelse(Species=="n/a",NA,Species))))))) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Correct_Family,Correct_Genus,Correct_Species,Notes) %>% 
  mutate(Sample_Number=as.numeric(Sample_Number))

#Merge together data frames

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

#### Abundance by Count ####
Abundance<-ID_Data_Official %>% 
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order) %>% 
  mutate(Abundance=length(Sample_Number)) %>% 
  ungroup()

Abundance_Plot<-ID_Data_Official %>% 
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot) %>% 
  mutate(Plot_Abundance=length(Sample_Number)) %>% 
  ungroup() %>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Plot_Abundance) %>% 
  unique()

Abundance_Order<-Abundance %>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Correct_Order,Plot,Abundance) %>% 
  unique() 

#### Abundance by Count: Family ####
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

Weight_20<-Weight_Data_20 %>%
  #change blocks to be numeric
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  #Correct order spellings
  mutate(Correct_Order=ifelse(Order=="Aranaea","Araneae",ifelse(Order=="Aranea","Araneae",ifelse(Order=="Hempitera","Hemiptera",ifelse(Order=="Cicadellidae","Hemiptera",ifelse(Order=="Lyaceidae","Hemiptera",ifelse(Order=="","Orthoptera",Order))))))) %>%
  #fix NA issue related to body parts
  mutate(Correct_Order=ifelse(Notes=="Body Parts","Body_Parts",ifelse(Notes=="Body parts","Body_Parts",ifelse(Notes=="unknown","unknown",Correct_Order)))) %>%
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Dry_Weight_g,Notes)

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
  mutate(Coll_Year_Bl_Trt_Pl=ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-1","dvac_2020_1_NG-1",
                                    ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-2","dvac_2020_1_NG-2",
                                           ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-3","dvac_2020_1_NG-3",
                                                  ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-4","dvac_2020_1_NG-4",
                                                         ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-5","dvac_2020_1_NG-5",
                                                                ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-1","dvac_2020_1_LG-6",
                                                                       ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-2","dvac_2020_1_LG-7",
                                                                              ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-3","dvac_2020_1_LG-8",
                                                                                     ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-4","dvac_2020_1_LG-9",
                                                                                            ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-5","dvac_2020_1_LG-10",
                                                                                                   ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-1","dvac_2020_1_HG-11",
                                                                                                          ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-2","dvac_2020_1_HG-12",
                                                                                                                 ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-3","dvac_2020_1_HG-13",
                                                                                                                        ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-4","dvac_2020_1_HG-14",
                                                                                                                               ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-5","dvac_2020_1_HG-15",
                                                                                                                                      ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-1","dvac_2020_2_NG-16",
                                                                                                                                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-2","dvac_2020_2_NG-17",
                                                                                                                                                    ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-3","dvac_2020_2_NG-18",
                                                                                                                                                           ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-4","dvac_2020_2_NG-19",
                                                                                                                                                                  ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-5","dvac_2020_2_NG-20",
                                                                                                                                                                         ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-1","dvac_2020_2_LG-21",
                                                                                                                                                                                ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-2","dvac_2020_2_LG-22",
                                                                                                                                                                                       ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-3","dvac_2020_2_LG-23",
                                                                                                                                                                                              ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-4","dvac_2020_2_LG-24",
                                                                                                                                                                                                     ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-5","dvac_2020_2_LG-25",
                                                                                                                                                                                                            ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-1","dvac_2020_2_HG-26",
                                                                                                                                                                                                                   ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-2","dvac_2020_2_HG-27",
                                                                                                                                                                                                                          ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-3","dvac_2020_2_HG-28",
                                                                                                                                                                                                                                 ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-4","dvac_2020_2_HG-29",
                                                                                                                                                                                                                                        ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-5","dvac_2020_2_HG-30",    
                                                                                                                                                                                                                                               ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-1","dvac_2020_3_NG-31",
                                                                                                                                                                                                                                                      ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-2","dvac_2020_3_NG-32",
                                                                                                                                                                                                                                                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-3","dvac_2020_3_NG-33",
                                                                                                                                                                                                                                                                    ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-4","dvac_2020_3_NG-34",
                                                                                                                                                                                                                                                                           ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-5","dvac_2020_3_NG-35",
                                                                                                                                                                                                                                                                                  ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-1","dvac_2020_3_LG-36",
                                                                                                                                                                                                                                                                                         ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-2","dvac_2020_3_LG-37",
                                                                                                                                                                                                                                                                                                ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-3","dvac_2020_3_LG-38",
                                                                                                                                                                                                                                                                                                       ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-4","dvac_2020_3_LG-39",
                                                                                                                                                                                                                                                                                                              ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-5","dvac_2020_3_LG-40",
                                                                                                                                                                                                                                                                                                                     ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-1","dvac_2020_3_HG-41",
                                                                                                                                                                                                                                                                                                                            ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-2","dvac_2020_3_HG-42",
                                                                                                                                                                                                                                                                                                                                   ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-3","dvac_2020_3_HG-43",
                                                                                                                                                                                                                                                                                                                                          ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-4","dvac_2020_3_HG-44",
                                                                                                                                                                                                                                                                                                                                                 ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-5","dvac_2020_3_HG-45", 
                                                                                                                                                                                                                                                                                                                                                        Coll_Year_Bl_Trt_Pl)))))))))))))))))))))))))))))))))))))))))))))) %>% 
  dplyr::select(Coll_Year_Bl_Trt_Pl,Sample_Number,Correct_Order,Dry_Weight_g,Notes) %>% 
  #RemovNAs from Dry weight
  filter(!is.na(Dry_Weight_g)) %>% 
  separate(Coll_Year_Bl_Trt_Pl, c("Coll_Year_Bl_Trt","Plot"), "-") 



#### Formatting and Cleaning Plant Species Data ####
#Create Long dataframe from wide dataframe and fix species issues
LongCov_SpComp<-gather(PlantComp,key="species","cover",12:70) %>% 
  dplyr::select(block,plot,grazing_treatment,added_total_excel,species,cover) %>% 
  filter(!species %in% c("dung","moss","rock","lichen","mushroom","litter","bare_ground","final_total","final_total_excel","Longpod.mustard..Erysimum.asperum..","Lygo.deomia","basal.rosette" )) %>%
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_PlantSp<-LongCov_SpComp%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(block,plot,grazing_treatment,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_PlantSp$plot<-as.factor(Relative_Cover_PlantSp$plot)

Relative_Cover_PlantSp_Clean<-Relative_Cover_PlantSp%>% 
  #change species codes to full species names
  mutate(Genus_Species=ifelse(species=="ALDE","Alyssum.desertorum",ifelse(species=="ANOC","Androsace.occidentalis",ifelse(species=="ANPA","Antennaria.parvifolia", ifelse(species=="ARDR","Artemisia.dracunculus",ifelse(species=="ARFR","Artemisia.frigida",ifelse(species=="ARPU","Aristida.purpurea",ifelse(species=="ASGR","Astragalus.gracilis",ifelse(species=="ASPU","Astragalus.purshii",ifelse(species=="BODA","Bouteloua.dactyloides",ifelse(species=="BOGR" ,"Bouteloua.gracilis",ifelse(species=="BRAR","Bromus.arvensis",ifelse(species=="BRTE","Bromus.tectorum",ifelse(species=="CADU","Carex.duriuscula",ifelse(species=="CAFI","Carex.filifolia",ifelse(species=="CHPR","Chenopodium.pratericola",ifelse(species=="COCA","Conyza.canadensis",ifelse(species=="DEPI","Descurainia.pinnata",ifelse(species=="HECO","Hesperostipa.comata",ifelse(species=="VUOC","Vulpia.octoflora",ifelse(species=="KOMA","Koeleria.macrantha",ifelse(species=="LOAR","Logfia.arvensis",ifelse(species=="LYJU","Lygodesmia.juncea",ifelse(species=="DRRE","Draba.reptans",ifelse(species=="HEHI","Hedeoma.hispida",ifelse(species=="LEDE","Lepidium.densiflorum",ifelse(species=="LIIN","Lithospermum.incisum",ifelse(species=="LIPU","Liatris.punctata",ifelse(species=="PEES","Pediomelum.esculentum", ifelse(species=="SPCR","Sporobolus.cryptandrus",ifelse(species=="POSE","Poa.secunda",ifelse(species=="SPCO","Sphaeralcea.coccinea",ifelse(species=="TRDU","Tragopogon.dubius",ifelse(species=="TAOF","Taraxacum.officinale",ifelse(species=="OESU","Oenotherea.suffrutescens", ifelse(species=="PASM","Pascopyrum.smithii",ifelse(species=="PLPA","Plantago.patagonica",ifelse(species== "OPPO","Opuntia.polyacantha",ifelse(species=="DECA","Dalea.candida",species))))))))))))))))))))))))))))))))))))))) %>% 
  dplyr::select(block,plot,grazing_treatment,Genus_Species,Relative_Cover) %>% 
  unique()

#Merge Relative Cover data and functional group data
RelCov_FunctionalGroups<-Relative_Cover_PlantSp_Clean %>% 
  full_join(Functional_Groups, relationship="many-to-many") %>% 
  filter(Relative_Cover!="NA")

#### Plot Level Arthropod Abundance by Grazing Treatment ####

## Weight
#Summing all weights by order within dataset, grazing treatment, block, and plot so that we can look at differences in order across plots
Weight_Data_Summed<-aggregate(Dry_Weight_g~Coll_Year_Bl_Trt+Plot+Correct_Order, data=Weight_Data_Official, FUN=sum, na.rm=FALSE) 

#Separating out Treatment_Plot into all distinctions again so that we can group based on different things
Weight_Data_Summed<-Weight_Data_Summed %>% 
  separate(Coll_Year_Bl_Trt, c("Collection_Method","Year","Block","Grazing_Treatment"), "_")

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
  mutate(Correct_Order="Plot")

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




#### Figure 2: (A,B): Average Plot Weight, (C,D): Order Proportion by Weight, (E,F): Order Proportion by Cover ####

##reorder bar graphs##
Weight_by_Grazing_dvac$Grazing_Treatment <- factor(Weight_by_Grazing_dvac$Grazing_Treatment, levels = c("NG", "LG", "HG"))

# 2020 Average Plot Weight
Dvac_2020_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2020),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.6, y=0.5, label="A. 2020 Plot Weight",size=20)

# Average Plot Weight
Dvac_2021_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2021),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.6, y=0.5, label="B. 2021 Plot Weight",size=20)+
  #no grazing is different than high grazing, low grazing is different than high grazing, no and low grazing are the same
  annotate("text",x=1,y=0.14,label="a",size=20)+ #no grazing
  annotate("text",x=2,y=0.12,label="a",size=20)+ #low grazing
  annotate("text",x=3,y=0.07,label="b",size=20) #high grazing

# Average Plot Weight
Dvac_2022_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2022),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55))+
  geom_text(x=1.6, y=0.5, label="C. 2022 Plot Weight",size=20)


# Proportion of Orders by Weight
Order_Weight_2020<-ggplot(subset(Relative_Weight,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.9, y=1.2, label="D. Abundance by Weight",size=20)

Order_Weight_2021<-ggplot(subset(Relative_Weight,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.9, y=1.2,label="E.Abundance by Weight",size=20)

Order_Weight_2022<-ggplot(subset(Relative_Weight,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#66676C","#403025","#B89984","#CABEB9","#72544D"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.9, y=1.2, label="F.Abundance by Weight",size=20)

Order_Count_2020<-ggplot(subset(Relative_Count,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.8, y=1.2,label="G.Abundance by Count",size=20)

Order_Count_2021<-ggplot(subset(Relative_Count,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.8, y=1.2, label="H.Abundance by Count",size=20)

Order_Count_2022<-ggplot(subset(Relative_Count,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#66676C","#403025","#B89984","#CABEB9","#72544D"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.8, y=1.2, label="I.Abundance by Count",size=20)

#### Create Figure 2 ####
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
Plot_Weight_D_2020_Glmm <- lmer(sqrt(Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2020))
anova(Plot_Weight_D_2020_Glmm) #not significant

#2021
Plot_Weight_D_2021_Glmm <- lmer(log(Plot_Weight) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed_dvac,Year==2021))
summary(Plot_Weight_D_2021_Glmm)
anova(Plot_Weight_D_2021_Glmm) # p=0.003987
###post hoc test for lmer test ##
summary(glht(Plot_Weight_D_2021_Glmm, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.0.56774), #LG-HG (0.00857), NG-HG (0.00256)

#2022
Plot_Weight_D_2022_Glmm <- lmer(log(Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2022))
anova(Plot_Weight_D_2022_Glmm) #not significant



#### Calculate Community Metrics: Weight Abundance ####
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
  summarize(Richness_Std=sd(richness),Richness_Mean=mean(richness),Richness_n=length(richness),
            Shannon_Std=sd(Shannon),Shannon_Mean=mean(Shannon),Shannon_n=length(Shannon),
            Evar_Std=sd(Evar,na.rm=T),Evar_Mean=mean(Evar,na.rm=T),Evar_n=length(Evar))%>%
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n),
         Shannon_St_Error=Shannon_Std/sqrt(Shannon_n),
         Evar_St_Error=Evar_Std/sqrt(Evar_n)) %>% 
  ungroup %>% 
  mutate(Grazing_Treatment_Fig=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

#### Figure 1: Diversity based on Arthropod Order Weight ####
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
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="A. 2020",size=20)

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
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=1, label="B. 2021",size=20)+
  #no grazing is different than high grazing, low grazing is not different than high grazing, no and low grazing not different
  annotate("text",x=1,y=0.38,label="a",size=20)+ #no grazing
  annotate("text",x=2,y=0.6,label="ab",size=20)+ #low grazing
  annotate("text",x=3,y=0.8,label="b",size=20) #high grazing


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
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=1, label="C. 2022",size=20)

#### Create Figure 1 ####
Shannon_2020_Weight+  
  Shannon_2021_Weight+
  Shannon_2022_Weight+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4000x2000

#### Normality: Order Shannon: Weight ####

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

#### Stats: Shannon's Diversity by Grazing Treatment: Weight####

# 2020 Weight
OrderShannon_2020_Glmm_Weight <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2020))
anova(OrderShannon_2020_Glmm_Weight) #not significant

# 2021 Weight
OrderShannon_2021_Glmm_Weight <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2021))
anova(OrderShannon_2021_Glmm_Weight) #0.005528
### post hoc test for lmer test ##
summary(glht(OrderShannon_2021_Glmm_Weight, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.09455), #LG-HG (0.09455), NG-HG (0.00178)

# 2022 Weight
OrderShannon_2022_Glmm_Weight <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2022))
anova(OrderShannon_2022_Glmm_Weight) #not significant


#### NMDS: By Order ####

#### Bray Curtis: By Order ####
#Create wide relative cover dataframe
Abundance_Wide_Weight<-Weight_Data_Summed %>%
  filter(!Correct_Order %in% c("Unknown","unknown", "Unknown_1","Body_Parts","Body Parts")) %>% 
  filter(Plot!="NA") %>% 
  spread(key=Correct_Order,value=Dry_Weight_g, fill=0) %>% 
  filter(Collection_Method=="dvac")

#### Make new data frame called BC_Data and run an NMDS 

BC_Data_Weight <- metaMDS(Abundance_Wide_Weight[,6:15])
#look at species signiciance driving NMDS 
intrinsics <- envfit(BC_Data_Weight, Abundance_Wide_Weight, permutations = 999)
head(intrinsics)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites <- 1:nrow(Abundance_Wide_Weight)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data_Weight <- Abundance_Wide_Weight[,1:5] #%>% 
#mutate(Trt_Year=paste(Grazing_Treatment,Year,sep="."))
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_Weight$points,col=as.factor(BC_Meta_Data_Weight$Year))

#Use the vegan ellipse function to make ellipses           
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_Weight,groups = as.factor(BC_Meta_Data_Weight$Year),kind = "se",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_Weight = data.frame(MDS1 = BC_Data_Weight$points[,1], MDS2 = BC_Data_Weight$points[,2],group=BC_Meta_Data_Weight$Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_Weight <- cbind(BC_Meta_Data_Weight,BC_NMDS_Weight)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_Weight<-ordiellipse(BC_Data_Weight, BC_Meta_Data_Weight$Year, display = "sites",
                                    kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_Weight <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_Weight$group)){
  BC_Ellipses_Weight <- rbind(BC_Ellipses_Weight, cbind(as.data.frame(with(BC_NMDS_Weight[BC_NMDS_Weight$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_Weight[[g]]$cov,BC_Ord_Ellipses_Weight[[g]]$center,BC_Ord_Ellipses_Weight[[g]]$scale)))
                                                        ,group=g))
}

#### NMDS: Weight: 2021 by Grazing ####

BC_Meta_Data_Weight_Grazing <- Abundance_Wide_Weight[,1:5] %>% 
  mutate(Trt_Year=paste(Grazing_Treatment,Year,sep="."))
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_Weight$points,col=as.factor(BC_Meta_Data_Weight_Grazing$Trt_Year))

#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_Weight,groups = as.factor(BC_Meta_Data_Weight_Grazing$Trt_Year),kind = "se",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_Weight_Grazing = data.frame(MDS1 = BC_Data_Weight$points[,1], MDS2 = BC_Data_Weight$points[,2],group=BC_Meta_Data_Weight_Grazing$Trt_Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_Weight_Grazing <- cbind(BC_Meta_Data_Weight_Grazing,BC_NMDS_Weight_Grazing)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_Weight_Grazing<-ordiellipse(BC_Data_Weight, BC_Meta_Data_Weight_Grazing$Trt_Year, display = "sites",
                                            kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_Weight_Grazing <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_Weight_Grazing$group)){
  BC_Ellipses_Weight_Grazing <- rbind(BC_Ellipses_Weight_Grazing, cbind(as.data.frame(with(BC_NMDS_Weight_Grazing[BC_NMDS_Weight_Grazing$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_Weight_Grazing[[g]]$cov,BC_Ord_Ellipses_Weight_Grazing[[g]]$center,BC_Ord_Ellipses_Weight_Grazing[[g]]$scale)))
                                                                        ,group=g))
}

#### Figure 3: NMDS  (A) by year and (B) 2021 by grazing treatment ####

##NMDS Figure: Year
#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
NMDS_Year<-ggplot(data = BC_NMDS_Graph_Weight, aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  geom_point(size=6, stroke = 2) +
  geom_path(data = BC_Ellipses_Weight, aes(x=NMDS1, y=NMDS2), size=4)+
  labs(color  = "", linetype = "", shape = "")+
  scale_color_manual(values=c("#413620","#9C6615","#C49B5A"),labels = c("2020","2021", "2022"),name="Year")+
  scale_linetype_manual(values=c("dashed","longdash","solid"),labels = c("2020","2021", "2022"),name="Year")+
  scale_shape_manual(values=c(0,1,2),labels = c("2020","2021", "2022"),name="Year")+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=1),colour=guide_legend(ncol=1),linetype=guide_legend(ncol=1))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  expand_limits(x=c(-2,2),y=c(-2,2))+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  scale_x_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),axis.title.x = element_text(size = 55),axis.text.x = element_text(size = 55),legend.position=c(0.16,0.82))
  
#annotate("text",x=-1,y=0,label="2020",size=20)+
  #annotate("text",x=0,y=0.3,label="2021",size=20)+
  #annotate("text",x=0.2,y=-0,label="2022",size=20) 

#NMSD Figure for 2021 by grazing treatment
NMDS_2021<-ggplot(data = subset(BC_NMDS_Graph_Weight_Grazing,group==c("HG.2021","LG.2021","NG.2021")), aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  geom_point(size=6, stroke = 2) +
  geom_path(data = subset(BC_Ellipses_Weight_Grazing,group==c("HG.2021","LG.2021","NG.2021")), aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "Grazing Regime", linetype = "Grazing Regime", shape = "Grazing Regime")+
  scale_color_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  scale_shape_manual(values=c(0,1,2), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  scale_linetype_manual(values=c("solid","longdash","dashed"),labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=1),colour=guide_legend(ncol=1),linetype=guide_legend(ncol=1))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  expand_limits(x=c(-2,2),y=c(-1,1))+
  scale_y_continuous(labels = label_number(accuracy = 0.5))+
  scale_x_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position=c(0.23,0.18))+
  annotate(geom="text", x=-1.7, y=1, label="B. 2021",size=20)
  #annotate("text",x=-0.5,y=0,label="Removal",size=20)+
  #annotate("text",x=0.5,y=0.2,label="Destock",size=20)+
  #annotate("text",x=0.8,y=-0.1,label="High",size=20) 

#### Create Figure 3 ####
NMDS_Year+
  NMDS_2021+
  plot_layout(ncol = 1,nrow = 2)
#save at 1500 x 2000

#### PERMANOVA: By Order: Weight ####

##PerMANOVA

#Make a new dataframe with the data from Wide_Relative_Cover all columns after 5
Species_Matrix_Weight <- Abundance_Wide_Weight[,6:ncol(Abundance_Wide_Weight)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_Weight <- Abundance_Wide_Weight[,1:5] %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

Environment_Matrix_Weight$Grazing_Treatment_Fact=as.factor(Environment_Matrix_Weight$Grazing_Treatment)
Environment_Matrix_Weight$Block_Fact=as.numeric(Environment_Matrix_Weight$Block)
Environment_Matrix_Weight$Plot_Fact=as.factor(Environment_Matrix_Weight$Plot)
Environment_Matrix_Weight$Year_Fact=as.factor(Environment_Matrix_Weight$Year)

#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
#Adding in the 'strata' function does not affect results - i can't figure out if I am doing in incorrectly or if they do not affect the results (seems unlikely though becuase everything is exactly the same)
PerMANOVA2_Weight <- adonis2(formula = Species_Matrix_Weight~Grazing_Treatment_Fact*Year_Fact + (1 | Block_Fact) , data=Environment_Matrix_Weight,permutations = 999, method = "bray")
#give a print out of the PermMANOVA
print(PerMANOVA2_Weight)  #Grazing (0.01), Year (0.001), GxYear (0.003)
#pairwise test
Posthoc_Weight_Year<-pairwise.adonis(Species_Matrix_Weight,factors=Environment_Matrix_Weight$Year, p.adjust.m = "BH")
Posthoc_Weight_Year   #2020-2021 (0.001), 2021-2022 (0.001), 2020-2022 (0.001)

Posthoc_Weight_Grazing_Year<-pairwise.adonis(Species_Matrix_Weight,factors=Environment_Matrix_Weight$Gr_Yr, p.adjust.m = "BH")
Posthoc_Weight_Grazing_Year #Significant: HG-NG (2021)


#### PERMDISP: By Order ####
Abundance_Wide_Weight_dispr<-Abundance_Wide_Weight %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

#Dvac
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_Weight <- vegdist(Species_Matrix_Weight)
#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_Results_Grazing_Weight <- betadisper(BC_Distance_Matrix_Weight,Abundance_Wide_Weight_dispr$Gr_Yr)
permutest(Dispersion_Results_Grazing_Weight,pairwise = T, permutations = 999) 

#### Supp Doc Figures ####
#### Supp Doc Figure 1. (A,B,C) Richness, (D,E,F) Evenness of Arthropod Community ####

# Richness 2020
Richness_2020<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2020),aes(x=Grazing_Treatment_Fig,y=Richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.x = element_blank(),axis.text.x = element_blank())+
  geom_text(x=1.8, y=8, label="A. 2020 Plot Richness",size=20)

# Richness 2021
#Graph of Weights from dvac by Grazing treatment- 2021
Richness_2021<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2021),aes(x=Grazing_Treatment_Fig,y=Richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1.8, y=8, label="B. 2021 Plot Richness",size=20)

# Richness 2022
Richness_2022<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2022),aes(x=Grazing_Treatment_Fig,y=Richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1.8, y=8, label="C. 2022 Plot Richness",size=20)

#Evenness

#Evenness 2020
Evenness_2020<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2020),aes(x=Grazing_Treatment_Fig,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.05))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.8, y=0.4, label="D. 2020 Plot Evenness",size=20)

# Evenness 2021
Evenness_2021<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2021),aes(x=Grazing_Treatment_Fig,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.05))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1.8, y=0.4, label="E. 2021 Plot Evenness",size=20)

# Evenness 2022
Evenness_2022<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2022),aes(x=Grazing_Treatment_Fig,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Order Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.05))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1.8, y=0.4, label="F. 2022 Plot Evenness",size=20)

#### Create Supp Doc Figure 1####
Richness_2020+
  Richness_2021+
  Richness_2022+
  Evenness_2020+  
  Evenness_2021+
  Evenness_2022+
  plot_layout(ncol = 3,nrow = 2)
#Save at 3000x2000

#### Normality: Order Richness ####

# Dvac 2020
dvac_2020_OrderRichness <- lm(data = subset(CommunityMetrics, Year == 2020 & Collection_Method=="dvac"),log(richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_OrderRichness) 
ols_test_normality(dvac_2020_OrderRichness) #normalish

# dvac 2021
dvac_2021_OrderRichness <- lm(data = subset(CommunityMetrics, Year == 2021 & Collection_Method=="dvac"),(richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_OrderRichness) 
ols_test_normality(dvac_2021_OrderRichness) #normalish

# dvac 2022
dvac_2022_OrderRichness <- lm(data = subset(CommunityMetrics, Year == 2022 & Collection_Method=="dvac"), (richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_OrderRichness) 
ols_test_normality(dvac_2022_OrderRichness) #normal

#### Stats: Richness ####

# 2020 Dvac
OrderRichness_D_2020_Glmm <- lmer(log(richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2020))
anova(OrderRichness_D_2020_Glmm) #not significant

# 2021 Dvac
OrderRichness_D_2021_Glmm <- lmer((richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2021 ))
anova(OrderRichness_D_2021_Glmm) #not significant 

# 2022 Dvac
OrderRichness_D_2022_Glmm <- lmer((richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2022))
anova(OrderRichness_D_2022_Glmm) #not significant

#### Normality: Order Evar: Weight ####

# Weight 2020
Weight_2020_OrderEvar <- lm(data = subset(CommunityMetrics_Weight, Year == 2020 & Collection_Method=="dvac"),1/(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2020_OrderEvar) 
ols_test_normality(Weight_2020_OrderEvar) #normalish

# Weight 2021
Weight_2021_OrderEvar <- lm(data = subset(CommunityMetrics_Weight, Year == 2021 & Collection_Method=="dvac"),log(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2021_OrderEvar) 
ols_test_normality(Weight_2021_OrderEvar) #normalish

# Weight 2022
Weight_2022_OrderEvar <- lm(data = subset(CommunityMetrics_Weight, Year == 2022 & Collection_Method=="dvac"),1/(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2022_OrderEvar) 
ols_test_normality(Weight_2022_OrderEvar) #normalish
#### Stats: Evenness ####

# 2020 Weight
OrderEvar_2020_Glmm_Weight <- lmer(1/(Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2020 ))
anova(OrderEvar_2020_Glmm_Weight) #not significant

# 2021 Weight
OrderEvar_2021_Glmm_Weight <- lmer((log(Evar)) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2021 ))
anova(OrderEvar_2021_Glmm_Weight) #not significant

# 2020 Weight
OrderEvar_2021_Glmm_Weight <- lmer((1/Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2021 ))
anova(OrderEvar_2021_Glmm_Weight) #not significant

#Relative Cover

#NMDS

#### Plant Species Analysis ####

#### Calculate Community Metrics ####
# uses codyn package and finds shannon's diversity 

#FK Diversity
Diversity_PlantSp <- community_diversity(df = RelCov_FunctionalGroups,
                                         replicate.var = "plot",
                                         abundance.var = "Relative_Cover")
#FK Evenness
Structure_PlantSp <- community_structure(df = RelCov_FunctionalGroups,
                                         replicate.var = "plot",
                                         abundance.var = "Relative_Cover",
                                         metric = "Evar") 

#Make a new data frame from "Extra_Species_Identity" to generate richness values for each research area
Richness_PlantSp<-RelCov_FunctionalGroups %>%  
  #group data frame by Watershed and exclosure
  group_by(grazing_treatment,plot,block) %>%
  #Make a new column named "Richness" and add the unique number of rows in the column "taxa" according to the groupings
  summarise(richness=length(Genus_Species)) %>%
  #stop grouping by watershed and exclosure
  ungroup()

#join the datasets
CommunityMetrics_PlantSp <- Diversity_PlantSp %>%
  full_join(Structure_PlantSp) %>% 
  select(-richness) %>% 
  full_join(Richness_PlantSp)

#make dataframe with averages
CommunityMetrics_PlantSp_Avg<-CommunityMetrics_PlantSp  %>% 
  group_by(grazing_treatment) %>%
  summarize(Richness_Std=sd(richness),Richness_Mean=mean(richness),Richness_n=length(richness),
            Shannon_Std=sd(Shannon),Shannon_Mean=mean(Shannon),Shannon_n=length(Shannon),
            Evar_Std=sd(Evar,na.rm=T),Evar_Mean=mean(Evar,na.rm=T),Evar_n=length(Evar))%>%
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n),
         Shannon_St_Error=Shannon_Std/sqrt(Shannon_n),
         Evar_St_Error=Evar_Std/sqrt(Evar_n)) %>% 
  ungroup()%>% 
  mutate(grazing_treatment_Fig=ifelse(grazing_treatment=="HG","High Impact Grazing",ifelse(grazing_treatment=="LG","Destock Grazing",ifelse(grazing_treatment=="NG","Cattle Removal",grazing_treatment))))

#### Supplemental Figure 2: Plant species (A) Richness, (B) Evenness, (C) Diversity, (D) Relative Cover, (E) NMDS  ####
##reorder bar graphs##
CommunityMetrics_PlantSp_Avg$grazing_treatment <- factor(CommunityMetrics_PlantSp_Avg$grazing_treatment, levels = c("NG", "LG", "HG"))

#### Richness Panel ####
Richness_PlantSp<-ggplot(CommunityMetrics_PlantSp_Avg,aes(x=grazing_treatment_Fig,y=Richness_Mean,fill=grazing_treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Plant Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=20)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.7, y=20, label="A.",size=20)

#### Diversity Panel ####
Shannon_PlantSp<-ggplot(CommunityMetrics_PlantSp_Avg,aes(x=grazing_treatment_Fig,y=Shannon_Mean,fill=grazing_treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Plant Diversity")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=3)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.7, y=3, label="B.",size=20)+
  #no grazing is different than high grazing, low grazing is different than high grazing, no and low grazing are the same
  annotate("text",x=1,y=2.3,label="a",size=20)+ #no grazing
  annotate("text",x=2,y=2.1,label="a",size=20)+ #low grazing
  annotate("text",x=3,y=2.5,label="b",size=20) #high grazing

#### Evenness Panel ####
Evar_PlantSp<-ggplot(CommunityMetrics_PlantSp_Avg,aes(x=grazing_treatment_Fig,y=Evar_Mean,fill=grazing_treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Plant Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.6)+
  scale_y_continuous(labels = label_number(accuracy = .01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.7, y=0.6, label="C.",size=20)+
  #no grazing is different than high grazing, low grazing is different than high grazing, no and low grazing are the same
  annotate("text",x=1,y=0.43,label="a",size=20)+ #no grazing
  annotate("text",x=2,y=0.42,label="a",size=20)+ #low grazing
  annotate("text",x=3,y=0.51,label="b",size=20) #high grazing

#### Relative Cover Panel ####
FG_RelCov_Avg<-RelCov_FunctionalGroups %>% 
  group_by(grazing_treatment) %>%
  summarize(RelCov_Std=sd(Relative_Cover),RelCov_Mean=mean(Relative_Cover),RelCov_n=length(Relative_Cover))%>%
  mutate(RelCov_St_Error=RelCov_Std/sqrt(RelCov_n)) %>% 
  ungroup()%>% 
  mutate(grazing_treatment_Fig=ifelse(grazing_treatment=="HG","High Impact Grazing",ifelse(grazing_treatment=="LG","Destock Grazing",ifelse(grazing_treatment=="NG","Cattle Removal",grazing_treatment))))

FG_RelCov_Avg$grazing_treatment <- factor(FG_RelCov_Avg$grazing_treatment, levels = c("NG", "LG", "HG"))

#Rel Cov
RelCov_PlantSp<-ggplot(FG_RelCov_Avg,aes(x=grazing_treatment_Fig,y=RelCov_Mean,fill=grazing_treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=RelCov_Mean-RelCov_St_Error,ymax=RelCov_Mean+RelCov_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Relative Cover")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=10)+
  scale_y_continuous(labels = label_number(accuracy = .01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.7, y=10, label="D.",size=20)+
  #NG and HG are different
  annotate("text",x=1,y=7.5,label="a",size=20)+ #no grazing
  annotate("text",x=2,y=8.5,label="ab",size=20)+ #low grazing
  annotate("text",x=3,y=8,label="b",size=20) #high grazing


#### NMDS Prep ####
RelCov_FunctionalGroups_Wide<-RelCov_FunctionalGroups %>%
  select(-c(Native_Introduced,Functional_Group,Annual_Perennial,Common.Name)) %>% 
  spread(key=Genus_Species,value=Relative_Cover, fill=0) 

#### Make new data frame called BC_Data and run an NMDS 

#dvac
BC_Data_PlantSp <- metaMDS(RelCov_FunctionalGroups_Wide[,4:41])
#look at species signiciance driving NMDS 
intrinsics <- envfit(BC_Data_PlantSp, RelCov_FunctionalGroups_Wide, permutations = 999)
head(intrinsics)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order Count
sites <- 1:nrow(RelCov_FunctionalGroups_Wide)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data_PlantSp <- RelCov_FunctionalGroups_Wide[,1:3] 
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_PlantSp$points,col=as.factor(BC_Meta_Data_PlantSp$grazing_treatment))

#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_PlantSp,groups = as.factor(BC_Meta_Data_PlantSp$grazing_treatment),kind = "se",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_PlantSp = data.frame(MDS1 = BC_Data_PlantSp$points[,1], MDS2 = BC_Data_PlantSp$points[,2],group=BC_Meta_Data_PlantSp$grazing_treatment)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_PlantSp <- cbind(BC_Meta_Data_PlantSp,BC_NMDS_PlantSp)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_PlantSp<-ordiellipse(BC_Data_PlantSp, BC_Meta_Data_PlantSp$grazing_treatment, display = "sites",
                                     kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_PlantSp <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_PlantSp$group)){
  BC_Ellipses_PlantSp <- rbind(BC_Ellipses_PlantSp, cbind(as.data.frame(with(BC_NMDS_PlantSp[BC_NMDS_PlantSp$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_PlantSp[[g]]$cov,BC_Ord_Ellipses_PlantSp[[g]]$center,BC_Ord_Ellipses_PlantSp[[g]]$scale)))
                                                          ,group=g))
}

#### NMDS Panel: Plant Community ####

#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
NMDS_PlantSp<-ggplot(data = BC_NMDS_Graph_PlantSp, aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = BC_Ellipses_PlantSp, aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "Grazing Regime", linetype = "Grazing Regime", shape = "Grazing Regime")+
  scale_color_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"),limits=c("NG","LG","HG"))+
  scale_linetype_manual(values=c("dashed","longdash","solid"), labels=c("Cattle Removal","Destock","High Impact Grazing"),limits=c("NG","LG","HG"))+
  scale_shape_manual(values=c(0,1,2), labels=c("Cattle Removal","Destock","High Impact Grazing"),limits=c("NG","LG","HG"))+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=1),colour=guide_legend(ncol=1),linetype=guide_legend(ncol=1))+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.position=c(0.63,0.88))+
  annotate(geom="text", x=-0.5, y=0.8, label="E.",size=20)


#### Create Supplemental Figure 2: Plant Metrics ####
Richness_PlantSp+
  Shannon_PlantSp+
  Evar_PlantSp+
  RelCov_PlantSp+
  NMDS_PlantSp+
  plot_layout(ncol = 3,nrow = 2)
#save at 3000x3000

#### Normality: Plant Species Community Metrics####
#Richness
Richness_PlantSp_Norm <- lm(data = CommunityMetrics_PlantSp,(richness)  ~ grazing_treatment)
ols_plot_resid_hist(Richness_PlantSp_Norm) 
ols_test_normality(Richness_PlantSp_Norm) #normal

#Shannon
Shannon_PlantSp_Norm <- lm(data = CommunityMetrics_PlantSp,(Shannon)  ~ grazing_treatment)
ols_plot_resid_hist(Shannon_PlantSp_Norm) 
ols_test_normality(Shannon_PlantSp_Norm) #normal

#Evar
Evar_PlantSp_Norm <- lm(data = CommunityMetrics_PlantSp,(Evar)  ~ grazing_treatment)
ols_plot_resid_hist(Evar_PlantSp_Norm) 
ols_test_normality(Evar_PlantSp_Norm) #normal


#### Stats: Plant Species Community Metrics####

# Richness
Richness_PlantSp_Glmm <- lmer((richness) ~ grazing_treatment + (1 | block) , data = CommunityMetrics_PlantSp)
anova(Richness_PlantSp_Glmm) #not significant

# Shannon
Shannon_PlantSp_Glmm <- lmer((Shannon) ~ grazing_treatment + (1 | block) , data = CommunityMetrics_PlantSp)
anova(Shannon_PlantSp_Glmm) #0.003476
# post hoc test for lmer test
summary(glht(Shannon_PlantSp_Glmm, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.16341), #LG-HG (8.98e-05), NG-HG (0.00815)

# Evar
Evar_PlantSp_Glmm <- lmer((Evar) ~ grazing_treatment + (1 | block) , data = CommunityMetrics_PlantSp)
anova(Evar_PlantSp_Glmm) #0.01525
# post hoc test for lmer test
summary(glht(Evar_PlantSp_Glmm, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.67105), #LG-HG (0.00606), NG-HG (0.01163)         

#### Stats: PERMANOVA: Plant Community  ####

##PerMANOVA
#Make a new dataframe with the data from Wide_Relative_Cover all columns after 5
Species_Matrix_PlantSp <- RelCov_FunctionalGroups_Wide[,4:ncol(RelCov_FunctionalGroups_Wide)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_PlantSp <- RelCov_FunctionalGroups_Wide[,1:3]

Environment_Matrix_PlantSp$Grazing_Treatment_Fact=as.factor(Environment_Matrix_PlantSp$grazing_treatment)
Environment_Matrix_PlantSp$Block_Fact=as.numeric(Environment_Matrix_PlantSp$block)
Environment_Matrix_PlantSp$Plot_Fact=as.factor(Environment_Matrix_PlantSp$plot)

#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
#Adding in the 'strata' function does not affect results - i can't figure out if I am doing in incorrectly or if they do not affect the results (seems unlikely though becuase everything is exactly the same)
PerMANOVA2_PlantSp <- adonis2(formula = Species_Matrix_PlantSp~Grazing_Treatment_Fact + (1 | Block_Fact) , data=Environment_Matrix_PlantSp,permutations = 999, method = "bray")
#give a print out of the PermMANOVA
print(PerMANOVA2_PlantSp)  #NS

#### Stats: PERMDISP: Plant Community  ####
#Dvac
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_PlantSp <- vegdist(Species_Matrix_PlantSp)
#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_Results_PlantSp <- betadisper(BC_Distance_Matrix_PlantSp,RelCov_FunctionalGroups_Wide$grazing_treatment)
permutest(Dispersion_Results_PlantSp,pairwise = T, permutations = 999) #NS

#### Normality: Relative Plant Community####
Normality_RelCov<- lm(data = RelCov_FunctionalGroups , log(Relative_Cover)  ~ grazing_treatment)
ols_plot_resid_hist(Normality_RelCov) 
ols_test_normality(Normality_RelCov) #not great but okay

#### Stats: Plant Relative Cover ####
RelCov_GLMM <- lmerTest::lmer(data = RelCov_FunctionalGroups , log(Relative_Cover) ~ grazing_treatment + (1|block))
anova(RelCov_GLMM, type = 3) #0.03647
# post hoc test for lmer test
summary(glht(RelCov_GLMM, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.5056), #LG-HG (0.1093), NG-HG (0.0351)


#### Feeding Guild Graph ####

Relative_Count_Family_Plot<-Abundance_Family_Guild %>% 
  filter(Plot!="NA" & Correct_Family!="NA") %>% 
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts" & Correct_Family!="Unknown") %>% 
  select(Year,Block,Grazing_Treatment,Plot,Correct_Order,Correct_Family,Guild,Abundance) %>% 
  unique() %>% 
  #add together all data of each orders across grazing treatments 
  group_by(Year,Grazing_Treatment,Guild,Plot) %>% 
  mutate(FeedingGuild_Abundance=sum(Abundance)) %>%
  ungroup() %>% 
  #add together all data within each grazing treatment for total "plot"count
  group_by(Year,Grazing_Treatment,Plot) %>% 
  mutate(Total_Abundance=sum(Abundance)) %>%
  ungroup() %>% 
  select(Year,Block,Grazing_Treatment,Guild,FeedingGuild_Abundance,Total_Abundance,Plot) %>% 
  unique() %>% 
  mutate(RelativeCount=FeedingGuild_Abundance/Total_Abundance) %>% 
  mutate(Trtm=paste(Grazing_Treatment,Guild,sep = "_"))

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
  ylab("Proportion of Feeding Guilds")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#714c42","#a0897b", "#9CA497","#c9d0c5","#798671","#1E3907"), name = "Feeding Guild")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=1.2,label="A.2020",size=20)

Feeding_Guild_2021<-ggplot(subset(Relative_Count_Family,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Guild, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Feeding Guilds")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#503930","#714c42","#a0897b", "#9CA497","#c9d0c5","#798671","#4e6b5d","#1E3907"), name = "Feeding Guild")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1, y=1.2,label="B.2021",size=20)

Feeding_Guild_2022<-ggplot(subset(Relative_Count_Family,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Guild, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Feeding Guilds")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#503930","#714c42","#a0897b", "#9CA497","#c9d0c5","#9CA497","#798671","#1E3907"), name = "Feeding Guild")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1, y=1.2,label="C.2022",size=20)

#### Create Feeding Guild Graph ####
Feeding_Guild_2020+
  Feeding_Guild_2021+
  Feeding_Guild_2022+
  plot_layout(ncol = 3,nrow = 1)
#Save at 3000x2000

  
#### Normality: Feeding_Guild Family ####

Normality_RelCov_Family_2020<- lm(data = subset(Relative_Count_Family_Plot, Year=="2020"), sqrt(RelativeCount)  ~ Grazing_Treatment*Guild)
ols_plot_resid_hist(Normality_RelCov_Family_2020) 
ols_test_normality(Normality_RelCov_Family_2020) #normal

Normality_RelCov_Family_2021<- lm(data = subset(Relative_Count_Family_Plot, Year=="2021"), log(RelativeCount)  ~ Grazing_Treatment*Guild)
ols_plot_resid_hist(Normality_RelCov_Family_2021) 
ols_test_normality(Normality_RelCov_Family_2021) #normal

Normality_RelCov_Family_2022<- lm(data = subset(Relative_Count_Family_Plot, Year=="2022"),log(RelativeCount)  ~ Grazing_Treatment*Guild)
ols_plot_resid_hist(Normality_RelCov_Family_2022) 
ols_test_normality(Normality_RelCov_Family_2022) #normal
  
#### Feeding Guild Stats ####
RelCov_Family_2020 <- lmerTest::lmer(data = subset(Relative_Count_Family_Plot, Year=="2020"), sqrt(RelativeCount)  ~ Grazing_Treatment*Guild + (1|Block))
anova(RelCov_Family_2020, type = 3) #feeding guild (<2e-16)

RelCov_Family_2021 <- lmerTest::lmer(data = subset(Relative_Count_Family_Plot, Year=="2021"), log(RelativeCount)  ~ Grazing_Treatment*Guild + (1|Block))
anova(RelCov_Family_2021, type = 3) #grazing (0.002), feeding guild (<2e-16), grazing:feeding guild (0.07)
summary(glht(RelCov_Family_2021, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #ns

RelCov_Family_2021_Trtm <- lmerTest::lmer(data = subset(Relative_Count_Family_Plot, Year=="2021"), sqrt(RelativeCount)  ~ Trtm + (1|Block))
anova(RelCov_Family_2021_Trtm, type = 3)
summary(glht(RelCov_Family_2021_Trtm, linfct = mcp(Trtm = "Tukey")), test = adjusted(type = "BH"))

RelCov_Family_2022 <- lmerTest::lmer(data = subset(Relative_Count_Family_Plot, Year=="2022"), sqrt(RelativeCount)  ~ Grazing_Treatment*Guild + (1|Block))
anova(RelCov_Family_2022, type = 3) #feeding guild (2e-16)

#### Averaging across paddocks before analyses ####
#### New way of analyzing data - KR Suggestions ####
#averaging plots by paddock (so replication will be 3 for each treatment)

#### Proportion by Abundance ####
## Abundance by Plot 
Abundance_Plot_Avg<-Abundance_Plot %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Collection_Method,Year,Paddock) %>% 
  mutate(Avg_Abundance=mean(Plot_Abundance)) %>% 
  ungroup() %>% 
  select(Collection_Method,Year,Block,Grazing_Treatment,Paddock,Avg_Abundance) %>% 
  unique()

## Abundance by Order
Abundance_Order_Avg<-Abundance_Order %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Collection_Method,Year,Paddock,Correct_Order) %>% 
  mutate(Avg_Abundance=mean(Abundance))%>% 
  ungroup() %>% 
  select(Collection_Method,Year,Block,Grazing_Treatment,Paddock,Correct_Order,Avg_Abundance) %>% 
  unique()

## Abundance by Family
Abundance_Family_Avg<-Abundance_Family_Guild %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Collection_Method,Year,Paddock,Correct_Order,Correct_Family,Guild) %>% 
  mutate(Avg_Abundance=mean(Abundance))%>% 
  ungroup() %>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Paddock,Correct_Order,Correct_Family,Guild,Avg_Abundance) %>% 
  unique() 

## Abundance by Feeding Guild
Abundance_Family_Guild_Avg<-Abundance_Family_Guild %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  filter(Guild!="") %>% 
  group_by(Year,Block,Grazing_Treatment,Paddock,Plot,Guild) %>% 
  summarise(PlotAbundance=sum(Abundance)) %>% 
  group_by(Year,Block,Grazing_Treatment,Paddock,Guild) %>% 
  summarise(Avg_Abundance=mean(PlotAbundance))%>% 
  ungroup() 




## Regular Abundance 
Abundance_Avg<-Abundance %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Collection_Method,Year,Paddock,Correct_Order) %>% 
  mutate(Avg_Abundance=mean(Abundance))%>% 
  ungroup() %>% 
  dplyr::select(Collection_Method,Year,Block,Paddock,Grazing_Treatment,Correct_Order,Avg_Abundance) %>% 
  unique() 


#### Average Biomass ####
Weight_Data_Summed_dvac_Avg<-Weight_Data_Summed_dvac %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Year,Paddock) %>% 
  mutate(Avg_Plot_Weight=mean(Plot_Weight)) %>% 
  select(Year,Block,Grazing_Treatment,Paddock,Avg_Plot_Weight) %>% 
  unique()


#### Proportion by Biomass ####

### Plot Level Abundance by Order by Grazing Treatment ###
Weight_by_Order_Dvac_Avg<-Weight_Data_Summed %>%  
  filter(Correct_Order!="Unknown_1") %>% 
  filter(Correct_Order!="Unknown") %>% 
  filter(Correct_Order!="unknown") %>% 
  filter(Correct_Order!="Snail") %>% 
  filter(Correct_Order!="Body_Parts") %>% 
  filter(Correct_Order!="Body Parts") %>% 
  filter(Plot!="NA") %>% 
  spread(key=Correct_Order,value=Dry_Weight_g, fill=0) %>% 
  gather(key="Correct_Order","Dry_Weight_g",6:15) %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Year,Paddock,Correct_Order) %>% 
  mutate(Avg_Dry_Weight_g=mean(Dry_Weight_g)) %>% 
  ungroup() %>% 
  select(Collection_Method,Year,Block,Grazing_Treatment,Paddock,Correct_Order,Avg_Dry_Weight_g) %>% 
  unique()


Weight_by_Order_Dvac_Avg_Summary<-Weight_by_Order_Dvac_Avg %>% 
  group_by(Collection_Method,Year, Grazing_Treatment, Correct_Order) %>%
  summarise(Average_Weight=mean(Avg_Dry_Weight_g),Weight_SD=sd(Avg_Dry_Weight_g),Weight_n=length(Avg_Dry_Weight_g)) %>%
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

#Summing weight data by plot and order, then averaging across plots, so we have one number per paddock per order
Weight_Data_Summed_Avg<-Weight_Data_Summed %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Year,Paddock) %>% 
  mutate(Avg_Dry_Weight_g=mean(Dry_Weight_g)) %>% 
  select(Collection_Method,Year,Block,Grazing_Treatment,Paddock,Correct_Order,Avg_Dry_Weight_g) %>% 
  unique()

#Summing weight data by plot then averaging across plots, so we have one number per paddock
Weight_Data_Summed_dvac_Avg<-Weight_Data_Summed %>% 
  filter(Collection_Method=="dvac") %>% 
  filter(Plot!="NA") %>% 
  #sum by plot 
  group_by(Year,Block,Grazing_Treatment,Plot) %>% 
  summarise(Plot_Weight=sum(Dry_Weight_g)) %>% 
  ungroup() %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Year,Paddock) %>% 
  mutate(Avg_Plot_Weight=mean(Plot_Weight)) %>% 
  select(Year,Block,Grazing_Treatment,Paddock,Avg_Plot_Weight) %>% 
  unique()


Weight_by_Grazing_dvac_Avg<-Weight_Data_Summed_dvac_Avg %>% 
  group_by(Year,Grazing_Treatment) %>% 
  summarise(Average_Weight=mean(Avg_Plot_Weight),Weight_SD=sd(Avg_Plot_Weight),Weight_n=length(Avg_Plot_Weight)) %>% 
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()%>% 
  mutate(Correct_Order="Plot")

#### Order Relative Weight ####

Relative_Weight_Avg<-Weight_by_Order_Dvac_Avg %>%  
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts") %>% 
  #add together all data of each orders across grazing treatments 
  group_by(Collection_Method,Year,Grazing_Treatment,Correct_Order) %>% 
  mutate(Order_Weight=sum(Avg_Dry_Weight_g)) %>%
  ungroup() %>% 
  #add together all data within each grazing treatment for total "plot" weight
  group_by(Year,Grazing_Treatment) %>% 
  mutate(Total_Weight=sum(Avg_Dry_Weight_g)) %>%
  ungroup() %>% 
  select(Year,Grazing_Treatment,Correct_Order,Order_Weight,Total_Weight) %>% 
  unique() %>% 
  mutate(RelativeWeight=Order_Weight/Total_Weight) %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_RelativeWeight=mean(RelativeWeight)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment)))) %>% 
  filter(Average_RelativeWeight!=0)

#### Order Relative Count ####

Relative_Count_Avg<-Abundance_Avg %>% 
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts") %>%
  #add together all data of each orders across grazing treatments 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  mutate(Order_Abundance=sum(Avg_Abundance)) %>%
  ungroup() %>% 
  #add together all data within each grazing treatment for total "plot"count
  group_by(Year,Grazing_Treatment) %>% 
  mutate(Total_Abundance=sum(Avg_Abundance)) %>%
  ungroup() %>% 
  select(Year,Grazing_Treatment,Correct_Order,Order_Abundance,Total_Abundance) %>% 
  unique() %>% 
  mutate(RelativeCount=Order_Abundance/Total_Abundance) %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_RelativeCount=mean(RelativeCount)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))%>% 
  filter(Average_RelativeCount!=0)





#### Figure 2: (A,B): Average Plot Weight, (C,D): Order Proportion by Weight, (E,F): Order Proportion by Cover ####

##reorder bar graphs##
Weight_by_Grazing_dvac_Avg$Grazing_Treatment <- factor(Weight_by_Grazing_dvac_Avg$Grazing_Treatment, levels = c("NG", "LG", "HG"))

# 2020 Average Plot Weight
Dvac_2020_Plot_Avg<-ggplot(subset(Weight_by_Grazing_dvac_Avg,Year==2020),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=0.5, label="(a)",size=20)

# Average Plot Weight
Dvac_2021_Plot_Avg<-ggplot(subset(Weight_by_Grazing_dvac_Avg,Year==2021),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=0.5, label="(b)",size=20)

# Average Plot Weight
Dvac_2022_Plot_Avg<-ggplot(subset(Weight_by_Grazing_dvac_Avg,Year==2022),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55))+
  geom_text(x=1, y=0.5, label="(c)",size=20)


# Proportion of Orders by Weight
Order_Weight_2020_Avg<-ggplot(subset(Relative_Weight_Avg,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=1.2, label="(d)",size=20)

Order_Weight_2021_Avg<-ggplot(subset(Relative_Weight_Avg,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=1.2,label="(e)",size=20)

Order_Weight_2022_Avg<-ggplot(subset(Relative_Weight_Avg,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#66676C","#403025","#B89984","#CABEB9","#72544D"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=1.2, label="(f)",size=20)

Order_Count_2020_Avg<-ggplot(subset(Relative_Count_Avg,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=1.2,label="(g)",size=20)

Order_Count_2021_Avg<-ggplot(subset(Relative_Count,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=1.2, label="(h)",size=20)

Order_Count_2022_Avg<-ggplot(subset(Relative_Count,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#66676C","#403025","#B89984","#CABEB9","#72544D"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=1.2, label="(i)",size=20)

#### Create Figure 2 ####
Dvac_2020_Plot_Avg+
  Dvac_2021_Plot_Avg+
  Dvac_2022_Plot_Avg+
  Order_Weight_2020_Avg +  
  Order_Weight_2021_Avg+
  Order_Weight_2022_Avg +
  Order_Count_2020_Avg +  
  Order_Count_2021_Avg+
  Order_Count_2022_Avg +
  plot_layout(ncol = 3,nrow = 3)
#save at 3000 x 3000

#### Normality: Plot Weights####
#2020
dvac_2020_Weight_Avg <- lm(data = subset(Weight_Data_Summed_dvac_Avg, Year == 2020), (Avg_Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_Weight_Avg) 
ols_test_normality(dvac_2020_Weight_Avg) #normal
#check for homoscedascity
leveneTest(data = subset(Weight_Data_Summed_dvac_Avg, Year == 2020), (Avg_Plot_Weight)  ~ Grazing_Treatment) 

#2021
dvac_2021_Weight_Avg <- lm(data = subset(Weight_Data_Summed_dvac_Avg, Year == 2021), (Avg_Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_Weight_Avg) 
ols_test_normality(dvac_2021_Weight_Avg) #normal
#check for homoscedascity
leveneTest(data = subset(Weight_Data_Summed_dvac_Avg, Year == 2021), (Avg_Plot_Weight)  ~ Grazing_Treatment) 

#2022
dvac_2022_Weight_Avg <- lm(data = subset(Weight_Data_Summed_dvac_Avg, Year == 2022), (Avg_Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_Weight_Avg) 
ols_test_normality(dvac_2022_Weight_Avg) #normal
#check for homoscedascity
leveneTest(data = subset(Weight_Data_Summed_dvac_Avg, Year == 2022), (Avg_Plot_Weight)  ~ Grazing_Treatment) 

#### Stats: Plot Weights by Grazing Treatment####
#2020
Plot_Weight_D_2020_Glmm_Avg <- lmer((Avg_Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac_Avg,Year==2020))
anova(Plot_Weight_D_2020_Glmm_Avg) #not significant

#2021
Plot_Weight_D_2021_Glmm_Avg <- lmer((Avg_Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac_Avg,Year==2021))
anova(Plot_Weight_D_2021_Glmm_Avg) #not significant

#2022
Plot_Weight_D_2022_Glmm_Avg <- lmer((Avg_Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac_Avg,Year==2022))
anova(Plot_Weight_D_2022_Glmm_Avg) #not significant


#### Calculate Community Metrics: Weight Abundance ####
# uses codyn package and finds shannon's diversity 
Weight_Data_Summed_2_Avg<-Weight_Data_Summed 
Diversity_Weight_Avg <- community_diversity(df = Weight_Data_Summed_2_Avg,
                                            time.var = "Year",
                                            replicate.var = c("Collection_Method","Block","Plot","Grazing_Treatment"),
                                            abundance.var = "Dry_Weight_g")
#Sweep Net Community Structure
Structure_Weight_Avg <- community_structure(df = Weight_Data_Summed_2_Avg,
                                            time.var = "Year",
                                            replicate.var = c("Collection_Method","Block","Plot","Grazing_Treatment"),
                                            abundance.var = "Dry_Weight_g",
                                            metric = "Evar")

#Make a new data frame from "Extra_Species_Identity" to generate richness values for each research area
Order_Richness_Weight_Avg<-ID_Data_Official %>%  
  select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order) %>% 
  unique() %>% 
  #group data frame by Watershed and exclosure
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot) %>%
  #Make a new column named "Richness" and add the unique number of rows in the column "taxa" according to the groupings
  summarise(richness=length(Correct_Order)) %>%
  #stop grouping by watershed and exclosure
  ungroup() %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Collection_Method,Year,Paddock) %>% 
  mutate(Avg_richness=mean(richness)) %>% 
  ungroup() %>% 
  select(Collection_Method,Year,Block,Grazing_Treatment,Paddock,Avg_richness) %>% 
  unique()

Order_Richness_Weight_Avg$Year=as.character(Order_Richness_Weight_Avg$Year)

#join the datasets
CommunityMetrics_Weight_Avg <- Diversity_Weight_Avg %>%
  full_join(Structure_Weight_Avg) %>% 
  select(-richness) %>% 
  full_join(Order_Richness_Weight_Avg) %>% 
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Collection_Method,Year,Paddock) %>% 
  mutate(Avg_richness=mean(Avg_richness)) %>% 
  mutate(Avg_Evar=mean(Evar)) %>% 
  mutate(Avg_Shannon=mean(Shannon)) %>% 
  ungroup() %>% 
  select(Collection_Method,Year,Block,Grazing_Treatment,Paddock,Avg_richness,Avg_Evar,Avg_Shannon) %>% 
  unique()

#make dataframe with averages
CommunityMetrics_Weight_Avg_Summary<-CommunityMetrics_Weight_Avg  %>% 
  group_by(Year,Grazing_Treatment) %>%
  summarize(Richness_Std=sd(Avg_richness),Richness_Mean=mean(Avg_richness),Richness_n=length(Avg_richness),
            Shannon_Std=sd(Avg_Shannon),Shannon_Mean=mean(Avg_Shannon),Shannon_n=length(Avg_Shannon),
            Evar_Std=sd(Avg_Evar,na.rm=T),Evar_Mean=mean(Avg_Evar,na.rm=T),Evar_n=length(Avg_Evar))%>%
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n),
         Shannon_St_Error=Shannon_Std/sqrt(Shannon_n),
         Evar_St_Error=Evar_Std/sqrt(Evar_n)) %>% 
  ungroup %>% 
  mutate(Grazing_Treatment_Fig=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

#### Figure 1: Diversity based on Arthropod Order Weight ####
##reorder bar graphs##
CommunityMetrics_Weight_Avg_Summary$Grazing_Treatment <- factor(CommunityMetrics_Weight_Avg_Summary$Grazing_Treatment, levels = c("NG", "LG", "HG"))


# 2020 
Shannon_2020_Weight_Avg<-ggplot(subset(CommunityMetrics_Weight_Avg_Summary,Year==2020),aes(x=Grazing_Treatment_Fig,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon Diversity")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="A. 2020",size=20)

# 2021 - Dvac
#Graph of Weights from dvac by Grazing treatment- 2021
Shannon_2021_Weight_Avg<-ggplot(subset(CommunityMetrics_Weight_Avg_Summary,Year==2021),aes(x=Grazing_Treatment_Fig,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon Diversity")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=1, label="B. 2021",size=20)+
  #no grazing is different than high grazing, low grazing is not different than high grazing, no and low grazing not different
  annotate("text",x=1,y=0.38,label="a",size=20)+ #no grazing
  annotate("text",x=2,y=0.6,label="b",size=20)+ #low grazing
  annotate("text",x=3,y=0.73,label="c",size=20) #high grazing


# 2022 - Dvac
Shannon_2022_Weight_Avg<-ggplot(subset(CommunityMetrics_Weight_Avg_Summary,Year==2022),aes(x=Grazing_Treatment_Fig,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon Diversity")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=1, label="C. 2022",size=20)

#### Create Figure 1####
Shannon_2020_Weight_Avg+  
  Shannon_2021_Weight_Avg+
  Shannon_2022_Weight_Avg+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4000x2000

#### Normality: Order Shannon: Weight ####

# Weight 2020
Weight_2020_OrderShannon <- lm(data = subset(CommunityMetrics_Weight_Avg, Year == 2020),(Avg_Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2020_OrderShannon) 
ols_test_normality(Weight_2020_OrderShannon) #normal
#check for homoscedascity
leveneTest(data = subset(CommunityMetrics_Weight_Avg, Year == 2020),(Avg_Shannon)  ~ Grazing_Treatment) 

# Weight 2021
Weight_2021_OrderShannon <- lm(data = subset(CommunityMetrics_Weight_Avg, Year == 2021),(Avg_Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2021_OrderShannon) 
ols_test_normality(Weight_2021_OrderShannon) #normal
#check for homoscedascity
leveneTest(data = subset(CommunityMetrics_Weight_Avg, Year == 2021),(Avg_Shannon)  ~ Grazing_Treatment)

# Weight 2020
Weight_2022_OrderShannon <- lm(data = subset(CommunityMetrics_Weight_Avg, Year == 2022),(Avg_Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2022_OrderShannon) 
ols_test_normality(Weight_2022_OrderShannon) #normal
#check for homoscedascity
leveneTest(data = subset(CommunityMetrics_Weight_Avg, Year == 2022),(Avg_Shannon)  ~ Grazing_Treatment)

#### Stats: Shannon's Diversity by Grazing Treatment: Weight####

# 2020 Weight
OrderShannon_2020_Glmm_Weight_Avg <- lmer((Avg_Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight_Avg,Year==2020))
anova(OrderShannon_2020_Glmm_Weight_Avg) #not significant

# 2021 Weight
OrderShannon_2021_Glmm_Weight_Avg <- lmer((Avg_Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight_Avg,Year==2021))
anova(OrderShannon_2021_Glmm_Weight_Avg) #0.01
### post hoc test for lmer test ##
summary(glht(OrderShannon_2021_Glmm_Weight_Avg, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.09455), #LG-HG (0.09455), NG-HG (0.00178)

# 2022 Weight
OrderShannon_2022_Glmm_Weight_Avg <- lmer((Avg_Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight_Avg,Year==2022))
anova(OrderShannon_2022_Glmm_Weight_Avg) #not significant


#### NMDS: By Order ####
#### Bray Curtis: By Order ####
#Create wide relative cover dataframe
Abundance_Wide_Weight_Avg<-Weight_Data_Summed_Avg %>%
  filter(!Correct_Order %in% c("Unknown","unknown", "Unknown_1","Body_Parts","Body Parts")) %>% 
  spread(key=Correct_Order,value=Avg_Dry_Weight_g, fill=0) %>% 
  filter(Collection_Method=="dvac") %>% 
  filter(Block!="NA")

#### Make new data frame called BC_Data and run an NMDS 

BC_Data_Weight_Avg <- metaMDS(Abundance_Wide_Weight_Avg[,6:15])

#look at species signiciance driving NMDS 
intrinsics <- envfit(BC_Data_Weight_Avg, Abundance_Wide_Weight_Avg, permutations = 999)
head(intrinsics)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites <- 1:nrow(Abundance_Wide_Weight_Avg)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data_Weight_Avg <- Abundance_Wide_Weight_Avg[,1:5] #%>% 
#mutate(Trt_Year=paste(Grazing_Treatment,Year,sep="."))
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_Weight_Avg$points,col=as.factor(BC_Meta_Data_Weight_Avg$Year))

#Use the vegan ellipse function to make ellipses           
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_Weight_Avg,groups = as.factor(BC_Meta_Data_Weight_Avg$Year),kind = "se",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_Weight_Avg = data.frame(MDS1 = BC_Data_Weight_Avg$points[,1], MDS2 = BC_Data_Weight_Avg$points[,2],group=BC_Meta_Data_Weight_Avg$Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_Weight_Avg <- cbind(BC_Meta_Data_Weight_Avg,BC_NMDS_Weight_Avg)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_Weight_Avg<-ordiellipse(BC_Data_Weight_Avg, BC_Meta_Data_Weight_Avg$Year, display = "sites",
                                        kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_Weight_Avg <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_Weight_Avg$group)){
  BC_Ellipses_Weight_Avg <- rbind(BC_Ellipses_Weight_Avg, cbind(as.data.frame(with(BC_NMDS_Weight_Avg[BC_NMDS_Weight_Avg$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_Weight_Avg[[g]]$cov,BC_Ord_Ellipses_Weight_Avg[[g]]$center,BC_Ord_Ellipses_Weight_Avg[[g]]$scale)))
                                                                ,group=g))
}

#### NMDS: Weight: 2021 by Grazing ####
BC_Meta_Data_Weight_Grazing_Avg <- Abundance_Wide_Weight_Avg[,1:5] %>% 
  mutate(Trt_Year=paste(Grazing_Treatment,Year,sep=".")) %>% 
  filter(Block!="NA")

BC_Data_Weight_Grazing_Avg <- metaMDS(Abundance_Wide_Weight_Avg[,6:15])

#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_Weight_Avg$points,col=as.factor(BC_Meta_Data_Weight_Grazing_Avg$Trt_Year))

#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_Weight_Avg,groups = as.factor(BC_Meta_Data_Weight_Grazing_Avg$Trt_Year),kind = "se",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_Weight_Grazing_Avg = data.frame(MDS1 = BC_Data_Weight_Avg$points[,1], MDS2 = BC_Data_Weight_Avg$points[,2],group=BC_Meta_Data_Weight_Grazing_Avg$Trt_Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_Weight_Grazing_Avg <- cbind(BC_Meta_Data_Weight_Grazing_Avg,BC_NMDS_Weight_Grazing_Avg)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_Weight_Grazing_Avg<-ordiellipse(BC_Data_Weight_Avg, BC_Meta_Data_Weight_Grazing_Avg$Trt_Year, display = "sites",
                                                kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_Weight_Grazing_Avg <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_Weight_Grazing_Avg$group)){
  BC_Ellipses_Weight_Grazing_Avg <- rbind(BC_Ellipses_Weight_Grazing_Avg,cbind(as.data.frame(with(BC_NMDS_Weight_Grazing_Avg[BC_NMDS_Weight_Grazing_Avg$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_Weight_Grazing_Avg[[g]]$cov,BC_Ord_Ellipses_Weight_Grazing_Avg[[g]]$center,BC_Ord_Ellipses_Weight_Grazing_Avg[[g]]$scale)))
                                                                               ,group=g))
}


#### Figure 3: NMDS  (A) by year and (B) 2021 by grazing treatment ####

##NMDS Figure: Year
#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
NMDS_Year_Avg<-ggplot(data = BC_NMDS_Graph_Weight_Avg, aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  geom_point(size=6, stroke = 2) +
  geom_path(data = BC_Ellipses_Weight_Avg, aes(x=NMDS1, y=NMDS2), size=4)+
  labs(color  = "", linetype = "", shape = "")+
  scale_color_manual(values=c("#413620","#9C6615","#C49B5A"),labels = c("2020","2021", "2022"),name="Year")+
  scale_linetype_manual(values=c("dashed","longdash","solid"),labels = c("2020","2021", "2022"),name="Year")+
  scale_shape_manual(values=c(0,1,2),labels = c("2020","2021", "2022"),name="Year")+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=1),colour=guide_legend(ncol=1),linetype=guide_legend(ncol=1))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  expand_limits(x=c(-2,2),y=c(-2,2))+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  scale_x_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),axis.title.x = element_blank(),axis.text.x = element_blank(),legend.position=c(0.1,0.18))+
  annotate(geom="text", x=-3, y=2, label="A.",size=20)
#annotate("text",x=-1,y=0,label="2020",size=20)+
#annotate("text",x=0,y=0.3,label="2021",size=20)+
#annotate("text",x=0.2,y=-0,label="2022",size=20) 

#NMSD Figure for 2021 by grazing treatment
NMDS_2021<-ggplot(data = subset(BC_NMDS_Graph_Weight_Grazing_Avg,group==c("HG.2021","LG.2021","NG.2021")), aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  geom_point(size=6, stroke = 2) +
  geom_path(data = subset(BC_Ellipses_Weight_Grazing_Avg,group==c("HG.2021","LG.2021","NG.2021")), aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "Grazing Regime", linetype = "Grazing Regime", shape = "Grazing Regime")+
  scale_color_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  scale_shape_manual(values=c(0,1,2), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  scale_linetype_manual(values=c("solid","longdash","dashed"),labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=1),colour=guide_legend(ncol=1),linetype=guide_legend(ncol=1))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  expand_limits(x=c(-2,2),y=c(-1,1))+
  scale_y_continuous(labels = label_number(accuracy = 0.5))+
  scale_x_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position=c(0.23,0.18))+
  annotate(geom="text", x=-1.7, y=1, label="B. 2021",size=20)
#annotate("text",x=-0.5,y=0,label="Removal",size=20)+
#annotate("text",x=0.5,y=0.2,label="Destock",size=20)+
#annotate("text",x=0.8,y=-0.1,label="High",size=20) 

#### Create Figure 3 ####
NMDS_Year_Avg+
  NMDS_2021+
  plot_layout(ncol = 1,nrow = 2)
#save at 1500 x 2000

#### PERMANOVA: By Order: Weight ####

##PerMANOVA
Abundance_Wide_Weight_Avg<-Abundance_Wide_Weight_Avg %>% 
  filter(Block!="NA")

#Make a new dataframe with the data from Wide_Relative_Cover all columns after 5
Species_Matrix_Weight_Avg <- Abundance_Wide_Weight_Avg[,6:ncol(Abundance_Wide_Weight_Avg)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_Weight_Avg <- Abundance_Wide_Weight_Avg[,1:5] %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

Environment_Matrix_Weight_Avg$Grazing_Treatment_Fact=as.factor(Environment_Matrix_Weight_Avg$Grazing_Treatment)
Environment_Matrix_Weight_Avg$Block_Fact=as.numeric(Environment_Matrix_Weight_Avg$Block)
Environment_Matrix_Weight_Avg$Year_Fact=as.factor(Environment_Matrix_Weight_Avg$Year)

#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
#Adding in the 'strata' function does not affect results - i can't figure out if I am doing in incorrectly or if they do not affect the results (seems unlikely though becuase everything is exactly the same)
PerMANOVA2_Weight_Avg <- adonis2(formula = Species_Matrix_Weight_Avg~Grazing_Treatment_Fact*Year_Fact + (1 | Block_Fact) , data=Environment_Matrix_Weight_Avg,permutations = 999, method = "bray")
#give a print out of the PermMANOVA
print(PerMANOVA2_Weight_Avg)  #Grazing (0.01), Year (0.001), GxYear (0.003)
#pairwise test
Posthoc_Weight_Year_Avg<-pairwise.adonis(Species_Matrix_Weight_Avg,factors=Environment_Matrix_Weight_Avg$Year, p.adjust.m = "BH")
Posthoc_Weight_Year_Avg   #2020-2021 (0.001), 2021-2022 (0.001), 2020-2022 (0.001)

Posthoc_Weight_Grazing_Year_Avg<-pairwise.adonis(Species_Matrix_Weight_Avg,factors=Environment_Matrix_Weight_Avg$Gr_Yr, p.adjust.m = "BH")
Posthoc_Weight_Grazing_Year_Avg #NS


#### PERMDISP: By Order ####
Abundance_Wide_Weight_dispr_Avg<-Abundance_Wide_Weight_Avg %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

#Dvac
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_Weight_Avg <- vegdist(Species_Matrix_Weight_Avg)
#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_Results_Grazing_Weight_Avg <- betadisper(BC_Distance_Matrix_Weight_Avg,Abundance_Wide_Weight_dispr_Avg$Gr_Yr)
permutest(Dispersion_Results_Grazing_Weight_Avg,pairwise = T, permutations = 999) #ns

#### Supp Doc Figures ####
#### Supp Doc Figure 1. (A,B,C) Richness, (D,E,F) Evenness of Arthropod Community ####

# Richness 2020
Richness_2020_Avg<-ggplot(subset(CommunityMetrics_Weight_Avg_Summary,Year==2020),aes(x=Grazing_Treatment_Fig,y=Richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.x = element_blank(),axis.text.x = element_blank())+
  geom_text(x=1.8, y=8, label="A. 2020 Plot Richness",size=20)

# Richness 2021
#Graph of Weights from dvac by Grazing treatment- 2021
Richness_2021_Avg<-ggplot(subset(CommunityMetrics_Weight_Avg_Summary,Year==2021),aes(x=Grazing_Treatment_Fig,y=Richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1.8, y=8, label="B. 2021 Plot Richness",size=20)

# Richness 2022
Richness_2022_Avg<-ggplot(subset(CommunityMetrics_Weight_Avg_Summary,Year==2022),aes(x=Grazing_Treatment_Fig,y=Richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Order Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1.8, y=8, label="C. 2022 Plot Richness",size=20)

#Evenness

#Evenness 2020
Evenness_2020_Avg<-ggplot(subset(CommunityMetrics_Weight_Avg_Summary,Year==2020),aes(x=Grazing_Treatment_Fig,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.05))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.8, y=0.4, label="D. 2020 Plot Evenness",size=20)

# Evenness 2021
Evenness_2021_Avg<-ggplot(subset(CommunityMetrics_Weight_Avg_Summary,Year==2021),aes(x=Grazing_Treatment_Fig,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.05))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1.8, y=0.4, label="E. 2021 Plot Evenness",size=20)

# Evenness 2022
Evenness_2022_Avg<-ggplot(subset(CommunityMetrics_Weight_Avg_Summary,Year==2022),aes(x=Grazing_Treatment_Fig,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Order Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.05))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1.8, y=0.4, label="F. 2022 Plot Evenness",size=20)

#### Create Supp Doc Figure 1####
Richness_2020_Avg+
  Richness_2021_Avg+
  Richness_2022_Avg+
  Evenness_2020_Avg+  
  Evenness_2021_Avg+
  Evenness_2022_Avg+
  plot_layout(ncol = 3,nrow = 2)
#Save at 3000x2000

#### Normality: Order Richness ####

# Dvac 2020
dvac_2020_OrderRichness_Avg <- lm(data = subset(CommunityMetrics_Weight_Avg, Year == 2020 & Collection_Method=="dvac"),(Avg_richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_OrderRichness_Avg) 
ols_test_normality(dvac_2020_OrderRichness_Avg) #normalish
#check for homoscedascity
leveneTest(data = subset(CommunityMetrics_Weight_Avg, Year == 2020 & Collection_Method=="dvac"),(Avg_richness)  ~ Grazing_Treatment)

# dvac 2021
dvac_2021_OrderRichness_Avg <- lm(data = subset(CommunityMetrics_Weight_Avg, Year == 2021 & Collection_Method=="dvac"),(Avg_richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_OrderRichness_Avg) 
ols_test_normality(dvac_2021_OrderRichness_Avg) #normalish
#check for homoscedascity
leveneTest(data = subset(CommunityMetrics_Weight_Avg, Year == 2021 & Collection_Method=="dvac"),(Avg_richness)  ~ Grazing_Treatment)

# dvac 2022
dvac_2022_OrderRichness_Avg <- lm(data = subset(CommunityMetrics_Weight_Avg, Year == 2022 & Collection_Method=="dvac"), (Avg_richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_OrderRichness_Avg) 
ols_test_normality(dvac_2022_OrderRichness_Avg) #normal
#check for homoscedascity
leveneTest(data = subset(CommunityMetrics_Weight_Avg, Year == 2022& Collection_Method=="dvac"),(Avg_richness)  ~ Grazing_Treatment)

#### Stats: Richness ####

# 2020 Dvac
OrderRichness_D_2020_Glmm_Avg <- lmer((Avg_richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight_Avg,Year==2020))
anova(OrderRichness_D_2020_Glmm_Avg) #not significant

# 2021 Dvac
OrderRichness_D_2021_Glmm_Avg <- lmer((Avg_richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight_Avg,Year==2021 ))
anova(OrderRichness_D_2021_Glmm_Avg) #not significant 

# 2022 Dvac
OrderRichness_D_2022_Glmm_Avg <- lmer((Avg_richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight_Avg,Year==2022))
anova(OrderRichness_D_2022_Glmm_Avg) #not significant

#### Normality: Order Evar: Weight ####

# Weight 2020
Weight_2020_OrderEvar_Avg <- lm(data = subset(CommunityMetrics_Weight_Avg, Year == 2020 & Collection_Method=="dvac"),1/(Avg_Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2020_OrderEvar_Avg) 
ols_test_normality(Weight_2020_OrderEvar_Avg) #normalish
#check for homoscedascity
leveneTest(data = subset(CommunityMetrics_Weight_Avg, Year == 2020 & Collection_Method=="dvac"),1/(Avg_Evar)  ~ Grazing_Treatment)

# Weight 2021
Weight_2021_OrderEvar_Avg <- lm(data = subset(CommunityMetrics_Weight_Avg, Year == 2021 & Collection_Method=="dvac"),(Avg_Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2021_OrderEvar_Avg) 
ols_test_normality(Weight_2021_OrderEvar_Avg) #normalish
#check for homoscedascity
leveneTest(data = subset(CommunityMetrics_Weight_Avg, Year == 2021 & Collection_Method=="dvac"),(Avg_Evar)  ~ Grazing_Treatment)

# Weight 2022
Weight_2022_OrderEvar_Avg <- lm(data = subset(CommunityMetrics_Weight_Avg, Year == 2022 & Collection_Method=="dvac"),(Avg_Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2022_OrderEvar_Avg) 
ols_test_normality(Weight_2022_OrderEvar_Avg) #normalish
#check for homoscedascity
leveneTest(data = subset(CommunityMetrics_Weight_Avg, Year == 2022 & Collection_Method=="dvac"),(Avg_Evar)  ~ Grazing_Treatment)

#### Stats: Evenness ####

# 2019 Weight
OrderEvar_2020_Glmm_Weight_Avg <- lmer(1/(Avg_Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight_Avg,Year==2020 ))
anova(OrderEvar_2020_Glmm_Weight_Avg) #not significant

# 2021 Weight
OrderEvar_2021_Glmm_Weight_Avg <- lmer(((Avg_Evar)) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight_Avg,Year==2021 ))
anova(OrderEvar_2021_Glmm_Weight_Avg) #not significant

# 2022 Weight
OrderEvar_2022_Glmm_Weight_Avg <- lmer((Avg_Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight_Avg,Year==2022 ))
anova(OrderEvar_2021_Glmm_Weight_Avg) #not significant

#### Feeding Guild Graph ####

Relative_Count_Family_Plot_Avg<-Abundance_Family_Guild %>% 
  filter(Block!="NA" & Correct_Family!="NA") %>% 
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts" & Correct_Family!="Unknown") %>% 
  select(Year,Block,Plot,Grazing_Treatment,Guild,Abundance) %>% 
  #add together all data of each orders across grazing treatments 
  group_by(Year,Plot,Block,Grazing_Treatment,Guild) %>% 
  mutate(FeedingGuild_Abundance=sum(Abundance)) %>%
  ungroup() %>% 
  select(Year,Block,Plot,Grazing_Treatment,Guild,FeedingGuild_Abundance) %>% 
  unique() %>% 
  group_by(Year,Block,Grazing_Treatment,Guild) %>% 
  mutate(AverageAbundance=mean(FeedingGuild_Abundance)) %>%
  select(Year,Block,Grazing_Treatment,Guild,AverageAbundance) %>% 
  unique() %>% 
  #add together all data within each grazing treatment for total "plot"count
  group_by(Year,Block,Grazing_Treatment) %>% 
  mutate(Total_Abundance=sum(AverageAbundance)) %>%
  ungroup() %>% 
  mutate(RelativeCount=AverageAbundance/Total_Abundance) %>% 
  mutate(Trtm=paste(Grazing_Treatment,Guild,sep = "_"))

Relative_Count_Family_Avg<-Relative_Count_Family_Plot_Avg %>%
  select(Year,Block,Grazing_Treatment,Guild,AverageAbundance) %>% 
  group_by(Year,Grazing_Treatment,Guild) %>% 
  mutate(FeedingGuild_Abundance=sum(AverageAbundance)) %>%
  ungroup() %>% 
  #add together all data within each grazing treatment for total "plot"count
  group_by(Year,Grazing_Treatment) %>% 
  mutate(Total_Abundance=sum(AverageAbundance)) %>%
  ungroup() %>% 
  select(Year,Grazing_Treatment,Guild,FeedingGuild_Abundance,Total_Abundance) %>% 
  unique() %>% 
  mutate(RelativeCount=FeedingGuild_Abundance/Total_Abundance) %>% 
  group_by(Year,Grazing_Treatment,Guild) %>% 
  summarise(Average_RelativeCount=mean(RelativeCount)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))


##reorder bar graphs##
Relative_Count_Family_Avg$Grazing_Treatment <- factor(Relative_Count_Family_Avg$Grazing_Treatment, levels = c("Cattle Removal", "Destock Grazing", "High Impact Grazing"))
Relative_Count_Family_Avg$Guild <- factor(Relative_Count_Family_Avg$Guild, levels = c("Detritivore","Parasitoid","Predator","Leaf-Chewing Herbivore","Leaf-Mining Herbivore","Pollen/Nectar-Eating Herbivore","Sap-Sucking Herbivore","Wood-Eating Herbivore","Other Herbivore"))

Feeding_Guild_2020_Avg<-ggplot(subset(Relative_Count_Family_Avg,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Guild, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Feeding Guilds")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#714c42","#a0897b", "#9CA497","#c9d0c5","#798671","#1E3907"), name = "Feeding Guild")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1, y=1.2,label="A.2020",size=20)

Feeding_Guild_2021_Avg<-ggplot(subset(Relative_Count_Family_Avg,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Guild, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Feeding Guilds")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#503930","#714c42","#a0897b", "#9CA497","#c9d0c5","#798671","#4e6b5d","#1E3907"), name = "Feeding Guild")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1, y=1.2,label="B.2021",size=20)

Feeding_Guild_2022_Avg<-ggplot(subset(Relative_Count_Family_Avg,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Guild, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Feeding Guilds")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#503930","#714c42","#a0897b", "#9CA497","#c9d0c5","#9CA497","#798671","#1E3907"), name = "Feeding Guild")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1, y=1.2,label="C.2022",size=20)

#### Create Feeding Guild Graph ####
Feeding_Guild_2020_Avg+
  Feeding_Guild_2021_Avg+
  Feeding_Guild_2022_Avg+
  plot_layout(ncol = 3,nrow = 1)
#Save at 3000x2000


#### Normality: Feeding_Guild Family ####

Normality_RelCov_Family_2020_Avg<- lm(data = subset(Relative_Count_Family_Plot_Avg, Year=="2020"), log(RelativeCount)  ~ Grazing_Treatment*Guild)
ols_plot_resid_hist(Normality_RelCov_Family_2020_Avg) 
ols_test_normality(Normality_RelCov_Family_2020_Avg) #normal
#check for homoscedascity ---get NAs
#leveneTest(data = subset(Relative_Count_Family_Plot_Avg, Year=="2020"), log(RelativeCount)  ~ Grazing_Treatment*Feeding.Guild)

Normality_RelCov_Family_2021_Avg<- lm(data = subset(Relative_Count_Family_Plot_Avg, Year=="2021"), log(RelativeCount)  ~ Grazing_Treatment*Guild)
ols_plot_resid_hist(Normality_RelCov_Family_2021_Avg) 
ols_test_normality(Normality_RelCov_Family_2021_Avg) #normal
#check for homoscedascity --- get NAs
#leveneTest(data = subset(Relative_Count_Family_Plot_Avg, Year=="2021"), 1/log(RelativeCount)  ~ Grazing_Treatment*Feeding.Guild)

Normality_RelCov_Family_2022_Avg<- lm(data = subset(Relative_Count_Family_Plot_Avg, Year=="2022"),log(RelativeCount)  ~ Grazing_Treatment*Guild)
ols_plot_resid_hist(Normality_RelCov_Family_2022_Avg) 
ols_test_normality(Normality_RelCov_Family_2022_Avg) #normal
#check for homoscedascity - get NAs
#leveneTest(data = subset(Relative_Count_Family_Plot_Avg, Year=="2020"), 1/log(RelativeCount)  ~ Grazing_Treatment*Feeding.Guild)

#### Feeding Guild Stats ####

#2020
RelCov_Family_2020_Avg <- lmerTest::lmer(data = subset(Relative_Count_Family_Plot_Avg, Year=="2020"), log(RelativeCount)  ~ Grazing_Treatment*Guild + (1|Block))
anova(RelCov_Family_2020_Avg, type = 3)

#2021
RelCov_Family_2021_Avg <- lmerTest::lmer(data = subset(Relative_Count_Family_Plot_Avg, Year=="2021"), log(RelativeCount)  ~ Grazing_Treatment*Guild + (1|Block))
anova(RelCov_Family_2021_Avg, type = 3) 

#2022
RelCov_Family_2022_Avg <- lmerTest::lmer(data = subset(Relative_Count_Family_Plot_Avg, Year=="2022"), log(RelativeCount)  ~ Grazing_Treatment*Guild + (1|Block))
anova(RelCov_Family_2022_Avg, type = 3)
summary(glht(RelCov_Family_2022_Avg, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH"))

#### NMDS using Count x Feeding Guild ####

#Create wide relative cover dataframe
#Change row 54 and 55 where we don't cant equate sample number to weight to be unique sample number so it can be used here
#2020 block 1 NG, plot 3
Abundance[54, "Sample_Number"] <- 10
Abundance[55, "Sample_Number"] <- 11
#2021 LG, plot 8
Abundance[944, "Sample_Number"] <- 2
Abundance[945, "Sample_Number"] <- 3

Abundance_Wide_Count<-Abundance_Family_Avg %>%
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Paddock,Feeding.Guild) %>% 
  mutate(Guild_Abundance=sum(Avg_Abundance)) %>% 
  select(-c(Correct_Order,Correct_Family,Avg_Abundance)) %>% 
  unique() %>%
  filter(Feeding.Guild!="") %>% 
  spread(key=Feeding.Guild,value=Guild_Abundance, fill=0) 

Abundance_Wide_Count$Year=as.character(Abundance_Wide_Count$Year)

#dvac
BC_Data_Count <- metaMDS(Abundance_Wide_Count[,6:14])
#look at species signiciance driving NMDS 
intrinsics <- envfit(BC_Data_Count, Abundance_Wide_Count, permutations = 999)
head(intrinsics)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order Count
sites <- 1:nrow(Abundance_Wide_Count)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data_Count <- Abundance_Wide_Count[,1:5] 
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_Count$points,col=as.factor(BC_Meta_Data_Count$Year))

#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_Count,groups = as.factor(BC_Meta_Data_Count$Year),kind = "sd",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_Count = data.frame(MDS1 = BC_Data_Count$points[,1], MDS2 = BC_Data_Count$points[,2],group=BC_Meta_Data_Count$Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_Count <- cbind(BC_Meta_Data_Count,BC_NMDS_Count)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_Count<-ordiellipse(BC_Data_Count, BC_Meta_Data_Count$Year, display = "sites",
                                   kind = "sd", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_Count <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_Count$group)){
  BC_Ellipses_Count <- rbind(BC_Ellipses_Count, cbind(as.data.frame(with(BC_NMDS_Count[BC_NMDS_Count$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_Count[[g]]$cov,BC_Ord_Ellipses_Count[[g]]$center,BC_Ord_Ellipses_Count[[g]]$scale)))
                                                      ,group=g))
}

#### NMDS Figures: By Order: Count ####

#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
ggplot(data = BC_NMDS_Graph_Count, aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = BC_Ellipses_Count, aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "", linetype = "", shape = "")+
  scale_color_manual(values=c("skyblue3","springgreen3","brown"),labels = c("2020","2021", "2022"),name="")+
  scale_linetype_manual(values=c(1,2,3),labels = c("2020","2021", "2022"),name="")+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=2),colour=guide_legend(ncol=2),linetype=guide_legend(ncol=2))+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.position="none")+
  annotate(geom="text", x=-2, y=0.8, label="Count",size=20)
#export at 2000 x 1800


####NMDS: Count: 2021 by Feeding Guild ####

BC_Meta_Data_Count_Grazing <- Abundance_Wide_Count[,1:5] %>% 
  mutate(Trt_Year=paste(Grazing_Treatment,Year,sep="."))
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_Count$points,col=as.factor(BC_Meta_Data_Count_Grazing$Trt_Year))

#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_Count,groups = as.factor(BC_Meta_Data_Count_Grazing$Trt_Year),kind = "sd",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_Count_Grazing = data.frame(MDS1 = BC_Data_Count$points[,1], MDS2 = BC_Data_Count$points[,2],group=BC_Meta_Data_Count_Grazing$Trt_Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_Count_Grazing <- cbind(BC_Meta_Data_Count_Grazing,BC_NMDS_Count_Grazing)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_Count_Grazing<-ordiellipse(BC_Data_Count, BC_Meta_Data_Count_Grazing$Trt_Year, display = "sites",
                                           kind = "sd", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_Count_Grazing <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_Count_Grazing$group)){
  BC_Ellipses_Count_Grazing <- rbind(BC_Ellipses_Count_Grazing, cbind(as.data.frame(with(BC_NMDS_Count_Grazing[BC_NMDS_Count_Grazing$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_Count_Grazing[[g]]$cov,BC_Ord_Ellipses_Count_Grazing[[g]]$center,BC_Ord_Ellipses_Count_Grazing[[g]]$scale)))
                                                                      ,group=g))
}

#### NMDS Figures: By Order: Count ####

#2021
#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
ggplot(data = subset(BC_NMDS_Graph_Count_Grazing,group==c("HG.2021","LG.2021","NG.2021")), aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = subset(BC_Ellipses_Count_Grazing,group==c("HG.2021","LG.2021","NG.2021")), aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "", linetype = "", shape = "")+
  scale_color_manual(values=c("thistle2","thistle3","thistle4"), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  scale_shape_manual(values=c(15,16,17), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  scale_linetype_manual(values=c(1,2,3),labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=2),colour=guide_legend(ncol=2),linetype=guide_legend(ncol=2))+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.position="none")+
  annotate(geom="text", x=-2, y=0.8, label="2021 Count",size=20)
#export at 2000 x 1800

#2022
#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
ggplot(data = subset(BC_NMDS_Graph_Count_Grazing,group==c("HG.2022","LG.2022","NG.2022")), aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = subset(BC_Ellipses_Count_Grazing,group==c("HG.2022","LG.2022","NG.2022")), aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "", linetype = "", shape = "")+
  scale_color_manual(values=c("thistle2","thistle3","thistle4"), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("HG.2022","LG.2022","NG.2022"))+
  scale_shape_manual(values=c(15,16,17), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("HG.2022","LG.2022","NG.2022"))+
  scale_linetype_manual(values=c(1,2,3),labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("HG.2022","LG.2022","NG.2022"))+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=2),colour=guide_legend(ncol=2),linetype=guide_legend(ncol=2))+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.position="none")+
  annotate(geom="text", x=-1, y=0.8, label="2022 Count",size=20)
#export at 2000 x 1800


#### PERMANOVA: By Order: Count ####

##PerMANOVA

#Make a new dataframe with the data from Wide_Relative_Cover all columns after 5
Species_Matrix_Count <- Abundance_Wide_Count[,6:ncol(Abundance_Wide_Count)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_Count <- Abundance_Wide_Count[,1:5] %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

Environment_Matrix_Count$Grazing_Treatment_Fact=as.factor(Environment_Matrix_Count$Grazing_Treatment)
Environment_Matrix_Count$Block_Fact=as.numeric(Environment_Matrix_Count$Block)
Environment_Matrix_Count$Year_Fact=as.factor(Environment_Matrix_Count$Year)

#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
#Adding in the 'strata' function does not affect results - i can't figure out if I am doing in incorrectly or if they do not affect the results (seems unlikely though becuase everything is exactly the same)
PerMANOVA2_Count <- adonis2(formula = Species_Matrix_Count~Grazing_Treatment_Fact*Year_Fact + (1 | Block_Fact) , data=Environment_Matrix_Count,permutations = 999, method = "bray")
#give a print out of the PermMANOVA
print(PerMANOVA2_Count)  #Grazing (0.01), Year (0.001), GxYear (0.003)
#pairwise test
Posthoc_Count_Year<-pairwise.adonis(Species_Matrix_Count,factors=Environment_Matrix_Count$Year, p.adjust.m = "BH")
Posthoc_Count_Year   #2020-2021 (0.001), 2021-2022 (0.001), 2020-2022 (0.001)
#pairwise test
Posthoc_Count_Grazing<-pairwise.adonis(Species_Matrix_Count,factors=Environment_Matrix_Count$Grazing_Treatment, p.adjust.m = "BH")
Posthoc_Count_Grazing  #ns

Posthoc_Count_Grazing_Year<-pairwise.adonis(Species_Matrix_Count,factors=Environment_Matrix_Count$Gr_Yr, p.adjust.m = "BH")
Posthoc_Count_Grazing_Year #Significant: HG-NG (2021)


#### PERMDISP: By Order ####
Abundance_Wide_Count_dispr<-Abundance_Wide_Count %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

#Dvac
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_Count <- vegdist(Species_Matrix_Count)
#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_Results_Grazing_Count <- betadisper(BC_Distance_Matrix_Count,Abundance_Wide_Count_dispr$Gr_Yr)
permutest(Dispersion_Results_Grazing_Count,pairwise = T, permutations = 999) 


#### Plant Species Analysis ####

#### Calculate Community Metrics ####
# uses codyn package and finds shannon's diversity 

RelCov_FunctionalGroups_Avg<-RelCov_FunctionalGroups %>%  
  mutate(Paddock=paste(block,grazing_treatment,sep="-")) %>% 
  select(-c(Native_Introduced,Functional_Group,Annual_Perennial,Common.Name)) %>% 
  spread(key=Genus_Species,value=Relative_Cover, fill=0) %>% 
  pivot_longer(cols = Alyssum.desertorum:Vulpia.octoflora,
               names_to="Genus_Species",
               values_to="Relative_Cover") %>% 
  group_by(Paddock,Genus_Species) %>% 
  mutate(Avg_Relative_Cover=mean(Relative_Cover))  %>% 
  select(-c(Relative_Cover,plot)) %>% 
  unique() %>% 
  filter(Avg_Relative_Cover!=0)

#FK Diversity
Diversity_PlantSp <- community_diversity(df = RelCov_FunctionalGroups_Avg,
                                         replicate.var = "Paddock",
                                         abundance.var = "Avg_Relative_Cover")
#FK Evenness
Structure_PlantSp <- community_structure(df = RelCov_FunctionalGroups_Avg,
                                         replicate.var = "Paddock",
                                         abundance.var = "Avg_Relative_Cover",
                                         metric = "Evar") 

#Make a new data frame from "Extra_Species_Identity" to generate richness values for each research area
Richness_PlantSp<-RelCov_FunctionalGroups_Avg %>%  
  #group data frame by Watershed and exclosure
  group_by(grazing_treatment,block,Paddock) %>%
  #Make a new column named "Richness" and add the unique number of rows in the column "taxa" according to the groupings
  summarise(richness=length(Genus_Species)) %>%
  #stop grouping by watershed and exclosure
  ungroup()

#join the datasets
CommunityMetrics_PlantSp <- Diversity_PlantSp %>%
  full_join(Structure_PlantSp) %>% 
  select(-richness) %>% 
  full_join(Richness_PlantSp)

#make dataframe with averages
CommunityMetrics_PlantSp_Avg<-CommunityMetrics_PlantSp  %>% 
  group_by(grazing_treatment) %>%
  summarize(Richness_Std=sd(richness),Richness_Mean=mean(richness),Richness_n=length(richness),
            Shannon_Std=sd(Shannon),Shannon_Mean=mean(Shannon),Shannon_n=length(Shannon),
            Evar_Std=sd(Evar,na.rm=T),Evar_Mean=mean(Evar,na.rm=T),Evar_n=length(Evar))%>%
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n),
         Shannon_St_Error=Shannon_Std/sqrt(Shannon_n),
         Evar_St_Error=Evar_Std/sqrt(Evar_n)) %>% 
  ungroup()%>% 
  mutate(grazing_treatment_Fig=ifelse(grazing_treatment=="HG","High Impact Grazing",ifelse(grazing_treatment=="LG","Destock Grazing",ifelse(grazing_treatment=="NG","Cattle Removal",grazing_treatment))))

#### Supplemental Figure 2: Plant species (A) Richness, (B) Evenness, (C) Diversity, (D) Relative Cover, (E) NMDS  ####
##reorder bar graphs##
CommunityMetrics_PlantSp_Avg$grazing_treatment <- factor(CommunityMetrics_PlantSp_Avg$grazing_treatment, levels = c("NG", "LG", "HG"))

#### Richness Panel ####
Richness_PlantSp<-ggplot(CommunityMetrics_PlantSp_Avg,aes(x=grazing_treatment_Fig,y=Richness_Mean,fill=grazing_treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Plant Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=25)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.7, y=25, label="(a)",size=20)

#### Diversity Panel ####
Shannon_PlantSp<-ggplot(CommunityMetrics_PlantSp_Avg,aes(x=grazing_treatment_Fig,y=Shannon_Mean,fill=grazing_treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Plant Diversity")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=3)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.7, y=3, label="(b)",size=20)

#### Evenness Panel ####
Evar_PlantSp<-ggplot(CommunityMetrics_PlantSp_Avg,aes(x=grazing_treatment_Fig,y=Evar_Mean,fill=grazing_treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Plant Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = .01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.7, y=0.4, label="(c)",size=20)

#### Relative Cover Panel ####
FG_RelCov_Avg<-RelCov_FunctionalGroups_Avg %>% 
  group_by(grazing_treatment) %>%
  summarize(RelCov_Std=sd(Avg_Relative_Cover),RelCov_Mean=mean(Avg_Relative_Cover),RelCov_n=length(Avg_Relative_Cover))%>%
  mutate(RelCov_St_Error=RelCov_Std/sqrt(RelCov_n)) %>% 
  ungroup()%>% 
  mutate(grazing_treatment_Fig=ifelse(grazing_treatment=="HG","High Impact Grazing",ifelse(grazing_treatment=="LG","Destock Grazing",ifelse(grazing_treatment=="NG","Cattle Removal",grazing_treatment))))

FG_RelCov_Avg$grazing_treatment <- factor(FG_RelCov_Avg$grazing_treatment, levels = c("NG", "LG", "HG"))

#Rel Cov
RelCov_PlantSp<-ggplot(FG_RelCov_Avg,aes(x=grazing_treatment_Fig,y=RelCov_Mean,fill=grazing_treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=RelCov_Mean-RelCov_St_Error,ymax=RelCov_Mean+RelCov_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Evar"
  ylab("Relative Cover")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=7.5)+
  scale_y_continuous(labels = label_number(accuracy = .01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.7, y=7.5, label="(d)",size=20)


#### NMDS Prep ####
RelCov_FunctionalGroups_Wide<-RelCov_FunctionalGroups_Avg %>%
  spread(key=Genus_Species,value=Avg_Relative_Cover, fill=0) 

#### Make new data frame called BC_Data and run an NMDS 

#dvac
BC_Data_PlantSp <- metaMDS(RelCov_FunctionalGroups_Wide[,4:41])
#look at species signiciance driving NMDS 
intrinsics <- envfit(BC_Data_PlantSp, RelCov_FunctionalGroups_Wide, permutations = 999)
head(intrinsics)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order Count
sites <- 1:nrow(RelCov_FunctionalGroups_Wide)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data_PlantSp <- RelCov_FunctionalGroups_Wide[,1:3] 
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_PlantSp$points,col=as.factor(BC_Meta_Data_PlantSp$grazing_treatment))

#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_PlantSp,groups = as.factor(BC_Meta_Data_PlantSp$grazing_treatment),kind = "se",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_PlantSp = data.frame(MDS1 = BC_Data_PlantSp$points[,1], MDS2 = BC_Data_PlantSp$points[,2],group=BC_Meta_Data_PlantSp$grazing_treatment)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_PlantSp <- cbind(BC_Meta_Data_PlantSp,BC_NMDS_PlantSp)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_PlantSp<-ordiellipse(BC_Data_PlantSp, BC_Meta_Data_PlantSp$grazing_treatment, display = "sites",
                                     kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_PlantSp <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_PlantSp$group)){
  BC_Ellipses_PlantSp <- rbind(BC_Ellipses_PlantSp, cbind(as.data.frame(with(BC_NMDS_PlantSp[BC_NMDS_PlantSp$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_PlantSp[[g]]$cov,BC_Ord_Ellipses_PlantSp[[g]]$center,BC_Ord_Ellipses_PlantSp[[g]]$scale)))
                                                          ,group=g))
}

#### NMDS Panel: Plant Community ####

#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
NMDS_PlantSp<-ggplot(data = BC_NMDS_Graph_PlantSp, aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = BC_Ellipses_PlantSp, aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "Grazing Regime", linetype = "Grazing Regime", shape = "Grazing Regime")+
  scale_color_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"),limits=c("NG","LG","HG"))+
  scale_linetype_manual(values=c("dashed","longdash","solid"), labels=c("Cattle Removal","Destock","High Impact Grazing"),limits=c("NG","LG","HG"))+
  scale_shape_manual(values=c(0,1,2), labels=c("Cattle Removal","Destock","High Impact Grazing"),limits=c("NG","LG","HG"))+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=1),colour=guide_legend(ncol=1),linetype=guide_legend(ncol=1))+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.position=c(0.63,0.88))+
  annotate(geom="text", x=-0.45, y=0.8, label="(e)",size=20)


#### Create Supplemental Figure 2: Plant Metrics ####
Richness_PlantSp+
  Shannon_PlantSp+
  Evar_PlantSp+
  RelCov_PlantSp+
  NMDS_PlantSp+
  plot_layout(ncol = 3,nrow = 2)
#save at 3000x3000


#### Normality: Plant Species Community Metrics####
#Richness
Richness_PlantSp_Norm <- lm(data = CommunityMetrics_PlantSp,(richness)  ~ grazing_treatment)
ols_plot_resid_hist(Richness_PlantSp_Norm) 
ols_test_normality(Richness_PlantSp_Norm) #normal
#check for homoscedascity
leveneTest(data = CommunityMetrics_PlantSp,(richness)  ~ grazing_treatment)

#Shannon
Shannon_PlantSp_Norm <- lm(data = CommunityMetrics_PlantSp,(Shannon)  ~ grazing_treatment)
ols_plot_resid_hist(Shannon_PlantSp_Norm) 
ols_test_normality(Shannon_PlantSp_Norm) #normal
#check for homoscedascity
leveneTest(data = CommunityMetrics_PlantSp,(Shannon)  ~ grazing_treatment)

#Evar
Evar_PlantSp_Norm <- lm(data = CommunityMetrics_PlantSp,(Evar)  ~ grazing_treatment)
ols_plot_resid_hist(Evar_PlantSp_Norm) 
ols_test_normality(Evar_PlantSp_Norm) #normal
#check for homoscedascity
leveneTest(data = CommunityMetrics_PlantSp,(Evar)  ~ grazing_treatment)


#### Stats: Plant Species Community Metrics####

# Richness
Richness_PlantSp_Glmm <- lmer((richness) ~ grazing_treatment + (1 | block) , data = CommunityMetrics_PlantSp)
anova(Richness_PlantSp_Glmm) #not significant

# Shannon
Shannon_PlantSp_Glmm <- lmer((Shannon) ~ grazing_treatment + (1 | block) , data = CommunityMetrics_PlantSp)
anova(Shannon_PlantSp_Glmm) #NS

# Evar
Evar_PlantSp_Glmm <- lmer((Evar) ~ grazing_treatment + (1 | block) , data = CommunityMetrics_PlantSp)
anova(Evar_PlantSp_Glmm) #NS     

#### Stats: PERMANOVA: Plant Community  ####

##PerMANOVA
#Make a new dataframe with the data from Wide_Relative_Cover all columns after 5
Species_Matrix_PlantSp <- RelCov_FunctionalGroups_Wide[,4:ncol(RelCov_FunctionalGroups_Wide)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_PlantSp <- RelCov_FunctionalGroups_Wide[,1:3]

Environment_Matrix_PlantSp$Grazing_Treatment_Fact=as.factor(Environment_Matrix_PlantSp$grazing_treatment)
Environment_Matrix_PlantSp$Block_Fact=as.numeric(Environment_Matrix_PlantSp$block)

#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
#Adding in the 'strata' function does not affect results - i can't figure out if I am doing in incorrectly or if they do not affect the results (seems unlikely though becuase everything is exactly the same)
PerMANOVA2_PlantSp <- adonis2(formula = Species_Matrix_PlantSp~Grazing_Treatment_Fact + (1 | Block_Fact) , data=Environment_Matrix_PlantSp,permutations = 999, method = "bray")
#give a print out of the PermMANOVA
print(PerMANOVA2_PlantSp)  #NS

#### Stats: PERMDISP: Plant Community  ####
#Dvac
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_PlantSp <- vegdist(Species_Matrix_PlantSp)
#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_Results_PlantSp <- betadisper(BC_Distance_Matrix_PlantSp,RelCov_FunctionalGroups_Wide$grazing_treatment)
permutest(Dispersion_Results_PlantSp,pairwise = T, permutations = 999) #NS

#### Normality: Relative Plant Community####
Normality_RelCov<- lm(data = RelCov_FunctionalGroups_Avg ,log(Avg_Relative_Cover)  ~ grazing_treatment)
ols_plot_resid_hist(Normality_RelCov) 
ols_test_normality(Normality_RelCov) #not great but okay
#check for homoscedascity
leveneTest(data = RelCov_FunctionalGroups_Avg ,log(Avg_Relative_Cover)  ~ grazing_treatment)

#### Stats: Plant Relative Cover ####
RelCov_GLMM <- lmerTest::lmer(data = RelCov_FunctionalGroups_Avg , log(Avg_Relative_Cover) ~ grazing_treatment + (1|block))
anova(RelCov_GLMM, type = 3) #NS




#### CCA by Year: pseudo rep ####

CCA_Data <- Abundance_Wide_Weight[,6:15]

#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
CCA_Meta_Data <- Abundance_Wide_Weight[,1:5]

#run the CCA
CCA_Year <- cca(CCA_Data ~ Year, data=CCA_Meta_Data)
CCA_Year

#pull scores to use for subsequent univariate analyses
scores(CCA_Year, c(1:9), scaling=3)

#find out what percentage of the variation is explained by each axis
CCA_Year$CCA$eig/sum(CCA_Year$CCA$eig)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(CCA_Year)    #ns
#test significance by terms (= PerMANOVA)
anova(CCA_Year, by = "terms")  
#justifies subsequent univariate tests for axes that are significant
anova(CCA_Year, by = "axis")  

plot(CCA_Year)

#### CCA by Grazing Treatment: pseudo rep ####

#run the CCA
CCA_Grazing <- cca(CCA_Data ~ Grazing_Treatment, data=CCA_Meta_Data)
CCA_Grazing

#pull scores to use for subsequent univariate analyses
scores(CCA_Grazing, c(1:9), scaling=3)

#find out what percentage of the variation is explained by each axis
CCA_Grazing$CCA$eig/sum(CCA_Grazing$CCA$eig)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(CCA_Grazing)    #ns
#test significance by terms (= PerMANOVA)
anova(CCA_Grazing, by = "terms")  

plot(CCA_Grazing)


#### CCA by Year*Grazing Treatment: pseudo rep ####

#run the CCA
CCA_Year_Grazing <- cca(CCA_Data ~ Year*Grazing_Treatment, data=CCA_Meta_Data)
summary(CCA_Year_Grazing)

#pull scores to use for subsequent univariate analyses
scores(CCA_Year_Grazing, c(1:9), scaling=3)

#find out what percentage of the variation is explained by each axis
CCA_Year_Grazing$CCA$eig/sum(CCA_Year_Grazing$CCA$eig)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(CCA_Year_Grazing)    #ns
#test significance by terms (= PerMANOVA)
anova(CCA_Year_Grazing, by = "terms")  
#justifies subsequent univariate tests for axes that are significant
anova(CCA_Year_Grazing, by = "axis")  


# better control -- remember to set scaling etc identically
plot(CCA_Year_Grazing, type="n", scaling="sites")
text(CCA_Year_Grazing, dis="cn", scaling="sites",cex=1)
points(CCA_Year_Grazing, pch=21, col="black", bg="blue", cex=1.5, scaling="sites")
text(CCA_Year_Grazing, "species", col="red", cex=1, scaling="sites")


plot(CCA_Year_Grazing,pch = 30, cex = 4)
dev.off()

#### RDA Year: pseudo rep ####

RDA_Year<- rda(CCA_Data ~ Year, data=CCA_Meta_Data)
summary(RDA_Year)

##### DB RDA 
RDA_Year_capscale <- capscale(CCA_Data ~ Year*Grazing_Treatment + Condition(Block), data=CCA_Meta_Data, dist="bray", sqrt.dist=T)
summary(RDA_Year_capscale)
anova(RDA_Year_capscale) #is this a good model fit? yes bc significant 
anova(RDA_Year_capscale,  by = "terms") #type 2
anova(RDA_Year_capscale,  by = "margin")  #type 3

plot(RDA_Year_capscale)


plot(RDA_Year_capscale,display = "species",xlim=c(-1,1),ylim=c(-1,1))
dev.off()

plot(RDA_Year_capscale, type="n") # Empty plot
with(CCA_Meta_Data, levels(as.factor(Grazing_Treatment))) #look at levels of grazing treatment (must be factor)
scl <- 3 ## scaling = 3 (apply number of treatments to scl)
colvec <- c("darkblue", "slateblue2", "forestgreen") #create color palet
plot(RDA_Year_capscale, type = "n", scaling = scl,xlim=c(-6,2),ylim=c(-6,2)) #blank plot that will use  model data with 3 scales
with(CCA_Meta_Data, points(RDA_Year_capscale, display = "sites", col = colvec[as.factor(Grazing_Treatment)],scaling = scl, pch = 21, bg = colvec[as.factor(Grazing_Treatment)])) #add in points according to grazing treatments
text(RDA_Year_capscale, display = "species", scaling = scl, cex = 0.8, col = "black") #add in order text
with(CCA_Meta_Data, legend("topleft", legend = levels(as.factor(Grazing_Treatment)), bty = "n", col = colvec, pch = 21, pt.bg = colvec)) #add in a legend
with(CCA_Meta_Data, ordiellipse(RDA_Year_capscale, groups=as.factor(Grazing_Treatment), display="sites", alpha = 127,scaling = scl,kind = "se",conf=0.95, lwd=2,col = colvec[as.factor(Grazing_Treatment)],label = T,bg = colvec[as.factor(Grazing_Treatment)]))#### RDA not working ####
CCA_Meta_Data<-CCA_Meta_Data %>% 
  mutate(Year_Grazing=paste(Year,Grazing_Treatment, sep="_"))
with(CCA_Meta_Data, ordiellipse(RDA_Year_capscale, groups=as.factor(Year_Grazing), display="sites", alpha = 127,scaling = scl,kind = "se",conf=0.95, lwd=2,col = colvec[as.factor(Year_Grazing)],label = T,bg = colvec[as.factor(Year_Grazing)]))#### RDA not working ####

dev.off()



#pull scores to use for subsequent univariate analyses
scores(RDA_Year, c(1:4), scaling=3)

#do some stats ####New from Karin ####
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(RDA_Year)    #ns
#test significance by terms (= PerMANOVA)
anova(RDA_Year, by = "terms")  
#justifies subsequent univariate tests for axes that are significant
anova(RDA_Year, by = "axis")  



#### RDA Grazing: pseudo rep  ####

RDA_Grazing <- rda(CCA_Data ~ Grazing_Treatment, data=CCA_Meta_Data)

summary(RDA_Grazing)

#pull scores to use for subsequent univariate analyses
scores(RDA_Grazing, c(1:4), scaling=3)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(RDA_Grazing)    #ns
#test significance by terms (= PerMANOVA)
anova(RDA_Grazing, by = "terms")  

plot(RDA_Grazing)

#### RDA Year and Grazing: pseudo rep ####

RDA_Year_Grazing <- rda(CCA_Data ~ Year*Grazing_Treatment, data=CCA_Meta_Data)

summary(RDA_Year_Grazing)

#pull scores to use for subsequent univariate analyses
scores(RDA_Year_Grazing, c(1:4), scaling=3)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(RDA_Year_Grazing)    #ns
#test significance by terms (= PerMANOVA)
anova(RDA_Year_Grazing, by = "terms")  
#justifies subsequent univariate tests for axes that are significant
anova(RDA_Year_Grazing, by = "axis")  

plot(RDA_Year_Grazing)
dev.off()

# better control -- remember to set scaling etc identically
plot(RDA_Year_Grazing, type="n", scaling="sites",xlim = c(-2,2),ylim=c(-2,2))
text(RDA_Year_Grazing, dis="cn", scaling="sites",cex=1)
points(RDA_Year_Grazing, pch=21, col="black", bg="blue", cex=1.5, scaling="sites")
text(RDA_Year_Grazing, "species", col="red", cex=1, scaling="sites")



#### CCA by Year: n=3 ####

CCA_Data_Avg <- Abundance_Wide_Weight_Avg[,6:15]

#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
CCA_Meta_Data_Avg <- Abundance_Wide_Weight_Avg[,1:5]

#run the CCA
CCA_Year_Avg <- cca(CCA_Data_Avg ~ Year, data=CCA_Meta_Data_Avg)
CCA_Year_Avg

#pull scores to use for subsequent univariate analyses
scores(CCA_Year_Avg, c(1:8), scaling=3)

#find out what percentage of the variation is explained by each axis
CCA_Year_Avg$CCA$eig/sum(CCA_Year_Avg$CCA$eig)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(CCA_Year_Avg)    #ns
#test significance by terms (= PerMANOVA)
anova(CCA_Year_Avg, by = "terms")  
#justifies subsequent univariate tests for axes that are significant
anova(CCA_Year_Avg, by = "axis")  

plot(CCA_Year_Avg)

#### CCA by Grazing Treatment:  n=3  ####

#run the CCA
CCA_Grazing_Avg <- cca(CCA_Data_Avg ~ Grazing_Treatment, data=CCA_Meta_Data_Avg)
CCA_Grazing_Avg

#pull scores to use for subsequent univariate analyses
scores(CCA_Grazing_Avg, c(1:8), scaling=3)

#find out what percentage of the variation is explained by each axis
CCA_Grazing_Avg$CCA$eig/sum(CCA_Grazing_Avg$CCA$eig)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(CCA_Grazing_Avg)    #ns
#test significance by terms (= PerMANOVA)
anova(CCA_Grazing_Avg, by = "terms")  

plot(CCA_Grazing_Avg)


#### CCA by Year*Grazing Treatment:  n=3  ####

#run the CCA
CCA_Year_Grazing_Avg <- cca(CCA_Data_Avg ~ Year*Grazing_Treatment, data=CCA_Meta_Data_Avg)
summary(CCA_Year_Grazing_Avg)

#pull scores to use for subsequent univariate analyses
scores(CCA_Year_Grazing_Avg, c(1:7), scaling=3)

#find out what percentage of the variation is explained by each axis
CCA_Year_Grazing_Avg$CCA$eig/sum(CCA_Year_Grazing_Avg$CCA$eig)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(CCA_Year_Grazing_Avg)    #ns
#test significance by terms (= PerMANOVA)
anova(CCA_Year_Grazing_Avg, by = "terms")  
#justifies subsequent univariate tests for axes that are significant
anova(CCA_Year_Grazing_Avg, by = "axis")  

plot(CCA_Year_Grazing_Avg)

#### RDA Year n=3  ####

RDA_Year_Avg<- rda(CCA_Data_Avg ~ Year, data=CCA_Meta_Data_Avg)

summary(RDA_Year_Avg)

#pull scores to use for subsequent univariate analyses
scores(RDA_Year_Avg, c(1:4), scaling=3)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(RDA_Year_Avg)    #ns
#test significance by terms (= PerMANOVA)
anova(RDA_Year_Avg, by = "terms")  
#justifies subsequent univariate tests for axes that are significant
anova(RDA_Year_Avg, by = "axis")  

plot(RDA_Year_Avg)

#### RDA Grazing  n=3 ####

RDA_Grazing_Avg <- rda(CCA_Data_Avg ~ Grazing_Treatment, data=CCA_Meta_Data_Avg)

summary(RDA_Grazing_Avg)

#pull scores to use for subsequent univariate analyses
scores(RDA_Grazing_Avg, c(1:4), scaling=3)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(RDA_Grazing_Avg)    #ns
#test significance by terms (= PerMANOVA)
anova(RDA_Grazing_Avg, by = "terms")  

plot(RDA_Grazing_Avg)

#### RDA Year and Grazing  n=3 ####

RDA_Year_Grazing_Avg <- rda(CCA_Data_Avg ~ Year*Grazing_Treatment, data=CCA_Meta_Data_Avg)

summary(RDA_Year_Grazing_Avg)

RDA_Year_Grazing_Avg <- rda(CCA_Data_Avg ~ Year*Grazing_Treatment, data=CCA_Meta_Data_Avg)

summary(RDA_Year_Grazing_Avg)

#pull scores to use for subsequent univariate analyses
scores(RDA_Year_Grazing_Avg, c(1:4), scaling=3)

#do some stats
#overall model significant; this uses vegan's anova.cca function; if NS, should not run univariate tests.
anova(RDA_Year_Grazing_Avg)    #ns
#test significance by terms (= PerMANOVA)
anova(RDA_Year_Grazing_Avg, by = "terms")  
#justifies subsequent univariate tests for axes that are significant
anova(RDA_Year_Grazing_Avg, by = "axis")  

plot(RDA_Year_Grazing_Avg)



#### Grasshopper Weights: Not averaged ####
Grasshopper_Weight<-Weight_Data_Official %>% 
  filter(Correct_Order=="Orthoptera") %>% 
  separate(Coll_Year_Bl_Trt,c("CollectionMethod","Year","Block","Grazing_Treatment"),sep="_") %>% 
  filter(Grazing_Treatment!="NA") %>% 
  mutate(Dry_Weight_g=ifelse(Dry_Weight_g==0.00000,0.0001,Dry_Weight_g)) %>% 
  mutate(Dry_Weight_mg=Dry_Weight_g*1000)

Grasshopper_Weight_Plot<-Grasshopper_Weight %>% 
  group_by(Year,Grazing_Treatment) %>%
  summarize(Weight_Std=sd(Dry_Weight_mg),Weight_Mean=mean(Dry_Weight_mg),Weight_n=length(Dry_Weight_mg))%>%
  mutate(Weight_St_Error=Weight_Std/sqrt(Weight_n)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment_Fig=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

##reorder bar graphs##
Grasshopper_Weight$Grazing_Treatment <- factor(Grasshopper_Weight$Grazing_Treatment, levels = c("NG", "LG", "HG"))


# 2020 
Grasshopper_2020_Weight<-ggplot(subset(Grasshopper_Weight_Plot,Year==2020),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+
  #scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=40, label="A. 2020",size=20)


# 2021
Grasshopper_2021_Weight<-ggplot(subset(Grasshopper_Weight_Plot,Year==2021),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=40, label="B. 2021",size=20)

# 2022
Grasshopper_2022_Weight<-ggplot(subset(Grasshopper_Weight_Plot,Year==2022),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+ 
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=40, label="C. 2022",size=20)


#### Create Grasshoper Figure: Not Averaged ####
Grasshopper_2020_Weight+  
  Grasshopper_2021_Weight+
  Grasshopper_2022_Weight+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4000x2000

####Individual Grasshopper Weight Normality: Not Averaged####

# Weight 2020
Grasshopper_Weight_2020 <- lm(data = subset(Grasshopper_Weight, Year == 2020),log(Dry_Weight_mg)  ~ Grazing_Treatment)
ols_plot_resid_hist(Grasshopper_Weight_2020) 
ols_test_normality(Grasshopper_Weight_2020) #normalish

# Weight 2021
Grasshopper_Weight_2021 <- lm(data = subset(Grasshopper_Weight, Year == 2021),log(Dry_Weight_mg)  ~ Grazing_Treatment)
ols_plot_resid_hist(Grasshopper_Weight_2021) 
ols_test_normality(Grasshopper_Weight_2021) #normalish

# Weight 2020
Grasshopper_Weight_2022 <- lm(data = subset(Grasshopper_Weight, Year == 2022),log(Dry_Weight_mg)  ~ Grazing_Treatment)
ols_plot_resid_hist(Grasshopper_Weight_2022) 
ols_test_normality(Grasshopper_Weight_2022) #normalish

#### Stats: Individual Grasshopper: Weight: Not Averaged ####

# 2020 Weight
Grasshopper_2020_Glmm_Weight <- lmer(log(Dry_Weight_mg) ~ Grazing_Treatment + (1 | Block) , data = subset(Grasshopper_Weight,Year==2020))
anova(Grasshopper_2020_Glmm_Weight) #not significant

# 2021 Weight
Grasshopper_2021_Glmm_Weight <- lmer(log(Dry_Weight_mg) ~ Grazing_Treatment + (1 | Block) , data = subset(Grasshopper_Weight,Year==2021))
anova(Grasshopper_2021_Glmm_Weight) #Grazing (0.01323)
### post hoc test for lmer test ##
summary(glht(Grasshopper_2021_Glmm_Weight, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.30), #LG-HG (0.009), NG-HG (0.0391)

# 2022 Weight
Grasshopper_2022_Glmm_Weight <- lmer(log(Dry_Weight_mg) ~ Grazing_Treatment + (1 | Block) , data = subset(Grasshopper_Weight,Year==2022))
anova(Grasshopper_2022_Glmm_Weight) #not signfiicant


#### Grasshopper Weights: averaged by plot####
Grasshopper_Weight_PlotAvg<-Grasshopper_Weight %>% 
  group_by(Year,Block,Grazing_Treatment,Plot) %>% 
  summarise(Avg_Plot_Weight=mean(Dry_Weight_mg))

Grasshopper_Weight_PlotAvg_Graph<-Grasshopper_Weight_PlotAvg %>% 
  group_by(Year,Grazing_Treatment) %>%
  summarize(Weight_Std=sd(Avg_Plot_Weight),Weight_Mean=mean(Avg_Plot_Weight),Weight_n=length(Avg_Plot_Weight))%>%
  mutate(Weight_St_Error=Weight_Std/sqrt(Weight_n)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment_Fig=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

##reorder bar graphs##
Grasshopper_Weight_PlotAvg_Graph$Grazing_Treatment <- factor(Grasshopper_Weight_PlotAvg_Graph$Grazing_Treatment, levels = c("NG", "LG", "HG"))


# 2020 
Grasshopper_2020_Weight_Plot<-ggplot(subset(Grasshopper_Weight_PlotAvg_Graph,Year==2020),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+
  #scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=40, label="A. 2020",size=20)


# 2021
Grasshopper_2021_Weight_Plot<-ggplot(subset(Grasshopper_Weight_PlotAvg_Graph,Year==2021),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=40, label="B. 2021",size=20)

# 2022
Grasshopper_2022_Weight_Plot<-ggplot(subset(Grasshopper_Weight_PlotAvg_Graph,Year==2022),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+ 
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=40, label="C. 2022",size=20)


#### Create Grasshoper Figure: Averaged by plot ####
Grasshopper_2020_Weight_Plot+  
  Grasshopper_2021_Weight_Plot+
  Grasshopper_2022_Weight_Plot+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4000x2000


####Individual Grasshopper Weight Normality: Averaged by Plot ####
  
# Weight 2020
Grasshopper_Weight_2020_Plot <- lm(data = subset(Grasshopper_Weight_PlotAvg, Year == 2020),log(Avg_Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(Grasshopper_Weight_2020_Plot) 
ols_test_normality(Grasshopper_Weight_2020_Plot) #normal

# Weight 2021
Grasshopper_Weight_2021_Plot <- lm(data = subset(Grasshopper_Weight_PlotAvg, Year == 2021),log(Avg_Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(Grasshopper_Weight_2021_Plot) 
ols_test_normality(Grasshopper_Weight_2021_Plot) #normal

# Weight 2020
Grasshopper_Weight_2022_Plot <- lm(data = subset(Grasshopper_Weight_PlotAvg, Year == 2022),log(Avg_Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(Grasshopper_Weight_2022_Plot) 
ols_test_normality(Grasshopper_Weight_2022_Plot) #normal

#### Stats: Individual Grasshopper: Weight: : Averaged by Plot####

# 2020 Weight
Grasshopper_2020_Glmm_Weight_Plot <- lmer(log(Avg_Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Grasshopper_Weight_PlotAvg,Year==2020))
anova(Grasshopper_2020_Glmm_Weight_Plot) #not significant

# 2021 Weight
Grasshopper_2021_Glmm_Weight_Plot <- lmer(log(Avg_Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Grasshopper_Weight_PlotAvg,Year==2021))
anova(Grasshopper_2021_Glmm_Weight_Plot) #Grazing (0.01323)
### post hoc test for lmer test ##
summary(glht(Grasshopper_2021_Glmm_Weight_Plot, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.30), #LG-HG (0.009), NG-HG (0.0391)

# 2022 Weight
Grasshopper_2022_Glmm_Weight_Plot <- lmer(log(Avg_Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Grasshopper_Weight_PlotAvg,Year==2022))
anova(Grasshopper_2022_Glmm_Weight_Plot) #not significant

#### Grasshopper Weights: averaged by Paddock####
Grasshopper_Weight_PaddockAvg<-Grasshopper_Weight %>%
  mutate(Paddock=paste(Block,Grazing_Treatment,sep="-")) %>% 
  group_by(Year,Paddock,Grazing_Treatment,Block) %>% 
  summarise(Avg_Paddock_Weight=mean(Dry_Weight_mg)) %>% 
  ungroup()

Grasshopper_Weight_PaddockAvg_Graph<-Grasshopper_Weight_PaddockAvg %>% 
  group_by(Year,Grazing_Treatment) %>%
  summarize(Weight_Std=sd(Avg_Paddock_Weight),Weight_Mean=mean(Avg_Paddock_Weight),Weight_n=length(Avg_Paddock_Weight))%>%
  mutate(Weight_St_Error=Weight_Std/sqrt(Weight_n)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment_Fig=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

##reorder bar graphs##
Grasshopper_Weight_PaddockAvg_Graph$Grazing_Treatment <- factor(Grasshopper_Weight_PaddockAvg_Graph$Grazing_Treatment, levels = c("NG", "LG", "HG"))

# 2020 
Grasshopper_2020_Weight_Paddock<-ggplot(subset(Grasshopper_Weight_PaddockAvg_Graph,Year==2020),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+
  #scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=40, label="A. 2020",size=20)


# 2021
Grasshopper_2021_Weight_Paddock<-ggplot(subset(Grasshopper_Weight_PaddockAvg_Graph,Year==2021),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=40, label="B. 2021",size=20)

# 2022
Grasshopper_2022_Weight_Paddock<-ggplot(subset(Grasshopper_Weight_PaddockAvg_Graph,Year==2022),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+ 
  theme(text = element_text(size = 55),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=0.85, y=40, label="C. 2022",size=20)


#### Create Grasshoper Figure: Averaged by Paddock ####
Grasshopper_2020_Weight_Paddock+  
  Grasshopper_2021_Weight_Paddock+
  Grasshopper_2022_Weight_Paddock+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4000x2000


####Individual Grasshopper Weight Normality: Averaged by Paddock ####

# Weight 2020
Grasshopper_Weight_2020_Paddock <- lm(data = subset(Grasshopper_Weight_PaddockAvg, Year == 2020),log(Avg_Paddock_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(Grasshopper_Weight_2020_Paddock) 
ols_test_normality(Grasshopper_Weight_2020_Paddock) #normal

# Weight 2021
Grasshopper_Weight_2021_Paddock <- lm(data = subset(Grasshopper_Weight_PaddockAvg, Year == 2021),log(Avg_Paddock_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(Grasshopper_Weight_2021_Paddock) 
ols_test_normality(Grasshopper_Weight_2021_Paddock) #normal

# Weight 2020
Grasshopper_Weight_2022_Paddock <- lm(data = subset(Grasshopper_Weight_PaddockAvg, Year == 2022),log(Avg_Paddock_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(Grasshopper_Weight_2022_Paddock) 
ols_test_normality(Grasshopper_Weight_2022_Paddock) #normal

#### Stats: Individual Grasshopper: Weight: : Averaged by Paddock####

# 2020 Weight
Grasshopper_2020_Glmm_Weight_Paddock <- lmer(log(Avg_Paddock_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Grasshopper_Weight_PaddockAvg,Year==2020))
anova(Grasshopper_2020_Glmm_Weight_Paddock) #not significant

# 2021 Weight
Grasshopper_2021_Glmm_Weight_Paddock <- lmer(log(Avg_Paddock_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Grasshopper_Weight_PaddockAvg,Year==2021))
anova(Grasshopper_2021_Glmm_Weight_Paddock) #Grazing (0.001)
### post hoc test for lmer test ##
summary(glht(Grasshopper_2021_Glmm_Weight_Paddock, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) 

# 2022 Weight
Grasshopper_2022_Glmm_Weight_Paddock <- lmer(log(Avg_Paddock_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Grasshopper_Weight_PaddockAvg,Year==2022))
anova(Grasshopper_2022_Glmm_Weight_Paddock) #not significant
### post hoc test for lmer test ##
summary(glht(Grasshopper_2022_Glmm_Weight_Paddock, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) 




#### Presentation Figure (A,B): Average Plot Weight, (C,D): Grasshoppper weight (E,F) Order Proportion by Weight, (G,H): Order Proportion by Cover ####

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
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(axis.title.y=element_text(size = 75),axis.text.y=element_text(size = 75),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")
  #geom_text(x=0.85, y=1, label="A. 2020",size=20)

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
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  #geom_text(x=0.85, y=1, label="B. 2021",size=20)+
  #no grazing is different than high grazing, low grazing is not different than high grazing, no and low grazing not different
  annotate("text",x=1,y=0.45,label="a",size=30)+ #no grazing
  annotate("text",x=2,y=0.66,label="ab",size=30)+ #low grazing
  annotate("text",x=3,y=0.87,label="b",size=30) #high grazing


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
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")
  #geom_text(x=0.85, y=1, label="C. 2022",size=20)

# 2020 Average Plot Weight
Dvac_2020_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2020),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Biomass (g)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 75),legend.text=element_text(size=75))
  #geom_text(x=1.6, y=0.5, label="A. 2020 Plot Weight",size=20)

# Average Plot Weight
Dvac_2021_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2021),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 75),legend.text=element_text(size=75))+
  #geom_text(x=1.6, y=0.5, label="B. 2021 Plot Weight",size=20)+
  #no grazing is different than high grazing, low grazing is different than high grazing, no and low grazing are the same
  annotate("text",x=1,y=0.14,label="a",size=30)+ #no grazing
  annotate("text",x=2,y=0.12,label="a",size=30)+ #low grazing
  annotate("text",x=3,y=0.07,label="b",size=30) #high grazing

# Average Plot Weight
Dvac_2022_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2022),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("Cattle Removal","Destock","High Impact Grazing"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 75))
  #geom_text(x=1.6, y=0.5, label="C. 2022 Plot Weight",size=20)

# 2020 
Grasshopper_2020_Weight_Plot<-ggplot(subset(Grasshopper_Weight_PlotAvg_Graph,Year==2020),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab(expression(paste("Individual Grasshopper \n Weight (mg)")))+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+
  #scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 75),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")
  #geom_text(x=0.85, y=40, label="A. 2020",size=20)


# 2021
Grasshopper_2021_Weight_Plot<-ggplot(subset(Grasshopper_Weight_PlotAvg_Graph,Year==2021),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+
  theme(text = element_text(size = 75),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank())+
  #geom_text(x=0.85, y=40, label="B. 2021",size=20)
  annotate("text",x=1,y=16.4,label="a",size=30)+ #no grazing
  annotate("text",x=2,y=17,label="a",size=30)+ #low grazing
  annotate("text",x=3,y=10,label="b",size=30) #high grazing

# 2022
Grasshopper_2022_Weight_Plot<-ggplot(subset(Grasshopper_Weight_PlotAvg_Graph,Year==2022),aes(x=Grazing_Treatment_Fig,y=Weight_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Weight_Mean-Weight_St_Error,ymax=Weight_Mean+Weight_St_Error),position=position_dodge(),width=0.2,size=2)+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Shannon"
  ylab("Average Individual Grasshopper Weight (mg)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#B6AD90","#A4AC86","#656D4A"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=40)+ 
  theme(text = element_text(size = 75),legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank())
  #geom_text(x=0.85, y=40, label="C. 2022",size=20)

# Proportion of Orders by Weight
Order_Weight_2020<-ggplot(subset(Relative_Weight,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion by Biomass")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 75),legend.text=element_text(size=75))
  #geom_text(x=1.9, y=1.2, label="D. Abundance by Weight",size=20)

Order_Weight_2021<-ggplot(subset(Relative_Weight,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 75),legend.text=element_text(size=75))
  #geom_text(x=1.9, y=1.2,label="E.Abundance by Weight",size=20)

Order_Weight_2022<-ggplot(subset(Relative_Weight,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#66676C","#403025","#B89984","#CABEB9","#72544D"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 75),legend.text=element_text(size=75))
  #geom_text(x=1.9, y=1.2, label="F.Abundance by Weight",size=20)

Order_Count_2020<-ggplot(subset(Relative_Count,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion by Abundance")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 75),legend.text=element_text(size=75))
  #geom_text(x=1.8, y=1.2,label="G.Abundance by Count",size=20)

Order_Count_2021<-ggplot(subset(Relative_Count,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#B89984"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 75),legend.text=element_text(size=75))
  #geom_text(x=1.8, y=1.2, label="H.Abundance by Count",size=20)

Order_Count_2022<-ggplot(subset(Relative_Count,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing  Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#845749","#FBECC5","#D3DEDF", "#789193","#BABEBF","#66676C","#403025","#B89984","#CABEB9","#72544D"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 75),legend.text=element_text(size=75))
  #geom_text(x=1.8, y=1.2, label="I.Abundance by Count",size=20)



Shannon_2020_Weight+  
  Shannon_2021_Weight+
  Shannon_2022_Weight+
  Dvac_2020_Plot+
  Dvac_2021_Plot+
  Dvac_2022_Plot+
  Grasshopper_2020_Weight_Plot+  
  Grasshopper_2021_Weight_Plot+
  Grasshopper_2022_Weight_Plot+
  Order_Weight_2020 +  
  Order_Weight_2021+
  Order_Weight_2022 +
  Order_Count_2020 +  
  Order_Count_2021+
  Order_Count_2022 +
  plot_layout(ncol = 3,nrow = 5)
#save at 5000 x 5000


Feeding_Guild_2022<-ggplot(Relative_Count_Family,aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Guild, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Regime")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Feeding Guilds")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#503930","#714c42","#a0897b", "#9CA497","#c9d0c5","#9CA497","#798671","#4e6b5d","#1E3907"), name = "Feeding Guild")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=5), legend.key.size = unit(3,"centimeters"))+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 75),legend.text=element_text(size=75),axis.title.y=element_blank(),axis.text.y=element_blank())+
  geom_text(x=1, y=1.2,label="C.2022",size=20)




###############

#### Absolute Arthropod Weight by Order Graphs ####

Order_Weight_1<-Weight_Data_Summed %>% 
  filter(Correct_Order!="Body Parts" & Correct_Order!="Body_Parts" & Correct_Order!="Unknown" & Correct_Order!="unknown" & Correct_Order!="Unknown_1")

Order_Weight_1$Grazing_Treatment <- factor(Order_Weight_1$Grazing_Treatment, levels = c("Cattle Removal", "Destock Grazing", "High Impact Grazing")) 


Order_Weight<-Order_Weight_1%>%  
  filter(Plot!="NA") %>% 
  group_by(Year,Grazing_Treatment,Block,Correct_Order) %>% 
  summarise(Average_Weight=mean(Dry_Weight_g,na.rm=T)) %>% 
  ungroup()
  summarise(Average_Weight=mean(Dry_Weight_g),Weight_SD=sd(Dry_Weight_g),Weight_n=length(Dry_Weight_g)) %>%
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()


Order_Weight_Faceted <- ggplot(
  Order_Weight,
  aes(x = Year, y = Average_Weight, fill = Grazing_Treatment)
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(
      ymin = Average_Weight - Weight_St_Error,
      ymax = Average_Weight + Weight_St_Error
    ),
    position = position_dodge(width = 0.9),
    width = 0.5, size = 1
  ) +
  facet_wrap(~ Correct_Order, scales = "free_y") +
  xlab("Year") +
  ylab("Order Biomass (g)") +
  scale_fill_manual(
    values = c("#e8c599", "#bc6022", "#b72818"),
    labels = c("Cattle Removal", "Destock", "High Impact Grazing")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
  theme(
    legend.background = element_blank(),
    legend.position   = c(0.85, 0.10),
    legend.title      = element_blank(),
    legend.text       = element_text(size = 14),
    strip.text        = element_text(size = 14, face = "bold"),
    axis.title.y      = element_text(size = 16),
    axis.text.y       = element_text(size = 12),
    axis.title.x      = element_text(size = 16),
    axis.text.x       = element_text(size = 12)
  )

Order_Weight_Faceted

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

#### Normality: Diptera Biomass ####
#2020
Diptera_Biomass_2020 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Diptera"))
ols_plot_resid_hist(Diptera_Biomass_2020) 
ols_test_normality(Diptera_Biomass_2020) #not normal

#2021
Diptera_Biomass_2021 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Diptera"))
ols_plot_resid_hist(Diptera_Biomass_2021) 
ols_test_normality(Diptera_Biomass_2021) #normal

#2022
Diptera_Biomass_2022 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Diptera"))
ols_plot_resid_hist(Diptera_Biomass_2022) 
ols_test_normality(Diptera_Biomass_2022) #normalish

#### Stats: Diptera Biomass ####
#2020
Diptera_Biomass_2020_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Diptera"))
anova(Diptera_Biomass_2020_Glmm) #not significant

#2021
Diptera_Biomass_2021_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Diptera"))
anova(Diptera_Biomass_2021_Glmm) #not significant

#2022
Diptera_Biomass_2022_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Diptera"))
anova(Diptera_Biomass_2022_Glmm) #not significant

#### Normality: Hemiptera Biomass ####
#2020
Hemiptera_Biomass_2020 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Hemiptera"))
ols_plot_resid_hist(Hemiptera_Biomass_2020) 
ols_test_normality(Hemiptera_Biomass_2020) #normal

#2021
Hemiptera_Biomass_2021 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Hemiptera"))
ols_plot_resid_hist(Hemiptera_Biomass_2021) 
ols_test_normality(Hemiptera_Biomass_2021) #normal

#2022
Hemiptera_Biomass_2022 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Hemiptera"))
ols_plot_resid_hist(Hemiptera_Biomass_2022) 
ols_test_normality(Hemiptera_Biomass_2022) #normal

#### Stats: Hemiptera Biomass ####
#2020
Hemiptera_Biomass_2020_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Hemiptera"))
anova(Hemiptera_Biomass_2020_Glmm) #not significant

#2021
Hemiptera_Biomass_2021_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Hemiptera"))
anova(Hemiptera_Biomass_2021_Glmm) #not significant

#2022
Hemiptera_Biomass_2022_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Hemiptera"))
anova(Hemiptera_Biomass_2022_Glmm) #not significant

#### Normality: Hymenoptera Biomass ####
#2020
Hymenoptera_Biomass_2020 <- lm(sqrt(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Hymenoptera"))
ols_plot_resid_hist(Hymenoptera_Biomass_2020) 
ols_test_normality(Hymenoptera_Biomass_2020) #normalish

#2021
Hymenoptera_Biomass_2021 <- lm((Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Hymenoptera"))
ols_plot_resid_hist(Hymenoptera_Biomass_2021) 
ols_test_normality(Hymenoptera_Biomass_2021) #normal

#2022
Hymenoptera_Biomass_2022 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Hymenoptera"))
ols_plot_resid_hist(Hymenoptera_Biomass_2022) 
ols_test_normality(Hymenoptera_Biomass_2022) #normalish

#### Stats: Hymenoptera Biomass ####
#2020
Hymenoptera_Biomass_2020_Glmm <- lmer(sqrt(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Hymenoptera"))
anova(Hymenoptera_Biomass_2020_Glmm) #not significant

#2021
Hymenoptera_Biomass_2021_Glmm <- lmer((Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Hymenoptera"))
anova(Hymenoptera_Biomass_2021_Glmm) #not significant

#2022
Hymenoptera_Biomass_2022_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Hymenoptera"))
anova(Hymenoptera_Biomass_2022_Glmm) #not significant

#### Normality: Orthoptera Biomass ####
#2020
Orthoptera_Biomass_2020 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Orthoptera"))
ols_plot_resid_hist(Orthoptera_Biomass_2020) 
ols_test_normality(Orthoptera_Biomass_2020) #normal

#2021
Orthoptera_Biomass_2021 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Orthoptera"))
ols_plot_resid_hist(Orthoptera_Biomass_2021) 
ols_test_normality(Orthoptera_Biomass_2021) #normal

#2022
Orthoptera_Biomass_2022 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Orthoptera"))
ols_plot_resid_hist(Orthoptera_Biomass_2022) 
ols_test_normality(Orthoptera_Biomass_2022) #normal

#### Stats: Orthoptera Biomass ####
#2020
Orthoptera_Biomass_2020_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2020 & Correct_Order == "Orthoptera"))
anova(Orthoptera_Biomass_2020_Glmm) #not significant

#2021
Orthoptera_Biomass_2021_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2021 & Correct_Order == "Orthoptera"))
anova(Orthoptera_Biomass_2021_Glmm) #significant
summary(glht(Orthoptera_Biomass_2021_Glmm, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.33), #LG-HG (0.0048), NG-HG (0.00278)


#2022
Orthoptera_Biomass_2022_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Orthoptera"))
anova(Orthoptera_Biomass_2022_Glmm) #not significant

#### Normality: Thysanoptera Biomass ####


#2022
Thysanoptera_Biomass_2022 <- lm(log(Dry_Weight_g) ~ Grazing_Treatment, data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Thysanoptera"))
ols_plot_resid_hist(Thysanoptera_Biomass_2022) 
ols_test_normality(Thysanoptera_Biomass_2022) #normalish

#### Stats: Thysanoptera Biomass ####
#2022
Thysanoptera_Biomass_2022_Glmm <- lmer(log(Dry_Weight_g) ~ Grazing_Treatment + (1 | Block:Grazing_Treatment) , data = subset(Weight_Data_Summed, Year == 2022 & Correct_Order == "Thysanoptera"))
anova(Thysanoptera_Biomass_2022_Glmm) #not significant