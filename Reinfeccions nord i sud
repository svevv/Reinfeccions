library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(eeptools)
library(wesanderson)
library(openxlsx)
library(hash)
library(tidyverse)
library(kableExtra)
library(scales)
library(car)

setwd("C:/Users/53637539K/Documents/R-SVEVV/Explotacions")
#rm(list=ls())

#today.slash <- format(Sys.Date(), "%d/%m/%Y")
#today.short <- format(Sys.Date(), "%d%m%y")
# if (as.POSIXlt(today.long)$wday == 1){
#   yesterday.short <- format(today.long - 3, "%d%m%y")
# } else {
#   yesterday.short <- format(today.long - 1, "%d%m%y")
# }

#Si les dates no son les actuals:
today.slash <- "17/06/2022"
today.short <- "170622"
yesterday.short <- "100622"


##### CREAR REINFECTADOS #####

#### NORD I MARESME: ####
#Import dataset:
if(!exists("TAR_NORD")){
  TAR_NORD <- read.csv(paste0(getwd(),"/db/",today.short,"_TAR_Nord.csv"),
                  sep=";", quote="", fill = TRUE, na.strings=c("",NA))
}

if(!exists("PCR_NORD")){
  PCR_NORD <- read.csv(paste0(getwd(),"/db/",today.short,"_PCR_Nord.csv"),
                  sep=";", quote="", fill = TRUE, na.strings=c("",NA))
}


TAR_REF_NORD <- subset(TAR_NORD, select=c("Id.Dades.del.Pacient.Sospitós","CIP.Vigent",
                                        "Situació.del.Pacient","Localitat","Edat.Exposició",
                                        "CIP.Vigent.1","Data.de.la.prova","Resultat.microbiològic",
                                        "Codi.SNOMED.de.la.prova"))
#TAR_REF_NORD <- subset(TAR_CAT, select=c(1,2,6,7,8,27,29,28,30))
PCR_REF_NORD <- subset(PCR_NORD, select=c("Id.Dades.del.Pacient.Sospitós","CIP.Vigent",
                                        "Situació.del.Pacient","Localitat","Edat.Exposició",
                                        "CIP","Data.Validació.Mostra","Resultat.microbiològic",
                                        "Laboratori"))
#PCR_REF_NORD <- subset(PCR_CAT, select=c(1,2,6,7,8,27,28,29,30))

#Eliminar test serologico de TAR:
summary(as.factor(TAR_REF_NORD$Codi.SNOMED.de.la.prova))
TAR_REF_NORD <- TAR_REF_NORD[TAR_REF_NORD$Codi.SNOMED.de.la.prova=="Antígen del coronavirus 2 del SARS",]
TAR_REF_NORD <- subset(TAR_REF_NORD, select=c(1,2,3,4,5,6,7,8))
TAR_REF_NORD$LAB <- 0 

names(PCR_REF_NORD) <- c("ID", "CIP_VIGENT", "SITUACIO_PACIENT", "LOCALITAT",
                    "EDAT", "CIP", "DATA_MOSTRA", "RESULTAT","LAB")

names(TAR_REF_NORD) <- c("ID", "CIP_VIGENT", "SITUACIO_PACIENT","LOCALITAT", 
                    "EDAT", "CIP", "DATA_MOSTRA", "RESULTAT","LAB")

PCR_REF_NORD$TIPUS_MOSTRA <- "PCR"
TAR_REF_NORD$TIPUS_MOSTRA <- "TAR"


data_ref_nord <- rbind(PCR_REF_NORD,TAR_REF_NORD)

data_ref_nord$DATA_MOSTRA <- str_replace(data_ref_nord$DATA_MOSTRA, "0020-", "2020-")
data_ref_nord$DATA_MOSTRA <- str_replace(data_ref_nord$DATA_MOSTRA, "0200-", "2020-")
data_ref_nord$DATA_MOSTRA <- str_replace(data_ref_nord$DATA_MOSTRA, "0021-", "2021-")

data_ref_nord$DATA_MOSTRA <- as.Date(data_ref_nord$DATA_MOSTRA, "%d/%m/%Y")

#Eliminar observaciones sin resultado
#data <- data[!is.na(data$RESULTAT),]
data_ref_nord <- data_ref_nord[!is.na(data_ref_nord$DATA_MOSTRA),]
data_ref_nord <- data_ref_nord[data_ref_nord$RESULTAT=="Positiu",]
data_ref_nord <- data_ref_nord[!is.na(data_ref_nord$RESULTAT),]
data_ref_nord <- data_ref_nord[!is.na(data_ref_nord$ID),]
data_ref_nord <- data_ref_nord[!is.na(data_ref_nord$CIP_VIGENT),]


#ordenados por ID y dentro por fechas:
data_ref_nord <-data_ref_nord[order(data_ref_nord$ID, data_ref_nord$DATA_MOSTRA),]

#Detecció de reinfeccions
data_ref_nord <- data_ref_nord %>%
  group_by(ID) %>% 
  filter(n()>1) %>% #Només ID amb més d'un positiu 
  mutate(MOSTRA_ANT=lag(DATA_MOSTRA)) %>%  #Data mostra immediatament anterior
  mutate(INTERVAL= strtoi(DATA_MOSTRA-lag(DATA_MOSTRA))) %>% #N dies entre mostra actual i l'anterior
  filter(INTERVAL>89)%>% #Només mostres amb interval superior als 60 dies
  mutate(DATA_INF=MOSTRA_ANT) %>% #S'assigna la data d'infecció
  mutate(DATA_REINF=DATA_MOSTRA) %>% #S'assigna la data de reinfecció
  mutate(N_REINFEC=1:n()) #Comptador de reinfeccions

reinfectats_nord <- data_ref_nord[!duplicated(data_ref_nord$CIP_VIGENT, fromLast = TRUE),]
reinfectats_nord$EDAT <- as.numeric(reinfectats_nord$EDAT)


reinfectats_nord <- reinfectats_nord %>%
  mutate(edat_estratificat = ifelse(EDAT < 5, "<5",
                                    ifelse(EDAT >=5 & EDAT < 15, "5-14",
                                           ifelse(EDAT >=15 & EDAT <25, "15-24",
                                                  ifelse(EDAT >=25 & EDAT < 35, "25-34",
                                                         ifelse(EDAT >=35 & EDAT <45, "35-44",
                                                                ifelse(EDAT >=45 & EDAT<55, "45-54",
                                                                       ifelse(EDAT >=55 & EDAT<65, "55-64",
                                                                              ifelse(EDAT >=65 & EDAT<75, "65-74",
                                                                                     ifelse(EDAT >=75 & EDAT<85, "75-84", 
                                                                                            ifelse(EDAT >=85 & EDAT<95, "85-94",
                                                                                                   ifelse(EDAT >=95, ">95", NA))))))))))))


#ordenados por ID y dentro por fechas:
reinfectats_nord <-reinfectats_nord[order(reinfectats_nord$ID, reinfectats_nord$DATA_MOSTRA),]
reinfectats.nous_nord <- reinfectats_nord[!duplicated(reinfectats_nord$CIP_VIGENT, fromLast = TRUE),]

write.xlsx(reinfectats.nous_nord, paste0(getwd(),"/Reinfeccions/Nord_i_Maresme/reinfectats_nord_tots",today.short,".xlsx"),row.names=FALSE)


#Crear excel només amb els casos nous:
reinfectats.vells_nord <- read.xlsx(paste0(getwd(),"/Reinfeccions/Nord_i_Maresme/reinfectats_nord_tots",yesterday.short,".xlsx"))

reinfectats_nord <- reinfectats.nous_nord %>% 
  filter(!CIP_VIGENT %in% reinfectats.vells_nord$CIP_VIGENT)

write.xlsx(reinfectats_nord, paste0(getwd(),"/Reinfeccions/Nord_i_Maresme/reinfectats_nord",today.short,".xlsx"),row.names=FALSE)





#### SUD: ####
#Import dataset:
if(!exists("TAR_SUD")){
  TAR_SUD <- read.csv(paste0(getwd(),"/db/",today.short,"_TAR_Sud.csv"),
                      sep=";", quote="", fill = TRUE, na.strings=c("",NA))
}

if(!exists("PCR_SUD")){
  PCR_SUD <- read.csv(paste0(getwd(),"/db/",today.short,"_PCR_Sud.csv"),
                      sep=";", quote="", fill = TRUE, na.strings=c("",NA))
}




TAR_REF_SUD <- subset(TAR_SUD, select=c("Id.Dades.del.Pacient.Sospitós","CIP.Vigent",
                                        "Situació.del.Pacient","Localitat","Edat.Exposició",
                                        "CIP.Vigent.1","Data.de.la.prova","Resultat.microbiològic",
                                        "Codi.SNOMED.de.la.prova"))
#TAR_REF_SUD <- subset(TAR_CAT, select=c(1,2,6,7,8,27,29,28,30))
PCR_REF_SUD <- subset(PCR_SUD, select=c("Id.Dades.del.Pacient.Sospitós","CIP.Vigent",
                                        "Situació.del.Pacient","Localitat","Edat.Exposició",
                                        "CIP","Data.Validació.Mostra","Resultat.microbiològic",
                                        "Laboratori"))
#PCR_REF_SUD <- subset(PCR_CAT, select=c(1,2,6,7,8,27,28,29,30))

#Eliminar test serologico de TAR:
summary(as.factor(TAR_REF_SUD$Codi.SNOMED.de.la.prova))
TAR_REF_SUD <- TAR_REF_SUD[TAR_REF_SUD$Codi.SNOMED.de.la.prova=="Antígen del coronavirus 2 del SARS",]
TAR_REF_SUD <- subset(TAR_REF_SUD, select=c(1,2,3,4,5,6,7,8))
TAR_REF_SUD$LAB <- 0 

names(PCR_REF_SUD) <- c("ID", "CIP_VIGENT", "SITUACIO_PACIENT", "LOCALITAT",
                        "EDAT", "CIP", "DATA_MOSTRA", "RESULTAT","LAB")

names(TAR_REF_SUD) <- c("ID", "CIP_VIGENT", "SITUACIO_PACIENT","LOCALITAT", 
                        "EDAT", "CIP", "DATA_MOSTRA", "RESULTAT","LAB")

PCR_REF_SUD$TIPUS_MOSTRA <- "PCR"
TAR_REF_SUD$TIPUS_MOSTRA <- "TAR"


data_ref_sud <- rbind(PCR_REF_SUD,TAR_REF_SUD)
data_ref_sud$DATA_MOSTRA <- str_replace(data_ref_sud$DATA_MOSTRA, "0020-", "2020-")
data_ref_sud$DATA_MOSTRA <- str_replace(data_ref_sud$DATA_MOSTRA, "0200-", "2020-")
data_ref_sud$DATA_MOSTRA <- str_replace(data_ref_sud$DATA_MOSTRA, "0021-", "2021-")

data_ref_sud$DATA_MOSTRA <- as.Date(data_ref_sud$DATA_MOSTRA, "%d/%m/%Y")

#Eliminar observaciones sin resultado
#data <- data[!is.na(data$RESULTAT),]
data_ref_sud <- data_ref_sud[!is.na(data_ref_sud$DATA_MOSTRA),]
data_ref_sud <- data_ref_sud[data_ref_sud$RESULTAT=="Positiu",]
data_ref_sud <- data_ref_sud[!is.na(data_ref_sud$RESULTAT),]
data_ref_sud <- data_ref_sud[!is.na(data_ref_sud$ID),]
data_ref_sud <- data_ref_sud[!is.na(data_ref_sud$CIP_VIGENT),]


#ordenados por ID y dentro por fechas:
data_ref_sud <-data_ref_sud[order(data_ref_sud$ID, data_ref_sud$DATA_MOSTRA),]

#Detecció de reinfeccions
data_ref_sud <- data_ref_sud %>%
  group_by(ID) %>% 
  filter(n()>1) %>% #Només ID amb més d'un positiu 
  mutate(MOSTRA_ANT=lag(DATA_MOSTRA)) %>%  #Data mostra immediatament anterior
  mutate(INTERVAL= strtoi(DATA_MOSTRA-lag(DATA_MOSTRA))) %>% #N dies entre mostra actual i l'anterior
  filter(INTERVAL>89)%>% #Només mostres amb interval superior als 60 dies
  mutate(DATA_INF=MOSTRA_ANT) %>% #S'assigna la data d'infecció
  mutate(DATA_REINF=DATA_MOSTRA) %>% #S'assigna la data de reinfecció
  mutate(N_REINFEC=1:n()) #Comptador de reinfeccions

reinfectats_sud <- data_ref_sud[!duplicated(data_ref_sud$CIP_VIGENT, fromLast = TRUE),]
reinfectats_sud$EDAT <- as.numeric(reinfectats_sud$EDAT)


reinfectats_sud <- reinfectats_sud %>%
  mutate(edat_estratificat = ifelse(EDAT < 5, "<5",
                                    ifelse(EDAT >=5 & EDAT < 15, "5-14",
                                           ifelse(EDAT >=15 & EDAT <25, "15-24",
                                                  ifelse(EDAT >=25 & EDAT < 35, "25-34",
                                                         ifelse(EDAT >=35 & EDAT <45, "35-44",
                                                                ifelse(EDAT >=45 & EDAT<55, "45-54",
                                                                       ifelse(EDAT >=55 & EDAT<65, "55-64",
                                                                              ifelse(EDAT >=65 & EDAT<75, "65-74",
                                                                                     ifelse(EDAT >=75 & EDAT<85, "75-84", 
                                                                                            ifelse(EDAT >=85 & EDAT<95, "85-94",
                                                                                                   ifelse(EDAT >=95, ">95", NA))))))))))))



#ordenados por ID y dentro por fechas:
reinfectats_sud <-reinfectats_sud[order(reinfectats_sud$ID, reinfectats_sud$DATA_MOSTRA),]
reinfectats.nous_sud <- reinfectats_sud[!duplicated(reinfectats_sud$CIP_VIGENT, fromLast = TRUE),]

write.xlsx(reinfectats.nous_sud, paste0(getwd(),"/Reinfeccions/Sud/reinfectats_sud_tots",today.short,".xlsx"),row.names=FALSE)


#Crear excel només amb els casos nous:
reinfectats.vells_sud <- read.xlsx(paste0(getwd(),"/Reinfeccions/Sud/reinfectats_sud_tots",yesterday.short,".xlsx"))

reinfectats_sud <- reinfectats.nous_sud %>% 
  filter(!CIP_VIGENT %in% reinfectats.vells_sud$CIP_VIGENT)

write.xlsx(reinfectats_sud, paste0(getwd(),"/Reinfeccions/Sud/reinfectats_sud",today.short,".xlsx"),row.names=FALSE)

