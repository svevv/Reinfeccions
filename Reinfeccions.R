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
library(zoo)

### nyapa data ultima modificació 


setwd("C:/Users/53637539K/Documents/R-SVEVV/Explotacions/db")

a <- read.csv("120722_TAR_SA.csv", sep=";", quote="", fill = TRUE, na.strings=c("",NA))
b <- read.csv("180722_TAR_Data.csv", sep = ";")

TAR <-  rbind(a, b)
TAR <- TAR[!duplicated(TAR), ] 

write.csv(TAR, file = "180722_TAR_SA.csv")

setwd("C:/Users/53637539K/Documents/R-SVEVV/Explotacions")

rm(list=ls())

today.long <- Sys.Date()
today.slash <- format(Sys.Date(), "%d/%m/%Y")
today.short <- format(Sys.Date(), "%d%m%y")

if (as.POSIXlt(today.long)$wday == 1){
  yesterday.short <- format(today.long - 3, "%d%m%y")
} else {
  yesterday.short <- format(today.long - 1, "%d%m%y")
}

#Si les dates no son les actuals:
today.slash <- "25/07/2022"
today.short <- "250722"
yesterday.short <- "220722"


##### CREAR REINFECTADOS #####
#Import dataset:
if(!exists("TAR")){
  TAR <- read.csv(paste0(getwd(),"/db/",today.short,"_TAR_SA.csv"),
                    sep=";", quote="", fill = TRUE, na.strings=c("",NA))
  }

if(!exists("PCR")){
  PCR <- read.csv(paste0(getwd(),"/db/",today.short,"_PCR_SA.csv"),
                    sep=";", quote="", fill = TRUE, na.strings=c("",NA))
  }


####### VARIANTS

PCR$Data.sequenciacio <- as.Date(PCR$Data.sequenciacio, "%d/%m/%Y")
PCR$Data.Validació.Mostra <- as.Date(PCR$Data.Validació.Mostra, "%d/%m/%Y")

PCR_variants <- PCR %>%
  filter(Resultat.microbiològic %in% c("Positiu", "Positiu informat manualment o aportat pel ciutadà")) %>%
  drop_na(Variant.COVID.resultant) %>%
  group_by(CIP.Vigent, ID.Mostra) %>%
  mutate(keep.delete = ifelse(abs(Data.sequenciacio - Data.Validació.Mostra) == min(abs(Data.sequenciacio - Data.Validació.Mostra)), 'keep', 'delete')) %>%
  filter(keep.delete == "keep") %>%
  select(c("CIP.Vigent", "Data.Validació.Mostra", "Id.Variants.COVID", "ID.Mostra", "Tipus.Tècnica", "Data.sequenciacio", "Resultat.de.la.sequenciacio.de.la.prova", "ID.Actual",
           "Variant.COVID.resultant", "Especificar.altres.resultats.de.la.sequenciacio.de.la.prova", "Mutacio.1", "Mutacio.2", "Mutacio.3",
           "Mutacio.4", "Mutacio.5", "Mutacio.6", "Mutacio.7", "Mutacio.8", "Mutacio.9", "Mutacio.10", "Laboratori", "Data.recepcio.VARCO", "Edat.Exposició", 
           "Nom.de.la.vacuna.1º.dosi", 
           "Data.d.administració.de.la.primera.vacuna",
           "Data.d.administració.de.la.segona.vacuna", "Data.d.administració.de.la.tercera.vacuna", "Hospitalitzat.", "Està.a.la.UCI."))
names(PCR_variants) <- c("CIP_Vigent",	"Dia Test PCR",	"Id Variants COVID",	"ID Mostra", "Tipus Tècnica",	"Data sequenciacio",
                         "Resultat de la sequenciacio de la prova",	"ID Actual", "Variant COVID resultant",
                         "Especificar altres resultats de la sequenciacio de la prova",	"Mutacio 1",	"Mutacio 2",
                         "Mutacio 3",	"Mutacio 4",	"Mutacio 5", "Mutacio 6",	"Mutacio 7",	"Mutacio 8",	"Mutacio 9",	"Mutacio 10",
                         "Laboratori",	"Data recepcio VARCO", 
                         "EDAT", "NOM_VACUNA", "DATA_VAC_1", "DATA_VAC_2", "DATA_VAC_3", "HOSP","UCI")
PCR_variants2 <- PCR_variants[!duplicated(PCR_variants), ]  


variants.nous <- dplyr::distinct(PCR_variants)
write.xlsx(variants.nous, paste0(getwd(),"/Variants/casos_variants_tots",today.short,".xlsx"),row.names=FALSE, overwrite=TRUE)

variants.vells <- read.xlsx(paste0(getwd(),"/Variants/casos_variants_tots",yesterday.short,".xlsx"))

variants <- variants.nous %>% 
  filter(!CIP_Vigent %in% variants.vells$CIP_Vigent)

write.xlsx(variants, paste0(getwd(),"/Variants/casos_variants",today.short,".xlsx"),row.names=FALSE, overwrite=TRUE)

######## FI VARIANTS

PCR_REF2 <- PCR %>%
  filter(Resultat.microbiològic %in% c("Positiu", "Positiu informat manualment o aportat pel ciutadà")) %>%
  drop_na(Variant.COVID.resultant) %>%
  group_by(CIP.Vigent, ID.Mostra) %>%
  mutate(keep.delete = ifelse(abs(Data.sequenciacio - Data.Validació.Mostra) == min(abs(Data.sequenciacio - Data.Validació.Mostra)), 'keep', 'delete')) %>%
  filter(keep.delete == "keep") %>%
  select(c("ID.Mostra", "Id.Dades.del.Pacient.Sospitós","CIP.Vigent",
           "Situació.del.Pacient","Localitat","Edat.Exposició", 
           "Nom.de.la.vacuna.1º.dosi", 
           "Data.d.administració.de.la.primera.vacuna",
           "Data.d.administració.de.la.segona.vacuna", "Data.d.administració.de.la.tercera.vacuna","CIP", "Hospitalitzat.", "Està.a.la.UCI.",
           "Data.Validació.Mostra","Resultat.microbiològic",
           "Laboratori.1", "Sexe", "Variant.COVID.resultant",  "Id.Resultat.Laboratori"))
PCR_REF2 <- PCR_REF2[!duplicated(PCR_REF2), ]  
PCR_REF2 <- PCR_REF2 %>%
  select("CIP.Vigent", "Variant.COVID.resultant", "Data.Validació.Mostra", "ID.Mostra")

PCR <- PCR %>%
  select(-Variant.COVID.resultant)
PCR_REF <- merge(PCR_REF2, PCR, by = c("CIP.Vigent", "Data.Validació.Mostra"), all=TRUE)


#TAR_REF <- subset(TAR, select=c(1,2,6,7,8,10,11,12,27,29,28,30))
#PCR_REF <- subset(PCR, select=c(1,2,6,7,8,10,11,12,27,28,29,30))
PCR_REF <- PCR_REF %>% select(c("ID.Mostra.x", "Id.Dades.del.Pacient.Sospitós","CIP.Vigent","Situació.del.Pacient",
                            "Localitat","Edat.Exposició","Data.Inici.Símptomes","Hospitalitzat.",
                            "Està.a.la.UCI.","CIP","Data.Validació.Mostra","Resultat.microbiològic",
                            "Laboratori.1", "Nom.de.la.vacuna.1º.dosi",
                            "Data.d.administració.de.la.primera.vacuna",
                            "Data.d.administració.de.la.segona.vacuna", "Data.d.administració.de.la.tercera.vacuna", "Sexe",  "Variant.COVID.resultant",  "Id.Resultat.Laboratori"))
TAR_REF <- TAR %>% select(c("ID.Mostra", "Id.Dades.del.Pacient.Sospitós","CIP.Vigent","Situació.del.Pacient",
                            "Localitat","Edat.Exposició","Data.Inici.Símptomes","Hospitalitzat.",
                            "Està.a.la.UCI.","CIP.Vigent.1","Data.de.la.prova",
                            "Resultat.microbiològic","Codi.SNOMED.de.la.prova", "Nom.de.la.vacuna.1º.dosi",
                            "Data.d.administració.de.la.primera.vacuna",
                            "Data.d.administració.de.la.segona.vacuna", "Data.d.administració.de.la.tercera.vacuna", "Sexe",  "Variant.COVID.resultant",  "Id.Test.ràpid"))

#Eliminar test serologico de TAR:
summary(TAR_REF$Codi.SNOMED.de.la.prova)

TAR_REF <- TAR_REF[TAR_REF$Codi.SNOMED.de.la.prova=="Antígen del coronavirus 2 del SARS",]

#TAR_REF <- subset(TAR_REF, select=c(1:14))
TAR_REF$LAB <- "0" 
TAR_REF <- TAR_REF %>%
  select(-Codi.SNOMED.de.la.prova)


names(PCR_REF) <- c("ID_MOSTRA", "ID", "CIP_VIGENT", "SITUACIO_PACIENT", "LOCALITAT",
                "EDAT","DATA_SIMPTOMES","HOSP","UCI", "CIP", "DATA_MOSTRA", 
                "RESULTAT","LAB", "NOM_VACUNA", "DATA_VAC_1", "DATA_VAC_2", "DATA_VAC_3", "SEXE", "VARIANT", "ID_TEST")
names(TAR_REF) <- c("ID_MOSTRA", "ID", "CIP_VIGENT", "SITUACIO_PACIENT", "LOCALITAT",
                    "EDAT","DATA_SIMPTOMES","HOSP","UCI", "CIP", "DATA_MOSTRA", 
                    "RESULTAT", "NOM_VACUNA", "DATA_VAC_1", "DATA_VAC_2", "DATA_VAC_3", "SEXE", "VARIANT", "ID_TEST", "LAB")

PCR_REF$TIPUS_MOSTRA <- "PCR"
TAR_REF$TIPUS_MOSTRA <- "TAR"
TAR_REF$DATA_MOSTRA <- as.Date(TAR_REF$DATA_MOSTRA, "%d/%m/%Y")

data_ref <- rbind(PCR_REF,TAR_REF)

#Es substitueix 0020 i 0021 per 2020 i 2021 - No deixar i revisar errors
data_ref$DATA_MOSTRA <- str_replace(data_ref$DATA_MOSTRA, "0020-", "2020-")
data_ref$DATA_MOSTRA <- str_replace(data_ref$DATA_MOSTRA, "0200-", "2020-")
data_ref$DATA_MOSTRA <- str_replace(data_ref$DATA_MOSTRA, "0021-", "2021-")

data_ref$DATA_MOSTRA <- as.Date(data_ref$DATA_MOSTRA)

#Eliminar observaciones sin resultado
#data <- data[!is.na(data$RESULTAT),]
data_ref <- data_ref[!is.na(data_ref$DATA_MOSTRA),]
data_ref <- data_ref[data_ref$RESULTAT %in% c("Positiu", "Positiu informat manualment o aportat pel ciutadà"), ]
data_ref <- data_ref[!is.na(data_ref$RESULTAT),]
data_ref <- data_ref[!is.na(data_ref$ID),]
data_ref <- data_ref[!is.na(data_ref$CIP_VIGENT),]
data_ref <- data_ref[!duplicated(data_ref$ID_TEST), ] 


#ordenados por ID y dentro por fechas:
data_ref <-data_ref[order(data_ref$ID, data_ref$DATA_MOSTRA),]

#Detecció de reinfeccions
data_ref <- data_ref %>%
  group_by(ID) %>% 
  filter(n()>1) %>% #Només ID amb més d'un positiu 
  mutate(MOSTRA_ANT=lag(DATA_MOSTRA)) %>%  #Data mostra immediatament anterior
  mutate(INTERVAL= strtoi(DATA_MOSTRA-lag(DATA_MOSTRA))) %>% #N dies entre mostra actual i l'anterior
  filter(INTERVAL>29)%>% #Només mostres amb interval superior als 60 dies
  mutate(DATA_INF=MOSTRA_ANT) %>% #S'assigna la data d'infecció
  mutate(DATA_REINF=DATA_MOSTRA) %>% #S'assigna la data de reinfecció
  mutate(N_REINFEC=1:n()) #Comptador de reinfeccions

data_ref <- data_ref[, c("ID", "CIP_VIGENT", "DATA_INF", "DATA_REINF", "INTERVAL", "N_REINFEC","TIPUS_MOSTRA",  "EDAT", "NOM_VACUNA", "DATA_VAC_1", "DATA_VAC_2", "DATA_VAC_3", 
                         "LOCALITAT", "DATA_SIMPTOMES", "HOSP", "UCI", "CIP", "DATA_MOSTRA", "RESULTAT", "LAB", "SITUACIO_PACIENT", "SEXE", "VARIANT", "ID_TEST")]


#ordenados por ID y dentro por fechas. Se incluyen todas las infecciones para un mismo CIP, indicando el número de reinfección.
reinfectats <-data_ref[order(data_ref$ID, data_ref$DATA_MOSTRA),]
reinfectats$EDAT <- as.numeric(reinfectats$EDAT)

reinfectats <- reinfectats %>%
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


write.xlsx(reinfectats, paste0(getwd(),"/Reinfeccions/reinfectats_referents_tots",today.short,".xlsx"),row.names=FALSE,overwrite=T)
####

#Crear excel només amb els casos nous:
reinfectats.vells <- read.xlsx(paste0(getwd(),"/Reinfeccions/reinfectats_referents_tots",yesterday.short,".xlsx"))
reinfectats.vells$DATA_INF <- as.Date(reinfectats.vells$DATA_INF, origin = "1899-12-30")
reinfectats.vells$DATA_REINF <- as.Date(reinfectats.vells$DATA_REINF, origin = "1899-12-30")
reinfectats.vells$DATA_MOSTRA <- as.Date(reinfectats.vells$DATA_MOSTRA, origin = "1899-12-30")

reinfectats.nous <- reinfectats %>% 
  filter(!CIP_VIGENT %in% reinfectats.vells$CIP_VIGENT)

write.xlsx(reinfectats.nous, paste0(getwd(),"/Reinfeccions/reinfectats_referents",today.short,".xlsx"),row.names=FALSE,overwrite=T)

reinfectats.30 <- reinfectats.nous %>%
 filter(INTERVAL<90)

write.xlsx(reinfectats.30, paste0(getwd(),"/Reinfeccions/30dies/reinfectats_30_",today.short,".xlsx"),row.names=FALSE,overwrite=T)
reinfectats.30.tots <- reinfectats %>%
  filter(INTERVAL<90)


write.xlsx(reinfectats.30.tots, paste0(getwd(),"/Reinfeccions/30dies/reinfectats_30_tots",today.short,".xlsx"),row.names=FALSE,overwrite=T)
