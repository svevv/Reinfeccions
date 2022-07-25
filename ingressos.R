############### INGRESSOS GENERALS



setwd("C:/Users/53637539K/Documents/R-SVEVV/Explotacions")

today.slash <- format(Sys.Date(), "%d/%m/%Y")
today.short <- format(Sys.Date(), "%d%m%y")
today.long <- Sys.Date()
if (as.POSIXlt(Sys.Date())$wday == 1){
  yesterday.short <- format(today.long - 3, "%d%m%y")
} else {
  yesterday.short <- format(today.long - 1, "%d%m%y")
}

if(!exists("PCR")){
  PCR <- read.csv(paste0(getwd(),"/db/",today.short,"_PCR_SA.csv"),
                  sep=";", quote="", fill = TRUE, na.strings=c("",NA))
}

PCR$Data.sequenciacio <- as.Date(PCR$Data.sequenciacio, "%d/%m/%Y")
PCR$Data.Validació.Mostra <- as.Date(PCR$Data.Validació.Mostra, "%d/%m/%Y")

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

PCR_VAC <- PCR_REF %>% select(c("ID.Mostra.x", "Id.Dades.del.Pacient.Sospitós","CIP.Vigent","Situació.del.Pacient",
                                "Localitat","Edat.Exposició","Data.Inici.Símptomes", "Nom.de.la.vacuna.1º.dosi",
                                "Data.d.administració.de.la.primera.vacuna",  "Data.d.administració.de.la.segona.vacuna", "Data.d.administració.de.la.tercera.vacuna", 
                                "CIP", "Hospitalitzat.", "Està.a.la.UCI.", "Data.Validació.Mostra","Resultat.microbiològic", 
                                "Laboratori.1",  "Sexe",  "Variant.COVID.resultant"))

names(PCR_VAC) <- c("ID_MOSTRA", "ID", "CIP_VIGENT", "SITUACIO_PACIENT", "LOCALITAT",
                    "EDAT", "DATA_INICI_SIMPTOMES", "NOM_VACUNA", "DATA_VAC_1", "DATA_VAC_2", "DATA_VAC_3", "CIP", "Hospitalització", "UCI",
                    "DATA_MOSTRA", "RESULTAT","LAB", "SEXE", "VARIANT")

PCR_VAC$TIPUS_MOSTRA <- "PCR"
PCR_VAC$DATA_MOSTRA <- as.Date(PCR_VAC$DATA_MOSTRA, "%d/%m/%Y")
PCR_VAC$DATA_INICI_SIMPTOMES <- as.Date(PCR_VAC$DATA_INICI_SIMPTOMES, "%d/%m/%Y")
#PCR_VAC <- PCR_VAC[!duplicated(PCR_VAC$CIP_VIGENT), ]
PCR_VAC <- PCR_VAC[PCR_VAC$RESULTAT=="Positiu",]
PCR_VAC <- PCR_VAC[!is.na(PCR_VAC$DATA_MOSTRA),]
PCR_VAC <- PCR_VAC[!is.na(PCR_VAC$RESULTAT),]
PCR_VAC <- PCR_VAC[!is.na(PCR_VAC$ID),]
PCR_VAC <- PCR_VAC[!is.na(PCR_VAC$CIP_VIGENT),]
PCR_VAC <- PCR_VAC[!is.na(PCR_VAC$DATA_INICI_SIMPTOMES),]



PCR_VAC <- PCR_VAC %>%
  filter(Hospitalització %in% c("Si", "Sí"))



write.xlsx(PCR_VAC, paste0(getwd(),"/Ingressos/ingressos_lab_tots",today.short,".xlsx"),row.names=FALSE, overwrite = TRUE)



PCR_yesterday <- read.xlsx(paste0(getwd(),"/Ingressos/ingressos_lab_tots",yesterday.short,".xlsx"))
PCR_yesterday$DATA_MOSTRA <- as.Date(PCR_yesterday$DATA_MOSTRA, origin = "1899-12-30")



ingressos.nous <- PCR_VAC %>%
  filter(!CIP_VIGENT %in% PCR_yesterday$CIP_VIGENT)



write.xlsx(ingressos.nous, paste0(getwd(),"/Ingressos/ingressos_referents",today.short,".xlsx"),row.names=FALSE,overwrite=T)


