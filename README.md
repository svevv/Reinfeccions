# Reinfeccions
#################Reinfeccions diàries##################
Dia anterior: preparar explotació (CCSVE: Cerca – Cerca multi consulta – Pacients sospitosos – Multi PCR CCSVE (guardar fitxer amb format ddmmaa_PCR_SA) i Multi TAR CCSVE (ddmmaa_TAR_SA). SA son les meves inicials (adaptar si cal)
Mateix dia:
1.	Descarregar explotacions PCR i TAR del dia anterior i guardar fitxer a C:\Users\53637539K\Documents\R-SVEVV\Explotacions\db
2.	Executar script sense la part que posa ##nyapa 
En cas que el dia anterior no funcioni l’explotació:
1.	Preparar explotació casos: afegim filtre “Data modif Dades del pacient sospitós” a la consulta SOSPITOSOS.CCSVE i fiquem dates que volem. Guardem com a SOSPITOSOS.CCSVE_DATA
a.	Si ha fallat PCR, afegir filtre “Data validació mostra” a PCR i fiquem les dates que volem. Guardem consulta com a PCR_DATA
b.	Si ha fallat TAR, afegir filtre “Data de la prova entre” a TAR i fiquem les dates que volem. Guardem consulta com a TAR_DATA
2.	Fer multiconsulta amb SOSPITOSOS.CCSVE_DATA i TAR/PCR_DATA. Guardar amb format (ddmmaa_PCR/TAR_Data).
3.	Executar la part ##nyapa per a obtenir fitxer TAR/PCR amb dades completes
 
4.	Continuem script de forma habitual
Un cop tenim els arxius de reinfeccions, s’han de penjar casos per enquestadors i per a seqüenciar.
1.	Per enquestadors:
a.	Agafem arxiu reinfectats_30_ddmmaa, filtrem columna DATA_REINF pel mes actual i penjem els casos a Teams – Equip – SVE VALLES ENQUESTADORS – General – Fitxers – REINFECCIONS – Excel REINFECCIÓ (pestanya casos 2022)
b.	Avisem que s’han penjat els casos pel xat de reinfeccions
2.	Per seqüenciar (Khadija)
a.	Agafem arxiu reinfectats_ddmmaa i filtrem columna DATA_INF per l’any 2022 i DATA_REINF per l’any 2022. Penjem els casos en format corresponent a Teams -  SVE VALLES – General – Fitxers – variants i resultats laboratori.xlsx (pestanya reinfeccions)

##################Ingressos diaris################
Amb la mateixa explotació PCR i TAR que hem fet el dia anterior, executem script ingressos. Filtrem per ingressos mes actual i penjem els casos en format corresponent a Teams -  SVE VALLES – General – Fitxers – variants i resultats laboratori.xlsx (pestanya reinfeccions)



#################Informe bisetmanal reinfeccions àrea metropolitana####################
Cada dos setmanes es fa un informe de reinfeccions de tota l’àrea metropolitana de Barcelona 
1.	Obrir els Scripts (Reinfeccions.R i Reinfeccions_nordisud.R) i canviar les dates. IMPORTANTISSIM comprovar quin va ser l’últim dia que es va fer i ficar aquesta com “yesterday.short”. Els arxius es troben a > R-SVEVV > Explotacions > Reinfeccions (Vallès), > Nord_i_Maresme (Nord), > Sud (Sud). 
2.	Executar el Script. 

Es creen dos excels a > R-SVEVV > Explotacions > Reinfeccions, dos a > R-SVEVV > Explotacions > Reinfeccions > Nord_i_Maresme, i dos a > R-SVEVV > Explotacions > Reinfeccions > Sud: 
1.	“reinfectats_referentsDMA.xlsx”, “reinfectats_nordDMA.xlsx”, reinfectats_sudDMA.xlsx”. Aquí només es guarden les noves reinfeccions, és a dir, les que s’han detectat des de que es va fer per últim cop i que no estaven aquell dia. 
2.	“reinfectats_referents_totsDMA.xlsx”, “reinfectats_nord_totsDMA.xlsx”, reinfectats_sud_totsDMA.xlsx”. Aquí es guarden totes les sospites de reinfecció. Fer servir aquest excel per fer l’informe: 
a.	Copiar i enganxar una copia de l’informe anterior per poder modificar-lo. Obrir l’informe i els excels “reinfectats_referents_totsDMA.xlsx”, “reinfectats_nord_totsDMA.xlsx”, reinfectats_sud_totsDMA.xlsx” d’avui i de l’últim dia. Copiar de la pestanya on es troben tots els reinfectats les fórmules i encapçalaments de les últimes columnes.  
b.	Afegir les pestanyes igual que els excels antics (amb els mateixos noms per desprès copiar i enganxar els gràfics i poder modificar-los més fàcilment. 
c.	Afegir una taula dinàmica. 
d.	Amb la taula dinàmica fer les taules de dades que es necessiten en cada pestanya diferent. 
e.	Afegir aquesta informació a la pestanya corresponent i copiar i enganxar el gràfic del excel antic. Modificar el gràfic per que estigui fet amb la informació del excel nou. 
f.	Enganxar els gràfics i taules corresponents a l’informe. 
g.	Canviar els comentaris i/o fer de nous. 
h.	Enviar els tres excels (“reinfectats_referents_totsDMA.xlsx”, “reinfectats_nord_totsDMA.xlsx”, reinfectats_sud_totsDMA.xlsx) i el word (“Informe_Reinfeccions_BCNMetro_DMA.docx”) a Arantxa Romero Tamarit: arantxa.romero@gencat.cat 


