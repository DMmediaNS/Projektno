library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(scales)
library(viridis)
library(crosstalk)
library(lubridate)
library(writexl)
library(shinycssloaders)

options(scipen = 999)




###Priprema podskupova za razlicite tabove same aplikacije

##Importovanje podataka iz excel tabele

Projectmedia<-read_excel("Projektno sufinansiranje medija.xlsx")




##OPSTI PREGLED TAB

#Podskup za ukupno dodeljeno sredstava po godinama

yearstable <- Projectmedia %>%
  
  group_by(GODINA) %>%
  
  summarise(`UKUPNA SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`))

#Formatiranje sredstava

yearstable$`UKUPNA SREDSTVA U EVRIMA` <- format(yearstable$`UKUPNA SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE)

yearstable$GODINA <- format(yearstable$GODINA, digits=0)

#Podskup za bar chart- institucije/davaoci javnog novca


barchartorgani <- Projectmedia%>%
  filter(!`SREDSTVA U EVRIMA`==0)

#Stavljanje svih opstina pod zajednicki naziv lokalne samouprave

barchartorgani$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` [barchartorgani$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture" & barchartorgani$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`!="Pokrajinski sekretarijat za kulturu i javno informisanje"] <- "Lokalne samouprave"

#Grupisanje prema organu koji dodeljuje sredstva sumiranje sa kolone sa sredstvima u evrima
#i redjanje od najvece vrednosti ka najmanjoj

barchartorgani <- barchartorgani %>%
  
  group_by(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`) %>%
  
  summarise(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`),n=n())%>%
  
  arrange(desc(`SREDSTVA U EVRIMA`))

#Skracivanje naziva za pokrajinski sekretarijat u grafikonu

barchartorgani$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`[barchartorgani$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`=="Pokrajinski sekretarijat za kulturu i javno informisanje"] <- "Pokrajinski sekretarijat"

#dodavanje Info kolone za pop-up informacije koje ce se pojaviti kada kursor predje preko grafikona

barchartorgani <- barchartorgani %>%
  
  mutate(Info = paste('<br>', "Institucija koja raspisuje konkurs:", 
                      
                      `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`, '<br>',
                      
                      "Ukupno novca:",format(`SREDSTVA U EVRIMA`,big.mark = ","), 'eur<br>',
                      
                      "Broj projekata:",n,"<br>"))


#Podskup za linechart za sve teme za sve godine 

linechartteme <- Projectmedia

#Iskljucivanje slucajeva kada sredstva nisu dodeljena

linechartteme <- linechartteme%>%
  filter(!`SREDSTVA U EVRIMA`==0)

# Promena NA vrednosti sa Nema informacija
linechartteme$`TEMA PROJEKTA`[is.na(linechartteme$`TEMA PROJEKTA`)] <- "Nema informacija"

#Formatiranje kolone teme u faktor zbor prikazivanja na linechart-u

linechartteme$`TEMA PROJEKTA` <- as.factor(linechartteme$`TEMA PROJEKTA`)

#Grupisanje prema godini i temi, sumiranje prema broju projekata po temi i
#kreiranje Info kolone  za pop-up informacije kada se predje kursorom

linechartteme <- linechartteme %>%
  
  group_by(GODINA,`TEMA PROJEKTA`) %>%
  
  summarize(n=n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info= paste("<br>","Tema:",`TEMA PROJEKTA`,"<br>",
                     
                     "Broj projekata po temi:",n,"<br>",
                     
                     "Ukupno dodeljeno sredstava po temi:",format(`SREDSTVA U EVRIMA`,big.mark = ","),"eur<br>",
                     
                     "Godina:",GODINA,"<br>"))


#Formatiranje kolone sa godinama

linechartteme$GODINA <- as.factor(linechartteme$GODINA)


##PODNOSIOCI PROJEKTA-TAB 

#Podskup za pretrazivu tabelu-izbor kolona za prikazivanje i iskljucivanje konkursa gde sredstva nisu dodeljena

tabelapodnosioci <- Projectmedia %>%
  
  select(`PODNOSILAC PROJEKTA`,`NAZIV MEDIJA`,`NAZIV PROJEKTA`,`TEMA PROJEKTA`,`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,GODINA,`SREDSTVA U DINARIMA`,`SREDSTVA U EVRIMA`) %>%
  
  filter(!`SREDSTVA U EVRIMA` == 0)

#formatiranje kolone sa sredstvima

tabelapodnosioci$`SREDSTVA U DINARIMA` <- format( tabelapodnosioci$`SREDSTVA U DINARIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE)

tabelapodnosioci$`SREDSTVA U EVRIMA` <- format( tabelapodnosioci$`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE)

#Podskup za malu tabelu sa ukupnom kolicinom novca po podnosiocu i broju medija


podnosiocipojsuma <- Projectmedia %>%
  
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarise(`UKUPNO DODELJENO U EVRIMA`= sum(`SREDSTVA U EVRIMA`)) 


podnosiocimediji <- Projectmedia

podnosiocimediji$`NAZIV MEDIJA`[ podnosiocimediji$`NAZIV MEDIJA`== "Produkcija"] <- NA

podnosiocimediji<- podnosiocimediji %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>% 
  
  summarize( `BROJ IDENTIFIKOVANIH MEDIJA ZA KOJE JE PODNOSILAC APLICIRAO` = n_distinct(`NAZIV MEDIJA`,na.rm = TRUE))

podnosiocipojsuma <- Projectmedia %>%
  
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarise(`UKUPNO DODELJENO U EVRIMA`= sum(`SREDSTVA U EVRIMA`)) 

podnosiociorgani<- Projectmedia %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>% 
  
  summarize( `BROJ ORGANA OD KOJIH JE PODNOSILAC DOBIO SREDSTVA` = n_distinct(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`))  


podnosiocitotal <- left_join(podnosiocipojsuma, podnosiocimediji, by = "PODNOSILAC PROJEKTA")
podnosiocitotal <-left_join(podnosiocitotal, podnosiociorgani,by = "PODNOSILAC PROJEKTA")
#Formatiranje kolone sa sredstvima

podnosiocitotal$`UKUPNO DODELJENO U EVRIMA` <- format( 
  
  podnosiocitotal$`UKUPNO DODELJENO U EVRIMA`, big.mark = ',', digits = 0, nsmall=0, scientific = FALSE)



##Podskup  za top 4 podnosioca projekta za barchart za sve godine od svih davalaca javnog novca 

barcharttop4podnosioci <- Projectmedia %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`), n=n()) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  top_n(4,`SREDSTVA U EVRIMA`)

#skracivanje imena zbog prikazivanja na grafikonu za top 4 podnosioca

barcharttop4podnosioci$`PODNOSILAC PROJEKTA`[barcharttop4podnosioci$`PODNOSILAC PROJEKTA`=="RADIO TELEVIZIJA NOVI PAZAR DOO NOVI PAZAR"] <- "RADIO TELEVIZIJA NOVI PAZAR"
barcharttop4podnosioci$`PODNOSILAC PROJEKTA`[barcharttop4podnosioci$`PODNOSILAC PROJEKTA`=="RADIO TELEVIZIJA BELLE AMIE DOO NIŠ"] <- "RADIO TELEVIZIJA BELLE AMIE"

barcharttop4podnosioci<- barcharttop4podnosioci %>%
  mutate(Info = paste('<br>', "Podnosilac:", 
                      
                      `PODNOSILAC PROJEKTA`, '<br>',
                      
                      "Ukupno dobijenih sredstava:", 
                      
                      format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Broj projekata:",n,"<br>"
                      
  ))



#podskup za podnosioce po godinama linechart



yearssredstvapodnosioci <- Projectmedia %>%
  
  group_by(`PODNOSILAC PROJEKTA`,`GODINA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Godina:", 
                      
                      `GODINA`, '<br>',
                      
                      "Ukupno sredstava:",
                      
                      format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>'
                      
  ))



#pie chart tema za pojedinacnog podnosioca


piecharttemepodnosiocimain <- Projectmedia 

# Promena NA vrednosti sa Nema informacija

piecharttemepodnosiocimain $`TEMA PROJEKTA`[is.na(piecharttemepodnosiocimain$`TEMA PROJEKTA`)] <- "Nema informacija"

#grupisanje prema podnosiocu i temi projekta, sumiranje i na osnovu broja projekata
#i na osnovu sredstava u EVRIMA dodavanje kolone sa informacijama za pop-up i
#dodavanje kolone sa bojama

piecharttemepodnosiocimain  <- piecharttemepodnosiocimain  %>%
  
  
  group_by(`PODNOSILAC PROJEKTA`,`TEMA PROJEKTA`) %>%
  
  summarize( n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Tema:", 
                      
                      `TEMA PROJEKTA`, '<br>',
                      
                      "Ukupno dobijeno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Učešće u dobijenim sredstvima:",round(`SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100,2),'%<br>',
                      
                      "Ukupan broj projekata:", n, "<br>")) %>%
  
  
  
  mutate( boje = case_when (  `TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                              
                              `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                              
                              `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                              
                              `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                              
                              `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                              
                              `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                              
                              `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                              
                              `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                              
                              `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF"
                              
  ))


#pie chart za sve teme podnosilaca

piecharttemepodnosall<-Projectmedia

#iskljucivanje konkursa gde nisu dodeljena sredstva

piecharttemepodnosall <- piecharttemepodnosall%>%
  filter(!`SREDSTVA U EVRIMA`==0)

# Promena NA vrednosti sa Nema informacija

piecharttemepodnosall$`TEMA PROJEKTA`[is.na(piecharttemepodnosall$`TEMA PROJEKTA`)] <- "Nema informacija"

#grupisanje  prema temi projekata, sumiranje i po broju projekata i prema sredstvima u dinarima,
#dodavanje kolone sa informacijama za pop-up i kolone sa bojama


piecharttemepodnosall <- piecharttemepodnosall %>%
  
  group_by(`TEMA PROJEKTA`) %>%
  
  summarize( n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Tema:", 
                      
                      `TEMA PROJEKTA`, '<br>',
                      
                      "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Učešće u dodeljenim sredstvima:", round( `SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100, 2),"%<br>",
                      
                      "Ukupan broj projekata:", n, "<br>"
                      
  )) %>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF"))

#Mini tabela sa medijima koji su registrovani u APR za datog izdavaca

#Importovanje podataka iz excel tabele za registrovane medije u APR-u

aprregistrovanimedijifin<- read_excel("aprregmediji2020.xlsx")

aprregistrovanimedijifin<-aprregistrovanimedijifin%>%
  select(-`MATIČNI BROJ PODNOSIOCA`)

##MINISTARSTVO KULTURE -TAB

#Podskup za sve teme  za koje je Ministarstvo kulture dalo novac 
#grupisanje prema temi, sumiranje prema broju projekata i sredstvima u EVRIMA,
#dodavanje kolone za informacije za pop-up i  kolone sa bojama

piecharttemekult <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Ministarstvo kulture") %>%
  
  group_by(`TEMA PROJEKTA`) %>%
  
  summarize(n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`))%>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF")) %>%
  
  mutate(Info=paste('<br>', "Tema:", 
                    
                    `TEMA PROJEKTA`, '<br>',
                    
                    "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                    
                    "Učešće u dobijenim sredstvima:",round(`SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100,2),'%<br>',
                    
                    "Ukupan broj projekata:",n,"<br>"
                    
  ))


#Podskup za top 4 podnosioca projekta kojima je Ministarstvo kulture dalo novac 

barcharttop4podnosiocikult <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Ministarstvo kulture") %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`),n=n()) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  top_n(4,`SREDSTVA U EVRIMA`)

#skracivanje imena zbog  lepseg prikazivanja na grafikonu

barcharttop4podnosiocikult$`PODNOSILAC PROJEKTA`[barcharttop4podnosiocikult$`PODNOSILAC PROJEKTA`=="PREDUZEĆE ZA PROIZVODNJU I DISTRIBUCIJU RTV PROGRAMA, TRGOVINU I USLUGE TV APATIN DRUŠTVO SA OGRANIČENOM ODGOVORNOŠĆU APATIN"] <- "TV APATIN DOO"
barcharttop4podnosiocikult$`PODNOSILAC PROJEKTA`[barcharttop4podnosiocikult$`PODNOSILAC PROJEKTA`=="RADIO TELEVIZIJA VRANJE D.O.O. VRANJE"] <- "RADIO TELEVIZIJA VRANJE"


barcharttop4podnosiocikult<-barcharttop4podnosiocikult%>%
  
  mutate(Info=paste('<br>', "Podnosilac:", 
                    
                    `PODNOSILAC PROJEKTA`, '<br>',
                    
                    "Ukupno dobijenih sredstava:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                    
                    "Broj projekata:",n,"<br>"
  ))



#podskup za linechart-koliko je para dato po temi po godinama
#biranje samo konkursa ministarstva kulture, grupisanje prema temi i godini
#sumiranje prema broju projekata i kolicini novca i dodavanja kolone za pop-up i za boje

linecharttemekult <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Ministarstvo kulture") %>%
  
  group_by(`TEMA PROJEKTA`,GODINA) %>%
  
  summarize(n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF")) %>%
  
  mutate(Info = paste("<br>","Tema:",`TEMA PROJEKTA`,"<br>",
                      
                      "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE),"eur<br>",
                      
                      "Broj projekata:",n,"<br>",
                      
                      "Godina:",GODINA,"<br>"))

#formatiranje kolone sa godinama

linecharttemekult$GODINA <- as.factor(linecharttemekult$GODINA)


#Podskup za pretrazivu tabelu ministarstva kulture

tabelakultura <- Projectmedia %>%
  
  select(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,`PODNOSILAC PROJEKTA`,`NAZIV MEDIJA`,`NAZIV PROJEKTA`,`TEMA PROJEKTA`,`GODINA`,`SREDSTVA U DINARIMA`,`SREDSTVA U EVRIMA`) %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Ministarstvo kulture")

#formatiranje numerickih kolona

tabelakultura$`SREDSTVA U DINARIMA` <- format( tabelakultura$`SREDSTVA U DINARIMA` , big.mark =',', digits = 0, nsmall = 0, scientific = FALSE)

tabelakultura$`SREDSTVA U EVRIMA` <- format( tabelakultura$`SREDSTVA U EVRIMA` , big.mark =',', digits = 0, nsmall = 0, scientific = FALSE)


##POKRAJINSKI SEKRETARIJAT-TAB

#Podskup sa svim  temama za koje je Pokrajinski sekretarijat dao novac 


piecharttemepoksek <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Pokrajinski sekretarijat za kulturu i javno informisanje") %>%
  
  group_by(`TEMA PROJEKTA`) %>%
  
  summarize( n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Tema:", 
                      
                      `TEMA PROJEKTA`, '<br>',
                      
                      "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Učešće u dodeljenim sredstvima:", round( `SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100,2),"%<br>",
                      
                      "Ukupan broj projekata:", n, "<br>"
                      
  )) %>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF"))

#podskup za linechart za pokrajinski sekretarijat koliko je para dato po pojedinacnoj temi po godinama

linecharttemepoksek <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Pokrajinski sekretarijat za kulturu i javno informisanje") %>%
  
  group_by(`TEMA PROJEKTA`,GODINA) %>%
  
  summarize(n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  mutate( boje = case_when( `TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF")) %>%
  
  mutate(Info = paste("<br>","Tema:",`TEMA PROJEKTA`,"<br>",
                      
                      "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE),"eur<br>",
                      
                      "Broj projekata:",n,"<br>",
                      
                      "Godina:",GODINA,"<br>"))

#Formatiranje kolone godine

linecharttemepoksek$GODINA <- as.factor(linecharttemepoksek$GODINA)


#Podskup za top 4 podnosioca kojima je Pokrajinski sekretarijat dao novac 

barcharttop4podnosiocipoksek <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Pokrajinski sekretarijat za kulturu i javno informisanje") %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`),n=n()) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  top_n(4,`SREDSTVA U EVRIMA`) 

#Skracivanje imena za lepsi prikaz na grafikonu

barcharttop4podnosiocipoksek$`PODNOSILAC PROJEKTA`[barcharttop4podnosiocipoksek$`PODNOSILAC PROJEKTA`=="DNEVNIK VOJVODINA PRESS DOO PREDUZEĆE ZA IZDAVANJE I ŠTAMPANJE NOVINA, NOVI SAD"] <- "DNEVNIK VOJVODINA PRESS"

barcharttop4podnosiocipoksek$`PODNOSILAC PROJEKTA`[barcharttop4podnosiocipoksek$`PODNOSILAC PROJEKTA`=="SAVEZ GLUVIH I NAGLUVIH VOJVODINE AUDIOLOŠKI CENTAR"] <- "SAVEZ GLUVIH I NAGLUVIH VOJVODINE"


# dodavanje kolone sa informacijama za pop-up

barcharttop4podnosiocipoksek <- barcharttop4podnosiocipoksek %>%
  
  mutate(Info = paste('<br>', "Podnosilac:", 
                      
                      `PODNOSILAC PROJEKTA`, '<br>',
                      
                      "Ukupno dobijena sredstava:", format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Broj projekata:",n,"<br>"
                      
  ))


#Podskup za pretrazivu tabelu pokrajinskog sekretarijata

tabelapoksek <- Projectmedia %>%
  
  select(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,`PODNOSILAC PROJEKTA`,`NAZIV MEDIJA`,`NAZIV PROJEKTA`,`TEMA PROJEKTA`,`GODINA`,`SREDSTVA U DINARIMA`,`SREDSTVA U EVRIMA`) %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Pokrajinski sekretarijat za kulturu i javno informisanje")

#Formatiranje kolone sa sredstvima

tabelapoksek$`SREDSTVA U DINARIMA` <- format( tabelapoksek$`SREDSTVA U DINARIMA` , big.mark =',', digits = 0, nsmall = 0,scientific = FALSE)
tabelapoksek$`SREDSTVA U EVRIMA` <- format( tabelapoksek$`SREDSTVA U EVRIMA` , big.mark =',', digits = 0, nsmall = 0, scientific = FALSE)


##LOKALNE SAMOUPRAVE-TAB

#Podskup za top 4 lokalne samouprave koje su dale najvise novca- mala tabela

tabela4opstine <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje" & `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  top_n(4) %>%
  
  rename(`ČETIRI OPŠTINE KOJE SU DALE NAJVIŠE SREDSTAVA`=`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)


#Formatiranje kolone za sredstva

tabela4opstine$`SREDSTVA U EVRIMA` <- format( tabela4opstine$`SREDSTVA U EVRIMA` , big.mark =',', digits = 0, nsmall = 0, scientific = FALSE)


# Podskup za malu tabelu  za koliko je ukupno sredstava dato po opstini

tabelaukupnopstina <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje" & `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`) %>%
  
  summarise(`UKUPNO DODELJENO U EVRIMA`= sum(`SREDSTVA U EVRIMA`)) %>%
  
  rename(`OPŠTINA` = `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)


#Formatiranje kolone sa sredstvima

tabelaukupnopstina$`UKUPNO DODELJENO U EVRIMA` <- format( 
  
  tabelaukupnopstina$`UKUPNO DODELJENO U EVRIMA`, big.mark = ',',digits = 0, nsmall = 0, scientific = FALSE )

#Podskup za veliku pretrazivu tabelu

tabelaopstinee <- Projectmedia %>%
  
  select(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`, `PODNOSILAC PROJEKTA`, `NAZIV MEDIJA`, `NAZIV PROJEKTA`, `TEMA PROJEKTA`, `GODINA`, `SREDSTVA U DINARIMA`, `SREDSTVA U EVRIMA`) %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`!="Pokrajinski sekretarijat za kulturu i javno informisanje" & `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture")


#Podskup za teme za koje su sve lokalne samouprave dale novac 


piecharttemeloksam <- Projectmedia 

#iskljucivanje konkursa na kojima nisu dodeljena sredstva 

piecharttemeloksam <- piecharttemeloksam%>%
  filter(!`SREDSTVA U EVRIMA`==0)


# Promena NA vrednosti sa Nema informacija

piecharttemeloksam$`TEMA PROJEKTA`[is.na(piecharttemeloksam$`TEMA PROJEKTA`)] <- "Nema informacija"


piecharttemeloksam <- piecharttemeloksam %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje"& `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`TEMA PROJEKTA`) %>%
  
  summarize( n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Tema:", 
                      
                      `TEMA PROJEKTA`, '<br>',
                      
                      "Ukupno dodeljeno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Učešće u dobijenim sredstvima:",round(`SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100,2),'%<br>',
                      
                      "Ukupan broj projekata:", n, "<br>")) %>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF"))

#Podskup za linechart kako su lokalne samouprave dodeljivale novac po godinama


yearssredstvaloksam <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje"& `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,`GODINA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Godina:", 
                      
                      `GODINA`, '<br>',
                      
                      "Ukupno u evrima:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), '<br>'
                      
  ))

#Podskup za top 4 podnosioca kojima su sve lokalne samouprave dale novac

barcharttop4podnosiociloksam <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje"& `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`),n=n()) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  top_n(4,`SREDSTVA U EVRIMA`) 


barcharttop4podnosiociloksam$`PODNOSILAC PROJEKTA`[barcharttop4podnosiociloksam$`PODNOSILAC PROJEKTA`=="RADIO TELEVIZIJA NOVI PAZAR DOO NOVI PAZAR"] <- "RADIO TELEVIZIJA NOVI PAZAR"
barcharttop4podnosiociloksam$`PODNOSILAC PROJEKTA`[barcharttop4podnosiociloksam$`PODNOSILAC PROJEKTA`=="RADIO TELEVIZIJA BELLE AMIE DOO NIŠ"] <- "RADIO TELEVIZIJA BELLE AMIE"



barcharttop4podnosiociloksam <- barcharttop4podnosiociloksam %>%
  
  mutate(Info = paste('<br>', "Podnosilac:", 
                      
                      `PODNOSILAC PROJEKTA`, '<br>',
                      
                      "Ukupno dobijena sredstava:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      "Broj projekata:",n,"<br>"
                      
  ))

#Podskup za teme po pojedinacnoj lok samoupravi

piecharttemeloksampoj <- Projectmedia


piecharttemeloksampoj  <- piecharttemeloksampoj %>%
  filter(!`SREDSTVA U EVRIMA`==0)

# Promena NA vrednosti sa Nema informacija

piecharttemeloksampoj$`TEMA PROJEKTA`[is.na(piecharttemeloksampoj$`TEMA PROJEKTA`)] <- "Nema informacija"


piecharttemeloksampoj <- piecharttemeloksampoj %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje"& `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,`TEMA PROJEKTA`) %>%
  
  summarize( n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Tema:", 
                      
                      `TEMA PROJEKTA`, '<br>',
                      
                      "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Učešće u dodeljenim sredstvima:",round(`SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100,2),'%<br>',
                      
                      "Ukupan broj projekata:", n, "<br>")) %>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF"))




