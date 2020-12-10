### UZL-Auswertung (REPORT)----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Date created:     2020-12-10
# Location created: Tellstrasse 32, Bern
# Last Entry:       2020-12-10
# Author:           Raphael S. von Bueren (GitHub: buerap)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Get started (Working directory, packages, ...)----
rm(list = ls(all = TRUE) )
Sys.setenv(TZ = "Europe/Zurich")
graphics.off()                    # Clear Graphic Window
cat( "\014" )                     # Clear Console ( =  CTRL L)

#install.packages("RSQLite")
library(tidyverse)
#library(Rmisc)
library(ggthemes)
#library(readxl)
#library(devtools)
#install_github("TobiasRoth/BDM")
#library(BDM)

theme_set(             # ggplot theme()-default Einstellungen
  theme_clean() +
    theme(
      legend.title = element_blank(), 
      legend.position = "top", 
      legend.background = element_rect(colour = "white"))
)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Connection to data base----
db <- src_sqlite(path = "database/DB_BDM.db", create = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Vorbereitung Daten----
# Pflanzen Z7
PL.Z7 <- tbl(db, "KD_Z7") %>%   
  filter(!is.na(yearPl)) %>%   
  filter(yearPl<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  
  left_join(tbl(db, "Raumdaten_Z7")) %>%  
  filter(Verdichtung_BDM == "nein") %>%   
  left_join(tbl(db, "STICHPROBE_Z7")) %>% 
  filter(BDM_aktuell == "ja") %>%         
  left_join(tbl(db, "PL") %>%  
              filter(!is.na(aID_SP)) %>%  
              left_join(tbl(db, "Arten"))) %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL) %>% 
  as_tibble()

# Tagfalter Z7
TF.Z7 <- tbl(db, "KD_Z7") %>%   
  filter(!is.na(yearBu)) %>%   
  filter(yearBu<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  
  left_join(tbl(db, "Raumdaten_Z7")) %>%  
  filter(Verdichtung_BDM == "nein") %>%   
  left_join(tbl(db, "STICHPROBE_Z7")) %>% 
  filter(BDM_aktuell == "ja") %>%         
  left_join(tbl(db, "TF") %>%  
              filter(!is.na(aID_SP)) %>%  
              left_join(tbl(db, "Arten"))) %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBu, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL) %>% 
  as_tibble()

# Voegel Z7
BI.Z7 <- tbl(db, "KD_Z7") %>%   
  filter(!is.na(yearBi)) %>%   
  filter(yearBi<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  
  left_join(tbl(db, "Raumdaten_Z7")) %>%  
  filter(Verdichtung_BDM == "nein") %>%   
  left_join(tbl(db, "STICHPROBE_Z7")) %>% 
  filter(BDM_aktuell == "ja") %>%         
  left_join(tbl(db, "BI") %>%  
              filter(!is.na(aID_SP)) %>%  
              left_join(tbl(db, "Arten"))) %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBi, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL) %>% 
  as_tibble()

# Mollusken Z9
MOL.Z9 <- tbl(db, "KD_Z9") %>%      
  filter(!is.na(yearMol)) %>%   
  filter(yearMol<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  
  left_join(tbl(db, "Raumdaten_Z9")) %>%  
  left_join(tbl(db, "STICHPROBE_Z9")) %>% 
  filter(BDM_aktuell == "ja") %>%        
  left_join(tbl(db, "MOL") %>%  
              filter(!is.na(aID_SP)) %>%  
              left_join(tbl(db, "Arten"))) %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMol, Hoehe, HN, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL) %>% 
  as_tibble()

# Moose Z9
MOOS.Z9 <- tbl(db, "KD_Z9") %>%      
  filter(!is.na(yearMoos)) %>%   
  filter(yearMoos<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  
  left_join(tbl(db, "Raumdaten_Z9")) %>%  
  left_join(tbl(db, "STICHPROBE_Z9")) %>% 
  filter(BDM_aktuell == "ja") %>%        
  left_join(tbl(db, "MOOS") %>%  
              filter(!is.na(aID_SP)) %>%  
              left_join(tbl(db, "Arten"))) %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMoos, Hoehe, HN, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL) %>% 
  as_tibble()

# Pflanzen Z9
PL.Z9 <- tbl(db, "KD_Z9") %>%      
  filter(!is.na(yearPl)) %>%   
  filter(yearPl<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  
  left_join(tbl(db, "Raumdaten_Z9")) %>%  
  left_join(tbl(db, "STICHPROBE_Z9")) %>% 
  filter(BDM_aktuell == "ja") %>%        
  left_join(tbl(db, "PL") %>%  
              filter(!is.na(aID_SP)) %>%  
              left_join(tbl(db, "Arten"))) %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl, Hoehe, HN, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL) %>% 
  as_tibble()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Deskriptiv: Beschreibung BDM Aufnahmen ----
Artengruppen <- list(PL.Z7 = PL.Z7, TF.Z7 = TF.Z7, BI.Z7 = BI.Z7, MOL.Z9 = MOL.Z9, MOOS.Z9 = MOOS.Z9, PL.Z9 = PL.Z9)
PL   <- tbl(db, "Arten") %>% filter(PL   == 1 & Z7Z9 == 1) %>% as.tibble %>% nrow()   # Anzahl Arten in der Schweiz, die im BDM aufgenommen werden könnten
TF   <- tbl(db, "Arten") %>% filter(TF   == 1 & Z7Z9 == 1) %>% as.tibble %>% nrow()   # dito
BI   <- tbl(db, "Arten") %>% filter(BI   == 1 & Z7Z9 == 1) %>% as.tibble %>% nrow()   # dito
MOL  <- tbl(db, "Arten") %>% filter(MOL  == 1 & Z7Z9 == 1) %>% as.tibble %>% nrow()   # dito
MOOS <- tbl(db, "Arten") %>% filter(MOOS == 1 & Z7Z9 == 1) %>% as.tibble %>% nrow()   # dito
ArtenzahlCH <- c(PL, TF, BI, MOL, MOOS, PL)

Beschreibung <- data.frame(row.names = names(Artengruppen))
for (i in 1:6){
  Beschreibung[i, "Standorte"] <- Artengruppen[[i]] %>%
    group_by(aID_STAO) %>%
    dplyr::summarise(Anfang = min(Aufnahmejahr),
                     Ende   = max(Aufnahmejahr)) %>% 
    nrow()
  Beschreibung[i, "Start"] <- Artengruppen[[i]] %>% 
    dplyr::select(Aufnahmejahr) %>%
    min()
  Beschreibung[i, "Ende"] <- Artengruppen[[i]] %>% 
    dplyr::select(Aufnahmejahr) %>%
    max()
  Beschreibung[i, "ArtenzahlCH"]  <- ArtenzahlCH[i]
  Beschreibung[i, "ArtenzahlBDM"] <- Artengruppen[[i]] %>% 
    group_by(aID_SP) %>%
      filter(!is.na(aID_SP)) %>% 
      dplyr::summarise(n_plots = n()) %>% 
    nrow()
  Beschreibung[i, "AnteilBDM"] <- round(Beschreibung[i, "ArtenzahlBDM"] / Beschreibung[i, "ArtenzahlCH"],3)
  Beschreibung[i, "ArtenzahlBDM_UZL"] <- Artengruppen[[i]] %>% 
    group_by(aID_SP) %>%
    filter(!is.na(aID_SP)) %>% 
    dplyr::summarise(n_plots = n()) %>% 
    left_join(tbl(db, "Arten") %>% as_tibble(),
              by = "aID_SP") %>% 
    filter(UZL == 1) %>%
    nrow()
  Beschreibung[i, "AnteilUZL"] <- round(Beschreibung[i, "ArtenzahlBDM_UZL"] / Beschreibung[i, "ArtenzahlBDM"], 3)
  Beschreibung[i, "ArtenzahlBDM_uebrige"] <- Artengruppen[[i]] %>% 
    group_by(aID_SP) %>%
    filter(!is.na(aID_SP)) %>% 
    dplyr::summarise(n_plots = n()) %>% 
    left_join(tbl(db, "Arten") %>% as_tibble(),
              by = "aID_SP") %>% 
    filter(UZL == 0) %>%
    nrow()
  Beschreibung[i, "Anteiluebrige"] <- round(Beschreibung[i, "ArtenzahlBDM_uebrige"] / Beschreibung[i, "ArtenzahlBDM"], 3)
  Beschreibung[i, "Kontrolle"] <- Beschreibung[i, "ArtenzahlBDM_UZL"] + Beschreibung[i, "ArtenzahlBDM_uebrige"]
}
print(Beschreibung)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## relativ vs. absolut ----











#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## END OF SCRIPT ----





















