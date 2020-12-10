### UZL-Auswertung (REPORT)----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Date created:     2020-12-10
# Location created: Tellstrasse 32, Bern
# Last Entry:       2020-12-10
# Author:           Raphael S. von Bueren (GitHub: buerap)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Get started ----
rm(list = ls(all = TRUE) )
Sys.setenv(TZ = "Europe/Zurich")
graphics.off()                    # Clear Graphic Window
cat( "\014" )                     # Clear Console ( =  CTRL L)

#install.packages("RSQLite")
library(tidyverse)
library(ggthemes)
library(scales)
#library(Rmisc)
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
## Vorbereitung Daten (HIER LOOP SCHREIBEN)----
# Pflanzen Z7
PL.Z7 <- tbl(db, "KD_Z7") %>%   
  filter(!is.na(yearPl)) %>%   
  filter(yearPl<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  
  left_join(tbl(db, "Raumdaten_Z7")) %>%  
  filter(Verdichtung_BDM == "nein") %>%   
  left_join(tbl(db, "STICHPROBE_Z7")) %>% 
  filter(BDM_aktuell == "ja") %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl, Hoehe, AntWald, AntLW, AntSiedlung)

PL.Z7_Artaufnahmen <- tbl(db, "KD_Z7") %>%   
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
    dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)

# Tagfalter Z7
TF.Z7 <- tbl(db, "KD_Z7") %>%   
  filter(!is.na(yearBu)) %>%   
  filter(yearBu<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  
  left_join(tbl(db, "Raumdaten_Z7")) %>%  
  filter(Verdichtung_BDM == "nein") %>%   
  left_join(tbl(db, "STICHPROBE_Z7")) %>% 
  filter(BDM_aktuell == "ja") %>%         
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBu, Hoehe, AntWald, AntLW, AntSiedlung)

TF.Z7_Artaufnahmen <- tbl(db, "KD_Z7") %>%   
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
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBu, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)

# Voegel Z7
BI.Z7 <- tbl(db, "KD_Z7") %>%   
  filter(!is.na(yearBi)) %>%   
  filter(yearBi<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  
  left_join(tbl(db, "Raumdaten_Z7")) %>%  
  filter(Verdichtung_BDM == "nein") %>%   
  left_join(tbl(db, "STICHPROBE_Z7")) %>% 
  filter(BDM_aktuell == "ja") %>%         
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBi, Hoehe, AntWald, AntLW, AntSiedlung)

BI.Z7_Artaufnahmen <- tbl(db, "KD_Z7") %>%   
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
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBi, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)


# Mollusken Z9
MOL.Z9 <- tbl(db, "KD_Z9") %>%      
  filter(!is.na(yearMol)) %>%   
  filter(yearMol<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  
  left_join(tbl(db, "Raumdaten_Z9")) %>%  
  left_join(tbl(db, "STICHPROBE_Z9")) %>% 
  filter(BDM_aktuell == "ja") %>%        
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMol, Hoehe, HN)

MOL.Z9_Artaufnahmen <- tbl(db, "KD_Z9") %>%      
  filter(!is.na(yearMol)) %>%   
  filter(yearMol<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  
  left_join(tbl(db, "Raumdaten_Z9")) %>%  
  left_join(tbl(db, "STICHPROBE_Z9")) %>% 
  filter(BDM_aktuell == "ja") %>%        
  left_join(tbl(db, "MOL") %>%  
              filter(!is.na(aID_SP)) %>%  
              left_join(tbl(db, "Arten"))) %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMol, Hoehe, HN, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)

# Moose Z9
MOOS.Z9 <- tbl(db, "KD_Z9") %>%      
  filter(!is.na(yearMoos)) %>%   
  filter(yearMoos<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  
  left_join(tbl(db, "Raumdaten_Z9")) %>%  
  left_join(tbl(db, "STICHPROBE_Z9")) %>% 
  filter(BDM_aktuell == "ja") %>%        
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMoos, Hoehe, HN)

MOOS.Z9_Artaufnahmen <- tbl(db, "KD_Z9") %>%      
  filter(!is.na(yearMoos)) %>%   
  filter(yearMoos<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  
  left_join(tbl(db, "Raumdaten_Z9")) %>%  
  left_join(tbl(db, "STICHPROBE_Z9")) %>% 
  filter(BDM_aktuell == "ja") %>%        
  left_join(tbl(db, "MOOS") %>%  
              filter(!is.na(aID_SP)) %>%  
              left_join(tbl(db, "Arten"))) %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMoos, Hoehe, HN, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)

# Pflanzen Z9
PL.Z9 <- tbl(db, "KD_Z9") %>%      
  filter(!is.na(yearPl)) %>%   
  filter(yearPl<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  
  left_join(tbl(db, "Raumdaten_Z9")) %>%  
  left_join(tbl(db, "STICHPROBE_Z9")) %>% 
  filter(BDM_aktuell == "ja") %>%
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl, Hoehe, HN)

PL.Z9_Artaufnahmen <- tbl(db, "KD_Z9") %>%      
  filter(!is.na(yearPl)) %>%   
  filter(yearPl<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  
  left_join(tbl(db, "Raumdaten_Z9")) %>%  
  left_join(tbl(db, "STICHPROBE_Z9")) %>% 
  filter(BDM_aktuell == "ja") %>%        
  left_join(tbl(db, "PL") %>%  
              filter(!is.na(aID_SP)) %>%  
              left_join(tbl(db, "Arten"))) %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl, Hoehe, HN, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Deskriptiv: Beschreibung BDM Aufnahmen ----
Artengruppen_Artaufnahmen <- list(PL.Z7   = PL.Z7_Artaufnahmen %>% as_tibble,
                                  TF.Z7   = TF.Z7_Artaufnahmen %>% as_tibble,
                                  BI.Z7   = BI.Z7_Artaufnahmen %>% as_tibble,
                                  MOL.Z9  = MOL.Z9_Artaufnahmen %>% as_tibble,
                                  MOOS.Z9 = MOOS.Z9_Artaufnahmen %>% as_tibble,
                                  PL.Z9   = PL.Z9_Artaufnahmen %>% as_tibble)
PL   <- tbl(db, "Arten") %>% filter(PL   == 1 & Z7Z9 == 1) %>% as_tibble %>% nrow()   # Anzahl Arten in der Schweiz, die im BDM aufgenommen werden könnten
TF   <- tbl(db, "Arten") %>% filter(TF   == 1 & Z7Z9 == 1) %>% as_tibble %>% nrow()   # dito
BI   <- tbl(db, "Arten") %>% filter(BI   == 1 & Z7Z9 == 1) %>% as_tibble %>% nrow()   # dito
MOL  <- tbl(db, "Arten") %>% filter(MOL  == 1 & Z7Z9 == 1) %>% as_tibble %>% nrow()   # dito
MOOS <- tbl(db, "Arten") %>% filter(MOOS == 1 & Z7Z9 == 1) %>% as_tibble %>% nrow()   # dito
ArtenzahlCH <- c(PL, TF, BI, MOL, MOOS, PL)

Beschreibung <- data.frame(row.names = names(Artengruppen_Artaufnahmen))
for (i in 1:6){
  Beschreibung[i, "Standorte"] <- Artengruppen_Artaufnahmen[[i]] %>%  
    group_by(aID_STAO) %>%
    dplyr::summarise(Anfang = min(Aufnahmejahr),
                     Ende   = max(Aufnahmejahr)) %>% 
    nrow()
  Beschreibung[i, "Start"] <- Artengruppen_Artaufnahmen[[i]] %>% 
    dplyr::select(Aufnahmejahr) %>%
    min()
  Beschreibung[i, "Ende"] <- Artengruppen_Artaufnahmen[[i]] %>% 
    dplyr::select(Aufnahmejahr) %>%
    max()
  Beschreibung[i, "ArtenzahlCH"]  <- ArtenzahlCH[i]
  Beschreibung[i, "ArtenzahlBDM"] <- Artengruppen_Artaufnahmen[[i]] %>% 
    group_by(aID_SP) %>%
      filter(!is.na(aID_SP)) %>% 
      dplyr::summarise(n_plots = n()) %>% 
    nrow()
  Beschreibung[i, "AnteilBDM"] <- round(Beschreibung[i, "ArtenzahlBDM"] / Beschreibung[i, "ArtenzahlCH"],3)
  Beschreibung[i, "ArtenzahlBDM_UZL"] <- Artengruppen_Artaufnahmen[[i]] %>% 
    group_by(aID_SP) %>%
    filter(!is.na(aID_SP)) %>% 
    dplyr::summarise(n_plots = n()) %>% 
    left_join(tbl(db, "Arten") %>% as_tibble(),
              by = "aID_SP") %>% 
    filter(UZL == 1) %>%
    nrow()
  Beschreibung[i, "AnteilUZL"] <- round(Beschreibung[i, "ArtenzahlBDM_UZL"] / Beschreibung[i, "ArtenzahlBDM"], 3)
  Beschreibung[i, "ArtenzahlBDM_uebrige"] <- Artengruppen_Artaufnahmen[[i]] %>%  
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
## relativ vs. absolut: Pflanzen Z7 ----
PL.Z7_Aufnahmen <- PL.Z7 %>% 
  left_join(
    tbl(db, "Pl") %>%  
      filter(!is.na(aID_SP)) %>% 
      filter(Z7 == 1) %>% 
      left_join(tbl(db, "Arten")) %>% 
      group_by(aID_KD) %>%
      dplyr::summarise(AZ = n(),
                       AZ_UZL = sum(UZL == 1),
                       AZ_UB = sum(UZL == 0))
      ) %>% as_tibble() %>% 
  replace_na(list(AZ = 0, AZ_UZL = 0, AZ_UB = 0)) %>% print() 

PL.Z7_absolut <- PL.Z7_Aufnahmen %>% 
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(UZL    = mean(AZ_UZL),
                   übrige = mean(AZ_UB)) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% 
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL: Pflanzen (Z7)") +
  ylim(0, NA) +
  labs(x = "Aufnahmejahr",
       y = "Anzahl Arten") +
  theme(legend.position = c(0.85, 0.15))
PL.Z7_absolut

PL.Z7_relativ <- PL.Z7_Aufnahmen %>% 
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(UZL    = (mean(AZ_UZL) / mean(PL.Z7_Aufnahmen$AZ_UZL)) - 1,
                   übrige = (mean(AZ_UB)  / mean(PL.Z7_Aufnahmen$AZ_UB))  - 1) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% 
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Pflanzen (Z7)") +
  labs(x = "Aufnahmejahr",
       y = "Veränderte Artenzahl relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))+
  scale_y_continuous(labels=percent)
PL.Z7_relativ

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## relativ andere Artengruppen ----
# Tagfalter Z7
TF.Z7_Aufnahmen <- TF.Z7 %>% 
  left_join(
    tbl(db, "TF") %>%  
      mutate_at( vars( c("Ind")), funs(if_else(is.na(Ind), 54, Ind))) %>%  # Datenbank an einer Stelle falsch -> manuell Wert ersetzen
      filter(!is.na(aID_SP)) %>% 
      left_join(tbl(db, "Arten")) %>% 
      group_by(aID_KD) %>%
      dplyr::summarise(AZ = n(),
                       AZ_UZL = sum(UZL == 1),
                       AZ_UB = sum(UZL == 0))
  ) %>% as_tibble() %>% 
  replace_na(list(AZ = 0, AZ_UZL = 0, AZ_UB = 0)) %>% print() 

TF.Z7_relativ <- TF.Z7_Aufnahmen %>% 
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(UZL    = (mean(AZ_UZL) / mean(TF.Z7_Aufnahmen$AZ_UZL)) - 1,
                   übrige = (mean(AZ_UB)  / mean(TF.Z7_Aufnahmen$AZ_UB))  - 1) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% 
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Tagfalter (Z7)") +
  labs(x = "Aufnahmejahr",
       y = "Veränderte Artenzahl relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))+
  scale_y_continuous(labels=percent)
TF.Z7_relativ






























PL.Z7 <- tbl(db, "KD_Z7") %>%   
  filter(!is.na(yearPl)) %>%   
  filter(yearPl<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  
  left_join(tbl(db, "Raumdaten_Z7")) %>%  
  filter(Verdichtung_BDM == "nein") %>%   
  left_join(tbl(db, "STICHPROBE_Z7")) %>% 
  filter(BDM_aktuell == "ja") %>% 
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl, Hoehe, AntWald, AntLW, AntSiedlung)
dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBu, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)
# left_join(tbl(db, "PL") %>%  
#             filter(!is.na(aID_SP)) %>%  
#             left_join(tbl(db, "Arten"))) %>% 
#dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL) %>% 
#as_tibble()




# Mollusken Z9
MOL.Z9 <- tbl(db, "KD_Z9") %>%      
  filter(!is.na(yearMol)) %>%   
  filter(yearMol<2020) %>% 
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  
  left_join(tbl(db, "Raumdaten_Z9")) %>%  
  left_join(tbl(db, "STICHPROBE_Z9")) %>% 
  filter(BDM_aktuell == "ja") %>%        
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMol, Hoehe, HN, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## END OF SCRIPT ----





















