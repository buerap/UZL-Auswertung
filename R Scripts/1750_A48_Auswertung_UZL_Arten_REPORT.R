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
library(Rmisc)
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
## Vorbereitung Daten (HIER evtl. einige löschen / NICHT als Loop schreiben)----
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
    dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Z7Z9)

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
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBu, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Z7Z9)

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
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBi, Hoehe, AntWald, AntLW, AntSiedlung, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Z7Z9)


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
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMol, Hoehe, HN, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Z7Z9)

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
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMoos, Hoehe, HN, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Z7Z9)

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
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl, Hoehe, HN, aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Z7Z9)

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
## relativ andere Artengruppen (alles in einen Plot machen) ----
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

# Vögel Z7
BI.Z7_Aufnahmen <- BI.Z7 %>% 
  left_join(
    tbl(db, "BI") %>%  
      filter(!is.na(aID_SP)) %>% 
      left_join(tbl(db, "Arten")) %>% 
      group_by(aID_KD) %>%
      dplyr::summarise(AZ = n(),
                       AZ_UZL = sum(UZL == 1),
                       AZ_UB = sum(UZL == 0))
  ) %>% as_tibble() %>% 
  replace_na(list(AZ = 0, AZ_UZL = 0, AZ_UB = 0)) %>% print() 

BI.Z7_relativ <- BI.Z7_Aufnahmen %>% 
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(UZL    = (mean(AZ_UZL) / mean(BI.Z7_Aufnahmen$AZ_UZL)) - 1,
                   übrige = (mean(AZ_UB)  / mean(BI.Z7_Aufnahmen$AZ_UB))  - 1) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% 
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Vögel (Z7)") +
  labs(x = "Aufnahmejahr",
       y = "Veränderte Artenzahl relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))+
  scale_y_continuous(labels=percent)
BI.Z7_relativ

# Mollusken Z9
MOL.Z9_Aufnahmen <- MOL.Z9 %>% 
  left_join(
    tbl(db, "MOL") %>%  
      filter(!is.na(aID_SP)) %>% 
      left_join(tbl(db, "Arten")) %>% 
      group_by(aID_KD) %>%
      dplyr::summarise(AZ = n(),
                       AZ_UZL = sum(UZL == 1),
                       AZ_UB = sum(UZL == 0))
  ) %>% as_tibble() %>% 
  replace_na(list(AZ = 0, AZ_UZL = 0, AZ_UB = 0)) %>% print() 

MOL.Z9_relativ <- MOL.Z9_Aufnahmen %>% 
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(UZL    = (mean(AZ_UZL) / mean(MOL.Z9_Aufnahmen$AZ_UZL)) - 1,
                   übrige = (mean(AZ_UB)  / mean(MOL.Z9_Aufnahmen$AZ_UB))  - 1) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% 
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Mollusken (Z9)") +
  labs(x = "Aufnahmejahr",
       y = "Veränderte Artenzahl relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))+
  scale_y_continuous(labels=percent)
MOL.Z9_relativ

# Moose Z9
MOOS.Z9_Aufnahmen <- MOOS.Z9 %>% 
  left_join(
    tbl(db, "MOOS") %>%  
      filter(!is.na(aID_SP)) %>% 
      left_join(tbl(db, "Arten")) %>% 
      group_by(aID_KD) %>%
      dplyr::summarise(AZ = n(),
                       AZ_UZL = sum(UZL == 1),
                       AZ_UB = sum(UZL == 0))
  ) %>% as_tibble() %>% 
  replace_na(list(AZ = 0, AZ_UZL = 0, AZ_UB = 0)) %>% print() 

MOOS.Z9_relativ <- MOOS.Z9_Aufnahmen %>% 
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(UZL    = (mean(AZ_UZL) / mean(MOOS.Z9_Aufnahmen$AZ_UZL)) - 1,
                   übrige = (mean(AZ_UB)  / mean(MOOS.Z9_Aufnahmen$AZ_UB))  - 1) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% 
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Moose (Z9)") +
  labs(x = "Aufnahmejahr",
       y = "Veränderte Artenzahl relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))+
  scale_y_continuous(labels=percent)
MOOS.Z9_relativ

# Pflanzen Z9
PL.Z9_Aufnahmen <- PL.Z9 %>% 
  left_join(
    tbl(db, "PL") %>%  
      filter(!is.na(aID_SP)) %>% 
      left_join(tbl(db, "Arten")) %>% 
      group_by(aID_KD) %>%
      dplyr::summarise(AZ = n(),
                       AZ_UZL = sum(UZL == 1),
                       AZ_UB = sum(UZL == 0))
  ) %>% as_tibble() %>% 
  replace_na(list(AZ = 0, AZ_UZL = 0, AZ_UB = 0)) %>% print() 

PL.Z9_relativ <- PL.Z9_Aufnahmen %>% 
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(UZL    = (mean(AZ_UZL) / mean(PL.Z9_Aufnahmen$AZ_UZL)) - 1,
                   übrige = (mean(AZ_UB)  / mean(PL.Z9_Aufnahmen$AZ_UB))  - 1) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% 
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Pflanzen (Z9)") +
  labs(x = "Aufnahmejahr",
       y = "Veränderte Artenzahl relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))+
  scale_y_continuous(labels=percent)
PL.Z9_relativ

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Übersichtsplot alle 6 (Entwicklung relativ) ----
gridExtra::grid.arrange(PL.Z7_relativ,
                        TF.Z7_relativ,
                        BI.Z7_relativ,
                        MOL.Z9_relativ,
                        MOOS.Z9_relativ,
                        PL.Z9_relativ,
                        ncol = 2, nrow = 3, as.table = F)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Trend vs. Landwirtschaft Z7 (NOCH NICHT FERTIG !!!) (AntLW > 20 % -> bei LW < 10% gibts komplett anderes Bild: UZL werden schlecht geschätzt --> mit Tobi Besprechen)----
trend.rel <- function(y, x, mean_value){  # glm mit poisson weil zähldaten, aber achtung wenn relativ !! -> wenn hier mit gaussian, dann gibts bei Z9 schönere resultate
  coef(glm(y ~ x, family = "gaussian"))[2]/(ifelse(mean_value == 0, 1, mean_value))  # wenn keine (z.B. UZL-)Arten vorkommen bei einem Standort bei allen Aufnahmejahren, wird Trend als Null angenommen (0 / 1)
}

# Pflanzen Z7
Trend_PL.Z7 <- PL.Z7_Aufnahmen %>%
  #filter(AntLW > 0.2) %>% 
  group_by(aID_STAO) %>%
  dplyr::summarise(Trend_UZL   = trend.rel(y = AZ_UZL, x = Aufnahmejahr, mean_value = mean(AZ_UZL)),
                   Trend_UB    = trend.rel(y = AZ_UB,  x = Aufnahmejahr, mean_value = mean(AZ_UB) ),
                   Landw       = mean(AntLW),
                   Wald        = mean(AntWald),
                   Siedlung    = mean(AntSiedlung),
                   Hoehe       = mean(Hoehe)) %>%
  dplyr::rename(UZL = Trend_UZL, UB = Trend_UB) %>% 
  reshape2::melt(measure.vars = c("UZL", "UB")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()

  ggplot(Trend_PL.Z7, aes(x = Landw, y = Trend, col = Artengruppe)) +
    #geom_point() +
    geom_smooth(method = "lm") +
    ggtitle("Entwicklung vs. Landwirtschaft : Pflanzen (Z7)") +
  scale_y_continuous(labels = percent)
  

mod <- lm(Trend ~ Artengruppe*Landw + Hoehe + I(Hoehe^2) + Wald, data = Trend_PL.Z7)
summary(mod)

mod2 <- lm(Trend ~ Artengruppe*Landw, data = Trend_PL.Z7)
summary(mod2)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Trend vs. Landwirtschaft Z9 (NOCH NICHT FERTIG !!!) ----
# Pflanzen Z7
Trend_PL.Z9 <- PL.Z9_Aufnahmen %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(Trend_UZL   = trend.rel(y = AZ_UZL, x = Aufnahmejahr, mean_value = mean(AZ_UZL)),
                   Trend_UB    = trend.rel(y = AZ_UB,  x = Aufnahmejahr, mean_value = mean(AZ_UB) ),
                   HN          = HN[1],
                   Hoehe       = mean(Hoehe)) %>%
  dplyr::rename(UZL = Trend_UZL, UB = Trend_UB) %>% 
  reshape2::melt(measure.vars = c("UZL", "UB")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()
Trend_PL.Z9$HN <- as_factor(Trend_PL.Z9$HN)

ggdata <- Trend_PL.Z9 %>%
  group_by(HN, Artengruppe) %>%
  dplyr::summarise(
    CI = CI(Trend)[1]-CI(Trend)[2],
    Trend = mean(Trend)
  )
ggplot(ggdata, aes(HN, Trend)) +
  geom_pointrange(
    aes(ymin = Trend-CI, ymax = Trend+CI, color = Artengruppe),
    position = position_dodge(0.3)
  ) +
  scale_y_continuous(labels = percent)





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Trend vs. Häufigkeit der Art (Z8 Daten) ----
trend.Occ <- function(y, x){
  coef(glm(y ~ x, family = "gaussian"))[2]
}
  
# Pflanzen Z7  
PL.Z7_Occ05_09 <- PL.Z7_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2010 & Aufnahmejahr > 2004) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen05_09 = n() / 375) 
  
PL.Z7_Occ10_14 <- PL.Z7_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2015 & Aufnahmejahr > 2009) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen10_14 = n() / 375) 

PL.Z7_Occ15_19 <- PL.Z7_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2020 & Aufnahmejahr > 2014) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen15_19 = n() / 375) 

PL.Z7_Occurence <- tbl(db, "Arten") %>%
  filter(PL   == 1 & Z7Z9 == 1) %>%
  left_join(PL.Z7_Occ05_09) %>% 
  left_join(PL.Z7_Occ10_14) %>% 
  left_join(PL.Z7_Occ15_19) %>%
  dplyr::select(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Anteil_Flaechen05_09, Anteil_Flaechen10_14, Anteil_Flaechen15_19) %>% 
  as_tibble() %>% 
  replace_na(list(Anteil_Flaechen05_09 = 0,
                  Anteil_Flaechen10_14 = 0,
                  Anteil_Flaechen15_19 = 0)) %>% 
  filter(Anteil_Flaechen05_09 != 0 | Anteil_Flaechen10_14 != 0 | Anteil_Flaechen15_19 != 0)

PL_Z7.Trend <- PL.Z7_Occurence %>% 
  gather("Tranche", "Anteil_Flaechen", -c(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)) %>% 
  mutate(Tranchenzahl = ifelse(Tranche == "Anteil_Flaechen05_09", 2007, ifelse(Tranche == "Anteil_Flaechen10_14", 2012, 2017))) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen) *2, # mal 2 damit Trend pro 10 Jahre
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
 dplyr::rename(Artengruppe = UZL) %>% 
  print()
PL_Z7.Trend$Artengruppe <- as.factor(PL_Z7.Trend$Artengruppe)
levels(PL_Z7.Trend$Artengruppe) <- c("UB", "UZL")
PL_Z7.Trend  # Trend = Anteil mehr Flächen (%) pro 10 Jahre

ggplot(PL_Z7.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = percent)
  
  
# Tagfalter Z7  
TF.Z7_Occ05_09 <- TF.Z7_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2010 & Aufnahmejahr > 2004) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen05_09 = n() / 375) 

TF.Z7_Occ10_14 <- TF.Z7_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2015 & Aufnahmejahr > 2009) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen10_14 = n() / 375) 

TF.Z7_Occ15_19 <- TF.Z7_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2020 & Aufnahmejahr > 2014) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen15_19 = n() / 375) 

TF.Z7_Occurence <- tbl(db, "Arten") %>%
  filter(TF   == 1 & Z7Z9 == 1) %>%
  left_join(TF.Z7_Occ05_09) %>% 
  left_join(TF.Z7_Occ10_14) %>% 
  left_join(TF.Z7_Occ15_19) %>%
  dplyr::select(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Anteil_Flaechen05_09, Anteil_Flaechen10_14, Anteil_Flaechen15_19) %>% 
  as_tibble() %>% 
  replace_na(list(Anteil_Flaechen05_09 = 0,
                  Anteil_Flaechen10_14 = 0,
                  Anteil_Flaechen15_19 = 0)) %>% 
  filter(Anteil_Flaechen05_09 != 0 | Anteil_Flaechen10_14 != 0 | Anteil_Flaechen15_19 != 0)

TF_Z7.Trend <- TF.Z7_Occurence %>% 
  gather("Tranche", "Anteil_Flaechen", -c(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)) %>% 
  mutate(Tranchenzahl = ifelse(Tranche == "Anteil_Flaechen05_09", 2007, ifelse(Tranche == "Anteil_Flaechen10_14", 2012, 2017))) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen) *2, # mal 2 damit Trend pro 10 Jahre
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
  dplyr::rename(Artengruppe = UZL) %>% 
  print()
TF_Z7.Trend$Artengruppe <- as.factor(TF_Z7.Trend$Artengruppe)
levels(TF_Z7.Trend$Artengruppe) <- c("UB", "UZL")
TF_Z7.Trend  # Trend = Anteil mehr Flächen (%) pro 10 Jahre

ggplot(TF_Z7.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = percent)

# Vögel Z7  
BI.Z7_Occ05_09 <- BI.Z7_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2010 & Aufnahmejahr > 2004) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen05_09 = n() / 375) 

BI.Z7_Occ10_14 <- BI.Z7_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2015 & Aufnahmejahr > 2009) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen10_14 = n() / 375) 

BI.Z7_Occ15_19 <- BI.Z7_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2020 & Aufnahmejahr > 2014) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen15_19 = n() / 375) 

BI.Z7_Occurence <- tbl(db, "Arten") %>%
  filter(BI   == 1 & Z7Z9 == 1) %>%
  left_join(BI.Z7_Occ05_09) %>% 
  left_join(BI.Z7_Occ10_14) %>% 
  left_join(BI.Z7_Occ15_19) %>%
  dplyr::select(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Anteil_Flaechen05_09, Anteil_Flaechen10_14, Anteil_Flaechen15_19) %>% 
  as_tibble() %>% 
  replace_na(list(Anteil_Flaechen05_09 = 0,
                  Anteil_Flaechen10_14 = 0,
                  Anteil_Flaechen15_19 = 0)) %>% 
  filter(Anteil_Flaechen05_09 != 0 | Anteil_Flaechen10_14 != 0 | Anteil_Flaechen15_19 != 0)

BI_Z7.Trend <- BI.Z7_Occurence %>% 
  gather("Tranche", "Anteil_Flaechen", -c(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)) %>% 
  mutate(Tranchenzahl = ifelse(Tranche == "Anteil_Flaechen05_09", 2007, ifelse(Tranche == "Anteil_Flaechen10_14", 2012, 2017))) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen) *2, # mal 2 damit Trend pro 10 Jahre
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
  dplyr::rename(Artengruppe = UZL) %>% 
  print()
BI_Z7.Trend$Artengruppe <- as.factor(BI_Z7.Trend$Artengruppe)
levels(BI_Z7.Trend$Artengruppe) <- c("UB", "UZL")
BI_Z7.Trend  # Trend = Anteil mehr Flächen (%) pro 10 Jahre

ggplot(BI_Z7.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = percent)

# Mollusken Z9
MOL.Z9_Occ05_09 <- MOL.Z9_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2010 & Aufnahmejahr > 2004) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen05_09 = n() / 1451) 

MOL.Z9_Occ10_14 <- MOL.Z9_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2015 & Aufnahmejahr > 2009) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen10_14 = n() / 1451) 

MOL.Z9_Occ15_19 <- MOL.Z9_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2020 & Aufnahmejahr > 2014) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen15_19 = n() / 1451) 

MOL.Z9_Occurence <- tbl(db, "Arten") %>%
  filter(MOL   == 1 & Z7Z9 == 1) %>%
  left_join(MOL.Z9_Occ05_09) %>% 
  left_join(MOL.Z9_Occ10_14) %>% 
  left_join(MOL.Z9_Occ15_19) %>%
  dplyr::select(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Anteil_Flaechen05_09, Anteil_Flaechen10_14, Anteil_Flaechen15_19) %>% 
  as_tibble() %>% 
  replace_na(list(Anteil_Flaechen05_09 = 0,
                  Anteil_Flaechen10_14 = 0,
                  Anteil_Flaechen15_19 = 0)) %>% 
  filter(Anteil_Flaechen05_09 != 0 | Anteil_Flaechen10_14 != 0 | Anteil_Flaechen15_19 != 0)

MOL_Z9.Trend <- MOL.Z9_Occurence %>% 
  gather("Tranche", "Anteil_Flaechen", -c(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)) %>% 
  mutate(Tranchenzahl = ifelse(Tranche == "Anteil_Flaechen05_09", 2007, ifelse(Tranche == "Anteil_Flaechen10_14", 2012, 2017))) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen) *2, # mal 2 damit Trend pro 10 Jahre
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
  dplyr::rename(Artengruppe = UZL) %>% 
  print()
MOL_Z9.Trend$Artengruppe <- as.factor(MOL_Z9.Trend$Artengruppe)
levels(MOL_Z9.Trend$Artengruppe) <- c("UB", "UZL")
MOL_Z9.Trend  # Trend = Anteil mehr Flächen (%) pro 10 Jahre

ggplot(MOL_Z9.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = percent)

# Moose Z9
MOOS.Z9_Occ05_09 <- MOOS.Z9_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2010 & Aufnahmejahr > 2004) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen05_09 = n() / 1448) %>% as_tibble

MOOS.Z9_Occ10_14 <- MOOS.Z9_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2015 & Aufnahmejahr > 2009) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen10_14 = n() / 1448) 

MOOS.Z9_Occ15_19 <- MOOS.Z9_Artaufnahmen %>% 
  filter(Aufnahmejahr < 2020 & Aufnahmejahr > 2014) %>% 
  filter(Z7Z9 == 1) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(Anteil_Flaechen15_19 = n() / 1448) 

MOOS.Z9_Occurence <- tbl(db, "Arten") %>%
  filter(MOOS   == 1 & Z7Z9 == 1) %>%
  left_join(MOOS.Z9_Occ05_09) %>% 
  left_join(MOOS.Z9_Occ10_14) %>% 
  left_join(MOOS.Z9_Occ15_19) %>%
  dplyr::select(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Anteil_Flaechen05_09, Anteil_Flaechen10_14, Anteil_Flaechen15_19) %>% 
  as_tibble() %>% 
  replace_na(list(Anteil_Flaechen05_09 = 0,
                  Anteil_Flaechen10_14 = 0,
                  Anteil_Flaechen15_19 = 0)) %>% 
  filter(Anteil_Flaechen05_09 != 0 | Anteil_Flaechen10_14 != 0 | Anteil_Flaechen15_19 != 0)

MOOS_Z9.Trend <- MOOS.Z9_Occurence %>% 
  gather("Tranche", "Anteil_Flaechen", -c(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)) %>% 
  mutate(Tranchenzahl = ifelse(Tranche == "Anteil_Flaechen05_09", 2007, ifelse(Tranche == "Anteil_Flaechen10_14", 2012, 2017))) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen) *2, # mal 2 damit Trend pro 10 Jahre
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
  dplyr::rename(Artengruppe = UZL) %>% 
  print()
MOOS_Z9.Trend$Artengruppe <- as.factor(MOOS_Z7.Trend$Artengruppe)
levels(MOOS_Z9.Trend$Artengruppe) <- c("UB", "UZL")
MOOS_Z9.Trend  # Trend = Anteil mehr Flächen (%) pro 10 Jahre

ggplot(MOOS_Z9.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = percent)

### nun für andere artengruppen genauso noch machen

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## END OF SCRIPT ----




















