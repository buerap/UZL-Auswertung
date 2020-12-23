### UZL-Auswertung (REPORT)----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Date created:     2020-12-10
# Location created: Tellstrasse 32, Bern
# Last Entry:       2020-12-22
# Author:           Raphael S. von Bueren (GitHub: buerap)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Get started ----
#rm(list = ls(all = TRUE) )
Sys.setenv(TZ = "Europe/Zurich")
graphics.off()                    # Clear Graphic Window
cat( "\014" )                     # Clear Console ( =  CTRL L)

library(tidyverse)
library(ggthemes)
library(scales)
library(Rmisc)

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
## Vorbereitung Daten ----
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
  geom_point(size = 2, alpha = 0.5) +
  geom_line(size = 0.1, show.legend = FALSE) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Pflanzen (Z7)") +
  ylim(0, NA) +
  labs(x = "Aufnahmejahr",
       y = "Durchschnittliche Artenzahl \npro Aufnahmefläche") +
  scale_x_continuous(limits = c(2001, 2019), breaks = seq(2001,2019,6)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.8,0.13),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  # theme(legend.position = "none") +
  theme(plot.margin = margin(5,20,5,5 ),
        plot.background = element_blank())
PL.Z7_absolut

PL.Z7_relativ <- PL.Z7_Aufnahmen %>% 
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(UZL    = (mean(AZ_UZL) / mean(PL.Z7_Aufnahmen$AZ_UZL)) - 1,
                   übrige = (mean(AZ_UB)  / mean(PL.Z7_Aufnahmen$AZ_UB))  - 1) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% 
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_line(size = 0.1, show.legend = FALSE) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Pflanzen (Z7)") + # ggtitle("")
  labs(x = "Aufnahmejahr",
       y = "Abweichung der Artenzahl vom \nMittelwert 2001 - 2019") +
  scale_x_continuous(limits = c(2001, 2019), breaks = seq(2001,2019,6)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.2,0.9),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  #theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.3, 0.3))
PL.Z7_relativ

PL.Z7_both <- gridExtra::grid.arrange(PL.Z7_absolut, PL.Z7_relativ, ncol = 2)
# ggsave(PL.Z7_both , file = "PL.Z7_both.png",
#        path = "R_PLOTS/REPORT",
#        width = 20, height = 10, units = "cm" )

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
  geom_point(size = 2, alpha = 0.5) +
  geom_line(size = 0.1, show.legend = FALSE) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Tagfalter (Z7)") +
  labs(x = "Aufnahmejahr",
       y = "Abweichung der Artenzahl vom \nMittelwert 2003 - 2019") +
  scale_x_continuous(limits = c(2001, 2019), breaks = seq(2001,2019,6)) +
  theme(legend.position = c(0.85, 0.15)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.8,0.13),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.3, 0.3))
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
  geom_point(size = 2, alpha = 0.5) +
  geom_line(size = 0.1, show.legend = FALSE) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Vögel (Z7)") +
  labs(x = "Aufnahmejahr",
       y = "Abweichung der Artenzahl vom \nMittelwert 2001 - 2019") +
  scale_x_continuous(limits = c(2001, 2019), breaks = seq(2001,2019,6)) +
  theme(legend.position = c(0.85, 0.15)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.8,0.13),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.3, 0.3))
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
  geom_point(size = 2, alpha = 0.5) +
  geom_line(size = 0.1, show.legend = FALSE) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Mollusken (Z9)") +
  labs(x = "Aufnahmejahr",
       y = "Abweichung der Artenzahl vom \nMittelwert 2001 - 2019") +
  scale_x_continuous(limits = c(2001, 2019), breaks = seq(2001,2019,6)) +
  theme(legend.position = c(0.85, 0.15)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.8,0.13),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) # HIER KEINE LIMITS
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
  geom_point(size = 2, alpha = 0.5) +
  geom_line(size = 0.1, show.legend = FALSE) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Moose (Z9)") +
  labs(x = "Aufnahmejahr",
       y = "Abweichung der Artenzahl vom \nMittelwert 2001 - 2019") +
  scale_x_continuous(limits = c(2001, 2019), breaks = seq(2001,2019,6)) +
  theme(legend.position = c(0.85, 0.15)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.8,0.13),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.3, 0.3))
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
  geom_point(size = 2, alpha = 0.5) +
  geom_line(size = 0.1, show.legend = FALSE) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Pflanzen (Z9)") +
  labs(x = "Aufnahmejahr",
       y = "Abweichung der Artenzahl vom \nMittelwert 2001 - 2019") +
  scale_x_continuous(limits = c(2001, 2019), breaks = seq(2001,2019,6)) +
  theme(legend.position = c(0.85, 0.15)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.8,0.13),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.3, 0.3))
PL.Z9_relativ

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Übersichtsplot alle 6 (Entwicklung relativ) ----
Entwicklung_relativ <- gridExtra::grid.arrange(PL.Z7_relativ,
                                               TF.Z7_relativ,
                                               BI.Z7_relativ,
                                               MOL.Z9_relativ,
                                               MOOS.Z9_relativ,
                                               PL.Z9_relativ,
                                               ncol = 2, nrow = 3, as.table = F)

# ggsave(Entwicklung_relativ , file = "Entwicklung_relativ.png",
#        path = "R_PLOTS/REPORT",
#        width = 25, height = 25, units = "cm" )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Trend vs. Landwirtschaft Z7 ----
trend.rel <- function(y, x, mean_value){
  coef(glm(y ~ x, family = "gaussian"))[2]/(ifelse(mean_value == 0, 1, mean_value))  # wenn keine (z.B. UZL-)Arten vorkommen bei einem Standort bei allen Aufnahmejahren, wird Trend als Null angenommen (0 / 1)
}

# Pflanzen Z7
Trend_PL.Z7 <- PL.Z7_Aufnahmen %>%
  filter(AntLW > 0.1) %>% 
  group_by(aID_STAO) %>%
  dplyr::summarise(Trend_UZL   = trend.rel(y = AZ_UZL, x = Aufnahmejahr, mean_value = mean(AZ_UZL)),
                   Trend_UB    = trend.rel(y = AZ_UB,  x = Aufnahmejahr, mean_value = mean(AZ_UB) ),
                   Landw       = mean(AntLW),
                   Wald        = mean(AntWald),
                   Siedlung    = mean(AntSiedlung),
                   Hoehe       = mean(Hoehe)) %>%
  dplyr::rename(UZL = Trend_UZL, übrige = Trend_UB) %>% 
  reshape2::melt(measure.vars = c("übrige", "UZL")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()

PL.Z7_Landw <- ggplot(Trend_PL.Z7, aes(x = Landw, y = Trend, col = Artengruppe)) +
  geom_point(size = 2, alpha = 0.5) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Pflanzen (Z7)") +
  labs(x = "Anteil Landwirtschaft innerhalb der Aufnahmefläche",
       y = "Entwicklung der Artenzahl pro Jahr") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.3,0.9),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  #theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0.1, 0.5, 0.9)) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

PL.Z7_plot <- ggplot(Trend_PL.Z7, aes(x = Landw, y = Trend, col = Artengruppe)) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  labs(x = "",
       y = "") +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 10, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 10, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0.1, 0.5, 0.9)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1))

PL.Z7_Landw_full <- PL.Z7_Landw +
  annotation_custom(
    ggplotGrob(PL.Z7_plot), 
    xmin = 0.6, xmax = 1, ymin = 0.03, ymax = 0.08
  )
PL.Z7_Landw_full

# ggsave(PL.Z7_Landw_full , file = "Entwicklung_Landw_Z7_PL.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )
  
mod <- lm(Trend ~ Artengruppe*Landw + Hoehe + I(Hoehe^2) + Wald, data = Trend_PL.Z7)
summary(mod)
mod2 <- lm(Trend ~ Artengruppe*Landw, data = Trend_PL.Z7)
summary(mod2)

# Tagfalter Z7
Trend_TF.Z7 <- TF.Z7_Aufnahmen %>%
  filter(AntLW > 0.1) %>% 
  group_by(aID_STAO) %>%
  dplyr::summarise(Trend_UZL   = trend.rel(y = AZ_UZL, x = Aufnahmejahr, mean_value = mean(AZ_UZL)),
                   Trend_UB    = trend.rel(y = AZ_UB,  x = Aufnahmejahr, mean_value = mean(AZ_UB) ),
                   Landw       = mean(AntLW),
                   Wald        = mean(AntWald),
                   Siedlung    = mean(AntSiedlung),
                   Hoehe       = mean(Hoehe)) %>%
  dplyr::rename(UZL = Trend_UZL, UB = Trend_UB) %>% 
  reshape2::melt(measure.vars = c("UB", "UZL")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()

TF.Z7_Landw <- ggplot(Trend_TF.Z7, aes(x = Landw, y = Trend, col = Artengruppe)) +
  geom_point(size = 2, alpha = 0.5) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Tagfalter (Z7)") +
  labs(x = "Anteil Landwirtschaft innerhalb der Aufnahmefläche",
       y = "Entwicklung der Artenzahl pro Jahr") +
  theme(legend.position = c(0.85, 0.15)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.8,0.13),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0.1, 0.5, 0.9)) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

TF.Z7_plot <- ggplot(Trend_TF.Z7, aes(x = Landw, y = Trend, col = Artengruppe)) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  labs(x = "",
       y = "") +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 10, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 10, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0.1, 0.5, 0.9)) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

TF.Z7_Landw_full <- TF.Z7_Landw +
  annotation_custom(
    ggplotGrob(TF.Z7_plot), 
    xmin = 0.6, xmax = 1, ymin = 0.15, ymax = 0.32
  )
TF.Z7_Landw_full

# ggsave(TF.Z7_Landw_full , file = "Entwicklung_Landw_Z7_TF.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )

mod <- lm(Trend ~ Artengruppe*Landw + Hoehe + I(Hoehe^2) + Wald, data = Trend_TF.Z7)
summary(mod)
mod2 <- lm(Trend ~ Artengruppe*Landw, data = Trend_TF.Z7)
summary(mod2)

# Vögel Z7
Trend_BI.Z7 <- BI.Z7_Aufnahmen %>%
  filter(AntLW > 0.1) %>% 
  group_by(aID_STAO) %>%
  dplyr::summarise(Trend_UZL   = trend.rel(y = AZ_UZL, x = Aufnahmejahr, mean_value = mean(AZ_UZL)),
                   Trend_UB    = trend.rel(y = AZ_UB,  x = Aufnahmejahr, mean_value = mean(AZ_UB) ),
                   Landw       = mean(AntLW),
                   Wald        = mean(AntWald),
                   Siedlung    = mean(AntSiedlung),
                   Hoehe       = mean(Hoehe)) %>%
  dplyr::rename(UZL = Trend_UZL, UB = Trend_UB) %>% 
  reshape2::melt(measure.vars = c("UB", "UZL")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()

BI.Z7_Landw <- ggplot(Trend_BI.Z7, aes(x = Landw, y = Trend, col = Artengruppe)) +
  geom_point(size = 2, alpha = 0.5) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Vögel (Z7)") +
  labs(x = "Anteil Landwirtschaft innerhalb der Aufnahmefläche",
       y = "Entwicklung der Artenzahl pro Jahr") +
  theme(legend.position = c(0.85, 0.15)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.8,0.13),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0.1, 0.5, 0.9)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(NA, 0.15))

BI.Z7_plot <- ggplot(Trend_BI.Z7, aes(x = Landw, y = Trend, col = Artengruppe)) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  labs(x = "",
       y = "") +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 10, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 10, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0.1, 0.5, 0.9)) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

BI.Z7_Landw_full <- BI.Z7_Landw +
  annotation_custom(
    ggplotGrob(BI.Z7_plot), 
    xmin = 0.6, xmax = 1, ymin = 0.055, ymax = 0.16
  )
BI.Z7_Landw_full

# ggsave(BI.Z7_Landw_full , file = "Entwicklung_Landw_Z7_BI.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )

mod <- lm(Trend ~ Artengruppe*Landw + Hoehe + I(Hoehe^2) + Wald, data = Trend_BI.Z7)
summary(mod)
mod2 <- lm(Trend ~ Artengruppe*Landw, data = Trend_BI.Z7)
summary(mod2)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Trend vs. Landwirtschaft Z9 ----
# Mollusken Z9
Trend_MOL.Z9 <- MOL.Z9_Aufnahmen %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(Trend_UB    = trend.rel(y = AZ_UB,  x = Aufnahmejahr, mean_value = mean(AZ_UB) ),
                   Trend_UZL   = trend.rel(y = AZ_UZL, x = Aufnahmejahr, mean_value = mean(AZ_UZL)),
                   HN          = HN[1],
                   Hoehe       = mean(Hoehe)) %>%
  dplyr::rename( UB = Trend_UB, UZL = Trend_UZL) %>% 
  reshape2::melt(measure.vars = c("UB", "UZL")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()
Trend_MOL.Z9$HN <- as_factor(Trend_MOL.Z9$HN)

ggdata <- Trend_MOL.Z9 %>%
  group_by(HN, Artengruppe) %>%
  dplyr::summarise(
    Trend_mean = mean(Trend),
    CI = CI(Trend)[1]-CI(Trend)[2])
ggdata <- ggdata %>%
  dplyr::filter(!is.na(Trend_mean))
ggdata$HN <- ggdata$HN %>% 
  recode_factor("Aecker" = "AK",
                "Wiesen, Weiden" = "WW",
                "Siedlung" = "SI",
                "Wald" = "WA",
                "Nicht genutzte Flaechen" = "NG",
                "Alpweiden" = "AL",
                "Gletscher, Wasser" = "GW")
MOL.Z9_HN <- ggplot(ggdata, aes(HN, Trend_mean)) +
  geom_pointrange(
    aes(ymin = Trend_mean-CI, ymax = Trend_mean+CI, color = Artengruppe),
    position = position_dodge(0.3),
    size = 1, alpha = 0.5
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  ggtitle("Mollusken (Z9)") +
  labs(x = "Hauptnutzung",
       y = "Entwicklung der Artenzahl pro Jahr") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 15, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 12, face = "italic", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  theme(legend.position = c(0.9,1.04),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 8)),
        legend.spacing.x = unit(0, 'cm'))# +
  #theme(legend.position = "none")
MOL.Z9_HN

# ggsave(MOL.Z9_HN , file = "Entwicklung_HN_Z9_MOL.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )

# Moose Z9
Trend_MOOS.Z9 <- MOOS.Z9_Aufnahmen %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(Trend_UB    = trend.rel(y = AZ_UB,  x = Aufnahmejahr, mean_value = mean(AZ_UB) ),
                   Trend_UZL   = trend.rel(y = AZ_UZL, x = Aufnahmejahr, mean_value = mean(AZ_UZL)),
                   HN          = HN[1],
                   Hoehe       = mean(Hoehe)) %>%
  dplyr::rename( UB = Trend_UB, UZL = Trend_UZL) %>% 
  reshape2::melt(measure.vars = c("UB", "UZL")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()
Trend_MOOS.Z9$HN <- as_factor(Trend_MOOS.Z9$HN)

ggdata <- Trend_MOOS.Z9 %>%
  group_by(HN, Artengruppe) %>%
  dplyr::summarise(
    Trend_mean = mean(Trend),
    CI = CI(Trend)[1]-CI(Trend)[2])
ggdata <- ggdata %>%
  dplyr::filter(!is.na(Trend_mean))
ggdata$HN <- ggdata$HN %>% 
  recode_factor("Aecker" = "AK",
                "Wiesen, Weiden" = "WW",
                "Siedlung" = "SI",
                "Wald" = "WA",
                "Nicht genutzte Flaechen" = "NG",
                "Alpweiden" = "AL",
                "Gletscher, Wasser" = "GW")
MOOS.Z9_HN <- ggplot(ggdata, aes(HN, Trend_mean)) +
  geom_pointrange(
    aes(ymin = Trend_mean-CI, ymax = Trend_mean+CI, color = Artengruppe),
    position = position_dodge(0.3),
    size = 1, alpha = 0.5
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  ggtitle("Moose (Z9)") +
  labs(x = "Hauptnutzung",
       y = "Entwicklung der Artenzahl pro Jahr") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 15, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 12, face = "italic", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  theme(legend.position = c(0.9,1.04),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 8)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none")
MOOS.Z9_HN

# ggsave(MOOS.Z9_HN , file = "Entwicklung_HN_Z9_MOOS.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )

# Pflanzen Z9
Trend_PL.Z9 <- PL.Z9_Aufnahmen %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(Trend_UB    = trend.rel(y = AZ_UB,  x = Aufnahmejahr, mean_value = mean(AZ_UB) ),
                   Trend_UZL   = trend.rel(y = AZ_UZL, x = Aufnahmejahr, mean_value = mean(AZ_UZL)),
                   HN          = HN[1],
                   Hoehe       = mean(Hoehe)) %>%
  dplyr::rename( UB = Trend_UB, UZL = Trend_UZL) %>% 
  reshape2::melt(measure.vars = c("UB", "UZL")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()
Trend_PL.Z9$HN <- as_factor(Trend_PL.Z9$HN)

ggdata <- Trend_PL.Z9 %>%
  group_by(HN, Artengruppe) %>%
  dplyr::summarise(
    Trend_mean = mean(Trend),
    CI = CI(Trend)[1]-CI(Trend)[2],
  )
ggdata <- ggdata %>%
  dplyr::filter(!is.na(Trend_mean))
ggdata$HN <- ggdata$HN %>% 
  recode_factor("Aecker" = "AK",
                "Wiesen, Weiden" = "WW",
                "Wald" = "WA",
                "Nicht genutzte Flaechen" = "NG",
                "Alpweiden" = "AL",
                "Gletscher, Wasser" = "GW",
                "Siedlung" = "SI")
PL.Z9_HN <- ggplot(ggdata, aes(HN, Trend_mean)) +
  geom_pointrange(
    aes(ymin = Trend_mean-CI, ymax = Trend_mean+CI, color = Artengruppe),
    position = position_dodge(0.3),
    size = 1, alpha = 0.5
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  ggtitle("Pflanzen (Z9)") +
  labs(x = "Hauptnutzung",
       y = "Entwicklung der Artenzahl pro Jahr") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 15, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 12, face = "italic", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  theme(legend.position = c(0.9,1.04),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 8)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none")
PL.Z9_HN

# ggsave(PL.Z9_HN , file = "Entwicklung_HN_Z9_PL.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )

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
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen),
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
 dplyr::rename(Artengruppe = UZL) %>% 
  print()
PL_Z7.Trend$Artengruppe <- as.factor(PL_Z7.Trend$Artengruppe)
levels(PL_Z7.Trend$Artengruppe) <- c("übrige", "UZL")
PL_Z7.Trend  # Trend = Anteil mehr Flächen (%) pro Jahr

PL.Z7_Arten <- ggplot(PL_Z7.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point(size = 2, alpha = 0.5) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Pflanzen (Z7)") +
  labs(x = "Häufigkeit (Anteil besetzter Flächen)",
       y = "Entwicklung der Häufigkeit pro Jahr") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.83, 1.04 ),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  #theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.5, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1))

PL.Z7_Arten_plot <- ggplot(PL_Z7.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  labs(x = "",
       y = "") +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.5, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.01))

PL.Z7_Arten_full <- PL.Z7_Arten +
  annotation_custom(
    ggplotGrob(PL.Z7_Arten_plot), 
    xmin = 0.5, xmax = 1.05, ymin = 0.004, ymax = 0.012
  )
PL.Z7_Arten_full

# ggsave(PL.Z7_Arten_full , file = "Arten_Entwicklung_Häufigkeit_Z7_PL.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )
  
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
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen),
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
  dplyr::rename(Artengruppe = UZL) %>% 
  print()
TF_Z7.Trend$Artengruppe <- as.factor(TF_Z7.Trend$Artengruppe)
levels(TF_Z7.Trend$Artengruppe) <- c("übrige", "UZL")
TF_Z7.Trend  # Trend = Anteil mehr Flächen (%) pro Jahr

TF.Z7_Arten <- ggplot(TF_Z7.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point(size = 2, alpha = 0.5) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Tagfalter (Z7)") +
  labs(x = "Häufigkeit (Anteil besetzter Flächen)",
       y = "Entwicklung der Häufigkeit pro Jahr") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.83, 1.04 ),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.5, 1), limits = c(0,1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1))

TF.Z7_Arten_plot <- ggplot(TF_Z7.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  labs(x = "",
       y = "") +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.5, 1), limits = c(0,1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1))

TF.Z7_Arten_full <- TF.Z7_Arten +
  annotation_custom(
    ggplotGrob(TF.Z7_Arten_plot), 
    xmin = 0.5, xmax = 1.05, ymin = 0.015, ymax = 0.0265
  )
TF.Z7_Arten_full

# ggsave(TF.Z7_Arten_full , file = "Arten_Entwicklung_Häufigkeit_Z7_TF.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )

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
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen),
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
  dplyr::rename(Artengruppe = UZL) %>% 
  print()
BI_Z7.Trend$Artengruppe <- as.factor(BI_Z7.Trend$Artengruppe)
levels(BI_Z7.Trend$Artengruppe) <- c("übrige", "UZL")
BI_Z7.Trend  # Trend = Anteil mehr Flächen (%) pro Jahr

BI.Z7_Arten <- ggplot(BI_Z7.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point(size = 2, alpha = 0.5) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Vögel (Z7)") +
  labs(x = "Häufigkeit (Anteil besetzter Flächen)",
       y = "Entwicklung der Häufigkeit pro Jahr") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.83, 1.04 ),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.5, 1), limits = c(0,1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1))

BI.Z7_Arten_plot <- ggplot(BI_Z7.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  labs(x = "",
       y = "") +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.5, 1), limits = c(0,1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.01))

BI.Z7_Arten_full <- BI.Z7_Arten +
  annotation_custom(
    ggplotGrob(BI.Z7_Arten_plot), 
    xmin = 0.5, xmax = 1.05, ymin = 0.006, ymax = 0.014
  )
BI.Z7_Arten_full

# ggsave(BI.Z7_Arten_full , file = "Arten_Entwicklung_Häufigkeit_Z7_BI.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )

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
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen),
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
  dplyr::rename(Artengruppe = UZL) %>% 
  print()
MOL_Z9.Trend$Artengruppe <- as.factor(MOL_Z9.Trend$Artengruppe)
levels(MOL_Z9.Trend$Artengruppe) <- c("übrige", "UZL")
MOL_Z9.Trend  # Trend = Anteil mehr Flächen (%) pro Jahr

MOL.Z9_Arten <- ggplot(MOL_Z9.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point(size = 2, alpha = 0.5) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Mollusken (Z9)") +
  labs(x = "Häufigkeit (Anteil besetzter Flächen)",
       y = "Entwicklung der Häufigkeit pro Jahr") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.83, 1.04 ),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.5, 1), limits = c(0,1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1))

MOL.Z9_Arten_plot <- ggplot(MOL_Z9.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  labs(x = "",
       y = "") +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.2, 0.4)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1))

MOL.Z9_Arten_full <- MOL.Z9_Arten +
  annotation_custom(
    ggplotGrob(MOL.Z9_Arten_plot), 
    xmin = 0.5, xmax = 1.05, ymin = 0.003, ymax = 0.0085
  )
MOL.Z9_Arten_full

# ggsave(MOL.Z9_Arten_full , file = "Arten_Entwicklung_Häufigkeit_Z9_MOL.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )

# Moose Z9
MOOS.Z9_Occ05_09 <- MOOS.Z9_Artaufnahmen %>%
  filter(Aufnahmejahr < 2010 & Aufnahmejahr > 2004) %>%
  filter(Z7Z9 == 1) %>% as_tibble %>%  # hier schon tibble weil sonst gehts hier zu lange
  group_by(aID_SP) %>%
  dplyr::summarise(Anteil_Flaechen05_09 = n() / 1448)

MOOS.Z9_Occ10_14 <- MOOS.Z9_Artaufnahmen %>%
  filter(Aufnahmejahr < 2015 & Aufnahmejahr > 2009) %>%
  filter(Z7Z9 == 1) %>% as_tibble %>%
  group_by(aID_SP) %>%
  dplyr::summarise(Anteil_Flaechen10_14 = n() / 1448)

MOOS.Z9_Occ15_19 <- MOOS.Z9_Artaufnahmen %>%
  filter(Aufnahmejahr < 2020 & Aufnahmejahr > 2014) %>%
  filter(Z7Z9 == 1) %>% as_tibble %>%
  group_by(aID_SP) %>%
  dplyr::summarise(Anteil_Flaechen15_19 = n() / 1448)

MOOS.Z9_Occurence <- tbl(db, "Arten") %>%
  filter(MOOS   == 1 & Z7Z9 == 1) %>% as_tibble %>%
  left_join(MOOS.Z9_Occ05_09) %>% 
  left_join(MOOS.Z9_Occ10_14) %>% 
  left_join(MOOS.Z9_Occ15_19) %>%
  dplyr::select(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Anteil_Flaechen05_09, Anteil_Flaechen10_14, Anteil_Flaechen15_19) %>% 
  replace_na(list(Anteil_Flaechen05_09 = 0,
                  Anteil_Flaechen10_14 = 0,
                  Anteil_Flaechen15_19 = 0)) %>% 
  filter(Anteil_Flaechen05_09 != 0 | Anteil_Flaechen10_14 != 0 | Anteil_Flaechen15_19 != 0)

MOOS_Z9.Trend <- MOOS.Z9_Occurence %>% 
  gather("Tranche", "Anteil_Flaechen", -c(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)) %>% 
  mutate(Tranchenzahl = ifelse(Tranche == "Anteil_Flaechen05_09", 2007, ifelse(Tranche == "Anteil_Flaechen10_14", 2012, 2017))) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen),
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
  dplyr::rename(Artengruppe = UZL) %>% 
  print()
MOOS_Z9.Trend$Artengruppe <- as.factor(MOOS_Z9.Trend$Artengruppe)
levels(MOOS_Z9.Trend$Artengruppe) <- c("übrige", "UZL")
MOOS_Z9.Trend  # Trend = Anteil mehr Flächen (%) pro Jahr

MOOS.Z9_Arten <- ggplot(MOOS_Z9.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point(size = 2, alpha = 0.5) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Moose (Z9)") +
  labs(x = "Häufigkeit (Anteil besetzter Flächen)",
       y = "Entwicklung der Häufigkeit pro Jahr") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.83, 1.04 ),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.5, 1), limits = c(0,1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1))

MOOS.Z9_Arten_plot <- ggplot(MOOS_Z9.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  labs(x = "",
       y = "") +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.15, 0.3)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.01))

MOOS.Z9_Arten_full <- MOOS.Z9_Arten +
  annotation_custom(
    ggplotGrob(MOOS.Z9_Arten_plot), 
    xmin = 0.5, xmax = 1.05, ymin = 0.0011, ymax = 0.0038
  )
MOOS.Z9_Arten_full

# ggsave(MOOS.Z9_Arten_full , file = "Arten_Entwicklung_Häufigkeit_Z9_MOOS.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )

# Pflanzen Z9
PL.Z9_Occ05_09 <- PL.Z9_Artaufnahmen %>%
  filter(Aufnahmejahr < 2010 & Aufnahmejahr > 2004) %>%
  filter(Z7Z9 == 1) %>% as_tibble %>%  # hier schon tibble weil sonst gehts hier zu lange
  group_by(aID_SP) %>%
  dplyr::summarise(Anteil_Flaechen05_09 = n() / 1449)

PL.Z9_Occ10_14 <- PL.Z9_Artaufnahmen %>%
  filter(Aufnahmejahr < 2015 & Aufnahmejahr > 2009) %>%
  filter(Z7Z9 == 1) %>% as_tibble %>%
  group_by(aID_SP) %>%
  dplyr::summarise(Anteil_Flaechen10_14 = n() / 1449)

PL.Z9_Occ15_19 <- PL.Z9_Artaufnahmen %>%
  filter(Aufnahmejahr < 2020 & Aufnahmejahr > 2014) %>%
  filter(Z7Z9 == 1) %>% as_tibble %>%
  group_by(aID_SP) %>%
  dplyr::summarise(Anteil_Flaechen15_19 = n() / 1449)

PL.Z9_Occurence <- tbl(db, "Arten") %>%
  filter(PL   == 1 & Z7Z9 == 1) %>% as_tibble %>% 
  left_join(PL.Z9_Occ05_09) %>% 
  left_join(PL.Z9_Occ10_14) %>% 
  left_join(PL.Z9_Occ15_19) %>%
  dplyr::select(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL, Anteil_Flaechen05_09, Anteil_Flaechen10_14, Anteil_Flaechen15_19) %>% 
  replace_na(list(Anteil_Flaechen05_09 = 0,
                  Anteil_Flaechen10_14 = 0,
                  Anteil_Flaechen15_19 = 0)) %>% 
  filter(Anteil_Flaechen05_09 != 0 | Anteil_Flaechen10_14 != 0 | Anteil_Flaechen15_19 != 0)

PL_Z9.Trend <- PL.Z9_Occurence %>% 
  gather("Tranche", "Anteil_Flaechen", -c(aID_SP, Gattung, Art, ArtD, Z8, UZL, UZL_Zielart, RL)) %>% 
  mutate(Tranchenzahl = ifelse(Tranche == "Anteil_Flaechen05_09", 2007, ifelse(Tranche == "Anteil_Flaechen10_14", 2012, 2017))) %>%
  group_by(aID_SP) %>% 
  dplyr::summarise(
    Trend = trend.Occ(x = Tranchenzahl, y = Anteil_Flaechen),
    Anteil_Flaechen_mean = mean(Anteil_Flaechen)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD, UZL), by = "aID_SP") %>% 
  dplyr::rename(Artengruppe = UZL) %>% 
  print()
PL_Z9.Trend$Artengruppe <- as.factor(PL_Z9.Trend$Artengruppe)
levels(PL_Z9.Trend$Artengruppe) <- c("übrige", "UZL")
PL_Z9.Trend  # Trend = Anteil mehr Flächen (%) pro Jahr

PL.Z9_Arten <- ggplot(PL_Z9.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  geom_point(size = 2, alpha = 0.5) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Pflanzen (Z9)") +
  labs(x = "Häufigkeit (Anteil besetzter Flächen)",
       y = "Entwicklung der Häufigkeit pro Jahr") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = c(0.83, 1.04 ),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, margin = margin(r = 0)),
        legend.spacing.x = unit(0, 'cm')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_blank()) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.5, 1), limits = c(0,1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1))

PL.Z9_Arten_plot <- ggplot(PL_Z9.Trend, aes(x = Anteil_Flaechen_mean, y = Trend, col = Artengruppe)) +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  labs(x = "",
       y = "") +
  theme(axis.title.x   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.title.y   = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)), #face="bold"),
        axis.text.x    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.ticks.length = unit(5, "pt")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(5,10,5,5),
        plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.15, 0.3)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.01))

PL.Z9_Arten_full <- PL.Z9_Arten +
  annotation_custom(
    ggplotGrob(PL.Z9_Arten_plot), 
    xmin = 0.5, xmax = 1.05, ymin = 0.0011, ymax = 0.0038
  )
PL.Z9_Arten_full

# ggsave(PL.Z9_Arten_full , file = "Arten_Entwicklung_Häufigkeit_Z9_PL.png",
#        path = "R_PLOTS/REPORT",
#        width = 12, height = 12, units = "cm" )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Übersichtsplot alle 6 (Trend Artniveau) ----
Trend_Artniveau <- gridExtra::grid.arrange(PL.Z7_Arten,
                                           TF.Z7_Arten,
                                           BI.Z7_Arten,
                                           MOL.Z9_Arten,
                                           MOOS.Z9_Arten,
                                           PL.Z9_Arten,
                                           ncol = 2, nrow = 3, as.table = F)

# ggsave(Trend_Artniveau , file = "Trend_Artniveau.png",
#        path = "R_PLOTS/REPORT",
#        width = 25, height = 25, units = "cm" )
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Trend Artentabelle ----
# Pflanzen Z7
PL_Z7.Trend %>%
  filter(Trend > 0.008) %>% 
  arrange(desc(Trend))
PL_Z7.Trend %>%
  filter(Trend < -0.006) %>% 
  arrange(desc(Trend))

# Tagfalter Z7
TF_Z7.Trend %>%
  filter(Trend > 0.014) %>% 
  arrange(desc(Trend))
TF_Z7.Trend %>%
  filter(Trend < -0.002) %>% 
  arrange(desc(Trend))

# Vögel Z7
BI_Z7.Trend %>%
  filter(Trend > 0.008) %>% 
  arrange(desc(Trend))
BI_Z7.Trend %>%
  filter(Trend < -0.002) %>% 
  arrange(desc(Trend))

# Mollusken Z9
MOL_Z9.Trend %>%
  filter(Trend > 0.003) %>% 
  arrange(desc(Trend))
MOL_Z9.Trend %>%
  filter(Trend < -0.001) %>% 
  arrange(desc(Trend))

# Moose Z9
MOOS_Z9.Trend %>%
  filter(Trend > 0.0015) %>% 
  arrange(desc(Trend))
MOOS_Z9.Trend %>%
  filter(Trend < -0.002) %>% 
  arrange(desc(Trend))

# Pflanzen Z9
PL_Z9.Trend %>%
  filter(Trend > 0.0015) %>% 
  arrange(desc(Trend))
PL_Z9.Trend %>%
  filter(Trend < -0.0015) %>% 
  arrange(desc(Trend))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Trend deskriptiv ----
Trend_all <- list(PL.Z7   = PL_Z7.Trend,
                  TF.Z7   = TF_Z7.Trend,
                  BI.Z7   = BI_Z7.Trend,
                  MOL.Z9  = MOL_Z9.Trend,
                  MOOS.Z9 = MOOS_Z9.Trend,
                  PL.Z9   = PL_Z9.Trend)
Trend_Tabelle <- data.frame(row.names = names(Trend_all))
for (i in 1:6){
  Trend_Tabelle[i, "Zunahme"] <- Trend_all[[i]] %>%
    filter(Trend > 0) %>%
    nrow
  Trend_Tabelle[i, "Abnahme"] <- Trend_all[[i]] %>%
    filter(Trend < 0) %>%
    nrow
  Trend_Tabelle[i, "stabil"] <- Trend_all[[i]] %>%
    filter(Trend == 0) %>%
    nrow
  Trend_Tabelle[i, "Summe"] <- sum(Trend_Tabelle[i,1:3])
  Trend_Tabelle[i, "Zunahme_UZL"] <- Trend_all[[i]] %>%
    filter(Artengruppe == "UZL") %>% 
    filter(Trend > 0) %>%
    nrow
  Trend_Tabelle[i, "Abnahme_UZL"] <- Trend_all[[i]] %>%
    filter(Artengruppe == "UZL") %>% 
    filter(Trend < 0) %>%
    nrow
  Trend_Tabelle[i, "stabil_UZL"] <- Trend_all[[i]] %>%
    filter(Artengruppe == "UZL") %>% 
    filter(Trend == 0) %>%
    nrow
  Trend_Tabelle[i, "Summe_UZL"] <- sum(Trend_Tabelle[i,5:7])
  Trend_Tabelle[i, "Zunahme_UB"] <- Trend_all[[i]] %>%
    filter(Artengruppe == "übrige") %>% 
    filter(Trend > 0) %>%
    nrow
  Trend_Tabelle[i, "Abnahme_UB"] <- Trend_all[[i]] %>%
    filter(Artengruppe == "übrige") %>% 
    filter(Trend < 0) %>%
    nrow
  Trend_Tabelle[i, "stabil_UB"] <- Trend_all[[i]] %>%
    filter(Artengruppe == "übrige") %>% 
    filter(Trend == 0) %>%
    nrow
  Trend_Tabelle[i, "Summe_UB"] <- sum(Trend_Tabelle[i,9:11])
  Trend_Tabelle[i, "Summe_UZL_UB"] <- sum(Trend_Tabelle[i,c("Summe_UZL", "Summe_UB")])
}
Trend_Tabelle

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## END OF SCRIPT ----