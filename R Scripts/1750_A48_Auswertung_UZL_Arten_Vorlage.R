### UZL-Auswertung ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Date created:     2020-10-12
# Location created: Hintermann&Weber, Reinach
# Last Entry:       2020-10-16
# Author:           Raphael S. von Bueren (GitHub: buerap)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Get started (Working directory, packages, ...)----
rm(list = ls(all = TRUE) )
Sys.setenv(TZ = "Europe/Zurich")
graphics.off()                    # Clear Graphic Window
cat( "\014" )                     # Clear Console ( =  CTRL L)

library(tidyverse)
library(Rmisc)
library(ggthemes)
library(readxl)
library(devtools)
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
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Connection to data base----
db <- src_sqlite(path = "database/DB_BDM_2020_08_20.db", create = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Entwicklung Z7-Tagfalter ----
# Datentabelle erstellen
dat <- tbl(db, "KD_Z7") %>%    # Kopfdaten
  filter(!is.na(yearBu)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z7")) %>%  # Raumdaten (z.B. Hoehe)
  filter(Verdichtung_BDM == "nein") %>%   # verdichtete Regionen Jura und Tessin bereinigen
  left_join(tbl(db, "STICHPROBE_Z7")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Hoehe, Aufnahmejahr = yearBu) %>% # spalten waehlen die ich brauche als flaecheninformationen
  left_join(
    tbl(db, "TF") %>%  # Tagfalteraufnahmen
      mutate_at( vars( c("Ind")), funs(if_else(is.na(Ind), 54, Ind))) %>% # Datenbank an einer Stelle falsch -> manuell Wert ersetzen
      filter(!is.na(aID_SP)) %>% # unbestimmte Arten rausfiltern
      left_join(tbl(db, "Arten")) %>% # Artaufnahmen
      group_by(aID_KD) %>%            # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
      dplyr::summarise(
        AZ = n(),                     # Summe der Gesamt-Artenzahl (AZ)
        AZ_UZL = sum(UZL == 1),       # Summe der UZL-Arten
        AZ_UB = sum(UZL == 0),        # Summe der uebrigen (nicht-UZL) Arten
        IZ = sum(Ind),                # Summe der Gesamt-Individuenzahl (IZ)
        IZ_UZL = sum(Ind[UZL == 1]),  # Summe der UZL-Individuen
        IZ_UB = sum(Ind[UZL == 0]),    # Summe der uebrigen (nicht-UZL) Arten
        IZperAZ = mean(Ind),
        IZperAZ_UZL = mean(Ind[UZL == 1]),
        IZperAZ_UB = mean(Ind[UZL == 0])
  )) %>%
  as_tibble() %>% 
  replace_na(list(AZ     = 0, AZ_UZL      = 0, AZ_UB      = 0,   # in denjenigen Flächen wo keine Aufnahmen
                  IZ     = 0, IZ_UZL      = 0, IZ_UB      = 0,   # waren gibts beim join ein NA
                  IZ     = 0, IZperAZ_UZL = 0, IZperAZ_UB = 0))
              
  #add_column(IZperAZ     = dat$IZ/dat$AZ,         # bei jeder Aufnahmeflaeche wird jedes Aufnahmejahr
   #          IZperAZ_UZL = dat$IZ_UZL/dat$AZ_UZL, # durchschnittliche Individuenzahl pro Art bestimmt
   #          IZperAZ_UB  = dat$IZ_UB/dat$AZ_UB)   # und dies fuer: UZL, Uebrige, gesamt  
      

# Plot Artenzahl
TF1 <- dat %>% 
  group_by(Aufnahmejahr) %>%  # gruppiert nach Aufnahmejahr die Mittelwerte aller Flaechen berechnen
  dplyr::summarise(
    UZL = mean(AZ_UZL),
    übrige = mean(AZ_UB)) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% # umwandeln in long-format
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL: Tagfalter (Z7)") +
  ylim(0, NA) +
  labs(x = "Aufnahmejahr",
       y = "Anzahl Arten") +
  theme(legend.position = c(0.85, 0.15))

# Plot Artenzahl relativ
TF2 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>%
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>%
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Tagfalter (Z7)") +
  ylim(0, 2) +
  labs(x = "Aufnahmejahr",
       y = "Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))

# Plot Individuenzahl
TF3 <- dat %>% 
  group_by(Aufnahmejahr) %>%  # gruppiert nach Aufnahmejahr die Mittelwerte aller Flaechen berechnen
  dplyr::summarise(
    UZL = mean(IZ_UZL),
    übrige = mean(IZ_UB)) %>% 
  gather("Artengruppe", "IZ", -Aufnahmejahr) %>% # umwandeln in long-format
  ggplot(aes(x = Aufnahmejahr, y = IZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("INDIVIDUENZAHL: Tagfalter (Z7)") +
  ylim(0, NA) +
  labs(x = "Aufnahmejahr",
       y = "Anzahl Individuen") +
  theme(legend.position = c(0.85, 0.15))

# Plot Individuenzahl relativ
TF4 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(IZ_UZL) / mean(dat$IZ_UZL),
    übrige = mean(IZ_UB)/ mean(dat$IZ_UB)) %>%
  gather("Artengruppe", "IZ", -Aufnahmejahr) %>%
  ggplot(aes(x = Aufnahmejahr, y = IZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("INDIVIDUENZAHL relativ: Tagfalter (Z7)") +
  ylim(0, 2) +
  labs(x = "Aufnahmejahr",
       y = "Individuen relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))

# Plot Individuen pro Arten
TF5 <- dat %>% 
  group_by(Aufnahmejahr) %>%  # gruppiert nach Aufnahmejahr die Mittelwerte aller Flaechen berechnen
  dplyr::summarise(
    UZL = mean(IZperAZ_UZL),
    übrige = mean(IZperAZ_UB)) %>% 
  gather("Artengruppe", "IZperAZ", -Aufnahmejahr) %>% # umwandeln in long-format
  ggplot(aes(x = Aufnahmejahr, y = IZperAZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("Individuen pro Arten: Tagfalter (Z7)") +
  ylim(0, NA) +
  labs(x = "Aufnahmejahr",
       y = "durchschnittliche Anzahl Individuen pro Art") +
  theme(legend.position = c(0.85, 0.15))


# plot on same page
gridExtra::grid.arrange(TF1, TF2, TF3, TF4, TF5, ncol = 2, nrow = 3)



# library(arm)
# d <- dat %>%
#   dplyr::select(aID_STAO, Jahr = Aufnahmejahr, IZ_UZL, IZ_UB) %>%
#   gather("Artengruppe", "Ind", -c(aID_STAO, Jahr)) %>%
#   mutate(Jahr_sd = (Jahr-2010) / 10)
# lmer(Ind ~ Jahr_sd * Artengruppe + (1|aID_STAO), data = d) %>%
#   summary
# 
# dat %>%
#   group_by(aID_STAO) %>%
#   dplyr::summarise(AZ = mean(AZ), Hoehe = mean(Hoehe)) %>%
#   ggplot(aes(x = Hoehe, y = AZ)) +
#   geom_point() +
#   geom_smooth()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z7-Voegel ----
# Datentabelle erstellen
dat <- tbl(db, "KD_Z7") %>%    # Kopfdaten
  filter(!is.na(yearBi)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z7")) %>%  # Raumdaten (z.B. Hoehe)
  filter(Verdichtung_BDM == "nein") %>%   # verdichtete Regionen Jura und Tessin bereinigen
  left_join(tbl(db, "STICHPROBE_Z7")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Hoehe, Aufnahmejahr = yearBi) %>% # spalten waehlen die ich brauche als flaecheninformationen
  left_join(
    tbl(db, "BI") %>%                  # Vogelaufnahmen (Pr1,2,3 sind character format`??`)
      filter(!is.na(aID_SP)) %>%       # unbestimmte Arten rausfiltern
      left_join(tbl(db, "Arten")) %>%  # Artaufnahmen
      group_by(aID_KD) %>%             # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
      dplyr::summarise(
        AZ = n(),                      # Summe der Gesamt-Artenzahl (AZ)
        AZ_UZL = sum(UZL == 1),        # Summe der UZL-Arten
        AZ_UB = sum(UZL == 0))) %>%    # Summe der uebrigen (nicht-UZL) Arten
  as_tibble()    %>%                   # HIER OHNE INDIVIDUEN
  remove_missing() # UNSCHOEN !! ##

summary(dat)



# Plot Artenzahl
BI1 <- dat %>% 
  group_by(Aufnahmejahr) %>%  # gruppiert nach Aufnahmejahr die Mittelwerte aller Flaechen berechnen
  dplyr::summarise(
    UZL = mean(AZ_UZL),
    übrige = mean(AZ_UB)) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% # umwandeln in long-format
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL: Vögel (Z7)") +
  ylim(0, NA) +
  labs(x = "Aufnahmejahr",
       y = "Anzahl Arten") +
  theme(legend.position = c(0.85, 0.15))

# Plot Artenzahl relativ
BI2 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>%
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>%
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Vögel (Z7)") +
  ylim(0, 2) +
  labs(x = "Aufnahmejahr",
       y = "Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))

gridExtra::grid.arrange(BI1, BI2, ncol = 2, nrow = 1)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z7-Pflanzen ----
# Datentabelle erstellen
dat <- tbl(db, "KD_Z7") %>%    # Kopfdaten
  filter(!is.na(yearPl)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z7")) %>%  # Raumdaten (z.B. Hoehe)
  filter(Verdichtung_BDM == "nein") %>%   # verdichtete Regionen Jura und Tessin bereinigen
  left_join(tbl(db, "STICHPROBE_Z7")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Hoehe, Aufnahmejahr = yearPl) %>% # spalten waehlen die ich brauche als flaecheninformationen
  left_join(
    tbl(db, "Pl") %>%  # Pflanzenaufnahmen
      filter(!is.na(aID_SP)) %>% # unbestimmte Arten rausfiltern
      filter(Z7 == 1) %>% 
      left_join(tbl(db, "Arten")) %>% # Artaufnahmen
      group_by(aID_KD) %>%            # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
      dplyr::summarise(
        AZ = n(),                     # Summe der Gesamt-Artenzahl (AZ)
        AZ_UZL = sum(UZL == 1),       # Summe der UZL-Arten
        AZ_UB = sum(UZL == 0),        # Summe der uebrigen (nicht-UZL) Arten
  )) %>%
  as_tibble()
summary(dat)


# Plot Artenzahl
Pl1 <- dat %>% 
  group_by(Aufnahmejahr) %>%  # gruppiert nach Aufnahmejahr die Mittelwerte aller Flaechen berechnen
  dplyr::summarise(
    UZL = mean(AZ_UZL),
    übrige = mean(AZ_UB)) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% # umwandeln in long-format
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL: Pflanzen (Z7)") +
  ylim(0, NA) +
  labs(x = "Aufnahmejahr",
       y = "Anzahl Arten") +
  theme(legend.position = c(0.85, 0.15))

# Plot Artenzahl relativ
Pl2 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>%
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>%
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Pflanzen (Z7)") +
  ylim(0, 2) +
  labs(x = "Aufnahmejahr",
       y = "Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))

# Plot UZl vs. uebrige relativ --> einfluss des beobachters?? oder jahresklima??
Pl3 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>%
  ggplot(aes(x = übrige, y = UZL)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Vergleich UZL vs. übrige Pflanzenarten (Z7)") +
  #ylim(0, 2) +
  labs(x = "übrige Arten relativ zum Durchschnitt 2003 - 2019",
       y = "UZL Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))


library(arm)
d <- dat %>%
  mutate(AZrel_UZL = AZ_UZL / mean(AZ_UZL)) %>% 
  mutate(AZrel_UB  = AZ_UB  / mean(AZ_UB)) %>% 
  dplyr::select(aID_STAO, Jahr = Aufnahmejahr, AZrel_UZL, AZrel_UB)
lmer(AZrel_UZL ~ AZrel_UB + (1|aID_STAO), data = d) %>% # random factor is standort
  summary

# alle flaechen plot UZl vs. uebrige relativ --> einfluss des beobachters?? oder jahresklima??
Pl4 <- d %>%
  ggplot(aes(x = AZrel_UB, y = AZrel_UZL)) +
  geom_point() +
  geom_smooth(method = "loess") +
  ggtitle("Vergleich UZL vs. übrige Pflanzenarten (Z7)") +
  xlim(0, 2.2) +
  ylim(0, 2.2) +
  labs(x = "übrige Arten relativ zum Durchschnitt 2003 - 2019",
       y = "UZL Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))


# plot on same page
gridExtra::grid.arrange(Pl1, Pl2, ncol = 2, nrow = 1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z9-Pflanzen ----
# Datentabelle erstellen
dat <- tbl(db, "KD_Z9") %>%    # Kopfdaten
  filter(!is.na(yearPl)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z9")) %>%  # Raumdaten (z.B. Hoehe)
  left_join(tbl(db, "STICHPROBE_Z9")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Hoehe, Aufnahmejahr = yearPl) %>% # spalten waehlen die ich brauche als flaecheninformationen
  left_join(
    tbl(db, "Pl") %>%  # Pflanzenaufnahmen
      filter(!is.na(aID_SP)) %>% # unbestimmte Arten rausfiltern
      filter(Z7 == 0) %>% 
      left_join(tbl(db, "Arten")) %>% # Artaufnahmen
      group_by(aID_KD) %>%            # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
      dplyr::summarise(
        AZ = n(),                     # Summe der Gesamt-Artenzahl (AZ)
        AZ_UZL = sum(UZL == 1),       # Summe der UZL-Arten
        AZ_UB = sum(UZL == 0),        # Summe der uebrigen (nicht-UZL) Arten
      )) %>%
  as_tibble() %>% 
  remove_missing() # UNSCHOEN !!! ##
summary(dat)


# Plot Artenzahl
Pl.1 <- dat %>% 
  group_by(Aufnahmejahr) %>%  # gruppiert nach Aufnahmejahr die Mittelwerte aller Flaechen berechnen
  dplyr::summarise(
    UZL = mean(AZ_UZL),
    übrige = mean(AZ_UB)) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% # umwandeln in long-format
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL: Pflanzen (Z9)") +
  ylim(0, NA) +
  labs(x = "Aufnahmejahr",
       y = "Anzahl Arten") +
  theme(legend.position = c(0.85, 0.15))

# Plot Artenzahl relativ --> Interaction??
Pl.2 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>%
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>%
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Pflanzen (Z9)") +
  ylim(0, 2) +
  labs(x = "Aufnahmejahr",
       y = "Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))

# Plot UZl vs. uebrige relativ --> einfluss des beobachters?? oder jahresklima??
Pl.3 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>%
  ggplot(aes(x = übrige, y = UZL)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Vergleich UZL vs. übrige Pflanzenarten (Z9)") +
  xlim(0.75, 1.2) +
  ylim(0.75, 1.2) +
  labs(x = "übrige Arten relativ zum Durchschnitt 2003 - 2019",
       y = "UZL Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))


library(arm)
d <- dat %>%
  mutate(AZrel_UZL = AZ_UZL / mean(AZ_UZL)) %>% 
  mutate(AZrel_UB  = AZ_UB  / mean(AZ_UB)) %>% 
  dplyr::select(aID_STAO, Jahr = Aufnahmejahr, AZrel_UZL, AZrel_UB)
lmer(AZrel_UZL ~ AZrel_UB + (1|aID_STAO), data = d) %>% # random factor is standort
  summary

# alle flaechen plot UZl vs. uebrige relativ --> einfluss des beobachters?? oder jahresklima??
Pl.4 <- d %>%
  ggplot(aes(x = AZrel_UB, y = AZrel_UZL)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Vergleich UZL vs. übrige Pflanzenarten (Z9)") +
  xlim(0, 3) +
  ylim(0, 15) +
  labs(x = "übrige Arten relativ zum Durchschnitt 2003 - 2019",
       y = "UZL Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))


# plot on same page
gridExtra::grid.arrange(Pl.1, Pl.2, ncol = 2, nrow = 1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z9-Moose ----
# Datentabelle erstellen
dat <- tbl(db, "KD_Z9") %>%      # Kopfdaten
  filter(!is.na(yearMoos)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z9")) %>%  # Raumdaten (z.B. Hoehe)
  left_join(tbl(db, "STICHPROBE_Z9")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Hoehe, Aufnahmejahr = yearMoos) %>% # spalten waehlen die ich brauche als flaecheninformationen
  left_join(
    tbl(db, "Moos") %>%  # Pflanzenaufnahmen
      filter(!is.na(aID_SP)) %>% # unbestimmte Arten rausfiltern
      left_join(tbl(db, "Arten")) %>% # Artaufnahmen
      group_by(aID_KD) %>%            # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
      dplyr::summarise(
        AZ = n(),                     # Summe der Gesamt-Artenzahl (AZ)
        AZ_UZL = sum(UZL == 1),       # Summe der UZL-Arten
        AZ_UB = sum(UZL == 0),        # Summe der uebrigen (nicht-UZL) Arten
      )) %>%
  as_tibble() %>% 
  remove_missing() # UNSCHOEN !!! ##
summary(dat)

# Plot Artenzahl
Pl.1 <- dat %>% 
  group_by(Aufnahmejahr) %>%  # gruppiert nach Aufnahmejahr die Mittelwerte aller Flaechen berechnen
  dplyr::summarise(
    UZL = mean(AZ_UZL),
    übrige = mean(AZ_UB)) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% # umwandeln in long-format
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL: Moose (Z9)") +
  ylim(0, NA) +
  labs(x = "Aufnahmejahr",
       y = "Anzahl Arten") +
  theme(legend.position = c(0.85, 0.15))

# Plot Artenzahl relativ --> Interaction??
Pl.2 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>%
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>%
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Moose (Z9)") +
  ylim(0, 2) +
  labs(x = "Aufnahmejahr",
       y = "Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))

# Plot UZl vs. uebrige relativ --> einfluss des beobachters?? oder jahresklima??
Pl.3 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>%
  ggplot(aes(x = übrige, y = UZL)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Vergleich UZL vs. übrige Moosarten (Z9)") +
  xlim(0.75, 1.2) +
  ylim(0.75, 1.2) +
  labs(x = "übrige Arten relativ zum Durchschnitt 2003 - 2019",
       y = "UZL Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))

# alle flaechen plot UZl vs. uebrige relativ --> einfluss des beobachters?? oder jahresklima??
Pl.4 <- d %>%
  ggplot(aes(x = AZrel_UB, y = AZrel_UZL)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Vergleich UZL vs. übrige Moosarten (Z9)") +
  xlim(0, 3) +
  ylim(0, 15) +
  labs(x = "übrige Arten relativ zum Durchschnitt 2003 - 2019",
       y = "UZL Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))


# plot on same page
gridExtra::grid.arrange(Pl.1, Pl.2, ncol = 2, nrow = 1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z9-Mollusken ----
# Datentabelle erstellen
dat <- tbl(db, "KD_Z9") %>%      # Kopfdaten
  filter(!is.na(yearMol)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z9")) %>%  # Raumdaten (z.B. Hoehe)
  left_join(tbl(db, "STICHPROBE_Z9")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Hoehe, Aufnahmejahr = yearMoos) %>% # spalten waehlen die ich brauche als flaecheninformationen
  left_join(
    tbl(db, "Moos") %>%  # Pflanzenaufnahmen
      filter(!is.na(aID_SP)) %>% # unbestimmte Arten rausfiltern
      left_join(tbl(db, "Arten")) %>% # Artaufnahmen
      group_by(aID_KD) %>%            # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
      dplyr::summarise(
        AZ = n(),                     # Summe der Gesamt-Artenzahl (AZ)
        AZ_UZL = sum(UZL == 1),       # Summe der UZL-Arten
        AZ_UB = sum(UZL == 0),        # Summe der uebrigen (nicht-UZL) Arten
      )) %>%
  as_tibble() %>% 
  remove_missing() # UNSCHOEN !!! ##
summary(dat)


# ----
# END OF SCRIPT




