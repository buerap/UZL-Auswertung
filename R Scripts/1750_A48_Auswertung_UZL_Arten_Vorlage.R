rm(list = ls(all = TRUE) )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Einstellungen ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)
library(Rmisc)
library(ggthemes)
library(readxl)
library(devtools)
#install_github("TobiasRoth/BDM")
#library(BDM)

# Connection to data base
db <- src_sqlite(path = "database/DB_BDM_2020_08_20.db", create = FALSE)

# Plot Einstellungen
theme_set(
  theme_clean() +
    theme(
      legend.title = element_blank(), 
      legend.position = "top", 
      legend.background = element_rect(colour = "white"))
)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Write function - eher nicht! ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

entwicklung <- function(y            = "Arten", 
                        Kopfdaten    = "KD_Z7",  # KD_Z7 # KD_Z9
                        Aufnahmeyahr = "yearBu", # yearPl (Pflanzen) # yearBi (Vogel) # yearBu (Tagfalter)  
){
  
}









#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z7-Tagfalter ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
  replace_na(list(IZ_UZL = 0, IZ_UB = 0, IZ = 0, IZperAZ_UZL = 0))
                    #%>%       # NA in Spalte UZL_Ind durch Null ersetzen
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
#   





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z7-Voegel ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dat <- tbl(db, "KD_Z7") %>%    # Kopfdaten
  filter(!is.na(yearBu)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z7")) %>%  # Raumdaten (z.B. Hoehe)
  filter(Verdichtung_BDM == "nein") %>%   # verdichtete Regionen Jura und Tessin bereinigen
  left_join(tbl(db, "STICHPROBE_Z7")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Hoehe, Aufnahmejahr = yearBu) %>% # spalten waehlen die ich brauche als flaecheninformationen
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
  remove_missing() ## UNSCHOEN !!!!! ####

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
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
  as_tibble() %>% 
  remove_missing() ## UNSCHOEN !!!!! ####
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




d <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>% 
  mutate(Jahr_sd = (Aufnahmejahr-2010) / 10)

0) select nötige spalten
1) AZ relativ berechnen (für jede fläche)
2) gather -> long format
3) mutate Jahr
4) lmer (somit von allen flächen und nicht nur von jahresdurchschnitten)




d <- dat %>%
  dplyr::select(aID_STAO, Jahr = Aufnahmejahr, IZ_UZL, IZ_UB) %>%
  gather("Artengruppe", "Ind", -c(aID_STAO, Jahr)) %>%
  mutate(Jahr_sd = (Jahr-2010) / 10)
lmer(Ind ~ Jahr_sd * Artengruppe + (1|aID_STAO), data = d) %>%
  summary

dat %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(AZ = mean(AZ), Hoehe = mean(Hoehe)) %>%
  ggplot(aes(x = Hoehe, y = AZ)) +
  geom_point() +
  geom_smooth()

# plot on same page
gridExtra::grid.arrange(Pl1, Pl2, ncol = 2, nrow = 1)






















#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z9-Pflanzen ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z9-Moose ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z9-Mollusken ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





