### UZL-Auswertung ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Date created:     2020-10-12
# Location created: Hintermann&Weber, Reinach
# Last Entry:       2020-11-02
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
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Connection to data base----
db <- src_sqlite(path = "database/DB_BDM_2020_08_20.db", create = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Deskriptiv: Z7-Tagfalter ----
dat <- tbl(db, "KD_Z7") %>%    # Kopfdaten
  filter(!is.na(yearBu)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z7")) %>%  # Raumdaten (z.B. Hoehe)
  filter(Verdichtung_BDM == "nein") %>%   # verdichtete Regionen Jura und Tessin bereinigen
  left_join(tbl(db, "STICHPROBE_Z7")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBu) %>%   # spalten waehlen die ich brauche als flaecheninformationen
  left_join(tbl(db, "TF") %>%  # Tagfalteraufnahmen
    mutate_at( vars( c("Ind")), funs(if_else(is.na(Ind), 54, Ind))) %>% # Datenbank an einer Stelle falsch -> manuell Wert ersetzen
    filter(!is.na(aID_SP)) %>%  # unbestimmte Arten rausfiltern
    left_join(tbl(db, "Arten"))) %>% 
  as_tibble()

# Zeitraum
dat %>% dplyr::select(Aufnahmejahr) %>% min() 
dat %>% dplyr::select(Aufnahmejahr) %>% max()  

# Anzahl Standorte
STAO <- dat %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(Anfang = min(Aufnahmejahr),
                   Ende   = max(Aufnahmejahr))
nrow(STAO)
    
# Gesamtanzahl 
TF <- dat %>%                                           
      group_by(aID_SP) %>%                              # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
      filter(!is.na(aID_SP)) %>%     
      dplyr::summarise(n_plots = n())
nrow(TF)

# Anteil
round(nrow(TF)/200,2)

# UZL vs. uebrige vs. nicht verwendet
UZL <- TF %>%
       left_join(tbl(db, "Arten") %>% as_tibble(),
                 by = "aID_SP") %>% 
       dplyr::select(aID_SP, n_plots, Gattung, Art, ArtD, UZL) %>% 
       print()

a <- UZL %>% dplyr::filter(UZL == 1) %>% nrow() %>% print()
b <- UZL %>% dplyr::filter(UZL == 0) %>% nrow() %>% print()
a+b



## Deskriptiv: Z7-Voegel ----
dat <- tbl(db, "KD_Z7") %>%    # Kopfdaten
  filter(!is.na(yearBi)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z7")) %>%  # Raumdaten (z.B. Hoehe)
  filter(Verdichtung_BDM == "nein") %>%   # verdichtete Regionen Jura und Tessin bereinigen
  left_join(tbl(db, "STICHPROBE_Z7")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBi) %>%   # spalten waehlen die ich brauche als flaecheninformationen
  left_join(tbl(db, "BI") %>%  # Tagfalteraufnahmen
              filter(!is.na(aID_SP)) %>%  # unbestimmte Arten rausfiltern
              left_join(tbl(db, "Arten"))) %>% 
  as_tibble()

# Zeitraum
dat %>% dplyr::select(Aufnahmejahr) %>% min() 
dat %>% dplyr::select(Aufnahmejahr) %>% max()  

# Anzahl Standorte
STAO <- dat %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(Anfang = min(Aufnahmejahr),
                   Ende   = max(Aufnahmejahr))
nrow(STAO)

# Gesamtanzahl 
BI <- dat %>%                                           
  group_by(aID_SP) %>%                              # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
  filter(!is.na(aID_SP)) %>% 
  dplyr::summarise(n_plots = n())
nrow(BI)

# Anteil
round(nrow(BI)/192,2)

# UZL vs. uebrige vs. nicht verwendet
UZL <- BI %>%
  left_join(tbl(db, "Arten") %>% as_tibble(),
            by = "aID_SP") %>% 
  dplyr::select(aID_SP, n_plots, Gattung, Art, ArtD, UZL) %>% 
  print()

a <- UZL %>% dplyr::filter(UZL == 1) %>% nrow() %>% print()
b <- UZL %>% dplyr::filter(UZL == 0) %>% nrow() %>% print()
a+b


## Deskriptiv: Z7-Pflanzen ----
dat <- tbl(db, "KD_Z7") %>%    # Kopfdaten
  filter(!is.na(yearPl)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z7")) %>%  # Raumdaten (z.B. Hoehe)
  filter(Verdichtung_BDM == "nein") %>%   # verdichtete Regionen Jura und Tessin bereinigen
  left_join(tbl(db, "STICHPROBE_Z7")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl) %>%   # spalten waehlen die ich brauche als flaecheninformationen
  left_join(tbl(db, "PL") %>%  # Tagfalteraufnahmen
              filter(!is.na(aID_SP)) %>%  # unbestimmte Arten rausfiltern
              left_join(tbl(db, "Arten"))) %>% 
  as_tibble()

# Zeitraum
dat %>% dplyr::select(Aufnahmejahr) %>% min() 
dat %>% dplyr::select(Aufnahmejahr) %>% max()  

# Anzahl Standorte
STAO <- dat %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(Anfang = min(Aufnahmejahr),
                   Ende   = max(Aufnahmejahr))
nrow(STAO)

# Gesamtanzahl 
PL <- dat %>%                                           
  group_by(aID_SP) %>%                              # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
  filter(!is.na(aID_SP)) %>% 
  dplyr::summarise(n_plots = n())
nrow(PL)

# Anteil
round(nrow(PL)/2712,2)

# UZL vs. uebrige vs. nicht verwendet
UZL <- PL %>%
  left_join(tbl(db, "Arten") %>% as_tibble(),
            by = "aID_SP") %>% 
  dplyr::select(aID_SP, n_plots, Gattung, Art, ArtD, UZL) %>% 
  print()

a <- UZL %>% dplyr::filter(UZL == 1) %>% nrow() %>% print()
b <- UZL %>% dplyr::filter(UZL == 0) %>% nrow() %>% print()
a+b

## Deskriptiv: Z9-Pflanzen ----
dat <- tbl(db, "KD_Z9") %>%    # Kopfdaten
  filter(!is.na(yearPl)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z9")) %>%  # Raumdaten (z.B. Hoehe)
  left_join(tbl(db, "STICHPROBE_Z9")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearPl) %>%   # spalten waehlen die ich brauche als flaecheninformationen
  left_join(tbl(db, "PL") %>%  # Tagfalteraufnahmen
              filter(!is.na(aID_SP)) %>%  # unbestimmte Arten rausfiltern
              left_join(tbl(db, "Arten"))) %>% 
  as_tibble()



# Zeitraum
dat %>% dplyr::select(Aufnahmejahr) %>% min() 
dat %>% dplyr::select(Aufnahmejahr) %>% max()  

# Anzahl Standorte
STAO <- dat %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(Anfang = min(Aufnahmejahr),
                   Ende   = max(Aufnahmejahr))
nrow(STAO)

# Gesamtanzahl 
PL <- dat %>%                                           
  group_by(aID_SP) %>%                              # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
  filter(!is.na(aID_SP)) %>% 
  dplyr::summarise(n_plots = n())
nrow(PL)

# Anteil
round(nrow(PL)/2712,2)

# UZL vs. uebrige vs. nicht verwendet
UZL <- PL %>%
  left_join(tbl(db, "Arten") %>% as_tibble(),
            by = "aID_SP") %>% 
  dplyr::select(aID_SP, n_plots, Gattung, Art, ArtD, UZL) %>% 
  print()

a <- UZL %>% dplyr::filter(UZL == 1) %>% nrow() %>% print()
b <- UZL %>% dplyr::filter(UZL == 0) %>% nrow() %>% print()
a+b


## Deskriptiv: Z9-Moose ----
dat <- tbl(db, "KD_Z9") %>%      # Kopfdaten
  filter(!is.na(yearMoos)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z9")) %>%  # Raumdaten (z.B. Hoehe)
  left_join(tbl(db, "STICHPROBE_Z9")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMoos) %>%   # spalten waehlen die ich brauche als flaecheninformationen
  left_join(tbl(db, "Moos") %>%  # Tagfalteraufnahmen
              filter(!is.na(aID_SP)) %>%  # unbestimmte Arten rausfiltern
              left_join(tbl(db, "Arten"))) %>% 
  as_tibble()


# Zeitraum
dat %>% dplyr::select(Aufnahmejahr) %>% min() 
dat %>% dplyr::select(Aufnahmejahr) %>% max()  

# Anzahl Standorte
STAO <- dat %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(Anfang = min(Aufnahmejahr),
                   Ende   = max(Aufnahmejahr))
nrow(STAO)

# Gesamtanzahl 
MOS <- dat %>%                                           
  group_by(aID_SP) %>%                              # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
  filter(!is.na(aID_SP)) %>% 
  dplyr::summarise(n_plots = n())
nrow(MOS)

# Anteil
round(nrow(MOS)/2712,2)

# UZL vs. uebrige vs. nicht verwendet
UZL <- MOS %>%
  left_join(tbl(db, "Arten") %>% as_tibble(),
            by = "aID_SP") %>% 
  dplyr::select(aID_SP, n_plots, Gattung, Art, ArtD, UZL) %>% 
  print()

a <- UZL %>% dplyr::filter(UZL == 1) %>% nrow() %>% print()
b <- UZL %>% dplyr::filter(UZL == 0) %>% nrow() %>% print()
a+b


## Deskriptiv: Z9-Mollusken ----
dat <- tbl(db, "KD_Z9") %>%      # Kopfdaten
  filter(!is.na(yearMol)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z9")) %>%  # Raumdaten (z.B. Hoehe)
  left_join(tbl(db, "STICHPROBE_Z9")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearMol) %>%   # spalten waehlen die ich brauche als flaecheninformationen
  left_join(tbl(db, "MOL") %>%  # Tagfalteraufnahmen
              filter(!is.na(aID_SP)) %>%  # unbestimmte Arten rausfiltern
              left_join(tbl(db, "Arten"))) %>% 
  as_tibble()


# Zeitraum
dat %>% dplyr::select(Aufnahmejahr) %>% min() 
dat %>% dplyr::select(Aufnahmejahr) %>% max()  

# Anzahl Standorte
STAO <- dat %>%
  group_by(aID_STAO) %>%
  dplyr::summarise(Anfang = min(Aufnahmejahr),
                   Ende   = max(Aufnahmejahr))
nrow(STAO)

# Gesamtanzahl 
MOL <- dat %>%                                           
  group_by(aID_SP) %>%                              # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
  filter(!is.na(aID_SP)) %>% 
  dplyr::summarise(n_plots = n())
nrow(MOL)

# Anteil
round(nrow(MOL)/2712,2)

# UZL vs. uebrige vs. nicht verwendet
UZL <- MOL %>%
  left_join(tbl(db, "Arten") %>% as_tibble(),
            by = "aID_SP") %>% 
  dplyr::select(aID_SP, n_plots, Gattung, Art, ArtD, UZL) %>% 
  print()

a <- UZL %>% dplyr::filter(UZL == 1) %>% nrow() %>% print()
b <- UZL %>% dplyr::filter(UZL == 0) %>% nrow() %>% print()
a+b


#  Entwicklung Z7-Tagfalter ----
# Datentabelle erstellen
dat <- tbl(db, "KD_Z7") %>%    # Kopfdaten
  filter(!is.na(yearBu)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z7")) %>%  # Raumdaten (z.B. Hoehe)
  filter(Verdichtung_BDM == "nein") %>%   # verdichtete Regionen Jura und Tessin bereinigen
  left_join(tbl(db, "STICHPROBE_Z7")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Hoehe, AntWald, AntLW, Aufnahmejahr = yearBu) %>% # spalten waehlen die ich brauche als flaecheninformationen
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


# Raumdaten miteinbeziehen
trend <- function(y,x){
  coef(lm(y ~ x, ))[2]
}

dat2 <- dat %>% # Artaufnahmen
      group_by(aID_STAO) %>%
      dplyr::summarise(
        Trend_UZL   = trend(AZ_UZL, Aufnahmejahr),
        Trend_UB    = trend(AZ_UB, Aufnahmejahr),
        Mean_AZ     = mean(AZ),
        Mean_AZ_UZL = mean(AZ_UZL),
        Mean_AZ_UB  = mean(AZ_UB),
        Landw       = mean(AntLW),
        Wald        = mean(AntWald),
        Hoehe       = mean(Hoehe),
      ) %>%
  dplyr::rename(UZL = Trend_UZL, UB = Trend_UB) %>% 
  reshape2::melt(measure.vars = c("UZL", "UB")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()

ggplot(dat2, aes(x = Landw, y = Trend, col = Artengruppe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Entwicklung vs. Landwirtschaft : Tagfalter (Z7)")

mod <- lm(Trend ~ Artengruppe*Landw + Hoehe + I(Hoehe^2) + Wald, data = dat2)
summary(mod)


# d <- dat2 %>% dplyr::filter(Artengruppe == "UZL")
# d1 <- dat2 %>% dplyr::filter(Landw >= 0.2)      # hier stürzt R immer ab in RStudio (nur in R klappts)
# d2 <- dat2 %>% dplyr::filter(Wald >= 0.2)
# d1 <- dat2[dat2$Landw >= 0.2,]















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
gridExtra::grid.arrange(TF1, TF2, ncol = 2, nrow = 1)



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
  dplyr::select(aID_KD, aID_STAO, Hoehe, AntWald, AntLW, Aufnahmejahr = yearBi) %>% # spalten waehlen die ich brauche als flaecheninformationen
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
  replace_na(list(AZ     = 0, AZ_UZL      = 0, AZ_UB      = 0,   # in denjenigen Flächen wo keine Aufnahmen
                  IZ     = 0, IZ_UZL      = 0, IZ_UB      = 0))  # waren gibts beim join ein NA
summary(dat)

# Raumdaten miteinbeziehen
trend <- function(y,x){
  coef(lm(y ~ x, ))[2]
}

dat2 <- dat %>% # Artaufnahmen
  group_by(aID_STAO) %>%
  dplyr::summarise(
    Trend_UZL   = trend(AZ_UZL, Aufnahmejahr),
    Trend_UB    = trend(AZ_UB, Aufnahmejahr),
    Mean_AZ     = mean(AZ),
    Mean_AZ_UZL = mean(AZ_UZL),
    Mean_AZ_UB  = mean(AZ_UB),
    Landw       = mean(AntLW),
    Wald        = mean(AntWald),
    Hoehe       = mean(Hoehe),
  ) %>%
  dplyr::rename(UZL = Trend_UZL, UB = Trend_UB) %>% 
  reshape2::melt(measure.vars = c("UZL", "UB")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()

ggplot(dat2, aes(x = Landw, y = Trend, col = Artengruppe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Entwicklung vs. Landwirtschaft : Vögel (Z7)")

mod <- lm(Trend ~ Artengruppe*Landw + Hoehe + I(Hoehe^2) + Wald, data = dat2)
summary(mod)


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
  dplyr::select(aID_KD, aID_STAO, Hoehe, AntWald, AntLW, Aufnahmejahr = yearPl) %>% # spalten waehlen die ich brauche als flaecheninformationen
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
  as_tibble()    %>%                   # HIER OHNE INDIVIDUEN
  replace_na(list(AZ     = 0, AZ_UZL      = 0, AZ_UB      = 0,   # in denjenigen Flächen wo keine Aufnahmen
                  IZ     = 0, IZ_UZL      = 0, IZ_UB      = 0))  # waren gibts beim join ein NA
summary(dat)

# Raumdaten miteinbeziehen
trend <- function(y,x){
  coef(lm(y ~ x, ))[2]
}

dat2 <- dat %>% # Artaufnahmen
  group_by(aID_STAO) %>%
  dplyr::summarise(
    Trend_UZL   = trend(AZ_UZL, Aufnahmejahr),
    Trend_UB    = trend(AZ_UB, Aufnahmejahr),
    Mean_AZ     = mean(AZ),
    Mean_AZ_UZL = mean(AZ_UZL),
    Mean_AZ_UB  = mean(AZ_UB),
    Landw       = mean(AntLW),
    Wald        = mean(AntWald),
    Hoehe       = mean(Hoehe),
  ) %>%
  dplyr::rename(UZL = Trend_UZL, UB = Trend_UB) %>% 
  reshape2::melt(measure.vars = c("UZL", "UB")) %>% 
  dplyr::rename(Artengruppe = variable, Trend = value) %>% 
  as_tibble()

ggplot(dat2, aes(x = Landw, y = Trend, col = Artengruppe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Entwicklung vs. Landwirtschaft : Pflanzen (Z7)")

mod <- lm(Trend ~ Artengruppe*Landw + Hoehe + I(Hoehe^2) + Wald, data = dat2)
summary(mod)


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
  as_tibble()    %>%                   # HIER OHNE INDIVIDUEN
  replace_na(list(AZ     = 0, AZ_UZL      = 0, AZ_UB      = 0,   # in denjenigen Flächen wo keine Aufnahmen
                  IZ     = 0, IZ_UZL      = 0, IZ_UB      = 0))  # waren gibts beim join ein NA
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
    tbl(db, "Moos") %>%  # Moosaufnahmen
      filter(!is.na(aID_SP)) %>% # unbestimmte Arten rausfiltern
      left_join(tbl(db, "Arten")) %>% # Artaufnahmen
      group_by(aID_KD) %>%            # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
      dplyr::summarise(
        AZ = n(),                     # Summe der Gesamt-Artenzahl (AZ)
        AZ_UZL = sum(UZL == 1),       # Summe der UZL-Arten
        AZ_UB = sum(UZL == 0),        # Summe der uebrigen (nicht-UZL) Arten
      )) %>%
  as_tibble()    %>%                   # HIER OHNE INDIVIDUEN
  replace_na(list(AZ     = 0, AZ_UZL      = 0, AZ_UB      = 0,   # in denjenigen Flächen wo keine Aufnahmen
                  IZ     = 0, IZ_UZL      = 0, IZ_UB      = 0))  # waren gibts beim join ein NA
summary(dat)

# Plot Artenzahl
Mo.1 <- dat %>% 
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
Mo.2 <- dat %>%
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
Mo.3 <- dat %>%
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
Mo.4 <- d %>%
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
gridExtra::grid.arrange(Mo.1, Mo.2, ncol = 2, nrow = 1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z9-Mollusken ----
# Datentabelle erstellen
dat <- tbl(db, "KD_Z9") %>%      # Kopfdaten
  filter(!is.na(yearMol)) %>%   # nur Aufnahmejahre miteinbeziehen
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9" | Aufnahmetyp == "Normalaufnahme_Z9") %>%  # Aufnahmetyp
  left_join(tbl(db, "Raumdaten_Z9")) %>%  # Raumdaten (z.B. Hoehe)
  left_join(tbl(db, "STICHPROBE_Z9")) %>% # schwer zugaengliche flaechen gehoeren nicht mehr zur stichprobe
  filter(BDM_aktuell == "ja") %>%         # dito
  dplyr::select(aID_KD, aID_STAO, Hoehe, Aufnahmejahr = yearMol) %>% # spalten waehlen die ich brauche als flaecheninformationen
  left_join(
    tbl(db, "MOL") %>%  # Molluskenaufnahmen
      filter(!is.na(aID_SP)) %>% # unbestimmte Arten rausfiltern
      left_join(tbl(db, "Arten")) %>% # Artaufnahmen
      group_by(aID_KD) %>%            # gruppiert nach Stichprobenflaechen die folgenden rechnungen durchfuehren
      dplyr::summarise(
        AZ = n(),                     # Summe der Gesamt-Artenzahl (AZ)
        AZ_UZL = sum(UZL == 1),       # Summe der UZL-Arten
        AZ_UB = sum(UZL == 0),        # Summe der uebrigen (nicht-UZL) Arten
      )) %>%
  as_tibble()    %>%                   # HIER OHNE INDIVIDUEN
  replace_na(list(AZ     = 0, AZ_UZL      = 0, AZ_UB      = 0,   # in denjenigen Flächen wo keine Aufnahmen
                  IZ     = 0, IZ_UZL      = 0, IZ_UB      = 0))  # waren gibts beim join ein NA
summary(dat)

# Plot Artenzahl
Mol.1 <- dat %>% 
  group_by(Aufnahmejahr) %>%  # gruppiert nach Aufnahmejahr die Mittelwerte aller Flaechen berechnen
  dplyr::summarise(
    UZL = mean(AZ_UZL),
    übrige = mean(AZ_UB)) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% # umwandeln in long-format
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL: Mollusken (Z9)") +
  ylim(0, NA) +
  labs(x = "Aufnahmejahr",
       y = "Anzahl Arten") +
  theme(legend.position = c(0.85, 0.15))

# Plot Artenzahl relativ --> Interaction??
Mol.2 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>%
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>%
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("ARTENZAHL relativ: Mollusken (Z9)") +
  ylim(0, 3) +
  labs(x = "Aufnahmejahr",
       y = "Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))

# Plot UZl vs. uebrige relativ --> einfluss des beobachters?? oder jahresklima??
Mol.3 <- dat %>%
  group_by(Aufnahmejahr) %>%
  dplyr::summarise(
    UZL = mean(AZ_UZL) / mean(dat$AZ_UZL),
    übrige = mean(AZ_UB)/ mean(dat$AZ_UB)) %>%
  ggplot(aes(x = übrige, y = UZL)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Vergleich UZL vs. übrige Molluskenarten (Z9)") +
  xlim(0.75, 1.2) +
  ylim(0.75, 1.2) +
  labs(x = "übrige Arten relativ zum Durchschnitt 2003 - 2019",
       y = "UZL Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))

# alle flaechen plot UZl vs. uebrige relativ --> einfluss des beobachters?? oder jahresklima??
Mol.4 <- d %>%
  ggplot(aes(x = AZrel_UB, y = AZrel_UZL)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Vergleich UZL vs. übrige Molluskenarten (Z9)") +
  xlim(0, 3) +
  ylim(0, 15) +
  labs(x = "übrige Arten relativ zum Durchschnitt 2003 - 2019",
       y = "UZL Arten relativ zum Durchschnitt 2003 - 2019") +
  theme(legend.position = c(0.85, 0.15))


# plot on same page
gridExtra::grid.arrange(Mol.1, Mol.2, ncol = 2, nrow = 1)








#  Übersichtsplot----
gridExtra::grid.arrange(TF1, BI1, Pl1, Pl.1, Mo.1, Mol.1, ncol = 2, nrow = 3)
gridExtra::grid.arrange(TF2, BI2, Pl2, Pl.2, Mo.2, Mol.2, ncol = 2, nrow = 3)



#  Trend Artniveau Z7-Tagfalter ---- 
flaechen <- 375  # Anzahl gültiger Aufnahmeflächen (Übertrag von deskriptiv)
dat0 <- tbl(db, "KD_Z7") %>%
  filter(!is.na(yearBu)) %>%  
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>%  
  left_join(tbl(db, "Raumdaten_Z7")) %>%  
  filter(Verdichtung_BDM == "nein") %>%   
  left_join(tbl(db, "STICHPROBE_Z7")) %>% 
  filter(BDM_aktuell == "ja") %>%         
  dplyr::select(aID_KD, aID_STAO, Aufnahmejahr = yearBu) %>% 
  left_join(
    tbl(db, "TF") %>%  
      mutate_at( vars( c("Ind")), funs(if_else(is.na(Ind), 54, Ind))) %>% # Datenbank an einer Stelle falsch -> manuell Wert ersetzen
      filter(!is.na(aID_SP)) %>%                                          
      left_join(tbl(db, "Arten"))) %>% 
  group_by(aID_SP, Aufnahmejahr) %>%                            
  dplyr::summarise(plots = n(), plots_percent = n()/flaechen, Gattung, Art, ArtD, UZL) %>% 
  filter(!is.na(aID_SP)) %>% 
  as_tibble() %>% 
  print()


# die Aufnahmejahre die fehlen müssen bei plots eine Null haben !!
aID_SP <- as.numeric(levels(as.factor(dat$aID_SP)))
Aufnahmejahr <- rep(2003:2019, length(aID_SP))
Aufnahmen <- as_tibble(data.frame(aID_SP, Aufnahmejahr)) %>% arrange(aID_SP) %>% print()
dat <- Aufnahmen %>%
  left_join(
    dat0 %>% dplyr::select(aID_SP, Aufnahmejahr, plots, plots_percent),
    by = c("aID_SP", "Aufnahmejahr"))
dat$plots <- ifelse(is.na(dat$plots), 0, dat$plots)
dat$plots_percent <- ifelse(is.na(dat$plots_percent), 0, dat$plots_percent)
dat <- dat %>% left_join(
  as_tibble(tbl(db, "Arten")),
  by = "aID_SP") %>% 
  dplyr::select(aID_SP, Aufnahmejahr, plots, plots_percent, Gattung, Art, ArtD, UZL)



# Trend function
trend <- function(y,x){
  coef(lm(y ~ x, ))[2]
}
trend.p <- function(y,x,d){
  ifelse(length(x)>2,summary(lm(y ~ x, ))$coefficients[2,4],NA) # bei nur einem oder zwei Jahren mit Funden gibts keinen p-Wert
}

d <- dat %>% group_by(aID_SP) %>%
  dplyr::summarise(plots_mean = mean(plots), plots_mean_percent = mean(plots)/flaechen) %>% 
  print()    # plots_mean: Durchschnittliche Anzahl plots in der eine Art von 2003-2019 vorkommt

dat2 <- dat %>% 
  left_join(d) %>% 
  group_by(aID_SP) %>%
  dplyr::summarise(
    Trend       = trend(plots, Aufnahmejahr),                    # absoluter Trend (neue Flaechen pro Jahr, in denen Art neu Auftritt)
    Trend.p     = trend.p(plots, Aufnahmejahr),                  # p-value
    Trend_prop  = trend((plots/plots_mean), Aufnahmejahr) ,      # relativer Trend (jährlich neue Flaechen relativ zum Durchschnitt 2003-2019 in denen die Art neu auftritt)
    Trend_prop.p= trend.p((plots/plots_mean), Aufnahmejahr) ,    # p-value
    plots_mean  = mean(plots_mean),
    plots_mean_percent = mean(plots_mean_percent),
    UZL         = mean(UZL)) %>% 
  left_join(
    tbl(db, "Arten") %>% 
      as_tibble() %>% 
      dplyr::select(aID_SP, Gattung, Art, ArtD),
    by = "aID_SP") %>% 
  rename(Artengruppe = UZL) %>% 
  print()
dat2$Artengruppe <- as.factor(dat2$Artengruppe)
levels(dat2$Artengruppe) <- c("UB", "UZL")

ggplot(dat2, aes(x = plots_mean_percent, y = Trend_prop, col = Artengruppe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Entwicklung vs. Häufigkeit : Tagfalter (Z7) - jeder Punkt eine Art") +
  xlab("Durchschnittliche Anteil Flaechen in denen Art vorkommt 2003-2019") +
  ylab("relativer Trend (jährlich neue Flaechen relativ zum Durchschnitt 2003-2019 in denen die Art neu auftritt)")

mod <- lm(Trend_prop ~ Artengruppe*plots_mean, data = dat2)
summary(mod)




arrange(dat2, desc(Trend_prop))

Zunahme <- dat2 %>%            # mit prop-Daten gibts dasselbe -> muss so sein!
  filter(Trend.p <= 0.05) %>% 
  filter(Trend > 0)

Abnahme <- dat2 %>%
  filter(Trend.p < 0.05) %>% 
  filter(Trend < 0)

stabil <- dat2 %>%
  filter(Trend.p > 0.05)


nrow(Zunahme)
table(Zunahme$Artengruppe)
nrow(Abnahme)
table(Abnahme$Artengruppe)
nrow(stabil)
table(stabil$Artengruppe)

nrow(Zunahme) + nrow(Abnahme) + nrow(stabil) 

boxplot(Trend ~ Artengruppe, data = dat2)



# d <- data.frame(x=c(1,2,3), y = c(3.5, 2,2))
# l <- lm(y~x,d)
# plot(d)
# abline(l)
# summary(l)

# END OF SCRIPT ----

