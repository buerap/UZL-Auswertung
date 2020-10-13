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
install_github("TobiasRoth/BDM")
library(BDM)

# Connection to data base
db <- src_sqlite(path = "DB/DB_BDM_2020_08_20.db", create = FALSE)

# Plot Einstellungen
theme_set(
  theme_clean() +
    theme(
      legend.title = element_blank(), 
      legend.position = "top", 
      legend.background = element_rect(colour = "white"))
)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Entwicklung Z7-Tagfalter ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dat <- tbl(db, "KD_Z7") %>% 
  filter(!is.na(yearBu)) %>%
  filter(Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z7" | Aufnahmetyp == "Normalaufnahme_Z7") %>% 
  left_join(tbl(db, "Raumdaten_Z7")) %>% 
  filter(Verdichtung_BDM == "nein") %>% 
  left_join(tbl(db, "STICHPROBE_Z7")) %>% 
  filter(BDM_aktuell == "ja") %>% 
  dplyr::select(aID_KD, aID_STAO, Hoehe, Aufnahmejahr = yearBu) %>% 
  left_join(
    tbl(db, "TF") %>% 
      filter(!is.na(aID_SP)) %>% 
      left_join(tbl(db, "Arten")) %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(
        AZ = n(),
        UZL_AZ = sum(UZL == 1),
        UB_AZ = sum(UZL == 0),
        UZL_Ind = sum(Ind[UZL == 1]),
        UB_Ind = sum(Ind[UZL == 0])
      )
  ) %>% 
  as_tibble() %>% 
  replace_na(list(UZL_Ind = 0)) # NA in Spalte UZL_Ind mit Null ersetzen (Null-Individuen auch wichtig)
  
# Plot machen
dat %>% 
  group_by(Aufnahmejahr) %>% 
  dplyr::summarise(
    UZL_AZ = mean(UZL_AZ),
    UZL_UB = mean(UB_AZ)) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% 
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ylim(0, NA) +
  labs(
    x = "",
    y = "Anzahl Arten"
  )

# Plot machen
dat %>% 
  group_by(Aufnahmejahr) %>% 
  dplyr::summarise(
    UZL_Ind = mean(UZL_Ind) / mean(dat$UZL_Ind),
    UB_Ind = mean(UB_Ind)/ mean(dat$UB_Ind)) %>% 
  gather("Artengruppe", "AZ", -Aufnahmejahr) %>% 
  ggplot(aes(x = Aufnahmejahr, y = AZ, col = Artengruppe)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  ylim(0, NA) +
  labs(
    x = "",
    y = "Anzahl Individuen"
  )

library(arm)
d <- dat %>% 
  dplyr::select(aID_STAO, Jahr = Aufnahmejahr, UZL_Ind, UB_Ind) %>% 
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
  






