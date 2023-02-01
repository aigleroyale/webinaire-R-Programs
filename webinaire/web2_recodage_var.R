#Chargement du jeu de données ----

library(questionr)
library(labelled)
library(tidyverse)
library(Hmisc)

data("hdv2003")
help("hdv2003")

view(hdv2003)
look_for(hdv2003)
look_for(hdv2003, "col")
glimpse(hdv2003)


hdv2003 <- hdv2003 %>% rename(bricolage = bricol)

hdv2003$fs <- factor(hdv2003$freres.soeurs)
hdv2003 <- hdv2003 %>% mutate(fs=factor(freres.soeurs)

describe(hdv2003$freres.soeurs)

data(fecondite)
look_for(femmes)
glimpse(femmes)

describe(femmes$region)
to_factor(femmes$region) %>% describe()

femmes <- femmes %>% remove_value_labels(region = 4)
describe(to_factor(femmes$region))

describe(to_factor(femmes$region, 
          levels = "prefixed", nolabel_to_na = TRUE))

femmes <- femmes %>% add_value_labels(region = c(Centre = 5))
to_factor(femmes$region, levels = "prefixed") %>% describe()

to_factor(femmes$region, levels = "prefixed", 
          drop_unused_labels = TRUE) %>% describe()

ff <- femmes %>% to_factor()
look_for(ff)
remove_value_labels()

describe(femmes$age)

describe(unclass(femmes$age))


ff <- femmes %>% unlabelled()
look_for(ff)

# Convertir une variable numerique en classe
look_for(hdv2003)


hdv2003$groupe_age <- cut(hdv2003$age,
                          include.lowest = TRUE,
                          right = FALSE,
                          dig.lab = 4,
                          breaks = c(18, 25, 40, 60, 75, 97))
describe(hdv2003$groupe_age)

# Recoder un facteur## Recodage de hdv2003$nivetud en hdv2003$niveau_etude
hdv2003$niveau_etude <- hdv2003$nivetud %>%
  fct_recode(
    "Primaire" = "N'a jamais fait d'etudes",
    "Primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
    "Primaire" = "Derniere annee d'etudes primaires",
    "Secondaire" = "1er cycle",
    "Secondaire" = "2eme cycle",
    "Technique" = "Enseignement technique ou professionnel court",
    "Technique" = "Enseignement technique ou professionnel long",
    "Superieur" = "Enseignement superieur y compris technique superieur"
  ) %>%
  fct_explicit_na("Manquant")


describe(hdv2003$niveau_etude)
hdv2003$niveau_etude <- hdv2003$niveau_etude %>%
       fct_relevel("Manquant", "Technique")

hdv2003 <- hdv2003 %>%
       mutate(
              groupe = if_else(age<40, "jeune", "moins jeune"),
              score = if_else(sexe =="Homme", (age^2)-33, (age^3)-225),
              groupe2 = case_when(
               sexe == "Homme" & age < 40 ~ "Hommes jeunes",
               sexe == "Homme" & age > 70 ~ "Hommes agés",
               sexe == "Femme" ~ "Femme",
               TRUE ~ "Aure"
              )
       )

hdv2003$groupe %>% describe()
hdv2003$score %>% describe()













