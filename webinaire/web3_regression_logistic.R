# Analyse de la pratique du sport
# Partie 1 ----------

## Chargement des packages

library(tidyverse)
library(questionr)
library(labelled)
library(gtsummary)
library(GGally)
library(effects)
library(ggeffects)
library(cowplot)

# je ne veux des tableaux en Français

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = "")

theme_gtsummary_mean_sd()


#chargement des données
data("hdv2003")

# voir les données
look_for(hdv2003)
view(hdv2003)

# Etiqueter les variables -----
hdv2003 <- hdv2003 %>%
       set_variable_labels(
        sport = "Pratique du sport",
        age = "Age",
        sexe = "Sexe",
        nivetud = "Niveau d'études",
        relig = "Rapport à la religion",
        heures.tv = "Heures quotidiennes de TV"
       )

# Analyse univarié
hdv2003 %>% 
       select(sport, age, sexe, nivetud, relig, heures.tv) %>% 
       tbl_summary(
              digits = all_categorical() ~ c(2, 0),
              statistic = all_categorical() ~ "{p}% [{n}]"
       )

# Recodages -------
## Recodage de hdv2003$age en hdv2003$groupe_age
hdv2003$groupe_age <- hdv2003$age %>% 
       cut(
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 25, 45, 60, 97)
) %>%
       fct_recode(
              "18-25" = "[18,25)",
              "25-45" = "[25,45)",
              "45-60" = "[45,60)",
              "60 et +" = "[60,97]"
       )


## Recodage de hdv2003$groupe_age
hdv2003$groupe_age <- hdv2003$groupe_age %>%
  fct_recode(
    "18-25" = "[18,25)",
    "25-45" = "[25,45)",
    "45-60" = "[45,60)",
    "60 et +" = "[60,97]"
  )
var_label(hdv2003$groupe_age) <- "Groupe d'âges"


hdv2003 %>% 
       select(sport, groupe_age, sexe, nivetud, relig, heures.tv) %>% 
       tbl_summary(
              digits = all_categorical() ~ c(2, 0),
              statistic = all_categorical() ~ "{p}% [{n}]"
       )

# Changer l'ordre des modalités

## Réordonnancement de hdv2003$sexe
hdv2003$sexe <- hdv2003$sexe %>%
  fct_relevel(
    "Femme", "Homme"
  )

## Recodage de hdv2003$nivetud en hdv2003$etudes
hdv2003$etudes <- hdv2003$nivetud %>%
  fct_recode(
    "Primaire" = "N'a jamais fait d'etudes",
    "Primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
    "Primaire" = "Derniere annee d'etudes primaires",
    "Secondaire" = "1er cycle",
    "Secondaire" = "2eme cycle",
    "Technique/Professionnel" = "Enseignement technique ou professionnel court",
    "Technique/Professionnel" = "Enseignement technique ou professionnel long",
    "Supérieur" = "Enseignement superieur y compris technique superieur"
  ) %>%
  fct_explicit_na("Manquant")
var_label(hdv2003$etudes) <- "Niveau d'études"



hdv2003 %>% 
       select(sport, groupe_age, sexe, etudes, relig, heures.tv) %>% 
       tbl_summary(
              digits = all_categorical() ~ c(2, 0),
              statistic = all_categorical() ~ "{p}% [{n}]"
       )

# Analyse descriptive bivariée -----


hdv2003 %>% 
       select(sport, groupe_age, sexe, etudes, relig, heures.tv) %>% 
       tbl_summary(
              digits = all_categorical() ~ c(2, 0),
              statistic = all_categorical() ~ "{p}% [{n}]",
              by = "sport"
       )


hdv2003 %>% 
       select(sport, groupe_age, sexe, etudes, relig, heures.tv) %>% 
       tbl_summary(
              digits = all_categorical() ~ c(2, 0),
              statistic = all_categorical() ~ "{p}% [{n}]",
              by = "sport",
              percent = "row"
       ) %>% 
       add_overall(last = TRUE) %>% 
       add_p()


# Visualisation bivariée
hdv2003 %>% 
  ggbivariate(
     outcome = "sport",
     explanatory = c("groupe_age", "sexe", "etudes", "relig", "heures.tv")
       )


hdv2003 %>% 
       ggbivariate("etudes", "groupe_age", )

# Calcul de la regression logistique ------
mod <- glm(
       formula = sport ~ groupe_age + sexe+ etudes + relig+ heures.tv,
       family = binomial(link = "logit"),
       data = hdv2003
)
mod
summary(mod)

mod %>% tbl_regression(intercept = TRUE)


b <- binomial()
str(b)

ggplot()+stat_function(fun = b$linkfun)
ggplot()+stat_function(fun = b$linkfun)+ ylim(-10,10)
b$linkinv(-0.38)
b$linkinv(-0.38-1.3)

# Les effets marginaux ------
mod %>% allEffects() %>% plot()


plot(ggeffect(mod, "sexe"))
ggeffect(mod)

ggeffect(mod) %>% 
       plot() %>% 
       cowplot::plot_grid(plotlist = .)

mod %>% tbl_regression(
       intercept = TRUE,
       exponentiate = TRUE,
       add_estimate_to_reference_row = TRUE
       
)

ggcoef_model(mod, exponentiate = TRUE)










