#ANALYSE DE SURVIE

# Chargement des donnéees ------


library(tidyverse)
library(labelled)
library(questionr)
#calcul sur les dates
library(lubridate)


data(fecondite)
enfants %>%  look_for()
enfants %>% freq.na()

# Recodages -------

## calcul de la durée d'observation
enfants <- enfants %>% 
       left_join(
              femmes %>% 
                select(id_femme, date_entretien),
              by = "id_femme"
       )

enfants <- enfants %>% 
       mutate(
          duree_observation = interval(date_naissance, date_entretien) %>% 
            time_length(unit = "month")
       )


enfants %>% 
       filter(duree_observation < 0) %>% 
       count()

enfants <- enfants %>%
   mutate(
      date_entretien = date_entretien %>% 
          recode_if(
                date_entretien < date_naissance,
                date_entretien %m+% months(1)
             ),
      duree_observation = interval(date_naissance, date_entretien) %>% 
        time_length(unit = "month"),
      duree_observation_mois_revolus = trunc(duree_observation)
  )

# Age au décès imputation

enfants <- enfants %>% 
       mutate(
         age_deces_impute = age_deces + runif(n())
       )

# Variable de survie
library(survival)

enfants <- enfants %>% 
       mutate(
              deces = if_else(survie==0,1,0),
              time = if_else(survie==0, age_deces_impute, duree_observation),
              time_revolu = if_else(survie==0, age_deces, duree_observation_mois_revolus)
       ) %>% 
       set_variable_labels(deces="Est décédé") %>% 
       set_value_labels(deces=c("en vie" =0, "décédé"=1))

##Variables explicatives

enfants <- enfants %>% 
       left_join(
         femmes %>% select(id_femme, id_menage, milieu, educ, 
                           date_naissace_mere=date_naissance,
                           nb_enf_ideal),
         by ="id_femme"
       ) %>% 
       left_join(
              menages %>% select(id_menage, structure, richesse),
              by ="id_menage"
       )

enfants <- enfants %>% 
       mutate(
              sexe = to_factor(sexe),
              richesse = to_factor(richesse),
              structure = to_factor(structure) %>% 
                     fct_drop() %>% 
                     fct_relevel("deux adultes de sexe opposé"),
              educ2 = to_factor(educ) %>% 
                     fct_recode(
                            "second/sup" ="secondaire ",
                            "second/sup" = "supérieur"
              ),
              age_mere_naissance = interval(date_naissance_mere, date_naissance) %>%
                     time_length(unit = "years")
       )



freq(enfants$structure)
freq(enfants$educ)


view(enfants)































