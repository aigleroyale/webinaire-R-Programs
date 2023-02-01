#Library -----

library(tidyverse)
library(gtsummary)
library(labelled)
library(questionr)

# voir nos variables
trial %>% look_for()

# tri à plat
trial %>% tbl_summary()

# voir les valeurs manquantes
freq.na(trial)

# regarder les thèmes disponibles
?theme_gtsummary_journal

#Séparateur de decimal
theme_gtsummary_journal("lancet")

#Interface en français
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

trial %>% 
       tbl_summary(
          include = c("grade", "age", "trt")
       )

trial %>% 
       tbl_summary(
              include = c(age:stage, starts_with("t"))
       )

#Tableaux croisés

trial %>% 
       tbl_summary(
              include = c(age, stage, response),
              by = trt
       )


trial %>% 
       tbl_summary(
              include = c(age, stage, response),
              by = trt
       ) %>% 
       add_overall(
              last = TRUE,
              col_label = "**Ensemble**, effectif total de {N}"
       )

# Personnalisé les statistiques 

trial %>% 
       tbl_summary(
              include = c(age, stage, response),
              by = trt,
              statistic = list(
                     age ~ "moy : {mean} [{sd}]",
                     all_continuous() ~ "moy : {mean} [{sd}]"
              )
       ) %>% 
       add_overall(
              last = TRUE,
              col_label = "**Ensemble**, effectif total de {N}"
       )

trial %>% 
       tbl_summary(
              include = c(age, stage, response),
              by = trt,
              statistic = list(
                     all_continuous() ~ "moy : {mean} [{sd}]"
              )
       ) %>% 
       add_overall(
              last = TRUE,
              col_label = "**Ensemble**, effectif total de {N}"
       )

trial %>% 
       tbl_summary(
              include = c(age, stage, response),
              by = trt,
              statistic = list(
                     all_continuous() ~ "moy : {mean} [{sd}]"
              )
       ) %>% 
       add_overall(
              last = TRUE,
       )

trial %>% 
       tbl_summary(
              include = c(age, stage, response),
              by = trt,
              statistic = list(
                     all_continuous() ~ "moy : {mean} [{sd}]",
                     all_categorical() ~"{p}% ({n}/{N})"
              ),
              sort = all_categorical() ~ "frequency",
              percent = "cell"
       ) %>% 
       add_overall(
              last = TRUE,
       )

trial %>% 
       tbl_summary(
              include = c(age, marker, grade),
              by = stage,
              statistic = list(
                     age ~ "{median} [{p25} - {p75}]",
                     marker ~ "{mean} ({sd})"
              ),
       ) %>% 
       add_stat_label(location = "column")

trial %>%
       tbl_summary(
              include = c(age, grade, death),
              type = list(
                     age ~"categorical",
                     death ~ "continuous",
                     grade ~ "dichotomous"
              ),
              value = grade ~ "III",
              label = grade ~ "Grade III"
       )


trial %>%
       tbl_summary(
        include = c(age, marker),
        type = c(age, marker) ~ "continuous2",
        statistic = all_continuous2() ~ c("{median} ({p25} ~ {p75})",
                                          "{mean} ({sd})",
                                          "{min} - {max}")
       )

trial %>%
       tbl_summary(
              include = c(age, stage),
              by = trt,
              digits = everything() ~ 1
       )


trial %>%
       tbl_summary(
              include = c(age, stage),
              by = trt,
              digits = list(
               all_continuous() ~ c(2, 1, 1),
               all_categorical() ~ c(0,1)
              )
       )



library(scales)
#renvoie les nombres mise en forme
number(c(0.1, 2.54, 10.1), accuracy = 0.01)

number(c(0.1, 2.54, 10.1), accuracy = 0.25)

#renvoie une fonction qui met en forme
f2 <- label_number(accuracy = 0.01, decimal.mark = "-")
f2(12.589)
pour_mille <- label_number(accuracy = 0.1, scale = 1000, suffix = "\u2030")
pour_mille(1.256)


trial %>%
        tbl_summary(
                include = c(age, stage),
                statistic = all_categorical() ~ "{n} ({p})",
                by = trt,
                digits = list(
                        all_continuous() ~ f2,
                        all_categorical() ~ c(0,pour_mille)
                )
        )

# enlever les données manquants

trial %>% 
        tbl_summary(
                include = c(age, response, grade),
                missing = "no"
        )

# Etiquette des variables avec labelled

iris %>% 
        look_for()
iris %>% 
        tbl_summary()

iris <- iris %>% 
    set_variable_labels(
        Petal.Length = "Longueur du pétale",
        Petal.Width = "Largeur du pétale"
        ) %>% 
    tbl_summary(
        label = list(
          Species ~"Espèces",
          Petal.Length~"MON ETIQUETTE"
        )
    )

iris %>% View()
iris3

trial %>% 
        tbl_summary(
                include = c(age, marker),
                by = trt,
                missing = "no") %>% 
        add_n(
                statistic= "{n}/{N}",
                col_label = "**Effectifs** (obs./total)",
                last = TRUE,
                footnote = TRUE
        )

trial %>% 
        tbl_summary(
                include = c(age, grade, stage),
                by = trt) %>% 
        bold_labels() %>% 
        italicize_labels()

tbl <-trial %>% 
        tbl_summary(
                include = c(age, grade),
                by = trt
        ) %>% 
        add_overall()

show_header_names(tbl)

tbl %>%
    modify_header(update = list(
        label ~ "**Variables**",
        all_stat_cols(stat_0 = FALSE) ~ "*{level}* (n={n}, {style_percent(p)}%)"
    )) %>% 
    modify_footnote(everything() ~ NA) %>% 
    modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~ "**Traitement**")

# Test de comparaison 

trial %>% 
        tbl_summary(
                include = c(trt, marker, age, response, stage),
                statistic = all_continuous() ~ "{mean} ({sd})",
                by = trt
        ) %>% 
        add_p(
                test = all_continuous()~"t.test",
                pvalue_fun = function(x) style_pvalue(x, digits = 2)
        ) %>% 
        separate_p_footnotes()


trial %>% 
        tbl_summary(
                include = c(trt, marker, age, response, stage),
                statistic = all_continuous() ~ "{mean} ({sd})",
                by = trt
        ) %>% 
        add_p(
                test = all_continuous()~"t.test",
                pvalue_fun = scales::label_pvalue(accuracy = 0.01, decimal.mark = ",")
        ) %>% 
        separate_p_footnotes()

# Intervall de confiance

trial %>% 
        tbl_summary(
                include = c(trt, marker, age, response, stage),
                statistic = list(
                        all_continuous() ~ "{mean}",
                        all_categorical() ~ "{p}%"),
                by = trt
        ) %>% 
        add_ci()


trial %>% 
        tbl_summary(
                include = c(marker, age, response),
                by = trt,
                statistic = list(
                        age ~ "{median}",
                        marker ~"{mean}",
                        all_categorical() ~"{p}%"
                )
        ) %>%
        add_stat_label(location = "column") %>% 
        add_ci(
                method = list(
                        age ~ "wilcox.test",
                        marker ~ "t.test"
                )
        )


trial %>% 
        tbl_summary(
                include = c(marker, age, response),
                by = trt,
                statistic = list(
                        age ~ "{median}",
                        marker ~"{mean}",
                        all_categorical() ~"{p}%"
                ),
                missing = "no"
        ) %>%
        add_p(
                test = list(
                        age ~ "kruskal.test",
                        marker ~"t.test"
                )
        )

# Différence entre tbl_summary et tbl_cross



trial %>% 
        tbl_cross(
                row = grade,
                col = trt,
                percent = "row"
        )

trial %>% 
        tbl_cross(
                row = grade,
                col = trt,
                percent = "row"
        ) %>% 
        ?add_p(source_note = TRUE)



trial %>% 
        tbl_summary(
                include = grade,
                by = trt,
                percent = "row",
                statistic = grade ~ "{mean}"
        )

#différence

trial %>% 
        tbl_summary(
                include = c(marker, age, response),
                by = trt,
                statistic = list(
                        all_continuous() ~ "{mean}",
                        all_categorical()~ "{p}%"
                ),
                digits = list(
                        all_categorical() ~ 1,
                        all_continuous() ~ 2
                ),
                missing = "no"
        ) %>% 
        add_difference()




































