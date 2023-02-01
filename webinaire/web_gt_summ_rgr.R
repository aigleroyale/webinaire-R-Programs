rm(list = ls())

library(gtsummary)
library(tidyverse)
library(labelled)


theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

mod <- glm(
       response ~ grade * age + trt,
       data = trial,
       family = binomial
)

mod %>% 
       tbl_regression(
          exponentiate = TRUE,
          include = c(age, grade, grade:age)
       )


mod %>% 
       tbl_regression(
              exponentiate = TRUE,
              include = c(age, grade, all_interaction()),
       )

mod %>% 
       tbl_regression(
              exponentiate = TRUE,
              label = age ~ "Age en années"
       )

mod %>% 
       tbl_regression(
              exponentiate = TRUE,
              label = 
                 grade:age ~ "Interaction entre grade et age"
       )


tbl <- mod %>% 
       tbl_regression(
              exponentiate = TRUE,
              label = age ~ "Age en années",
              intercept = TRUE
       )

show_header_names(tbl)

tbl %>% 
     modify_header(
        estimate ~"**Odds Ratio**"
     ) %>% 
        modify_footnote(estimate ~ NA, abbreviation = TRUE)

mod %>% 
     tbl_regression(
         exponentiate = TRUE,
         conf.level = 0.9
     )


mod %>% 
       tbl_regression(
              exponentiate = TRUE
       ) %>% 
       add_significance_stars(
          hide_ci = FALSE,
          hide_p = FALSE,
          hide_se = TRUE
       )


mod %>% 
       tbl_regression(
          show_single_row = trt
       )


mod %>% 
       tbl_regression(
              show_single_row = all_dichotomous(),
              label = trt ~ "Traitement B vs Traitement A"
       )

mod %>% 
       tbl_regression(
              show_single_row = all_dichotomous(),
              label = trt ~ "Traitement B vs Traitement A"
       ) %>% 
       bold_labels()

mod %>% 
       tbl_regression(
              show_single_row = all_dichotomous(),
              label = trt ~ "Traitement B vs Traitement A"
       ) %>% 
       bold_labels() %>% 
       italicize_labels()


mod %>% 
       tbl_regression(
          estimate_fun = scales::label_number(accuracy = 0.001),
          pvalue_fun = scales::label_number(accuracy = 0.001, add_p = TRUE, decimal.mark =",")
       )



mod %>% 
       tbl_regression(
              estimate_fun = function(x) {style_sigfig(x, digits = 3)},
              pvalue_fun = scales::label_number(accuracy = 0.001, add_p = TRUE, decimal.mark =",")
       )


mod %>% 
       tbl_regression(
              estimate_fun = ~ {style_sigfig(.x, digits = 3)},
              pvalue_fun = ~ style_pvalue(.x, digits = 2)
       )

mod %>% 
       tbl_regression(
              estimate_fun = purrr::partial(style_sigfig, digits = 3),
              pvalue_fun =  purrr::partial(style_pvalue, digits = 2)
       )


mod %>% 
       tbl_regression(
          exponentiate = TRUE,
          add_estimate_totreference_rows = TRUE
          )


mod %>% 
       tbl_regression() %>% 
       add_global_p(keep=TRUE)



mod %>% 
       tbl_regression() %>% 
       add_vif()

mod %>% 
       tbl_regression(exponentiate = TRUE) %>% 
       plot()


mod %>% 
       GGally::ggcoef_model(exponentiate = TRUE)

mod %>% 
       broom::tidy()

mod %>% 
       broom.helpers::tidy_plus_plus()
mod %>% broom::glance()

mod %>% 
    tbl_regression() %>% 
    add_glance_table(include = c("AIC", "nobs"))



# Combinaison des tableaux

t1 <- glm(response ~ trt, data = trial, family = binomial) %>% 
       tbl_regression(exponentiate = TRUE)


t2 <-
       glm(response ~ grade + marker + age + trt, trial, family = binomial) %>% 
       tbl_regression(exponentiate = TRUE)


 

tbl_stack(
       list(t1, t2),
       group_header = c("Modele bivarié", "Modele multivarié")
) 


tbl_merge(
       list(t1, t2),
       tab_spanner = c("Modele bivarié", "Modele multivarié")
)

tmp <- trial %>% 
       select(age, grade, stage, trt)
levels(tmp$grade) <- c("Grade I", "Grade II", "Grade III")

tmp %>% 
       tbl_strata(
              strata = grade,
              .tbl_fun = 
                     ~.x %>% tbl_summary(by=trt, missing = "no") %>% 
                     add_n()
       )

# Analyse univarié

tbl_uni <- trial %>% 
       select(response, age, grade, stage) %>% 
       tbl_uvregression(
          method = glm,
          y = response,
          method.args = list(family = binomial),
          exponentiate = TRUE,
          hide_n = TRUE
       )

tbl_uni

tbl_desc <- trial %>% 
       tbl_summary(
         by = response,
         include = c(age, grade, stage)
       ) %>% 
       add_p()

tbl_multi <-
       glm(response ~ age + grade + stage, data = trial, family = binomial) %>% 
       tbl_regression(exponentiate=TRUE)

tbl_merge(
       list( tbl_desc, tbl_uni, tbl_multi),
       tab_spanner = c("**Analyse descriptive**", "Modèles univarié", "Model bivarié")
)

# tbl_continous()

trial %>% 
      tbl_continuous(
        by = trt,
        include = c(stage, grade),
        variable = age,
        statistic = ~ "{mean}",
        digits = ~ 2
      )


# tbl_custom_summary

trial %>% 
      tbl_custom_summary(
        by = trt,
        include = c(stage, grade),
        stat_fns = ~ continuous_summary("age"),
        statistic = ~ "{mean} ({sd})",
        digits = ~ 2,
        overall_row = TRUE
      ) %>% 
       add_overall()

trial %>% 
      tbl_custom_summary(
         by = trt,
         include = grade,
         stat_fns = ~ proportion_summary("stage", value = c("T3", "T4")),
         statistic = ~ "{prop} [{conf.low} - {conf.high}]",
         digits = ~ 2
      )


trial %>% 
       tbl_custom_summary(
              by = trt,
              include = grade,
              stat_fns = ~ proportion_summary("stage", value = c("T3", "T4")),
              statistic = ~ "{prop}% [{conf.low} - {conf.high}]",
              digits = ~ scales::label_percent(accuracy = .1, suffix = "")
       ) %>% 
       modify_footnote(all_stat_cols() ~ "Prop de T3/T4 [IC 95%]")


#ratio_summary

trial %>% 
      tbl_custom_summary(
        by = trt,
        include = stage,
        stat_fns = ~ ratio_summary("response", "ttdeath"),
        statistic = ~ "{ratio} ({num}/{denom})",
        digits = ~ c(3, 0, 0)
      )


trial %>% 
       tbl_summary(
              by = trt,
              include = c(age, marker),
              statistic = ~ "{mean}"
       ) %>% 
       add_difference()

##tb_surv

library(survival)
install.packages("survminer")
library(survminer)
km <- survfit(Surv(ttdeath, death) ~ trt, trial)
survminer:: ggsurvplot(km)

km %>% 
       tbl_survfit(
              times = c(0, 6, 16),
              label_header = "**Mois {time}**"
                   )

km %>% 
       tbl_survfit(probs = c(0.25, 0.5,0.75))

#bstfun
devtools::install_github("ddsjoberg/bstfun")




















