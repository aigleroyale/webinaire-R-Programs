#Analyse descriptive du fichier trial ----
install.packages('Hmisc')
install.packages("gtsummary")
install.packages("esquisse")
install.packages("GGally")
library(tidyverse)
library(labelled)
library(readxl)
library(questionr)
library(gtsummary)
library(esquisse)
library(GGally)
library(Hmisc)



trial <- read_excel("C:/Users/33753/Downloads/trial.xlsx")
save(trial)

# Statistique univariée ----
# une variable qantitative
mean(trial$age, na.rm= TRUE)
median(trial$age, na.rm = TRUE)
quantile(trial$age, na.rm =TRUE, probs = 0:5/5)
quantile(trial$age, na.rm =TRUE, probs = c(.1, .23, .58))
min(trial$age, na.rm = TRUE)
range(trial$age, na.rm = TRUE)
var(trial$age, na.rm = TRUE)
sd(trial$age, na.rm = TRUE)
summary(trial$age)

# variable categorielle

table(trial$stage)
xtabs(~ stage, data = trial)
xtabs(~stage + grade, data = trial)
freq(trial$stage)
freq(trial$stage, cum = TRUE)

#gtsummary

tbl_summary(trial, include = c('age','stage'))

theme_gtsummary_language("fr", decimal.mark = ',', big.mark = "")
tbl_summary(trial, include = c("age", "stage"))


var_label(trial$age) <- "Age en années"
var_label(trial$trt) <- "Traitement"
tbl_summary(trial, include = c('age','stage'))

# Statistique bivariée / Graphiques bivariés ----

ggplot(trial)+
       aes(x=stage, y =age, fill=trt)+
       geom_boxplot()+
       scale_fill_hue()+
       labs(title = "Titre de mon graphique", subtitle = "Sous-titre",
            caption = "Enquête 2020")+
       theme_minimal()+
       theme(legend.position = "bottom")


# Tableaux croisés ----
tbl_summary(
       trial, 
       by ="trt",
       percent = "row",
       statistic = list(
              all_continuous() ~ "{mean} ({sd})"
       )
       )

tbl_summary(
       trial, 
       by ="trt",
       percent = "row",
       statistic = list(
              all_continuous() ~ "{mean} [et : {sd}]"
       )
)

tbl_summary(
       trial, 
       by ="trt",
       percent = "row",
       statistic = list(
              all_continuous() ~ "med : {median}/ moy : {mean}"
       )
)

theme_gtsummary_mean_sd()

add_overall(
       tbl_summary(
       trial,
       by="trt",
       percent = "row",
       digits = list(
              all_categorical() ~ c(0, 1),
              all_continuous() ~ c(1,1)
       )
)
)

# %>%

trial %>%
       tbl_summary(
        by ="trt",
        percent = "row"
       ) %>%
       add_overall(last = TRUE) %>%
       add_p()

t1 <- trial %>%
       tbl_summary(
              include = c("trt","age","grade"),
              by = "trt"
       )

t2 <- trial %>%
       tbl_summary(
              include = c("trt","age","grade"),
              by = "stage"
       )
tbl_merge(list(t1, t2))


# ggbivariate()

ggbivariate(
       trial,
       outcome = "trt",
       explanatory = c("age", "stage", "grade")
)

ggtable(
       trial,
       columnsX = c("trt", "stage"),
       columnsY = c("age", "grade")
)

ggtable(
       trial,
       columnsX = c("trt", "stage"),
       columnsY = c("age", "grade"),
       cells = "row.prop",
       fill = "std.resid"
)



























