# Analyse des correspondances multiples ou Analyse factorielles des correspondance

rm(list=ls())

#packages ------
install.packages("ade4")
install.packages("FactoMineR")

library(ade4)
library(FactoMineR)
library(tidyverse)

# Exemple introductif ------

ggplot(iris)+
  aes(x=Petal.Length, y = Petal.Width)+
  geom_point()


# ACP ------
res <- iris %>% 
     select(starts_with("Petal")) %>% 
     dudi.pca(nf = 2, scannf = FALSE)

# Explorer l'analyse
install.packages("explor")
library(explor)
res %>% explor()


res <- explor::prepare_results(.)
explor::PCA_ind_plot(
       res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
       ind_lab_min_contrib = 0, col_var = NULL, labels_size = 9, point_opacity = 0.5,
       opacity_var = NULL, point_size = 64, ellipses = FALSE, transitions = FALSE,
       labels_positions = NULL, xlim = c(-2.7, 2.94), ylim = c(-2.84, 2.8))

## exemple plus complet ------

library(questionr)
data("hdv2003")

hdv2003$grpage <- cut(
       hdv2003$age, c(16, 25, 45, 65, 93), 
       right = FALSE, 
       include.lowest = TRUE)

hdv2003$etud <- hdv2003$nivetud
levels(hdv2003$etud) <- c(
       "Primaire", "Primaire", "Primaire", "Secondaire", "Secondaire",
       "Technique/Professionnel", "Technique/Professionnel", "Supérieur"
)

acm <- hdv2003 %>% 
       select(grpage, sexe, etud, peche.chasse, cinema, cuisine, bricol,
              sport, lecture.bd) %>% 
       dudi.acm(scannf = FALSE, nf = Inf)

#explor::explor(acm)

res <- explor::prepare_results(acm)
explor::MCA_var_plot(
 res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
 var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = NULL,
 size_range = c(10, 300), labels_size = 10, point_size = 56, transitions = TRUE,
 labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.64, 1.93),
 ylim = c(-2.04, 1.52))


# Faco=toextra
install.packages("factoextra")
library(factoextra)

screeplot(acm, main = "ACM")

fviz_screeplot(acm, choice = "eigenvalue")

fviz_screeplot(acm)

summary(acm)

View(acm)

s.corcircle(acm$co, clabel = .7)

fviz_mca_var(acm, repel = TRUE)

# Graphique à la main

res <- explor::prepare_results(acm)
explor::MCA_var_plot(
 res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
 var_lab_min_contrib = 0, col_var = "Variable", 
 symbol_var = NULL, size_var = NULL,
 size_range = c(10, 300), labels_size = 10, point_size = 56, 
 transitions = TRUE,
 labels_positions = NULL, labels_prepend_var = FALSE)

res
acm$co

fviz_mca_biplot(acm)

#Boxplot

boxplot(acm)
d2 <- hdv2003 %>%
       select(grpage, sexe, etud, 
              peche.chasse, cinema, 
              cuisine, bricol,
              sport, lecture.bd)

acm <- dudi.acm(d2, scannf = FALSE, nf = Inf)

boxplot(acm, xax = 1)

boxplot(acm, xax = 2)

#Visualisation des contributions sur les axes

fviz_contrib(acm, choice = "var", axes = 1)
fviz_contrib(acm, choice = "var", axes = 2)

par(mfrow = c(2, 2))
for (i in 1:4) barplot(acm$cr[, i], 
                       names.arg = row.names(acm$cr), 
                       las = 2, main = paste("Axe", i))

par(mfrow = c(1, 1))
fviz_mca_ind(acm, geom = "point", alpha.ind = .25)

#un autre packages

install_github("larmarange/JLutils")
library(JLutils)
library(devtools)
?install_github
s.freq(acm)


# Ade4

s.hist(acm$li, clabel = 0)

#Nuage individu en fonction du sex
s.class(acm$li, d2$sexe, col = c("red", "darkgreen"))

fviz_mca_ind(acm, geom = "point", habillage = d2$sexe, addEllipses = TRUE)

s.class(acm$li, hdv2003$relig)

fviz_mca_ind(acm, geom = "point", habillage = hdv2003$relig, addEllipses = FALSE)

# Variable illustrative







































