# Classification ascendante hierachique CAH-------

# charger les librairies -----------
library(tidyverse)
library(questionr)
data(hdv2003)

## Recodage de hdv2003$age en hdv2003$groupe_age
hdv2003$groupe_age <- cut(hdv2003$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 25, 45, 65, 97)
)

## Recodage de hdv2003$nivetud en hdv2003$etude
hdv2003$etude <- hdv2003$nivetud %>%
       fct_recode(
              "Primaire" = "N'a jamais fait d'etudes",
              "Primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
              "Primaire" = "Derniere annee d'etudes primaires",
              "Secondaire" = "1er cycle",
              "Secondaire" = "2eme cycle",
              "Tech/Pro" = "Enseignement technique ou professionnel court",
              "Tech/Pro" = "Enseignement technique ou professionnel long",
              "Supérieur" = "Enseignement superieur y compris technique superieur"
       ) %>%
       fct_explicit_na("Non documenté")

d2 <- hdv2003 %>% 
       select(groupe_age, sexe, etude, peche.chasse, cinema, cuisine,
              bricol, sport, lecture.bd)

library(ade4)

acm <- dudi.acm(d2,scannf = FALSE, nf = Inf)
d2 %>% nrow()

# Calculuer la matrice de distance

md <- dist.dudi(acm)

# calculer les distances avec GOWER

library(cluster)
md_gower <- daisy(d2, metric = "gower")

# Calcul du dendogramme

arbre <- hclust(md, method = "ward.D2")
plot(arbre, label = FALSE)

arbre_gower <- hclust(md_gower, method = "ward.D2")
plot(arbre_gower, label = FALSE)

#Répresenter un arbre

plot(arbre, labels = FALSE)


# Où couper le dendogramme
rect.hclust(arbre, 4, border = "red")


# Avec packages
install.packages("dendextend")
library(dendextend)
color_branches(arbre, k =5) %>% ggplot(labels = FALSE)
dendextend::color_branches(arbre, k =5) %>% ggplot(labels = FALSE)


library(factoextra)
fviz_dend(arbre, k=5)

library(viridis)
library(viridisLite)
library(cividis)



# Saut d'inerties
inertie <- sort(arbre$height, decreasing = TRUE)
plot(inertie[1:20], type = "s")

inertie_gower <- sort(arbre_gower$height, decreasing = TRUE)
plot(inertie_gower[1:20], type = "s")

install.packages("WeightedCluster")
library(WeightedCluster)
as.clustrange(arbre, diss = md)
as.clustrange(arbre, md) %>% plot()
as.clustrange(arbre_gower, md) %>% plot()

d2$type <- cutree(arbre, 3)
d2$type_gower <- cutree(arbre_gower, 3)
freq(d2$type_gower)
freq(d2$type)

hdv2003$type <- cutree(arbre, 3)
hdv2003$type_gower <- cutree(arbre_gower, 3)
freq(hdv2003$type_gower)
freq(hdv2003$type)


























