# Packages
library(tidyverse)
library(gtsummary)
library(labelled)

donnees <- read.csv("C:/Users/33753/Downloads/trajpro.csv")

# 

donnees$generation <- factor(
       donnees$generation,
       levels = 1:3,
       labels = c("1930-1938", "1939-1945", "1946-1950")
)


donnees %>% look_for("generation")
#View(donnees)

# package analyse des sequences
library(TraMineR)


labels <- c(
       agric = "agriculture exploitants", #1
       acce = "artisans, commerçants et chefs d'entreprise",#2
       cadr = "cadre et professions intellectiuelles supérieures",#3
       pint = "professions intermediaires",#4
       empl = "employés",#5
       ouvr = "ouvriers", #6
       etud = "etudes", #7
       inact = "inactivités",#8
       smil = "service militaire"#9
)

#definir nos données de sequences

seq <- seqdef(
       donnees %>% select(csp1:csp37),
       alphabet = 1:9,
       states = names(labels),
       labels = names(labels)
)

#dissimilarité entre sequence
seq.om <- seqdist(seq, method = "LCS") %>% 
       as.dist()

seq.arbre <- hclust(seq.om, method = "ward.D2")

plot(seq.arbre, labels = FALSE)

seq.arbre$height %>% 
       sort(decreasing = TRUE) %>% 
       head(20) %>% 
       plot(type="s")

#partition en 5

seq.part <- cutree(seq.arbre, k =5) %>% 
       factor(levels = 1:5,labels = paste("classe", 1:5))

questionr::freq(seq.part)

#Representation graphique des sequences

## les chronogrammes

seqdplot(seqdata = seq)

seqdplot(seqdata = seq, group = seq.part, xtlab =14:50)

seqIplot(seqdata = seq, group = seq.part, xtlab =14:50)# les tapis de seq

ordre <- seq.om %>% 
       cmdscale(k=1)
seqIplot(seq, group = seq.part, xtlab =14:50, sortv=ordre)

#
install.packages("seqhandbook")
library(seqhandbook)

seq_heatmap(seq, seq.arbre, labCol=14:50)


#Avec ggplot2

donnees$id <- 1:nrow(donnees)
donnees$classe <- seq.part
donnees$ordre <- ordre %>% rank(ties.method = "random")


library(tidyr)
long <- donnees %>% 
       pivot_longer(
         cols = csp1:csp37,
         names_to = "annee",
         values_to = "csp"
       )

long$csp <- factor(
       long$csp,
       levels = 1:9,
       labels = labels
)

long$age <- long$annee %>% 
       str_sub(start = 4) %>% 
       as.integer()+13

ggplot(long)+
       aes(x = age, y = factor(ordre), fill=csp)+
       geom_raster()+
       theme_bw()+
       scale_fill_brewer(palette="Set3")+
       facet_grid(rows = vars(classe), scales = "free_y", space = "free_y")+
       scale_y_discrete(label=NULL)+
       scale_x_continuous(
              limits = c(14, 50), breaks = c(14,20,25,30,35,40,45,50))

#graphique de frequence
seqfplot(seq, group = seq.part, xtabs=14:50)

#homogénéité des classes

seqmsplot(seq, group = seq.part, xtabs=14:50)

#la durée moyenne passée dans chaque état
seqrplot(seq, group = seq.part, xtabs=14:50, dist.matrix=seq.om)



























































