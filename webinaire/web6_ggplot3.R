library(tidyverse)
library(labelled)
library(questionr)
library(scales)
library(GGally)
install.packages("khroma")
library(khroma)


install.packages("extrafont")
library(extrafont)
extrafont::fonts()

load("C:/Users/33753/OneDrive - etu.unistra.fr/Documents/webinaire/connaissances.RData")
view(quest)

look_for(quest)

freq.na(quest)

# pivot_longer -----

conn <- quest %>% 
       select(id, starts_with("conn_")) %>% 
       pivot_longer(
              cols = starts_with("conn_"),
              names_to = "question",
              values_to = "reponse"
       )

conn <- quest %>% 
       select(id, starts_with("conn_")) %>% 
       pivot_longer(
              cols = starts_with("conn_"),
              names_to = "question",
              values_to = "reponse"
       )

#transforer les na en nsp

conn$reponse <- conn$reponse %>% 
       fct_explicit_na("NSP")

## Réordonnancement de conn$reponse
conn$reponse <- conn$reponse %>%
       fct_relevel("non", "NSP", "oui") %>% 
       fct_recode("ne sait pas / manquant" = "NSP")

conn$etiquette <- conn$question %>%
       fct_recode(
              "R est disponible seulement pour Windows" = "conn_a",
              "R possède un puissant moteur graphique" = "conn_b",
              "Il est possible de réaliser des modèles mixtes avec R" = "conn_c",
              "Le package 'foreign' est le seul permettant d'importer des fichiers de données SPSS" = "conn_d",
              "Il n'est pas possible de produire un rapport PDF avec R" = "conn_e",
              "R peut gérer des données d'enquêtes avec un plan d'échantillonnage complexe" = "conn_f",
              "R est utilisée par des scientifiques de toutes disciplines, y compris des sciences sociales" = "conn_g"
       )

conn$etiquette <- conn$question %>%
       fct_recode(
              "R est disponible seulement pour Windows" = "conn_a",
              "R possède un puissant moteur graphique" = "conn_b",
              "Il est possible de réaliser des modèles mixtes avec R" = "conn_c",
              "Le package 'foreign' est le seul permettant\nd'importer des fichiers de données SPSS" = "conn_d",
              "Il n'est pas possible de produire un rapport PDF avec R" = "conn_e",
              "R peut gérer des données d'enquêtes avec un plan d'échantillonnage complexe" = "conn_f",
              "R est utilisée par des scientifiques de toutes disciplines, y compris des sciences sociales" = "conn_g"
       )

ggplot(conn)+
       aes(x=question, fill = reponse)+
       geom_bar(position = "fill")+
       theme(legend.position = "bottom")

ggplot(conn)+
       aes(x=etiquette, fill = reponse)+
       geom_bar(position = "fill")+
       theme(legend.position = "bottom", 
             axis.text.x = element_text(angle = 90))


ggplot(conn)+
       aes(x=etiquette, fill = reponse)+
       geom_bar(position = "fill", width = 0.7)+
       scale_x_discrete(labels = label_wrap(50))+
       scale_y_continuous(labels = label_percent(accuracy = 0.1, suffix = "%"),
                          limits = c(.1, 1.5))+
       theme(legend.position = "bottom",
             axis.text.x = element_text(angle = 90)
       ) + 
       coord_flip()

ggplot(conn)+
       aes(x=etiquette, fill = reponse, label = after_stat(prop), accuracy=1, 
           by = etiquette) +
       geom_bar(position = "fill", width = 0.7)+
       geom_text(position = position_fill(0.5), 
                 stat = "prop",
                 colour = "white", fontface = "bold", size = 4)+
       scale_x_discrete(labels = label_wrap(50))+
       scale_y_continuous(labels = label_percent(), expand = expansion(0,0))+
       theme(legend.position = "bottom",
             axis.text.x = element_text(angle = 90)
       ) + 
       coord_flip()

f <- function(x){
     res <- percent(x, accuracy = 1)
     res[x < .05]<- percent(x[x < .05], accuracy = 1, suffix = "")
     res[x < .01]<- ""
     res
}

ggplot(conn) +
       aes(x=etiquette, 
           fill = reponse, 
           label = f(after_stat(prop)), 
           accuracy=1, 
           by = etiquette) +
       geom_bar(position = "fill", 
                width = 0.7) +
       geom_text(position = position_fill(0.5), 
                 stat = "prop",
                 colour = "white", 
                 fontface = "bold", 
                 size = 4) +
       scale_x_discrete(labels = label_wrap(50)) +
       scale_y_continuous(labels = label_percent(),
                          expand = expansion(0,0)) +
       xlab("") +
       ylab("") +
       theme_minimal() +
       theme(legend.position = "bottom",
             axis.text.x = element_text(angle = 90)
       ) + 
       coord_flip()


ggplot(conn) +
       aes(x=etiquette, 
           fill = reponse, 
           label = f(after_stat(prop)), 
           accuracy=1, 
           by = etiquette) +
       geom_bar(position = "fill", 
                width = 0.7) +
       geom_text(position = position_fill(0.5), 
                 stat = "prop",
                 colour = "white", 
                 fontface = "bold", 
                 size = 4) +
       scale_x_discrete(labels = label_wrap(50)) +
       scale_y_continuous(labels = label_percent(),
                          expand = expansion(0,0)) +
       xlab("") +
       ylab("") +
       theme_minimal() +
       theme(legend.position = "bottom",
             axis.text.x = element_text(angle = 90)
       ) + 
       coord_flip()


ggplot(conn) +
       aes(x=etiquette, 
           fill = reponse, 
           label = f(after_stat(prop)), 
           accuracy=1, 
           by = etiquette) +
       geom_bar(position = "fill", 
                width = 0.7) +
       geom_text(position = position_fill(0.5), 
                 stat = "prop",
                 colour = "white", 
                 fontface = "bold", 
                 size = 4) +
       scale_x_discrete(labels = label_wrap(50)) +
       scale_y_continuous(labels = label_percent(),
                          expand = expansion(0,0)) +
       scale_fill_manual(values = c("#AA3377", "#BBBBBB", "#4477AA"))+
       xlab("") + ylab("") + labs(fill="")+
       guides(fill = guide_legend(reverse = TRUE))+
       theme_minimal(base_family = "Arial Narrow") +
       theme(legend.position = "bottom",
             panel.grid = element_blank(),
             axis.text.x = element_blank()
       ) + 
       coord_flip()

#Trier ou changer l'ordre
conn$etiquette <- conn$etiquette %>% 
       fct_reorder(conn$reponse == "oui", .fun = "sum")

conn$correcte <- conn$question %>%
       fct_recode(
              "bonne réponse : non" = "conn_a",
              "bonne réponse : oui" = "conn_b",
              "bonne réponse : oui" = "conn_c",
              "bonne réponse : non" = "conn_d",
              "bonne réponse : non" = "conn_e",
              "bonne réponse : oui" = "conn_f",
              "bonne réponse : oui" = "conn_g"
       ) %>%
       fct_relevel("bonne réponse : oui")


p <- ggplot(conn) +
       aes(x=etiquette, 
           fill = reponse, 
           label = f(after_stat(prop)), 
           accuracy=1, 
           by = etiquette) +
       geom_bar(position = "fill", 
                width = 0.7) +
       geom_text(position = position_fill(0.5), 
                 stat = "prop",
                 colour = "white", 
                 fontface = "bold", 
                 size = 4) +
       scale_x_discrete(labels = label_wrap(50)) +
       scale_y_continuous(labels = label_percent(),
                          expand = expansion(0,0)) +
       scale_fill_manual(values = c("#AA3377", "#BBBBBB", "#4477AA"))+
       xlab("") + ylab("") + 
       labs(fill="", caption = "Enquête réalisée auprès de 500 individus")+
       guides(fill = guide_legend(reverse = TRUE))+
       theme_minimal(base_family = "Arial Narrow") +
       theme(legend.position = "bottom",
             panel.grid = element_blank(),
             axis.text.x = element_blank(),
             plot.title.position = "plot",
             plot.caption.position = "plot",
             plot.caption = element_text(face = "italic", hjust = 0),
             plot.margin = margin()
       ) + 
       coord_flip()+
       facet_grid(row = vars(correcte), scales = "free", space = "free")+
       ggtitle(
              "Connaissance sur R",
              subtitle = "Pour chacune de ces affirmations, diriez-vous qu'elle est correcte ?"
       )

ggsave("connaissance.png", plot = p, width = 16, height = 10, units = "cm",
       scale = 1.5)
p




