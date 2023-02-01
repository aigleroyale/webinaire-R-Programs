# Importer les packages -----

library(ggplot2)
library(tidyverse)
library(labelled)
library(GGally)
library(readr)

# Importer nos données --------

debt <- read_csv("C:/Users/33753/Downloads/debt.csv")

debt <- debt %>% 
       select(-1) %>% 
       mutate( Decade = factor(Year %/% 10 + 10)) %>% 
       arrange(Country, Year)

# Graphique ------

qplot(data = debt, x = Year, y = growth)

## Sous graphique selon une variable

qplot(data = debt, x = Year, y = growth) +
       facet_wrap(~Country)

qplot(data = debt, x = Year, y = growth, geom = c("line", "point")) +
       facet_wrap(~Country)

ggplot(debt) +
       aes(x=Year, y = growth)+
       geom_line()+
       geom_point(color = "red")+
       facet_wrap(~Country)

ggplot(debt) +
       aes(x=Year, y = growth, colour = Country)+
       geom_line()+
       geom_point(alpha = 0.5)+
       facet_wrap(~Country)

ggplot(debt) +
       aes(x=Year, y = growth, colour = Country)+
       geom_point(alpha = 0.5, size = 3)


ggplot(debt) +
       aes(x=Year, y = growth, colour = Country, size = ratio) +
       geom_point(alpha = 0.5)


ggplot(debt, aes(x = Year, y = growth)) + geom_point()

p <- ggplot(debt)+
       aes(y=growth, x=ratio)

p + geom_point()+
    facet_wrap(~Decade, ncol = 2)

p + geom_point()+
    facet_grid(cols = vars(Decade))

p <- p + geom_point()+
       facet_grid(cols = vars(Decade))+
       scale_x_continuous(breaks = c(0, 100, 200))
       scale_x_log10()

p + aes(color = ratio < 90)

p + geom_smooth(method = "lm", color = "black")

p + geom_density_2d()
p + geom_violin()

p <- p + 
    aes(color = ratio < 90) +
    scale_color_brewer(palette = 'Set1', labels = c("ratio > 90"),
                       "ratio < 90")

p + geom_hline(yintercept = 0, color = "red", size = 2)+
    ggtitle("Données Reinhart et Rogoff corrigés", subtitle = "1946-2009")+
    xlab("Ratio dette / PIB")+
    ylab("Taux de croissance du PIB")+
    labs(colour = "Ratio Dette / PIB")+
    theme_bw()+
    theme(legend.position = "top")

ggsave("mongraph.png", width = 20, height = 20, 
       dpi = 600, units = "cm", plot = p, scale = 1.5)








