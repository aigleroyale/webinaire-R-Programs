# Graphiques uni et bi-variés

library(tidyverse)
library(questionr)
d <- data(hdv2003)
library(gtsummary)
library(labelled)
library(scales)
library(GGally)

trial %>%  look_for()
hdv2003 %>%  look_for()

# Variable continue -------

ggplot(hdv2003)+
       aes(x = age)+
       geom_histogram(binwidth = 2, fill = "orange", color = "black")


ggplot(hdv2003)+
       aes(x = age)+
       geom_histogram(breaks = seq(15, 100, 5), fill = "orange",
                      color = "black")

ggplot(hdv2003)+
       aes(x = age)+
       geom_histogram(breaks = c(18, 20, 30, 50, 80, 97), fill = "orange",
                      color = "black")

ggplot(hdv2003)+
       aes(x = age, y = after_stat(count) / after_stat(width))+
       geom_histogram(breaks = c(18, 20, 30, 40, 50, 60, 70, 80, 97), fill = "orange",
                      color = "black")

# Variable continue et catégorielle

ggplot(hdv2003)+
       aes(x = age, fill = sexe) +
       geom_histogram()+
       facet_grid(cols = vars(sexe))

ggplot(hdv2003)+
       aes(x = age, y = after_stat(density), fill = sexe) +
       geom_histogram()+
       facet_grid(cols = vars(sexe))

ggplot(hdv2003)+
       aes(x = age, color = sexe, fill = sexe) +
       geom_density()+
       facet_grid(cols = vars(sexe))

ggplot(hdv2003)+
       aes(x = age, color = sexe) +
       geom_density()

ggplot(hdv2003)+
       aes(x = age) +
       geom_histogram(fill="orange", alpha = 0.5, color = "grey50")+
       geom_freqpoly(size=1)

ggplot(hdv2003)+
       aes(x = age, y = after_stat(density)) +
       geom_histogram(fill="orange", alpha = 0.5, color = "grey50")+
       geom_density()+
       facet_grid(cols= vars(sexe))

# Boite à moustaches

ggplot(hdv2003)+
       aes(x = sexe, y = age, fill=sport)+
       geom_boxplot()

ggplot(hdv2003)+
       aes(x = sexe, y = age, fill=sport)+
       geom_violin()

ggplot(hdv2003)+
       aes(x = sexe, y = age, color=sport)+
       geom_violin()+
       geom_point(alpha = 0.5)

ggplot(hdv2003)+
       aes(x = sexe, y = age)+
       geom_violin()+
       geom_point(alpha = 0.5, position = "jitter")

ggplot(hdv2003)+
       aes(x = sexe, y = age)+
       geom_violin()+
       geom_point(alpha = 0.5, position = position_jitter(height = 0, width = 0.05))



hdv2003 <- hdv2003 %>% arrange(age) %>% mutate(rang = 1:n())
ggplot(hdv2003) +
       aes(x = rang, y = age)+
       geom_point()

# Annoter un graphique

p <- ggplot(hdv2003) +
       aes(x = sexe, y = age, fill = sport)+
       geom_boxplot()
p + annotate("text", x=1.5, y = 85, label = "MON ETIQUETTE")       

p + annotate("rect", xmin=2.5, xmax = 4, ymin = 15, ymax=21, alpha=0.2)       

p

# Difference en  facet_drid et facet_wrap


ggplot(hdv2003)+
       aes(x = age) +
       geom_histogram(fill="orange", alpha = 0.5, color = "grey50")+
       facet_grid(cols = vars(relig), rows = vars(sexe))

ggplot(hdv2003)+
       aes(x = age) +
       geom_histogram(fill="orange", alpha = 0.5, color = "grey50")+
       facet_wrap(vars(relig, sexe))

# Pyramide des âges
install.packages("ggcharts")
library(ggcharts)
data(popch)
pyramid_chart(popch, age, pop, sex)

# Variables catégorielles diagramme en barre ----
trial
ggplot(trial) +
   aes(x = stage, fill = grade)+
   geom_bar()

ggplot(trial) +
       aes(x = stage, fill = grade)+
       geom_bar(position = "dodge")
       
ggplot(trial) +
       aes(x = stage, fill = grade)+
       geom_bar(position = "fill")+
       scale_y_continuous(labels = label_percent(suffix = " %"))

ggplot(trial) +
       aes(x = stage, fill = grade, label = after_stat(count))+
       geom_bar()+
       geom_text(stat = "count", position = position_stack(0.5))

ggplot(trial) +
       aes(x = stage, fill = grade)+
       geom_bar()+
       geom_text(
         mapping = aes(label = after_stat(count)),
         stat = "count", position = position_stack(0.5))

ggplot(trial) +
       aes(x = stage, fill = grade, label = after_stat(count))+
       geom_bar(position = "dodge")+
       geom_text(stat = "count", position = position_dodge(0.9), vjust =0)

ggplot(trial) +
       aes(x = stage, fill = grade, 
           label = percent(after_stat(prop), accuracy = 1),
           by = grade
          ) +
       geom_bar(position = "fill") +
       geom_text(stat = "prop", position = position_fill(0.5))+
       facet_wrap(vars(trt))



# Exemple
x <- c(1.2, 0.123, 10.4235, 0.22, 14598.5825)
number(x)
number(x, decimal.mark = ",", big.mark = "'", prefix = "$", suffix = "CAD")
number(x, accuracy = 1, scale = 1/1000000)




