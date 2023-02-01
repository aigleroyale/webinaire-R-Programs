#Chargement de packages ----

library(dplyr)
library(nycflights13)


# Importation des données ----
data("flights")
view(flights)
?flights

data("airports")
view(airports)

data("airlines")
view(airlines)

# Analyse ----
# Selection

slice(airports, 10)
slice(airports, 10:45)
slice(airports, 10, 12, 14)
slice(airports, c(10, 12))

pos <- c(10, 12, 14)

tmp <- slice(airports, pos)

# Filtrer

flights[flights$month==1, ]

filter(flights, month == 1)
filter(flights, dep_delay   >= 10 & dep_delay  <= 15)

filter(flights, dep_delay < 0 | dep_delay > 60)
filter(flights, dep_delay >=10, dep_delay <=15)

filter(flights, distance == max(distance))

filter(flights, distance == min(distance), na.rm=TRUE)

subset(flights, dep_delay < 0)

# Selection

select(airports, lat, lon)
data("airports")
select(airports, -lat, -lon)
select(flights, starts_with("dep"))
select(flights, ends_with("e"))

select(flights, day:dest)
select(flights, where(is.integer))

v <- c("month", "dest", "dep_time", "company")

select(flights, v)
select(flights, all_of(v))
select(flights, any_of(v))

select(airports, latitude = lat, longitude = lon ) 

select(airports, lat, everything())

select(airports, everything())
pull(airports, lat)

# Renomer le nom de la variable
rename(airports, latitude = lat)
relocate(airports, lon)

# Demo du pipe

select(slice(filter(flights, dep_delay < 0), 1:10), month, day)

flights %>%
       filter(dep_delay <0) %>%
       slice(1:10) %>%
       select(month, day)

# Trier un fichier

flights %>%
       arrange(dep_delay)

flights %>%
       arrange(month, day)

flights %>%
       arrange(desc(month, day))

flights %>%
       arrange(desc(dep_delay))


# Modifier une variable
airports %>%
       mutate(alt_m = alt / 3.2808)

flights %>%
       mutate(distance_km = distance / 0.62137,
       vitesse_km = distance_km / air_time * 60
              ) %>%
       select(distance_km, vitesse_km)

# Operation par groupes
flights %>% group_by(month)

flights %>% group_by(month) %>% slice(1)

flights %>% 
       group_by(month) %>%
       mutate(moy_months = mean(dep_delay, na.rm =TRUE))


flights %>% 
       group_by(month, day) %>%
       summarise(moy_months = mean(dep_delay, na.rm =TRUE))

# Function a fenettre
v <- c(4, 6, 7, 8, 9, 1)
rank(v)

v <- c(2, 6, 2, 8, 2, 1)
rank(v)
rank(v, ties.method = "first")
rank(v, ties.method = "random")

lead(v)
lag(v)

w <- flights %>%
       group_by(carrier, month, day) %>%
       arrange(dep_time) %>%
       mutate(dep_delay_precedent =lag(dep_delay),
              acroissement = dep_delay > dep_delay_precedent)

table(w)
w %>%select(acroissement) %>% tbl_summary()

# Présentation des données
glimpse(airports)

# Fusion de tables ----













