rm(list= ls())

library(evir)
library("quantmod")
library("PerformanceAnalytics")
library("rugarch")
library("car")
library("FinTS")
library("fBasics")
library("tseries")
library("astsa")
library("urca")
library(readxl)
library(dplyr)
library(TTR)
library(fExtremes)
library(tidyr)
library(gridExtra)
library("forecast")
library(ggthemes)
library(ggplot2)
library(biwavelet)
library("corrr")
library("ExtremeBounds")
library("sandwich")

showClass("GARCHspec")
options(digits = 4)

getSymbols("WTI",src='yahoo', from= "2007-01-04", to = "2020-01-04")
getSymbols("^GSPC",src='yahoo', from= "2007-01-04", to = "2020-01-04")

data1 <- read.csv("C:/Users/33753/Downloads/londonfixes-current-clean_1990_.csv")
data2 <- read.csv("C:/Users/33753/Downloads/VIX.csv")
data3 <- read.csv("C:/Users/33753/Downloads/USEPUINDXD.csv)





