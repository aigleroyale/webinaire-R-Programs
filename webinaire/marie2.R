library(readr)
library(xts)
library(fGarch)

library(moments)
library(ggplot2)
library(tseries)
library(rugarch)

library(extRemes)

library(secr)

library(PerformanceAnalytics)

library(quantmod)

library(rugarch)
library(car)
library(FinTS)
library(fBasics)
library(tseries)
library(astsa)
library(MTS)
library(tidyverse)

rm(list=ls())

date_i <- "2007-01-01"
date_f <- "2020-01-01"

getSymbols("WTI", src = "yahoo", from = date_i, to = date_f)
getSymbols("^GSPC", src = "yahoo", from = date_i, to = date_f)

GSPC




