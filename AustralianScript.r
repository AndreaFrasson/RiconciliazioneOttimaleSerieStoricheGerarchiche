rm(list = ls())
library(FoReco)
library(forecast)
library(tidyverse)
library(tsibble)
library(hts)
library(fabletools)
library(dplyr)
library(splitTools)
library(drat)
library(xgboost)
library(randomForest)



#v4 
AT <- read.csv("D:/Documenti/Tesina/TourismData_v4.csv")[2:79]
colnames(AT)[1:2] <- c("Year", "Month")
#-----------------------------------------------------
#Esplorazione dei dati
#Creazione di una struttura di dati coerente per le analisi preliminari
Y = AT[1,]$Year
for (j in 1:240) {
  if(is.na(AT[j,]$Year)){
    AT[j,]$Year = Y
  }
  else Y = AT[j,]$Year
}

#Creazione di una variabile che possa fare da indice per le serie storiche
##
AT$Index = paste(as.character(AT$Year), AT$Month, sep = " ")

AT %>%
  mutate(Index = yearmonth(Index)) %>%
  as_tsibble(index = Index) -> AT_ts

#Fare alcune considerazioni circa la struttura dei dati, come sono divisi, i motivi del
#viaggio e le regioni australiane ecc e poi plottare

par(mfrow = c(2,2))

#####PLOT LIVELLO DISAGGREGATO#########
diss_plot <- AT_ts[,c(1:3,79)] %>%
  mutate(AAA = AAA/100) %>% 
  as_tsibble(index = Index)

autoplot(diss_plot, AAA) +
  labs(title = "AAA Tourism flow",
       y = "Passengers", x = "Year")


#####PLOT TOTAL################
OnlyData <- AT[,3:78]
AT_hts <- hts(OnlyData, characters = c(1,1,1),BottomNames)
#Summing matrix
AT_hts %>%
  smatrix(.)

Lev0 <- AT_hts %>%
  aggts(levels = 0) %>%
  as_tsibble()

Lev0$index <- AT$Index

Lev0 %>%
  mutate(index = yearmonth(index)) %>%
  as_tsibble(index = index) -> Lev0

autoplot(Lev0, value) +
  labs(title = "Total flow Australian tourism",
       y = "Passengers", x = "Year")

#############################################################################
#rolling-origin evaluation approach
OnlyData <- AT[,3:78]
OnlyData %>%
  colnames() %>%
  as.list() -> BottomNames

name_bts <- data.frame(State = c(BottomNames %>%
                                   gsub('.{2}$', '',.)
                                ),
                                
                                Zones = c(BottomNames %>%
                                            gsub('^.', '',.) %>%
                                            gsub('.$', '',.)
                                ),
                                
                                Region = c(BottomNames %>%
                                             gsub('^.{2}', '',.)
                                )
                      )

C <- Cmatrix( ~ State/Zones/Region, name_bts, sep = "")

#Divisione del dataset in una parte di train e una parte di test
set.seed(5)
inds <- partition(AT$AAA, p = c(train = 0.7, test = 0.3), type = "blocked")
train <- AT[inds$train, 3:78]
test <- AT[inds$test, 3:78]

#Previsioni per il set di train, con un modello arima stimato dalla funzione
fit <- lapply(1:76, function(i) auto.arima(train[,i]))
f_bts <- lapply(1:76, function(i) forecast(train[,i], h = 1, model = fit[[i]], level = 0.95)$mean)
f_bts %>%
  as.matrix() %>%
  t() %>%
  htsrec(., comb = "bu", C = C)


as.vector(f_bts)
m <- auto.arima(train$AAA)
f <- forecast(train$AAA, h = 1, model = m)$mean

dim(C)
length(f_bts)











