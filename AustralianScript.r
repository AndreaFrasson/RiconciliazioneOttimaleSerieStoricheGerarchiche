rm(list = ls())
library(FoReco)
library(forecast)
library(tidyverse)
library(tsibble)
library(hts)
library(fabletools)

#V3 con pourpose of travel
#AT <- read.csv("D:/Documenti/Tesina/TourismData_v3.csv")
#v4 del prof
AT <- read.csv("D:/Documenti/Tesina/TourismData_v4.csv")
AT <- AT[,2:79]
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
#AT$Index = paste(as.character(AT$Year), AT$Month, sep = " ")

#AT %>%
#  mutate(Index = yearmonth(Index)) %>%
#  as_tsibble(index = Index) -> AT

#Fare alcune considerazioni circa la struttura dei dati, come sono divisi, i motivi del
#viaggio e le regioni australiane ecc e poi plottare
#Hir <- hts(OnlyData, BottomNames, characters = c(1,1,1))
#par(mfrow = c(2,2))
#plot.gts(Hir, levels = 0)
#plot.gts(Hir, levels = 1)
#plot.gts(Hir, levels = 2)
#plot.gts(Hir, levels = 3)

#view(tourism)
#dim(tourism)
#dim(AT)

OnlyData <- AT[,3:78]

AT[,3:78] %>%
  colnames() %>%
  as.list() -> BottomNames

data_bts <- data.frame(State = c(BottomNames %>%
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


Cmatrix( ~ State/Zones/Region, data_bts, sep = "")




