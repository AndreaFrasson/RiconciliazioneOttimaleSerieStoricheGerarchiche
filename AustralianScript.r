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
AT <- read.csv("C:/Users/Andrea Frasson/Desktop/Tesina/TourismData_v4.csv")[2:79]
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

#####################
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

#view(tourism)
#dim(tourism)
#dim(AT)

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
#DIVISIONE DEL DATASET IN UNA PARTE DI TRAIN E UNA PARTE DI TEST
#CREAZIONE DELLE GERARCHIE PER EFFETTUARE LE PREVISIONI
set.seed(5)


#####################################################################
#########     Forecasting experiment      ###########################
#####################################################################
for(h in 36:1) {
  
  inds <- partition(AT$AAA, p = c(train = 1-h/nrow(AT), test = h/nrow(AT)), type = "blocked")
  
  train_bts <- AT[inds$train, 3:78]
  sapply(1:nrow(train_bts), function(i) as.matrix(C) %*% as.matrix(t(train_bts[i, ])) ) %>%
    t() -> train_uts
  colnames(train_uts) <- C@Dimnames[[1]]
  hts_train <- cbind(train_uts, train_bts)
  
  test_bts <- AT[inds$test, 3:78]
  sapply(1:nrow(test_bts), function(i) as.matrix(C) %*% as.matrix(t(test_bts[i, ])) ) %>%
    t() -> test_uts
  colnames(test_uts) <- C@Dimnames[[1]]
  hts_test <- cbind(test_uts, test_bts)
  
  fitted <- lapply(1:ncol(hts_train), function(i) auto.arima(hts_train[,i]))
  
  basef <- lapply(1:ncol(hts_train), function(i) forecast(hts_train[,i], h, model = fitted[[i]], level = 0.95)$mean)
  sapply(1:h, function(i) as.double(basef[[1]][i])) %>%
    as.matrix() -> M_basef
  for(j in 2:ncol(hts_train)) {
    M_basef <- cbind(M_basef, sapply(1:h, function(i) as.double(basef[[j]][i])))
  }
  colnames(M_basef) <- colnames(hts_test)
  
  
  #Costruzione matrice di residui
  
  Mres <- lapply(1:ncol(hts_train), function(i) forecast(hts_train[,i], h, model = fitted[[i]], level = 0.95)$residuals)
  sapply(1:nrow(hts_train), function(i) as.double(Mres[[1]][i])) %>%
    as.matrix() -> RES
  for(j in 2:ncol(hts_train)) {
    RES <- cbind(RES, sapply(1:nrow(hts_train), function(i) as.double(Mres[[j]][i])))
  }
  
  
  ##############################################
  ##Riconciliazione
  #RICONCILIAZIONE BOTTOM-UP
  M_basef %>%
    htsrec(., comb = "bu", C = C) -> cs_bu
  
  #MASE PER 1 PASSO DI PREVISIONE GENERALE
  Metrics::mase(actual = hts_test[1,], predicted = cs_bu$recf[1,], step_size = 1)
  #MASE PER L'ULTIMO PASSO DI PREVISIONE GENERALE
  Metrics::mase(actual = hts_test[h,], predicted = cs_bu$recf[h,], step_size = 1)
  
  mase_bu <- c(
    #MASE PER IL TOTALE
    Metrics::mase(actual = hts_test[,1], predicted = cs_bu$recf[,1], step_size = 1),
    #MASE PER IL LIVELLO 1
    Metrics::mase(actual = hts_test[,2:8], predicted = cs_bu$recf[,2:8], step_size = 1),
    #MASE PER IL LIVELLO 2
    Metrics::mase(actual = hts_test[,9:29], predicted = cs_bu$recf[,9:29], step_size = 1),
    #MASE PER IL LIVELLO 3
    Metrics::mase(actual = hts_test[,30:105], predicted = cs_bu$recf[,30:105], step_size = 1))
  
  #RICONCILIAZIONE TOP-DOWN
  # average historical proportions
  props <- colMeans(hts_train[1:nrow(hts_train),-c(1:29)]/hts_train[1:nrow(hts_train),1])
  M_basef[,1] %>%
    tdrec(., C = C, weights = props) -> cs_td
  
  #MASE PER 1 PASSO DI PREVISIONE GENERALE
  Metrics::mase(actual = hts_test[1,], predicted = cs_td[1,], step_size = 1)
  #MASE PER 36 PASSO DI PREVISIONE GENERALE
  Metrics::mase(actual = hts_test[h,], predicted = cs_td[h,], step_size = 1)
  
  mase_td <- c(
    #MASE PER IL TOTALE
    Metrics::mase(actual = hts_test[,1], predicted = cs_td[,1], step_size = 1),
    #MASE PER IL LIVELLO 1
    Metrics::mase(actual = hts_test[,2:8], predicted = cs_td[,2:8], step_size = 1),
    #MASE PER IL LIVELLO 2
    Metrics::mase(actual = hts_test[,9:29], predicted = cs_td[,9:29], step_size = 1),
    #MASE PER IL LIVELLO 3
    Metrics::mase(actual = hts_test[,30:105], predicted = cs_td[,30:105], step_size = 1))
  
  
  #MINT SAMPLE
  M_basef %>%
    htsrec(., comb = "sam", C = C, res = RES) -> cs_minsam
  
  #MASE PER 1 PASSO DI PREVISIONE GENERALE
  Metrics::mase(actual = hts_test[1,], predicted = cs_minsam$recf[1,], step_size = 1)
  #MASE PER 36 PASSO DI PREVISIONE GENERALE
  Metrics::mase(actual = hts_test[h,], predicted = cs_minsam$recf[h,], step_size = 1)
  
  mase_minsam <- c(
    #MASE PER IL TOTALE
    Metrics::mase(actual = hts_test[,1], predicted = cs_minsam$recf[,1], step_size = 1),
    #MASE PER IL LIVELLO 1
    Metrics::mase(actual = hts_test[,2:8], predicted = cs_minsam$recf[,2:8], step_size = 1),
    #MASE PER IL LIVELLO 2
    Metrics::mase(actual = hts_test[,9:29], predicted = cs_minsam$recf[,9:29], step_size = 1),
    #MASE PER IL LIVELLO 3
    Metrics::mase(actual = hts_test[,30:105], predicted = cs_minsam$recf[,30:105], step_size = 1))
  
  #MINT SHRINK
  M_basef %>%
    htsrec(., comb = "shr", C = C, res = RES) -> cs_minshr
  
  #MASE PER 1 PASSO DI PREVISIONE GENERALE
  Metrics::mase(actual = hts_test[1,], predicted = cs_minshr$recf[1,], step_size = 1)
  #MASE PER 36 PASSO DI PREVISIONE GENERALE
  Metrics::mase(actual = hts_test[h,], predicted = cs_minshr$recf[h,], step_size = 1)
  
  mase_minshr <- c(
    #MASE PER IL TOTALE
    Metrics::mase(actual = hts_test[,1], predicted = cs_minshr$recf[,1], step_size = 1),
    #MASE PER IL LIVELLO 1
    Metrics::mase(actual = hts_test[,2:8], predicted = cs_minshr$recf[,2:8], step_size = 1),
    #MASE PER IL LIVELLO 2
    Metrics::mase(actual = hts_test[,9:29], predicted = cs_minshr$recf[,9:29], step_size = 1),
    #MASE PER IL LIVELLO 3
    Metrics::mase(actual = hts_test[,30:105], predicted = cs_minshr$recf[,30:105], step_size = 1))
  
  Errors <- matrix(c(mase_bu, mase_td, mase_minsam, mase_minshr), ncol = 4, byrow = T)
  colnames(Errors) <- c("Level 0", "Level 1", "Level 2", "Level 3")
  rownames(Errors) <- c("BU", "TD", "MinT Sample", "MinT Shrink")
  print(h)
  print(Errors)
}

