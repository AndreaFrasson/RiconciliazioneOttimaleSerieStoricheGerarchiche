rm(list = ls())
library(FoReco)
library(forecast)
library(tidyverse)
library(tsibble)
library(hts)
library(fabletools)
library(dplyr)
library(splitTools)

#v4 del prof
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
#Forecasting experiment
OnlyData %>%
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

C <- Cmatrix( ~ State/Zones/Region, data_bts, sep = "")

####################################
#Machine Learining algorithm

#1. The series are split into a series of training sets and test sets, with each
#training set comprising the first p < n observations (for p = q, q + 1, . . . , n − 1)
#and the corresponding test set comprising only the observations at time p + 1.
set.seed(5)
inds <- partition(AT$AAA, p = c(train = 0.75, test = 0.25), type = "blocked")
train <- AT[inds$train, 3:78]
test <- AT[inds$test, 3:78]

#2. A forecasting model is fitted to each series in each training set and one-step-
#ahead forecasts are produced for each test set.
fit <- lapply(1:76, function(i) auto.arima(train[,i]))
forecasts <- lapply(1:76, function(i) forecast(test[,i], h = 1, model = fit[[i]]))


#3. A separate ML model (either a RF or XGB) is built for predicting each of the
#mk bottom series of the hierarchy. The training set of each model consists
#of n − p observations and m + 1 variables. The first m variables (used as
#predictors or inputs) are the one-step ahead forecasts produced during the
#rolling origin process for the m series of the hierarchy, and the last variable
#(the response or target) is the actual value of the bottom-level series at the
#corresponding times. The loss function of the models is the sum of squared
#errors, and the hyper-parameters of the ML models are determined either
#arbitrarily by the user or through an optimization procedure.








