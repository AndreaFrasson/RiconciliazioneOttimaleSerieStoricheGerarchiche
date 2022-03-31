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
library(Metrics)

RollingOrigin <- function(BottomOsservation, h) {
  #############################################################################
  #rolling-origin evaluation experiment
  
  
  OnlyData <- BottomOsservation[,3:78]
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
  
  
  #Data frame per contenere gli errori ai vari istanti di previsione
  MaseByLevel <- data.frame(Level0 = double(),
                            Level1 = double(),
                            Level2 = double(),
                            Level3 = double())
  GeneralError <- c()
  
  
  for(t in h:1) {
    
    inds <- partition(BottomOsservation$AAA, p = c(train = 1-t/nrow(AT), test = t/nrow(AT)), type = "blocked")
    
    train_bts <- BottomOsservation[inds$train, 3:78]
    sapply(1:nrow(train_bts), function(i) as.matrix(C) %*% as.matrix(t(train_bts[i, ])) ) %>%
      t() -> train_uts
    colnames(train_uts) <- C@Dimnames[[1]]
    hts_train <- cbind(train_uts, train_bts)
    
    test_bts <- BottomOsservation[inds$test, 3:78]
    sapply(1:nrow(test_bts), function(i) as.matrix(C) %*% as.matrix(t(test_bts[i, ])) ) %>%
      t() -> test_uts
    colnames(test_uts) <- C@Dimnames[[1]]
    hts_test <- cbind(test_uts, test_bts)
    
    fitted <- lapply(1:ncol(hts_train), function(i) auto.arima(hts_train[,i]))
    
    basef <- lapply(1:ncol(hts_train), function(i) forecast(hts_train[,i], t, model = fitted[[i]], level = 0.95)$mean)
    sapply(1:t, function(i) as.double(basef[[1]][i])) %>%
      as.matrix() -> M_basef
    for(j in 2:ncol(hts_train)) {
      M_basef <- cbind(M_basef, sapply(1:t, function(i) as.double(basef[[j]][i])))
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
    Metrics::mase(actual = hts_test[t,], predicted = cs_bu$recf[t,], step_size = 1)
    
    mase_bu <- c(
      #MASE PER IL TOTALE
      Metrics::mase(actual = hts_test[,1], predicted = cs_bu$recf[,1], step_size = 1),
      #MASE PER IL LIVELLO 1
      Metrics::mase(actual = hts_test[,2:8], predicted = cs_bu$recf[,2:8], step_size = 1),
      #MASE PER IL LIVELLO 2
      Metrics::mase(actual = hts_test[,9:29], predicted = cs_bu$recf[,9:29], step_size = 1),
      #MASE PER IL LIVELLO 3
      Metrics::mase(actual = hts_test[,30:105], predicted = cs_bu$recf[,30:105], step_size = 1))
    
    MaseByLevel <- rbind(MaseByLevel, mase_bu)
    
    #RICONCILIAZIONE TOP-DOWN
    # average historical proportions
    props <- colMeans(hts_train[1:nrow(hts_train),-c(1:29)]/hts_train[1:nrow(hts_train),1])
    M_basef[,1] %>%
      tdrec(., C = C, weights = props) -> cs_td
    
    #MASE PER 1 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[1,], predicted = cs_td[1,], step_size = 1)
    #MASE PER 36 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[h,], predicted = cs_td[t,], step_size = 1)
    
    mase_td <- c(
      #MASE PER IL TOTALE
      Metrics::mase(actual = hts_test[,1], predicted = cs_td[,1], step_size = 1),
      #MASE PER IL LIVELLO 1
      Metrics::mase(actual = hts_test[,2:8], predicted = cs_td[,2:8], step_size = 1),
      #MASE PER IL LIVELLO 2
      Metrics::mase(actual = hts_test[,9:29], predicted = cs_td[,9:29], step_size = 1),
      #MASE PER IL LIVELLO 3
      Metrics::mase(actual = hts_test[,30:105], predicted = cs_td[,30:105], step_size = 1))
    
    MaseByLevel <- rbind(MaseByLevel, mase_td)
    
    
    #MINT SAMPLE
    M_basef %>%
      htsrec(., comb = "sam", C = C, res = RES) -> cs_minsam
    
    #MASE PER 1 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[1,], predicted = cs_minsam$recf[1,], step_size = 1)
    #MASE PER 36 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[h,], predicted = cs_minsam$recf[t,], step_size = 1)
    
    mase_minsam <- c(
      #MASE PER IL TOTALE
      Metrics::mase(actual = hts_test[,1], predicted = cs_minsam$recf[,1], step_size = 1),
      #MASE PER IL LIVELLO 1
      Metrics::mase(actual = hts_test[,2:8], predicted = cs_minsam$recf[,2:8], step_size = 1),
      #MASE PER IL LIVELLO 2
      Metrics::mase(actual = hts_test[,9:29], predicted = cs_minsam$recf[,9:29], step_size = 1),
      #MASE PER IL LIVELLO 3
      Metrics::mase(actual = hts_test[,30:105], predicted = cs_minsam$recf[,30:105], step_size = 1))
    
    MaseByLevel <- rbind(MaseByLevel, mase_minsam)
    
    
    #MINT SHRINK
    M_basef %>%
      htsrec(., comb = "shr", C = C, res = RES) -> cs_minshr
    
    #MASE PER 1 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[1,], predicted = cs_minshr$recf[1,], step_size = 1)
    #MASE PER 36 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[h,], predicted = cs_minshr$recf[t,], step_size = 1)
    
    mase_minshr <- c(
      #MASE PER IL TOTALE
      Metrics::mase(actual = hts_test[,1], predicted = cs_minshr$recf[,1], step_size = 1),
      #MASE PER IL LIVELLO 1
      Metrics::mase(actual = hts_test[,2:8], predicted = cs_minshr$recf[,2:8], step_size = 1),
      #MASE PER IL LIVELLO 2
      Metrics::mase(actual = hts_test[,9:29], predicted = cs_minshr$recf[,9:29], step_size = 1),
      #MASE PER IL LIVELLO 3
      Metrics::mase(actual = hts_test[,30:105], predicted = cs_minshr$recf[,30:105], step_size = 1))
    
    
    MaseByLevel <- rbind(MaseByLevel, mase_minshr)
    print(h)
    print(MaseByLevel)
  }
  
  return(MaseByLevel)
}
