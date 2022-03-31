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
library(TSPred)
source("Utility.r")



RollingOrigin <- function(BottomOsservation, h) {
  #############################################################################
  #rolling-origin evaluation experiment
  #BottomOsservation <- AT
  #h <- 36
  
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
  MASEByLevel <- data.frame(Level0 = double(),
                            Level1 = double(),
                            Level2 = double(),
                            Level3 = double())
  RMSSEByLevel <- data.frame(Level0 = double(),
                            Level1 = double(),
                            Level2 = double(),
                            Level3 = double())
  
  
  for(t in h:1) {
    
    #t <- h
    
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
    
    mase_bu <- sapply(1:ncol(cs_bu$recf), 
                      function(i) mase(actual = hts_test[,i], predicted = cs_bu$recf[,i], h = t, train = hts_train[,i], s = 12))
    
    rmsse_bu <- sapply(1:ncol(cs_bu$recf), 
                      function(i) rmsse(actual = hts_test[,i], predicted = cs_bu$recf[,i], h = t, train = hts_train[,i], s = 12))
     
    MASEByLevel <- rbind(MASEByLevel, c(mase_bu[1], mean(mase_bu[2:8]), mean(mase_bu[9:29]), mean(mase_bu[30:105])))
    RMSSEByLevel <- rbind(RMSSEByLevel, c(rmsse_bu[1], mean(rmsse_bu[2:8]), mean(rmsse_bu[9:29]), mean(rmsse_bu[30:105])))
    
    #RICONCILIAZIONE TOP-DOWN
    # average historical proportions
    props <- colMeans(hts_train[1:nrow(hts_train),-c(1:29)]/hts_train[1:nrow(hts_train),1])
    M_basef[,1] %>%
      tdrec(., C = C, weights = props) -> cs_td
    
    #MASE PER 1 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[1,], predicted = cs_td[1,], step_size = 1)
    #MASE PER 36 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[h,], predicted = cs_td[t,], step_size = 1)
    
    mase_td <- sapply(1:ncol(cs_bu$recf), 
                      function(i) mase(actual = hts_test[,i], predicted = cs_td[,i], h = t, train = hts_train[,i], s = 12))
    
    rmsse_td <- sapply(1:ncol(cs_bu$recf), 
                       function(i) rmsse(actual = hts_test[,i], predicted = cs_td[,i], h = t, train = hts_train[,i], s = 12))
    
    MASEByLevel <- rbind(MASEByLevel, c(mase_td[1], mean(mase_td[2:8]), mean(mase_td[9:29]), mean(mase_td[30:105])))
    RMSSEByLevel <- rbind(RMSSEByLevel, c(rmsse_td[1], mean(rmsse_td[2:8]), mean(rmsse_td[9:29]), mean(rmsse_td[30:105])))
    
    
    #MINT SAMPLE
    M_basef %>%
      htsrec(., comb = "sam", C = C, res = RES) -> cs_minsam
    
    #MASE PER 1 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[1,], predicted = cs_minsam$recf[1,], step_size = 1)
    #MASE PER 36 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[h,], predicted = cs_minsam$recf[t,], step_size = 1)
    
    mase_minsam <- sapply(1:ncol(cs_bu$recf), 
                      function(i) mase(actual = hts_test[,i], predicted = cs_minsam$recf[,i], h = t, train = hts_train[,i], s = 12))
    
    rmsse_minsam <- sapply(1:ncol(cs_bu$recf), 
                       function(i) rmsse(actual = hts_test[,i], predicted = cs_minsam$recf[,i], h = t, train = hts_train[,i], s = 12))
    
    MASEByLevel <- rbind(MASEByLevel, c(mase_minsam[1], mean(mase_minsam[2:8]), mean(mase_minsam[9:29]), mean(mase_minsam[30:105])))
    RMSSEByLevel <- rbind(RMSSEByLevel, c(rmsse_minsam[1], mean(rmsse_minsam[2:8]), mean(rmsse_minsam[9:29]), mean(rmsse_minsam[30:105])))
    
    
    #MINT SHRINK
    M_basef %>%
      htsrec(., comb = "shr", C = C, res = RES) -> cs_minshr
    
    #MASE PER 1 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[1,], predicted = cs_minshr$recf[1,], step_size = 1)
    #MASE PER 36 PASSO DI PREVISIONE GENERALE
    Metrics::mase(actual = hts_test[h,], predicted = cs_minshr$recf[t,], step_size = 1)
    
    mase_minshr <- sapply(1:ncol(cs_bu$recf), 
                          function(i) mase(actual = hts_test[,i], predicted = cs_minshr$recf[,i], h = t, train = hts_train[,i], s = 12))
    
    rmsse_minshr <- sapply(1:ncol(cs_bu$recf), 
                           function(i) rmsse(actual = hts_test[,i], predicted = cs_minshr$recf[,i], h = t, train = hts_train[,i], s = 12))
    
    MASEByLevel <- rbind(MASEByLevel, c(mase_minshr[1], mean(mase_minshr[2:8]), mean(mase_minshr[9:29]), mean(mase_minshr[30:105])))
    RMSSEByLevel <- rbind(RMSSEByLevel, c(rmsse_minshr[1], mean(rmsse_minshr[2:8]), mean(rmsse_minshr[9:29]), mean(rmsse_minshr[30:105])))
    
  }
  
  return(MASEByLevel, RMSSEByLevel)
}
