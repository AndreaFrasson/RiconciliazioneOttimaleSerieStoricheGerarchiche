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

#Rolling-Origin evaluation experiment


RollingOrigin <- function(BottomOsservation, O) {
  
  #Generazione della matrice C (e quindi S' = [C | I])
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
  
  #Data frame per contenere gli errori ai vari istanti di previsione per livello
  
  MASEByLevel <- data.frame(Level0 = double(),
                            Level1 = double(),
                            Level2 = double(),
                            Level3 = double())
  
  RMSSEByLevel <- data.frame(Level0 = double(),
                            Level1 = double(),
                            Level2 = double(),
                            Level3 = double())
  
  AMSEByLevel <- data.frame(Level0 = double(),
                             Level1 = double(),
                             Level2 = double(),
                             Level3 = double())
  h <- 12
  
  for(t in O:1) {

      
    #Divisione iterativa del dataset in una parte di train e una parte di test
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
    
    #Aggiornamento dei modelli ad ogni passo, in funzione al dataset di train e di test
    fitted <- lapply(1:ncol(hts_train), function(i) auto.arima(hts_train[,i]))
    
    
    if(h > nrow(hts_test)) {
      h <- h - 1 
    }
    
    #Generazione delle previsioni base, per ogni serie del modello
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
    
    
    ##Riconciliazione con metodi differenti
   
     #RICONCILIAZIONE BOTTOM-UP
    M_basef %>%
      htsrec(., comb = "bu", C = C) -> cs_bu
    
    mase_bu <- sapply(1:ncol(cs_bu$recf), 
                      function(i) mase(actual = hts_test[1:h,i], predicted = cs_bu$recf[,i], h = t, train = hts_train[,i], s = 12))
    
    amse_bu <- sapply(1:ncol(cs_bu$recf), 
                       function(i) amse(actual = hts_test[1:h,i], predicted = cs_bu$recf[,i], h = t, train = hts_train[,i], s = 12))
    
     
    MASEByLevel <- rbind(MASEByLevel, c(mase_bu[1], mean(mase_bu[2:8]), mean(mase_bu[9:29]), mean(mase_bu[30:105])))
    AMSEByLevel <- rbind(AMSEByLevel, c(amse_bu[1], mean(amse_bu[2:8]), mean(amse_bu[9:29]), mean(amse_bu[30:105])))
    
    
    #RICONCILIAZIONE TOP-DOWN
    # average historical proportions
    props <- colMeans(hts_train[1:nrow(hts_train),-c(1:29)]/hts_train[1:nrow(hts_train),1])
    M_basef[,1] %>%
      tdrec(., C = C, weights = props) -> cs_td
    
    
    mase_td <- sapply(1:ncol(cs_td), 
                      function(i) mase(actual = hts_test[1:h,i], predicted = cs_td[,i], h = t, train = hts_train[,i], s = 12))
    
    amse_td <- sapply(1:ncol(cs_td), 
                       function(i) amse(actual = hts_test[1:h,i], predicted = cs_td[,i], h = t, train = hts_train[,i], s = 12))
    
    MASEByLevel <- rbind(MASEByLevel, c(mase_td[1], mean(mase_td[2:8]), mean(mase_td[9:29]), mean(mase_td[30:105])))
    AMSEByLevel <- rbind(AMSEByLevel, c(amse_td[1], mean(amse_td[2:8]), mean(amse_td[9:29]), mean(amse_td[30:105])))    
    
    #MINT SAMPLE
    M_basef %>%
      htsrec(., comb = "sam", C = C, res = RES) -> cs_minsam
   
    mase_minsam <- sapply(1:ncol(cs_minsam$recf), 
                      function(i) mase(actual = hts_test[1:h,i], predicted = cs_minsam$recf[,i], h = t, train = hts_train[,i], s = 12))
 
    amse_minsam <- sapply(1:ncol(cs_minsam$recf), 
                           function(i) amse(actual = hts_test[1:h,i], predicted = cs_minsam$recf[,i], h = t, train = hts_train[,i], s = 12))
    
    MASEByLevel <- rbind(MASEByLevel, c(mase_minsam[1], mean(mase_minsam[2:8]), mean(mase_minsam[9:29]), mean(mase_minsam[30:105])))
    AMSEByLevel <- rbind(AMSEByLevel, c(amse_minsam[1], mean(amse_minsam[2:8]), mean(amse_minsam[9:29]), mean(amse_minsam[30:105])))    
    
    
    #MINT SHRINK
    M_basef %>%
      htsrec(., comb = "shr", C = C, res = RES) -> cs_minshr
    
    mase_minshr <- sapply(1:ncol(cs_minshr$recf), 
                          function(i) mase(actual = hts_test[1:h,i], predicted = cs_minshr$recf[,i], h = t, train = hts_train[,i], s = 12))
    
    amse_minshr <- sapply(1:ncol(cs_minshr$recf), 
                           function(i) amse(actual = hts_test[1:h,i], predicted = cs_minshr$recf[,i], h = t, train = hts_train[,i], s = 12))
    
    MASEByLevel <- rbind(MASEByLevel, c(mase_minshr[1], mean(mase_minshr[2:8]), mean(mase_minshr[9:29]), mean(mase_minshr[30:105])))
    AMSEByLevel <- rbind(AMSEByLevel, c(amse_minshr[1], mean(amse_minshr[2:8]), mean(amse_minshr[9:29]), mean(amse_minshr[30:105])))
  }
  
  #Lista di matrici con gli errori per livello di aggregazione
  return(list(MASEByLevel, AMSEByLevel))
}
