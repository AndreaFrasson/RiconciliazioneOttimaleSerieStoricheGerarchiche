source("RollingOrigin.r")

#v4 
AT <- read.csv("Data/TourismData_v4.csv")[2:79]
colnames(AT)[1:2] <- c("Year", "Month")
#Creazione di una struttura di dati coerente per le analisi preliminari
Y = AT[1,]$Year
for (j in 1:240) {
  if(is.na(AT[j,]$Year)){
    AT[j,]$Year = Y
  }
  else Y = AT[j,]$Year
}

#Stima del modello solamente una volta
Errors <- RollingOrigin(AT, 36)
#4:30

method <- factor(c("BU", "TD", "MinTSam", "MinTShr"))
MASE <- cbind(Errors[[1]], method)
colnames(MASE) <- c("Livello 0", "Livello 1", "Livello 2", "Livello 3", "method")
RMSSE <- cbind(Errors[[2]], method)
colnames(RMSSE) <- c("Livello 0", "Livello 1", "Livello 2", "Livello 3", "method")

#Errori medi per ogni livello con metodi diversi
#Livello 0
mean(MASE[MASE$method == "BU", 1])
mean(MASE[MASE$method == "TD", 1])
mean(MASE[MASE$method == "MinTSam", 1])
mean(MASE[MASE$method == "MinTShr", 1])

#Livello 1
mean(MASE[MASE$method == "BU", 2])
mean(MASE[MASE$method == "TD", 2])
mean(MASE[MASE$method == "MinTSam", 2])
mean(MASE[MASE$method == "MinTShr", 2])

#Livello 2
mean(MASE[MASE$method == "BU", 3])
mean(MASE[MASE$method == "TD", 3])
mean(MASE[MASE$method == "MinTSam", 3])
mean(MASE[MASE$method == "MinTShr", 3])

#Livello 3
mean(MASE[MASE$method == "BU", 4])
mean(MASE[MASE$method == "TD", 4])
mean(MASE[MASE$method == "MinTSam", 4])
mean(MASE[MASE$method == "MinTShr", 4])



#Errori medi per ogni livello con metodi diversi
#Livello 0
mean(RMSSE[RMSSE$method == "BU", 1])
mean(RMSSE[RMSSE$method == "TD", 1])
mean(RMSSE[RMSSE$method == "MinTSam", 1])
mean(RMSSE[RMSSE$method == "MinTShr", 1])

#Livello 1
mean(RMSSE[RMSSE$method == "BU", 2])
mean(RMSSE[RMSSE$method == "TD", 2])
mean(RMSSE[RMSSE$method == "MinTSam", 2])
mean(RMSSE[RMSSE$method == "MinTShr", 2])

#Livello 2
mean(RMSSE[RMSSE$method == "BU", 3])
mean(RMSSE[RMSSE$method == "TD", 3])
mean(RMSSE[RMSSE$method == "MinTSam", 3])
mean(RMSSE[RMSSE$method == "MinTShr", 3])

#Livello 3
mean(RMSSE[RMSSE$method == "BU", 4])
mean(RMSSE[RMSSE$method == "TD", 4])
mean(RMSSE[RMSSE$method == "MinTSam", 4])
mean(RMSSE[RMSSE$method == "MinTShr", 4])


#Boxplot per livello
#MASE
MASE %>%
  ggplot(aes( x = method, y = `Livello 0`)) +
  geom_boxplot() 
MASE %>%
  ggplot(aes( x = method, y = `Livello 1`)) +
  geom_boxplot() 
MASE %>%
  ggplot(aes( x = method, y = `Livello 2`)) +
  geom_boxplot() 
MASE %>%
  ggplot(aes( x = method, y = `Livello 3`)) +
  geom_boxplot() 

#RMSSE
RMSSE %>%
  ggplot(aes( x = method, y = `Livello 0`)) +
  geom_boxplot(ylim = c(0,20)) 
RMSSE %>%
  ggplot(aes( x = method, y = `Livello 1`)) +
  geom_boxplot() 
RMSSE %>%
  ggplot(aes( x = method, y = `Livello 2`)) +
  geom_boxplot() 
RMSSE %>%
  ggplot(aes( x = method, y = `Livello 3`)) +
  geom_boxplot() 



#-----------------------------------------------------
#Esplorazione dei dati
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
