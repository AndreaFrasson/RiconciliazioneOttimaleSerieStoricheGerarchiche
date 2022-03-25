
getMASE <- function(basef, test, train, nsample, h, s = 12) {
  num <- ((nsample-s)/h) * sum(abs(test[1]-basef[1]))
  den <- sum(sapply(13:nrow(train), function(i) abs(train[i, 1] - train[i-s, 1])))
  num/den
}
