

mase <- function(actual, predicted, h, train, s = 12) {
  n <- length(train)
  ((n-s)/h)*(sum(ae(actual, predicted))) / sum(sapply((s+1):n, function(i) ae(train[i], train[i-s])))
}


rmsse <- function(actual, predicted, h, train, s = 12) {
  n <- length(train)
  ((n-s)/h)*sqrt((sum(se(actual, predicted))) / sum(sapply((s+1):n, function(i) se(train[i], train[i-s]))) )
}

amse <- function(actual, predicted, h, train, s = 12) {
  n <- length(train)
  ((n-s)/h)*abs(sum(actual - predicted)) / sqrt( sum(sapply((s+1):n, function(i) ae(train[i], train[i-s]))) )
}
