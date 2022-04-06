
mase <- function(actual, predicted, h, train, s = 12) {
  n <- length(train)
  ((n-s)/h)*(sum(ae(actual, predicted))) / sum(sapply((s+1):n, function(i) ae(train[i], train[i-s])))
}

amse <- function(actual, predicted, h, train, s = 12) {
  n <- length(train)
  ((n-s)/h)*abs(sum(actual - predicted)) / sum(sapply((s+1):n, function(i) ae(train[i], train[i-s])))
}
