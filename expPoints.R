source("runEpoch.R")
expPoints <- function(down, toGo, toEndzone, fieldPos){
  n <- 100
  result <- seq(1:n)
  for(i in 1:n){
    result[i] <- runEpoch(down, toGo, toEndzone, fieldPos)
  }
  mean(result)
}

expPoints(1, 10, 50, 50)