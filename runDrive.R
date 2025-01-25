runDrive <- function(down, toGo, toEndzone, fieldPos){
  range1 <- 0:100
  range2 <- 101:120

  values <- c(range1, range2)
  
  probabilities <- c(rep(0.8 / length(range1), length(range1)), 
                   rep(0.2 / length(range2), length(range2)))
  return(sample(values, size = 1, prob = probabilities))
}