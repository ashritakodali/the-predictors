source("epoch.R")
top_level_function <- function(down, ytg, fp, n) {
  epochs_vector <- rep(0, n) 
  for (x in 1:n) {
    epochs_vector[x] <- simulate_epoch(down, ytg, fp)
  }
  return(mean(epochs_vector))
}