
# Top-level function 
simulate_expected_points <- function(down, ytg, fp, n = 1000) {

  results <- numeric(n)
  
  # Loop through epochs
  for (i in 1:n) {
    # store points from one epoch
    results[i] <- epoch_function(down, ytg, fp)
  }
  #get average points
  expected_points <- mean(results)
  return(expected_points)
}

