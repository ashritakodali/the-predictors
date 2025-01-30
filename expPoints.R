source("runEpoch.R")     #Link file with function to run epoch
expPoints <- function(down, toGo, toEndzone, fieldPos){   #Function to calculate expected points
  n <- 100              #Set number of simulations
  result <- seq(1:n)    #Create vector to store results in
  for(i in 1:n){        #Loop over chosen length of simulation
    result[i] <- runEpoch(down, toGo, toEndzone, fieldPos)    #Run epoch function and store in result vector
  }
  mean(result)          #Find average result
}

expPoints(1, 10, 50, 50)