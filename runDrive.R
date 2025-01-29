runDrive <- function(down, toGo, toEndzone, fieldPos){     #Create drive function
  range1 <- 0:100           #Define ranges of yards split by no score and score
  range2 <- 101:120

  values <- c(range1, range2)       #Create vector of all possible field positions
  
  probabilities <- c(rep(0.8 / length(range1), length(range1)),      #Assign probabilities to each possible yard position
                   rep(0.2 / length(range2), length(range2)))        #Probabilities do not mean anything right now
  sample(values, size = 1, prob = probabilities)             #Randomly select field position
}