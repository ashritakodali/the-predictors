simulate_drive <- function(state) {
  
  state$fp <- state$fp + runif(1, 0, 120)  # double check to see whether
  # or not this makes sense
  
  state$down <- 1
  state$ytg <- 10
  # ask what is the point of resetting and even 
  # having these arguments if we never actually call them
  # in our other functions
  
  state
}