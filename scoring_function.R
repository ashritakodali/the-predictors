source("drive.R")

#check if a score occurred
scoring <- function(state) {
  
  # If state is NULL, the game has ended
  if (is.null(state)) {
    return(list(score = NA, new_state = NA)) 
  }
  
  fp <- state$fp
  
  # Determine the score based on field position
  if (fp > 100 & fp <= 110) {
    return(list(score = 7, new_state = state))  # Touchdown
  } else if (fp > 110 & fp <= 120) {
    return(list(score = 3, new_state = state))  # Field goal
  } else if (fp == 0) {
    return(list(score = -7, new_state = state))  # Opponent touchdown
  } else {
    return(list(score = NA, new_state = state))  # No score
  }
}


