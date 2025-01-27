# Get uniform probability distribution for field positions (0-120 yards)
get_field_position_probs <- function() {
  rep(1 / 121, 121) 
}

# Simulate one drive for a team
drive <- function(state) {
  down <- state$down
  ytg <- state$ytg
  fp <- state$fp
  
  new_fp <- sample(0:120, 1, prob = get_field_position_probs())
  
  # Check if the drive results in a score
  if (new_fp == 100) {
    # Opponent scores a touchdown (reaches your end zone)
    return(list(score = -7, new_state = NA)) 
  } else if (new_fp == 0) {
    # Your team scores a touchdown
    return(list(score = 7, new_state = NA))  
  } else {
    # Continue the drive with the new field position
    new_state <- list(down = 1, ytg = 10, fp = new_fp)
    return(list(score = NA, new_state = new_state))
  }
}



