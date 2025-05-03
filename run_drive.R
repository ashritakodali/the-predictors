#setwd("C:/Users/nblau/Documents/Stat 4800/project")
source('final_run_play.R')

# This is the run_drive function.  For now, our only goal is to make sure that
# run_epoch is working, so we don't really need anything like a realistic
# run_drive function.  We just need one that will give us outcomes we can
# play with.  The run_drive function will output a state.


run_drive <- function(D, YTG, FP, weather) {
  print(paste("Starting FP:", FP))
  
  # get new state
  new_state <- run_play2(D, YTG, FP, weather)
  
  # Check if we should return the state or run a new play
  if(new_state$exit_drive == 0){
    
    # if we should stay with the current drive, simply call it again with the
    # new state and pass the weather argument
    return(run_drive(new_state$D, new_state$YTG, new_state$FP, weather))  
    
  } else {
    
    # otherwise, return the current state to the run_epoch function
    return(list(down = new_state$D, ytg = new_state$YTG, fp = new_state$FP)) 
  }
}

# Test the function
#run_drive(1, 10, 5, weather)  


 
