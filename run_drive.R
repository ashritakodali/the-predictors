source('run_play.R')

# This is the run_drive function.  For now, our only goal is to make sure that
# run_epoch is working, so we don't really need anything like a realistic
# run_drive function.  We just need one that will give us outcomes we can
# play with.  The run_drive function will output a state.

run_drive <- function(D, YTG, FP){
  
  # get new state
  new_state <- run_play(D, YTG, FP)
  
  # Check if we should return the state or run a new play
  if(new_state$exit_drive==0){
    
    # if we should stay with the current drive, simply call it again with the
    # new state
    run_drive(new_state$D, new_state$YTG, new_state$FP)
    
  } else {
    
    # otherwise, return the current state to the run_epoch function
    list(down=new_state$D, ytg=new_state$YTG, fp=new_state$FP)
    
  }
  
}
