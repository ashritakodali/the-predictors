
source("drive.R")
source("scoring_function.R")

#Simulates sequence of drives
epoch_function <- function(initial_state, max_drives = 10) {
  current_state <- initial_state
  total_score <- 0
  drive_count <- 0
  
  while (drive_count < max_drives) {
    drive_count <- drive_count + 1
    
    #one drive
    drive_result <- drive(current_state)
    
    result <- scoring(drive_result$new_state)
    
    # Update total score and state
    if (!is.na(result$score)) {
      total_score <- total_score + result$score
    }
    current_state <- result$new_state
    
  }
  
  return(total_score)
}

############ TESTING ##################
initial_state <- list(down = 1, ytg = 10, fp = 50)

result <- epoch_function(initial_state)
print(paste("expected score:", result))


