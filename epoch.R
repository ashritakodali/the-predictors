source("drive.R")
simulate_epoch <- function(down, ytg, fp) {
  state <- list(down = down, ytg = ytg, fp = fp)
  score <- NA
  team <- 1  # starting with the reference team
  # positive scores mean reference team scored
  # negative scores mean opponent scored
  
  max_drives <- 100 # ensures we are not stuck in a loop
  drive_count <- 0
  
  while (is.na(score) & drive_count < max_drives) {
    state <- simulate_drive(state)
    if (state$fp > 100 & state$fp <= 110) {
      score <- 3 * team
    } else if (state$fp > 110) {
      score <- 7 * team
    }
    
    team <- -team # now the opposing team will go
    drive_count <- drive_count + 1
  }
  
  score
}