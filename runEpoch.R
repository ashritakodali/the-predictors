source("runDrive.R")
runEpoch <- function(down, toGo, toEndzone, fieldPos){
  team <- 1
  toEndzone <- runDrive(1, 10, toEndzone, toEndzone)
  count <- 0
  while(toEndzone <= 100 & count != 10){
    team <- team*-1
    count <- count + 1
    toEndzone <- runDrive(1, 10, toEndzone, toEndzone)
  }
  if(toEndzone > 110){
    result <- 3*team
  }
  else if (toEndzone > 100){
    result <- 7*team
  }
  else{
    result <- 0
  }
  return(result)
}