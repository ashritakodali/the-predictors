source("runDrive.R")          #link file with function to run a drive
runEpoch <- function(down, toGo, toEndzone, fieldPos){      #Epoch function
  team <- 1               #Preset team variable as team 1
  toEndzone <- runDrive(1, 10, toEndzone, toEndzone)    #Run drive function and store result
  count <- 0              #Initialize count variable
  max <- 10               #Define max # of drives before ending function
  while(toEndzone <= 100 & count != max){    #Check there is no score and hasn't reached max drives
    team <- team*-1       #Switch possession
    count <- count + 1    #Increment count variable by 1
    toEndzone <- runDrive(1, 10, toEndzone, toEndzone)    #Rerun drive function with new field position
  }
  if(toEndzone > 110){    #Check if ball is past 110 yards (field goal)
    result <- 3*team      #Assign 3 points multiplied by possession variable
  }
  else if (toEndzone > 100){    #Check if ball is between 100 and 110 yards (touchdown)
    result <- 7*team            #Assign 7 points multipled by possession variable
  }
  else{
    result <- 0           #Assign 0 points if reach max drives
  }
  result                  #Print result
}