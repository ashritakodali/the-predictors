#setwd("C:/Users/nblau/Downloads")
#samplingData <- readRDS('pbp2014-2024.rds')
library(tidyverse)
#setwd("C:/Users/nblau/Documents/Stat 4800/project")
source('getyg_weather.R')

# Fit a logistic regression model for field goal attempts
fgData <- samplingData %>% filter(play_type == 'field_goal')
fgData$field_goal_result <- ifelse(fgData$field_goal_result == 'blocked', 'missed', fgData$field_goal_result)
fgData$field_goal_factor <- as.numeric(factor(fgData$field_goal_result, levels = c("missed", "made"))) - 1
fgLogisReg <- glm(field_goal_factor ~ yardline_100, data = fgData, family = binomial)

library(nnet)

# Fit a multinomial logistic regression model for fourth down choices
fourthDownData <- samplingData %>% filter(down == 4 & play_type %in% c('punt', 'field_goal', 'pass', 'run'))
fourthDownData$fourth_down_choice <- ifelse(fourthDownData$play_type %in% c('pass', 'run'), 'go_for_it', fourthDownData$play_type)
fourthDownModel <- multinom(fourth_down_choice ~ yardline_100 + ydstogo, data = fourthDownData)

# Function to simulate going for it on 4th down
go_for_it <- function(down, ytg, newfp, ydsGained){
  # If new field position is 0, it is a touchdown and exit to add points
  if (newfp == 0){
    list(D = 1, YTG = 0, FP = 105, exit_drive = 1)
  }
  # If they gain more yards than they need, return a first down with new field position
  else if (ytg <= ydsGained){
    list(D = 1, YTG = 10, FP = newfp, exit_drive = 0)
  }
  # If they don't gain enough yards, return a first down for the other team
  else{
    list(D = 1, YTG = 10, FP = 100-newfp, exit_drive = 1)
  }
}

# Function to simulate a field goal attempt
field_goal <- function(down, ytg, fp){
  # If attempting field goal, use model to see
  fgMakeRate <- predict(fgLogisReg, newdata = data.frame(yardline_100 = fp), type = "response")
  result <- rbinom(1, 1, fgMakeRate)
  # If they make the field goal, set field position to 115 and leave function
  if (result == 1){
    list(D = 1, YTG = 10, FP = 115, exit_drive = 1)
  }
  # If they miss the field goal, switch possessions and set new field position
  else{
    list(D = 1, YTG = 10, FP = 93 - fp, exit_drive = 1)
  }
}

# Function to simulate punting
punt <- function(down, ytg, fp){
  puntDistance <- 30
  list(D = 1, YTG = 10, FP = 100+puntDistance-fp, exit_drive = 1)
}

#Actual function to run each play
run_play2 <- function(down, ytg, fp, weather){
  print(paste("Weather condition:", weather))
  # Set turnover rate, mean and standard deviation for yards gained distribution
  turnoverRate <- 0.05
  ydMean <- 3
  ydSD <- 2

  # Sample to see if a turnover occurs
  check_turnover <- 'no'
  if (check_turnover == 'yes'){
    list(D = 1, YTG = 10, FP = 100-fp, exit_drive=1)
  }
  # No turnover
  else{
    
    result <- getyg(fp, weather)
    
    ydsGained <- as.numeric(result[1])
    print(ydsGained)
    
    #debug
    print(class(fp))
    
    play_choice <- result[2]
    
    #debug
    print(class(play_choice))

    if (is.na(ydsGained)) {
      stop("ydsGained could not be converted to numeric!")
    }

    fp <- as.numeric(fp)

    if (is.na(fp)) {
      stop("fp is not numeric!")
    }
    
    # Set new field position based on yards gained
    newfp <- fp - ydsGained
    
    # Touchdown logic
    if (newfp <= 0) {
      return(list(D = 1, YTG = 0, FP = 105, exit_drive = 1))  # Signal TD
    }
    
    # Cap new field position between 0 and 100 
    newfp <- max(min(newfp, 100), 0)
    
    # Check the new field position
    print(newfp)
    
    if (play_choice %in% c('Run Fumble', 'Pass Interception', 'Pass Fumble', 'QB Sack Fumble')){
      list(D = 1, YTG = 10, FP = 100-fp, exit_drive=1)
    }
    
    # Check if it is 1st, 2nd, or 3rd down
    if (down < 4){
      # If new field position is 0, it is a touchdown and exit to add points
      if (newfp == 0){
        list(D = 1, YTG = 0, FP = 105, exit_drive = 1)
      }
      # If they gain more yards than they need, return a first down with new field position
      else if (ytg <= ydsGained){
        list(D = 1, YTG = 10, FP = newfp, exit_drive = 0)
      }
      # If they don't gain enough yards, return the next down with the new field position
      # and new yards to gain but keep possession the same
      else{
        list(D = down+1, YTG = ytg-ydsGained, FP = newfp, exit_drive = 0)
      }
    }
    
    # If 4th down, follow this progression
    else{
      # Predict probabilities for new data
      probs <- predict(fourthDownModel, newdata = data.frame(yardline_100 = fp, ydstogo = ytg), type = "probs")
      result <- sample(c('field goal', 'go for it', 'punt'), 1, prob = probs)
      if (result == 'field goal'){
        field_goal(down, ytg, fp)
      }
      else if (result == 'go for it'){
        go_for_it(down, ytg, newfp, ydsGained)
      }
      else{
        punt(down, ytg, fp)
      }
    }
  }
}





