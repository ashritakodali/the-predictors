###########################################################################
#Updating run play function with logistic and multinomial regression models 
###########################################################################

library(tidyverse)
library(nnet)

# Function to simulate going for it on 4th down
go_for_it <- function(down, ytg, newfp, ydsGained){
  # If new field position is 0, it is a touchdown and exit to add points
  if (newfp == 0){
    list(D = 1, YTG = 10, FP = 105, exit_drive = 1)
  }
  # If they gain more yards than they need, return a first down with new field position
  else if (ytg <= ydsGained){
    list(D = 1, YTG = 10, FP = newfp, exit_drive = 0)
  }
  # If they don't gain enough yards, return a first down for the other team
  else{
    list(D = 1, YTG = 10, FP = newfp, exit_drive = 1)
  }
}

## Model success of field goal using logistic regression 

# Fit logistic regression model

#Read in the data
#data <- readRDS("pbp2014-2024.rds")
#View(data)
#filter data for field goals and convert field goal result to binary variable
data_fg <- data %>%
  filter(play_type == "field_goal") %>% 
  mutate(fg_made = ifelse(field_goal_result == "made", 1, 0)) 
  
# logistic regression model using field position 
fg_model <- glm(fg_made ~ yardline_100, data = data_fg, family = binomial)

# Function to predict field goal probability
predict_fg <- function(fp) {
  prob <- predict(fg_model, newdata = data.frame(yardline_100 = fp), type = "response")
  prob
}

# Function to simulate a field goal attempt
field_goal <- function(down, ytg, fp){
  # If attempting field goal, sample to see if they make it or not
  #predict prob of making field goal
  fgMakeRate <- predict_fg(fp)
  result <- sample(c('made', 'miss'), 1, prob=c(fgMakeRate, 1-fgMakeRate))
  # If they make the field goal, set field position to 115 and leave function
  if (result == 'made'){
    list(D = 1, YTG = 10, FP = 115, exit_drive = 1)
  }
  # If they miss the field goal, switch possessions and set new field position
  else{
    list(D = 1, YTG = 10, FP = 93 - fp, exit_drive = 1)
  }
}


#### filtering fourth down data for punt, go for it, and field goal
# grouped "run" and "pass" into "go for it" category 
fourth_down_data <- data %>%
  filter(down == 4, play_type %in% c("field_goal", "punt", "pass", "run")) %>%
  mutate(
    play_type = case_when(
      play_type %in% c("pass", "run") ~ "go_for_it",  # Group passes and runs as "go_for_it"
      TRUE ~ play_type  # Keep "field_goal" and "punt" unchanged
    )
  )

# Fit multinomial logistic regression
fourth_down_model <- multinom(play_type ~ yardline_100 + ydstogo, data = fourth_down_data)

# function to predict play decision
predict_fourth_down_decision <- function(fp, ytg) {
  probs <- predict(fourth_down_model, newdata = data.frame(yardline_100 = fp, ydstogo = ytg), type = "probs")
  decision <- sample(c("go_for_it", "punt", "field_goal"), 1, prob = probs)
  return(decision)
}

# Function to simulate punting
punt <- function(down, ytg, fp){
  puntDistance <- 30
  list(D = 1, YTG = 10, FP = 100+puntDistance-fp, exit_drive = 1)
}

#Actual function to run each play
run_play <- function(down, ytg, fp){
  turnoverRate <- 0.05
  ydMean <- 3
  ydSD <- 2
  
  # Check for turnover
  check_turnover <- sample(c('yes', 'no'), 1, prob = c(turnoverRate, 1 - turnoverRate))
  if (check_turnover == 'yes'){
    return(list(D = 1, YTG = 10, FP = fp, exit_drive = 1))
  }
  
  # No turnover, determine yards gained
  ydsGained <- round(rnorm(1, ydMean, ydSD), 0)
  newfp <- fp - ydsGained
  
  # If 1st, 2nd, or 3rd down
  if (down < 4){
    if (newfp == 0) return(list(D = 1, YTG = 10, FP = 105, exit_drive = 1))  # Touchdown
    else if (ytg <= ydsGained) return(list(D = 1, YTG = 10, FP = newfp, exit_drive = 0))  # First down
    else return(list(D = down + 1, YTG = ytg - ydsGained, FP = newfp, exit_drive = 0))  # Next down
  }
  
  # Making 4th down decision based on multinomial models 
  decision <- predict_fourth_down_decision(fp, ytg)
  
  if (decision == "field_goal") return(field_goal(down, ytg, fp))
  else if (decision == "punt") return(punt(down, ytg, fp))
  else return(go_for_it(down, ytg, newfp, ydsGained))  # Go for it
}

#############################################
#Testing function 
#############################################
# Example data
tests <- data.frame(
  yardline_100 = c(25, 50, 75), 
  yds_to_go = c(1, 5, 10)        
)

# Predict for test cases
test_predictions <- apply(tests, 1, function(row) {
  predict_fourth_down_decision(row["yardline_100"], row["yds_to_go"])
})

tests$predicted_decision <- test_predictions
print(tests)

