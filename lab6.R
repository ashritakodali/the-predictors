library(tidyverse)
nhlData <- read.csv('nhl_pbp20162017.csv')

#Question 1 - Shot Model based on Time and Score Differential
convert_to_minutes <- function(time_str) {
  parts <- strsplit(time_str, ":")[[1]]  # Split string by ":"
  minutes <- as.numeric(parts[1])       # Extract minutes
  seconds <- as.numeric(parts[2])       # Extract seconds
  return(minutes + (seconds / 60))      # Convert to total minutes
}

# Apply function to create new column for time category
nhlData <- nhlData %>%
  mutate(
    time_minutes = sapply(Time_Elapsed, convert_to_minutes),  # Convert to total minutes
    category = case_when(
      Period == 1 & time_minutes < 5  ~ 1,
      Period == 1 & time_minutes < 10 ~ 2,
      Period == 1 & time_minutes < 15 ~ 3,
      Period == 1 ~ 4,
      Period == 2 & time_minutes < 5  ~ 5,
      Period == 2 & time_minutes < 10 ~ 6,
      Period == 2 & time_minutes < 10 ~ 7,
      Period == 2 ~ 8,
      Period == 3 & time_minutes < 5  ~ 9,
      Period == 3 & time_minutes < 10 ~ 10,
      Period == 3 & time_minutes < 10 ~ 11,
      Period == 3 ~ 12,
      Period >= 4 ~ 13,
      TRUE ~ 14
    )
  )

#Turn time category column into a factor variable
nhlData$category <- as.factor(nhlData$category)
#Extract only shots data
shotsData <- nhlData %>% filter(Event %in% c('BLOCK', 'SHOT', 'MISS', 'GOAL'))
#Create score differential column
shotsData$differential <- ifelse(shotsData$Ev_Team==shotsData$Away_Team, shotsData$Away_Score-shotsData$Home_Score, shotsData$Home_Score-shotsData$Away_Score)
#Create poisson data by grouping by Game, Event Team, and Time Category to get
#number of shots by each team per time category and average score differential
q1ShotsData <- shotsData %>% group_by(Game_Id, Ev_Team, category) %>% summarise(numShots = n(), scoreDiff = mean(differential))
#Create shots model using poisson regression taking in score differential and time category
q1_model <- glm(numShots ~ scoreDiff  + category, family = poisson, data = q1ShotsData)
summary(q1_model)


# Question 2 - Shot Model Based on Location and Score Differential
#Ignore all data that has NA values for shot location
q2shotsData <- shotsData %>% filter(xC != 'NA')
#Group the data into 32 categories
q2shotsData <- q2shotsData %>%
  mutate(
    location_category = case_when(
      xC <= -75 & yC <= -25  ~ 1,
      xC <= -75 & yC <= 0  ~ 2,
      xC <= -75 & yC <= 25  ~ 3,
      xC <= -75 & yC <= 50  ~ 4,
      xC <= -50 & yC <= -25  ~ 5,
      xC <= -50 & yC <= 0  ~ 6,
      xC <= -50 & yC <= 25  ~ 7,
      xC <= -50 & yC <= 50  ~ 8,
      xC <= -25 & yC <= -25  ~ 9,
      xC <= -25 & yC <= 0  ~ 10,
      xC <= -25 & yC <= 25  ~ 11,
      xC <= -25 & yC <= 50  ~ 12,
      xC <= 0 & yC <= -25  ~ 13,
      xC <= 0 & yC <= 0  ~ 14,
      xC <= 0 & yC <= 25  ~ 15,
      xC <= 0 & yC <= 50  ~ 16,
      xC <= 25 & yC <= -25  ~ 17,
      xC <= 25 & yC <= 0  ~ 18,
      xC <= 25 & yC <= 25  ~ 19,
      xC <= 25 & yC <= 50  ~ 20,
      xC <= 50 & yC <= -25  ~ 21,
      xC <= 50 & yC <= 0  ~ 22,
      xC <= 50 & yC <= 25  ~ 23,
      xC <= 50 & yC <= 50  ~ 24,
      xC <= 75 & yC <= -25  ~ 25,
      xC <= 75 & yC <= 0  ~ 26,
      xC <= 75 & yC <= 25  ~ 27,
      xC <= 75 & yC <= 50  ~ 28,
      yC <= -25  ~ 29,
      yC <= 0  ~ 30,
      yC <= 25  ~ 31,
      yC <= 50  ~ 32,
      TRUE ~ 33
    )
  )
#Change the location variable to a factor
q2shotsData$location_category <- as.factor(q2shotsData$location_category)
#Create poisson data by grouping by Game, Event Team, and Time Category to get
#number of shots by each team per location category and average score differential
q2shotsData <- q2shotsData %>% group_by(Game_Id, Ev_Team, location_category) %>% summarise(numShots = n(), scoreDiff = mean(differential))
#Create shots model using poisson regression taking in score differential and location category
q2_model <- glm(numShots ~ scoreDiff  + location_category, family = poisson, data = q2shotsData)
summary(q2_model)

# Question 3 - Success Rate Model by Time, Score Differential, and Total Shots
#Extract data that only includes shots on goal
onGoalData <- shotsData %>% filter(Event %in% c('MISS', 'GOAL'))
#Get number of shots on goal grouped by Game, Event Team, and Time Category
q3onGoalData <- onGoalData %>% group_by(Game_Id, Ev_Team, category) %>% summarise(numOnGoal = n())
#Combine the number of shots and number of shots on goal data into one data frame
q3Data <- left_join(q1ShotsData, q3onGoalData, by=c('Game_Id'='Game_Id', 'Ev_Team'='Ev_Team', 'category'='category'))
#Replace all NAs (rows with no shots on goal) with 0
q3Data[is.na(q3Data)] <- 0
#Calculate the success rate for each row (shots on goal / total shots)
q3Data$Success_Rate <- q3Data$numOnGoal/q3Data$numShots
#Create success shots model using binomial regression with the link function being
#logit, uses score differential, time category, and total shots to predict success rate
q3_model <- glm(Success_Rate ~ scoreDiff  + category + numShots, family = binomial(link = "logit"), data = q3Data, weights = numShots)
summary(q3_model)
