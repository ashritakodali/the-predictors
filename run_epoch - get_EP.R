#setwd("C:/Users/nblau/Downloads")
source("run_drive.R")
source("utils.R")

run_epoch <- function(down, ytg, fp, weather) {

  team_status <- -1

  # set max number of drives
  max_drives <- 10 

  # set drive counter
  cumulative_drives <- 0 

  # initialize flag
  no_score <- TRUE


  print("Remember, the state of the first drive should be the input state.")

  # run loop
  while(no_score & (cumulative_drives < max_drives)) {


    team_status <- team_status * -1

    # increment the run drive
    cumulative_drives <- cumulative_drives + 1

    print(paste0("starting down: ", down, ", ytg: ", ytg, ", fp: ", fp,
                 ", drive number: ", cumulative_drives,
                 ", team status flag: ", team_status,"."))

    # run drive
    tmp_state <- run_drive(down, ytg, fp, weather)

    # reassign variables 
  
    down <- tmp_state$down
    ytg <- tmp_state$ytg
    fp <- tmp_state$fp

    # flip the score flag if there was a score
    no_score <- (fp <= 100)
  }


  score <- team_status * compute_score(fp)


  print(paste0("final fp: ", fp,
               ", scoring drive number: ", cumulative_drives,
               ", team status flag: ", team_status, ", score: ", score, "."))

  # and then finally return the score for the get_EP function to use.

  score

}

################################################################
## GET EP FUNCTION #############################################
################################################################

get_EP <- function(down, ytg, fp, weather) {
  
  n <- 1000 
  
  # create a vector to store our simulations
  points <- rep(NA, n)

  
  for(i in 1:n) {
    points[i] <- run_epoch(down, ytg, fp, weather)
  }
  
  mean(points)
  
}

# List weather conditions 
weather_conditions <- c("sunny", "cloudy", "snowing", "humid", "windy", "foggy", "raining", "indoors", "cold", "hot")

# States we would like to test
test_states <- list(
  list(down = 1, ytg = 10, fp = 5), #red zone
  list(down = 4, ytg = 2, fp = 5), #red zone
  list(down = 3, ytg = 6, fp = 5), #red zone
  list(down = 1, ytg = 10, fp = 15), #red zone
  list(down = 4, ytg = 2, fp = 15), #red zone
  list(down = 3, ytg = 6, fp = 15), #red zone
  list(down = 1, ytg = 10, fp = 40), #non red zone
  list(down = 1, ytg = 10, fp = 80), #non red zone 
  list(down = 4, ytg = 2, fp = 40), #non red zone
  list(down = 4, ytg = 2, fp = 80), #non red zone 
  list(down = 3, ytg = 6, fp = 40), #non red zone
  list(down = 3, ytg = 6, fp = 80) #non red zone 
)

# Initialize empty list to collect results
ep_matrix <- list()

# Define unique combinations
state_labels <- sapply(test_states, function(s) paste0("D", s$down, "_YTG", s$ytg, "_FP", s$fp))

# Loop over weather condition and game state 
for (weather in weather_conditions) {
  ep_values <- numeric()
  for (state in test_states) {
    ep <- get_EP(state$down, state$ytg, state$fp, weather)
    ep_values <- c(ep_values, round(ep, 3))
  }
  ep_matrix[[weather]] <- ep_values
}

# Put into data frame
ep_table <- data.frame(
  State = state_labels,
  do.call(cbind, ep_matrix),
  row.names = NULL
)

#print(ep_table)

library(knitr)
knitr::kable(ep_table, caption = "Expected Points by Weather Condition")
kable(ep_table, caption = "Expected Points by Weather Condition")
library(kableExtra)

ep_table_df <- data.frame(
  State = apply(ep_table, 1, function(x) paste("Down:", x[1])),
  `Sunny` = ep_table[, 2],
  `Cloudy` = ep_table[, 3],
  `Snowing` = ep_table[, 4],
  `Humid` = ep_table[, 5],
  `Windy` = ep_table[, 6],
  `Foggy` = ep_table[, 7],
  `Raining` = ep_table[, 8],
  `Indoors` = ep_table[, 9],
  `Cold` = ep_table[, 10],
  `Hot` = ep_table[, 11]
)
#write.csv(ep_table_df, "weather_table.csv", row.names = TRUE)

# Create kable table
ep_table_df %>%
  kable("html", caption = "Average Expected Points (EP) by Weather Condition and Game State") %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, background = "#f2f2f2")  


