library(tidyverse)
library(dplyr)
library(nnet)
library(dplyr)
library(stringr)
library(MASS)
library(ggplot2)

# read in sampling data
#setwd("C:/Users/nblau/Downloads")
samplingData <- readRDS('pbp2014-2024.rds')

# weather column cleaing code
samplingData <- samplingData %>%
  mutate(
    description = str_extract(weather, "^[A-Za-z ]+?(?= Temp:)"),
    humidity = str_extract(weather, "(Humidity:|H:)\\s*(\\d+)%") %>%
      str_extract("\\d+") %>%
      as.numeric(),
    wind_dir = str_extract(weather, "(Wind:|W:)\\s*([A-Z]{1,20})") %>%
      str_extract("[A-Z]{1,3}"),
    wind_speed = str_extract(weather, "(Wind:|W:).*?(\\d+)\\s*mph") %>%
      str_extract("\\d+") %>%
      as.numeric()
  )

samplingData <- samplingData %>%
  mutate(
    description_lower = str_to_lower(description),
    
    is_sunny = if_else(str_detect(description_lower, "sun|clear|fair|dry"), 1, 0),
    is_cloudy = if_else(str_detect(description_lower, "cloud|clound|coudy|clouidy|overcast") &
                          !str_detect(description_lower, "sunny with some clouds"), 1, 0),
    is_snowing = if_else(str_detect(description_lower, "snow|flurries|freezing rain|sleet"), 1, 0),
    is_haze_fog = if_else(str_detect(description_lower, "haze|fog|mist|hazey|shallow fog|hazy"), 1, 0),
    is_raining = if_else(str_detect(description_lower, "rain|shower|drizzle|storm") & 
                           !str_detect(description_lower, "zero percent chance of rain|no chance of rain|snow showers|freezing rain|mostly cloudy with light rain forecasted|cloudy with rain likely"), 1, 0),
    is_indoors = if_else(str_detect(description_lower, "indoor|control|controlled"),1,0),
    is_hot = if_else(str_detect(description_lower, "hot|warm|very warm|unseasonably warm"), 1, 0),
    is_cold = if_else(str_detect(description_lower, "cold|freezing|frigid|very cold|bitterly cold|chilly"), 1, 0),
    is_humid = if_else(humidity >= 65, 1, 0),
    is_windy = if_else(wind_speed >= 15, 1, 0)
  )

data = samplingData

#Find the distributions 

############
## 1) IS HOT 
############

data_hot = data %>% filter(is_hot == 1)
#View(data_hot) 
nrow(data_hot) #n=1034

##############################################
############# RED ZONE PLAYS #################
##############################################

data_hot_red = data_hot %>% filter(yardline_100<=20)
#View(data_hot_red) #n=162
nrow(data_hot_red) #n=162

# data_hot %>%
#   filter(is.na(yardline_100)) %>%
#   View() # n=66

################
## 1) Run Fumble 
################
data_hot_red_run_fumble = data_hot_red %>% filter(rush_attempt == 1 &
                                                    fumble_lost == 1) 
#View(data_hot_red_run_fumble)  
nrow(data_hot_red_run_fumble) #n=0 


# Getting distribution from all run fumbles in the red zone since our n=0
data_run_fumble_red = data %>% filter(rush_attempt == 1 & fumble_lost == 1 & yardline_100 <=20)

ggplot(data_run_fumble_red, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution
mean(data_run_fumble_red$yards_gained)
sd(data_run_fumble_red$yards_gained)

#rnorm(1,mean=1.2,sd=3.8)

#################
## 2) Run Success
#################
data_hot_red_run_success = data_hot_red %>%filter(play_type=="run" &
                                                    (fumble_lost==0))
#View(data_hot_red_run_success) 
nrow(data_hot_red_run_success) #n=62

ggplot(data_hot_red_run_success, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution 
mean(data_hot_red_run_success$yards_gained)
sd(data_hot_red_run_success$yards_gained)

#rnorm(1,mean=2.61,sd=2.91)

###########################################
## 3) Pass Incomplete - 0 yards gained 
###########################################

###############
## 4) Pass Success
###############
data_hot_red_pass_success = data_hot_red %>%filter(complete_pass==1)
#View(data_hot_red_pass_success) 
nrow(data_hot_red_pass_success) #n=28

ggplot(data_hot_red_pass_success, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution 
mean(data_hot_red_pass_success$yards_gained)
sd(data_hot_red_pass_success$yards_gained)

#rnorm(1,mean=6.61,sd=3.63)

######################################
## 5) Pass Interception - 0 yards gained
######################################

#################
## 6) Pass Fumble 
#################
data_hot_red_pass_fumble = data_hot_red %>%filter(pass_attempt==1 &
                                                    fumble_lost==1)

#View(data_hot_red_pass_fumble) 
nrow(data_hot_red_pass_fumble) #n=1

# Getting distribution from all run fumbles in the red zone since our n=1
data_pass_fumble_red = data %>% filter(pass_attempt == 1 & fumble_lost == 1 & yardline_100 <=20)

ggplot(data_pass_fumble_red, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution

mean(data_hot_red_pass_fumble$yards_gained) #hot weather value 

mean(data_pass_fumble_red$yards_gained)
sd(data_pass_fumble_red$yards_gained)

# Take mean value between -10 and -2, half the sd 

# rnorm(1,mean= -6,sd=3.90)

#############
## 7) QB Sack
#############
data_hot_red_sack = data_hot_red %>%filter(sack==1)
#View(data_hot_red_sack) 
nrow(data_hot_red_sack) #n=4

ggplot(data_hot_red_sack, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution
mean(data_hot_red_sack$yards_gained)
sd(data_hot_red_sack$yards_gained)

# rnorm(1,mean= -6.25,sd=3.5)

####################
## 8) QB Sack Fumble
####################
data_hot_red_sack_fumble = data_hot_red %>%filter(sack==1 & fumble_lost == 1)
#View(data_hot_red_sack_fumble) 
nrow(data_hot_red_sack_fumble) #n=1

# Getting distribution from all sack fumbles in the red zone since our n=1
data_pass_sack_fumble_red = data %>% filter(sack == 1 & fumble_lost == 1 & yardline_100 <=20)


ggplot(data_pass_sack_fumble_red, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()


# Gamma distribution - flip because of left skew 
flip <- -1 * data_pass_sack_fumble_red$yards_gained
# Remove zeros
flip_no_zero <- flip[flip > 0]
fit_gamma <- fitdistr(flip_no_zero, "gamma")
fit_gamma$estimate  # gives shape and rate (mean = shape/rate)

# -1*rgamma(1,shape= 2.57,rate=0.36)

# Shift distribution towards the data point we do have 
shift = data_pass_sack_fumble_red$yards_gained - 2 

flip <- -1 * shift
# Remove zeros
flip_no_zero <- flip[flip > 0]
fit_gamma <- fitdistr(flip_no_zero, "gamma")
fit_gamma$estimate 

# -1*rgamma(1,shape= 3.59,rate=0.41)

########
## RATES 
########
data_hot_red_incomplete_pass = data_hot_red %>%filter(incomplete_pass == 1) #n=18
nrow(data_hot_red_incomplete_pass)
data_hot_red_pass_interception = data_hot_red %>%filter(interception == 1) #n=1
nrow(data_hot_red_pass_interception)

counts <- c(
  nrow(data_hot_red_run_fumble),
  nrow(data_hot_red_run_success),
  nrow(data_hot_red_incomplete_pass),
  nrow(data_hot_red_pass_success),
  nrow(data_hot_red_pass_interception),
  nrow(data_hot_red_pass_fumble),
  nrow(data_hot_red_sack),
  nrow(data_hot_red_sack_fumble)
  
)

play_type_rates <- counts / sum(counts)
play_type_rates

original <- c(0.000000000, 0.539130435, 0.156521739, 0.243478261,
              0.008695652, 0.008695652, 0.034782609, 0.008695652)

# Give first play small probability of happening (0.001) and adjust other probabilities accordingly
adjusted <- original
adjusted[1] <- 0.001
adjusted[2:8] <- original[2:8] * (0.999 / sum(original[2:8]))
round(adjusted, 3)
# 0.001 0.539 0.156 0.243 0.009 0.009 0.035 0.009

##############################################
########### NON RED ZONE PLAYS ###############
##############################################

data_hot_non_red = data_hot %>% filter(yardline_100>20)
#View(data_hot_non_red) 
nrow(data_hot_non_red) #n=806

################
## 1) Run Fumble 
################
data_hot_non_red_run_fumble = data_hot_non_red %>% filter(rush_attempt == 1 &
                                                            fumble_lost == 1) 
#View(data_hot_non_red_run_fumble) 
nrow(data_hot_non_red_run_fumble) #n=3

ggplot(data_hot_non_red_run_fumble, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution 
mean(data_hot_non_red_run_fumble$yards_gained)
sd(data_hot_non_red_run_fumble$yards_gained)

#rnorm(1,mean=3.67,sd=3.51)

#################
## 2) Run Success
#################
data_hot_non_red_run_success = data_hot_non_red %>% filter(play_type=="run" &
                                                             (fumble_lost==0))
#View(data_hot_non_red_run_success) 
nrow(data_hot_non_red_run_success) #n=244


ggplot(data_hot_non_red_run_success, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Gamma distribution - need to shift values due to negative numbers 

# Find the minimum value
min_val <- min(data_hot_non_red_run_success$yards_gained, na.rm = TRUE)
# Shift the data so the smallest value is just above 0
yards_shifted_run <- data_hot_non_red_run_success$yards_gained - min_val + 1

fit_gamma <- fitdistr(yards_shifted_run, "gamma")
fit_gamma$estimate  # gives shape and rate (mean = shape/rate)

#rgamma(1,shape=5.32,rate=0.38)

###########################################
## 3) Pass Incomplete - 0 yards gained 
###########################################

##################
## 4) Pass Success
##################
data_hot_non_red_pass_success = data_hot_non_red %>%filter(complete_pass==1)
#View(data_hot_non_red_pass_success) 
nrow(data_hot_non_red_pass_success) #n=228

ggplot(data_hot_non_red_pass_success, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Gamma distribution - need to shift values due to negative numbers 
# Find the minimum value
min_val <- min(data_hot_non_red_pass_success$yards_gained, na.rm = TRUE)
# Shift the data so the smallest value is just above 0
yards_shifted <- data_hot_non_red_pass_success$yards_gained - min_val + 1

fit_gamma <- fitdistr(yards_shifted, "gamma")
fit_gamma$estimate  # gives shape and rate (mean = shape/rate)

#rgamma(1,shape=3.90,rate=0.21)

########################################
## 5) Pass Interception - 0 yards gained
########################################

#################
## 6) Pass Fumble 
#################
data_hot_non_red_pass_fumble = data_hot_non_red %>%filter(pass_attempt==1 &
                                                            fumble_lost==1)

#View(data_hot_non_red_pass_fumble) 
nrow(data_hot_non_red_pass_fumble) #n=1


# Getting distribution from all run fumbles in the red zone since our n=1
data_pass_fumble_red = data %>% filter(pass_attempt == 1 & fumble_lost == 1 & yardline_100 >20)


ggplot(data_pass_fumble_red , aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution
# Shift distribution towards the data point we do have 
shift = data_pass_fumble_red$yards_gained + 2 

mean(shift)
sd(shift)

# rnorm(1,mean= 2.73 ,sd= 11.50)

#############
## 7) QB Sack
#############

data_hot_non_red_sack = data_hot_non_red %>%filter(sack==1)
#View(data_hot_non_red_sack) 
nrow(data_hot_non_red_sack) #n=28 

ggplot(data_hot_non_red_sack, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution
mean(data_hot_non_red_sack$yards_gained)
sd(data_hot_non_red_sack$yards_gained)

# rnorm(1,mean= -6.93,sd=2.92)

####################
## 8) QB Sack Fumble
####################
data_hot_non_red_sack_fumble = data_hot_non_red %>%filter(sack==1 & fumble_lost == 1)
#View(data_hot_non_red_sack_fumble) 
nrow(data_hot_non_red_sack_fumble) #n=0

# Getting distribution from all sack fumbles not in the red zone since our n=0
data_sack_fumble_non_red = data %>% filter(sack == 1 & fumble_lost == 1 & yardline_100 >20)

ggplot(data_sack_fumble_non_red, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

flip <- -1 * data_sack_fumble_non_red$yards_gained
# Remove zeros
flip_no_zero <- flip[flip > 0]
fit_gamma <- fitdistr(flip_no_zero, "gamma")
fit_gamma$estimate 

# cut the sd in half
# -1*rgamma(1,shape= 11.63,rate=1.42)

########
## RATES 
########
data_hot_non_red_incomplete_pass = data_hot_non_red %>%filter(incomplete_pass == 1) #n=119
data_hot_non_red_pass_interception = data_hot_non_red %>%filter(interception == 1) #n=7

counts <- c(
  nrow(data_hot_non_red_run_fumble),
  nrow(data_hot_non_red_run_success),
  nrow(data_hot_non_red_incomplete_pass),
  nrow(data_hot_non_red_pass_success),
  nrow(data_hot_non_red_pass_interception),
  nrow(data_hot_non_red_pass_fumble),
  nrow(data_hot_non_red_sack),
  nrow(data_hot_non_red_sack_fumble)
  
)

play_type_rates <- counts / sum(counts)
play_type_rates


original <- c(0.004761905,0.387301587, 0.188888889, 0.361904762, 0.011111111, 0.001587302, 0.044444444, 0.000000000)

# Give last play type small probability of happening (0.001) and adjust other probabilities accordingly
adjusted <- original
adjusted[8] <- 0.001
adjusted[1:7] <- original[1:7] * (0.999 / sum(original[1:7]))
round(adjusted, 3)

#0.005 0.387 0.189 0.362 0.011 0.002 0.044 0.001

############
## 2) IS COLD
############

data_cold = data %>% filter(is_cold == 1)
#View(data_cold) 
nrow(data_cold) #n=4050

##############################################
############# RED ZONE PLAYS #################
##############################################

data_cold_red = data_cold %>% filter(yardline_100<=20)
#View(data_cold_red) 
nrow(data_cold_red) #n=589

# data_cold %>%
#   filter(is.na(yardline_100)) %>%
#   View() # n=267

################
## 1) Run Fumble 
################
data_cold_red_run_fumble = data_cold_red %>% filter(rush_attempt == 1 &
                                                    fumble_lost == 1) 
#View(data_cold_red_run_fumble)
nrow(data_cold_red_run_fumble) # n=1

# Getting distribution from all run fumbles in the red zone since our n=1
data_run_fumble_red = data %>% filter(rush_attempt == 1 & fumble_lost == 1 & yardline_100 <=20)

ggplot(data_run_fumble_red, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution 

# Shift toward the data point we do have(-3)
shift = data_run_fumble_red$yards_gained-2
mean(shift)
sd(shift)

#rnorm(1,mean=-0.80,sd=3.80)

#################
## 2) Run Success
#################
data_cold_red_run_success = data_cold_red %>%filter(play_type=="run" &
                                                    (fumble_lost==0))
#View(data_cold_red_run_success)
nrow(data_cold_red_run_success) #n=183

 
ggplot(data_cold_red_run_success, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution 
mean(data_cold_red_run_success$yards_gained)
sd(data_cold_red_run_success$yards_gained)

#rnorm(1,mean=2.78,sd=3.40)

###########################################
## 3) Pass Incomplete - 0 yards gained 
###########################################

##################
## 4) Pass Success
##################
data_cold_red_pass_success = data_cold_red %>%filter(complete_pass==1)
#View(data_cold_red_pass_success) 
nrow(data_cold_red_pass_success) #124

ggplot(data_cold_red_pass_success, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution 
mean(data_cold_red_pass_success$yards_gained)
sd(data_cold_red_pass_success$yards_gained)

#rnorm(1,mean=6.98,sd=5.29)

######################################
## 5) Pass Interception - 0 yards gained
######################################

#################
## 6) Pass Fumble 
#################
data_cold_red_pass_fumble = data_cold_red %>%filter(pass_attempt==1 &
                                                    fumble_lost==1)
#View(data_cold_red_pass_fumble) 
nrow(data_cold_red_pass_fumble) #n=0

# Getting distribution from all pass fumbles in the red zone since our n=0
data_pass_fumble_red = data %>% filter(pass_attempt == 1 & fumble_lost == 1 & yardline_100 <=20)

ggplot(data_pass_fumble_red, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

mean(data_pass_fumble_red$yards_gained)
sd(data_pass_fumble_red$yards_gained)

# halve the sd 
#rnorm(1, mean = -2.03, sd = 3.90)

#############
## 7) QB Sack
#############
data_cold_red_sack = data_cold_red %>%filter(sack==1)
#View(data_cold_red_sack) 
nrow(data_cold_red_sack) #n=7

ggplot(data_cold_red_sack, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Gamma distribution - flip because of left skew 
flip <- data_cold_red_sack$yards_gained * -1

fit_gamma <- fitdistr(flip, "gamma")
fit_gamma$estimate  # gives shape and rate (mean = shape/rate)

# -1*rgamma(1,shape=5.30,rate=0.38)

####################
## 8) QB Sack Fumble
####################
data_cold_red_sack_fumble = data_cold_red %>%filter(sack==1 & fumble_lost == 1)
#View(data_cold_red_sack_fumble)  
nrow(data_cold_red_sack_fumble) #n=0

# Getting distribution from all sack fumbles in the red zone since our n=0
data_sack_fumble_red = data %>% filter(sack == 1 & fumble_lost == 1 & yardline_100 <=20)

ggplot(data_sack_fumble_red , aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Gamma distribution - flip because of left skew 
flip <- data_sack_fumble_red$yards_gained * -1

fit_gamma <- fitdistr(flip, "gamma")
fit_gamma$estimate  # gives shape and rate (mean = shape/rate)

# halve the sd 
# -1*rgamma(1,shape=11.64,rate=1.42)

########
## RATES 
########
data_cold_red_incomplete_pass = data_cold_red %>%filter(incomplete_pass == 1) #n=84
nrow(data_cold_red_incomplete_pass)
data_cold_red_pass_interception = data_cold_red %>%filter(interception == 1) #n=4
nrow(data_cold_red_pass_interception)

counts <- c(
  nrow(data_cold_red_run_fumble),
  nrow(data_cold_red_run_success),
  nrow(data_cold_red_incomplete_pass),
  nrow(data_cold_red_pass_success),
  nrow(data_cold_red_pass_interception),
  nrow(data_cold_red_pass_fumble),
  nrow(data_cold_red_sack),
  nrow(data_cold_red_sack_fumble)
  
)

play_type_rates <- counts / sum(counts)
play_type_rates

# adjust rates so zero probability ones have a small probability (0.001) and adjust other rates accordingly

original <- c(0.002481390, 0.454094293, 0.208436725, 0.307692308,
              0.009925558, 0.000000000, 0.017369727, 0.000000000)

# Assign 0.001 to the two zero entries 
adjusted <- original
adjusted[c(6, 8)] <- 0.001

# Remaining indices to scale
to_scale <- setdiff(1:8, c(6, 8))

adjusted[to_scale] <- original[to_scale] * (0.998 / sum(original[to_scale]))

round(adjusted, 3)

#0.002 0.453 0.208 0.307 0.010 0.001 0.017 0.001

##############################################
########### NON RED ZONE PLAYS ###############
##############################################

data_cold_non_red = data_cold %>% filter(yardline_100>20)
#View(data_cold_non_red) 
nrow(data_cold_non_red) #n=3194

################
## 1) Run Fumble 
################
data_cold_non_red_run_fumble = data_cold_non_red %>% filter(rush_attempt == 1 &
                                                            fumble_lost == 1)
#View(data_cold_non_red_run_fumble) 
nrow(data_cold_non_red_run_fumble) #n=4

ggplot(data_cold_non_red_run_fumble, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Gamma distribution - need to shift values due to negative numbers 

# Find the minimum value
min_val <- min(data_cold_non_red_run_fumble$yards_gained, na.rm = TRUE)
# Shift the data so the smallest value is just above 0
yards_shifted_run <- data_cold_non_red_run_fumble$yards_gained - min_val + 1

fit_gamma <- fitdistr(yards_shifted_run, "gamma")
fit_gamma$estimate  # gives shape and rate (mean = shape/rate)

#rgamma(1,shape=1.27,rate=0.13)

#################
## 2) Run Success
#################
data_cold_non_red_run_success = data_cold_non_red %>% filter(play_type=="run" &
                                                             (fumble_lost==0))
#View(data_cold_non_red_run_success) 
nrow(data_cold_non_red_run_success) #n=986

ggplot(data_cold_non_red_run_success, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Gamma distribution - need to shift values due to negative numbers 

# Find the minimum value
min_val <- min(data_cold_non_red_run_success$yards_gained, na.rm = TRUE)
# Shift the data so the smallest value is just above 0
yards_shifted_run <- data_cold_non_red_run_success$yards_gained - min_val + 1

fit_gamma <- fitdistr(yards_shifted_run, "gamma")
fit_gamma$estimate  # gives shape and rate (mean = shape/rate)

#rgamma(1,shape=8.21,rate=0.57)

###########################################
## 3) Pass Incomplete - 0 yards gained 
###########################################

##################
## 4) Pass Success
##################
data_cold_non_red_pass_success = data_cold_non_red %>%filter(complete_pass==1)
#View(data_cold_non_red_pass_success) 
nrow(data_cold_non_red_pass_success) #n=848

ggplot(data_cold_non_red_pass_success, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Gamma distribution - need to shift values due to negative numbers 
# Find the minimum value
min_val <- min(data_cold_non_red_pass_success$yards_gained, na.rm = TRUE)
# Shift the data so the smallest value is just above 0
yards_shifted <- data_cold_non_red_pass_success$yards_gained - min_val + 1

fit_gamma <- fitdistr(yards_shifted, "gamma")
fit_gamma$estimate  # gives shape and rate (mean = shape/rate)

#rgamma(1,shape=4.80,rate=0.24)

########################################
## 5) Pass Interception - 0 yards gained
########################################

#################
## 6) Pass Fumble 
#################
data_cold_non_red_pass_fumble = data_cold_non_red %>%filter(pass_attempt==1 &
                                                            fumble_lost==1)

nrow(data_cold_non_red_pass_fumble) #n=15

ggplot(data_cold_non_red_pass_fumble, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Normal distribution
mean(data_cold_non_red_pass_fumble$yards_gained)
sd(data_cold_non_red_pass_fumble$yards_gained)

# rnorm(1,mean= -2.6,sd=9.94)

#############
## 7) QB Sack
#############

data_cold_non_red_sack = data_cold_non_red %>%filter(sack==1)
#View(data_cold_non_red_sack) 
nrow(data_cold_non_red_sack) #n=92

ggplot(data_cold_non_red_sack, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Gamma distribution - flip because of left skew 
flip <- -1 * data_cold_non_red_sack$yards_gained
# Remove zeros
flip_no_zero <- flip[flip > 0]
fit_gamma <- fitdistr(flip_no_zero, "gamma")
fit_gamma$estimate  # gives shape and rate (mean = shape/rate)

# -1*rgamma(1,shape=4.20,rate=0.59)

####################
## 8) QB Sack Fumble
####################
data_cold_non_red_sack_fumble = data_cold_non_red %>%filter(sack==1 & fumble_lost == 1)
#View(data_cold_non_red_sack_fumble) 
nrow(data_cold_non_red_sack_fumble) #n=10

ggplot(data_cold_non_red_sack_fumble, aes(x = yards_gained)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#FF5733", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribution of Yards Gained",
    x = "Yards Gained",
    y = "Density"
  ) +
  theme_minimal()

# Gamma distribution - flip because of left skew 
flip <- -1 * data_cold_non_red_sack_fumble$yards_gained
fit_gamma <- fitdistr(flip, "gamma")
fit_gamma$estimate  # gives shape and rate (mean = shape/rate)

# -1*rgamma(1,shape=2.25,rate=0.31)

########
## RATES 
########
data_cold_non_red_incomplete_pass = data_cold_non_red %>%filter(incomplete_pass == 1) #n=541
nrow(data_cold_non_red_incomplete_pass)
data_cold_non_red_pass_interception = data_cold_non_red %>%filter(interception == 1) #n=41
nrow(data_cold_non_red_pass_interception)

counts <- c(
  nrow(data_cold_non_red_run_fumble),
  nrow(data_cold_non_red_run_success),
  nrow(data_cold_non_red_incomplete_pass),
  nrow(data_cold_non_red_pass_success),
  nrow(data_cold_non_red_pass_interception),
  nrow(data_cold_non_red_pass_fumble),
  nrow(data_cold_non_red_sack),
  nrow(data_cold_non_red_sack_fumble)
  
)

play_type_rates <- counts / sum(counts)
round(play_type_rates, 3)

#0.002 0.389 0.213 0.334 0.016 0.006 0.036 0.004


