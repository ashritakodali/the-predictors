# read in sampling data
samplingData <- readRDS('pbp2014-2024.rds')

# looking at the weather data, we want a column for the description of weather,
# the temperature as a numeric, the humidity as a numeric, the direction of the wind
# and the speed of the wind

# cloudy, # windy, # sunny, # raining, # snow, # haze/fog

library(dplyr)
library(stringr)


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


sum(is.na(samplingData$is_sunny))
