
getyg=function(fp, weather){
  if(weather=='sunny'){
    rzRates <- c(0.003, 0.468, 0.216, 0.273, 0.011, 0.001, 0.026, 0.002)
    rates <-   c(0.003, 0.401, 0.179, 0.362, 0.013, 0.002, 0.037, 0.003)
    #Sunny Red Zone
    red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    red_zone_yards=c(
      rnorm(1,mean=1.1,sd=3.4),          # Run Fumble
      rnorm(1,mean=2.8,sd=3.7),        # Run Success
      0,                                 # Pass Incomplete
      rnorm(1,mean=6.9,sd=5),        # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean=5.3,sd=6.2),        # Pass Fumble
      -1*rgamma(1,shape=2.8,rate=0.4), # QB Sack
      -1*rgamma(1,shape=2.4,rate=0.3)  # QB Sack Fumble
    )
    
    #Sunny Non Red Zone
    non_red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    non_red_zone_yards=c(
      rnorm(1,mean=2,sd=5.4),          # Run Fumble
      rexp(1,rate=0.2),        # Run Success
      0,                                 # Pass Incomplete
      rexp(1,rate=0.1),       # Pass Success
      0,                                 # Pass Interception
      rexp(1,rate=0.1),        # Pass Fumble
      -1*rexp(1,rate=0.2), # QB Sack
      -1*rexp(1,rate=0.1)  # QB Sack Fumble
    )
  }
  
  if(weather=='cloudy'){
    rzRates <- c(0.004,0.46,0.222,0.269,0.012,0.001,0.03,0.002)
    rates <-   c(0.003,0.399,0.181,0.363,0.013,0.002,0.036,0.003)
    #Cloudy Red Zone
    red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    red_zone_yards=c(
      rnorm(1,mean=1.1,sd=3.9),          # Run Fumble
      rnorm(1,mean=2.9,sd=3.7),        # Run Success
      0,                                 # Pass Incomplete
      rnorm(1,mean=6.8,sd=5),        # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean=5.5,sd=5),        # Pass Fumble
      -1*rgamma(1,shape=3,rate=0.5), # QB Sack
      -1*rgamma(1,shape=2.2,rate=0.3)  # QB Sack Fumble
    )
    
    #Cloudy Non Red Zone
    non_red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    non_red_zone_yards=c(
      rnorm(1,mean=2.5,sd=6.2),          # Run Fumble
      rexp(1,rate=0.2),        # Run Success
      0,                                 # Pass Incomplete
      rexp(1,rate=0.1),       # Pass Success
      0,                                 # Pass Interception
      rexp(1,rate=0.1),        # Pass Fumble
      -1*rexp(1,rate=0.2), # QB Sack
      -1*rexp(1,rate=0.1)  # QB Sack Fumble
    )
  }
  
  if(weather=='snowing'){
    rzRates <- c(0.0018, 0.4879, 0.1939, 0.2588, 0.0106, 0.0013, 0.0245, 0.0020)
    rates <-   c(0.0009, 0.4532, 0.1892, 0.3053, 0.0138, 0.0018, 0.0344, 0.0032)
    #Snowing Red Zone
    red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    red_zone_yards=c(
      rnorm(1,mean=-1,sd=3.79),          # Run Fumble
      rnorm(1,mean=2.63,sd=3.63),        # Run Success
      0,                                 # Pass Incomplete
      rnorm(1,mean=6.8,sd=4.98),        # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean=5.46,sd=5.9),        # Pass Fumble
      -1*rgamma(1,shape=6.36,rate=3.79), # QB Sack
      -1*rgamma(1,shape=2.57,rate=0.36)  # QB Sack Fumble
    )
    
    #Snowy Non Red Zone
    non_red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    non_red_zone_yards=c(
      rgamma(1,shape=2.41,rate=6.17),          # Run Fumble
      rnorm(1,mean=5.42,sd=7.61),        # Run Success
      0,                                 # Pass Incomplete
      rnorm(1,mean=12.3,sd=10.52),       # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean=9.7,sd=10.05),        # Pass Fumble
      rnorm(1,mean=-6.61,sd=3.49), # QB Sack
      -1*rgamma(1,shape=2.91,rate=0.36)  # QB Sack Fumble
    )
  }
  
  if(weather=='humid'){
    rzRates <- c(0.0037, 0.4623, 0.2052, 0.2706, 0.0116, 0.0013, 0.0276, 0.0020)
    rates <-   c(0.0028, 0.4001, 0.1819, 0.3627, 0.0131, 0.0023, 0.0369, 0.0025)
    #Humid Red Zone
    red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    red_zone_yards=c(
      rgamma(1,shape=1.28,rate=3.79),          # Run Fumble
      rnorm(1,mean=2.85,sd=3.7),        # Run Success
      0,                                 # Pass Incomplete
      rnorm(1,mean=6.83,sd=5.06),        # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean=5.56,sd=6.97),        # Pass Fumble
      -1*rgamma(1,shape=3.57,rate=0.51), # QB Sack
      -1*rgamma(1,shape=2.48,rate=0.34)  # QB Sack Fumble
    )
    
    #Humid Non Red Zone
    non_red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    non_red_zone_yards=c(
      rgamma(1,shape=2,rate=5.19),          # Run Fumble
      rnorm(1,mean=4.78,sd=6.7),        # Run Success
      0,                                 # Pass Incomplete
      rnorm(1,mean=11.76,sd=10.44),        # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean=9.7,sd=10.05),        # Pass Fumble
      rnorm(1,mean=-6.61,sd=3.49), # QB Sack
      -1*rgamma(1,shape=3.2,rate=0.4)  # QB Sack Fumble
    )
  }
  
  if(weather == 'windy'){
    rzRates <- c(0.0028, 0.4673, 0.2044, 0.2671, 0.0093, 0.0011, 0.0287, 0.0026)
    rates <-   c(0.0030, 0.4100, 0.1878, 0.3467, 0.0141, 0.0024, 0.0353, 0.0032)
    #Windy Red Zone
    red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    red_zone_yards=c(
      rgamma(1,shape=3.56,rate=1.24),          # Run Fumble
      rnorm(1,mean=2.79,sd=3.69),        # Run Success
      0,                                 # Pass Incomplete
      rnorm(1,mean=6.87,sd=5.01),        # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean=3,sd=5.74),        # Pass Fumble
      -1*rgamma(1,shape=3.43,rate=0.52), # QB Sack
      -1*rgamma(1,shape=1.91,rate=0.23)  # QB Sack Fumble
    )
    
    #Windy Non Red Zone
    non_red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    non_red_zone_yards=c(
      rnorm(1,mean=3.63,sd=8.05),          # Run Fumble
      rnorm(1,mean=4.75,sd=6.7),        # Run Success
      0,                                 # Pass Incomplete
      rnorm(1,mean=11.53,sd=10.31),        # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean=8.19,sd=10.2),        # Pass Fumble
      -1*rgamma(1,shape=3.55,rate=0.52), # QB Sack
      -1*rgamma(1,shape=3.42,rate=0.41)  # QB Sack Fumble
    )
  }
  
  # FOGGY
  if(weather == 'foggy'){
    rzRates <- c(
      0.0014492754, 0.2483221477, 0.0850111857, 0.1208053691,
      0.0022371365, 0.0025362319, 0.0134228188, 0.0014492754)
    rates <- c(
      0.0028985507, 0.4355072464, 0.1681159420, 0.3376811594,
      0.0144927536, 0.0050724638, 0.0333333333, 0.0028985507)
    red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    red_zone_yards=c(
      rnorm(1,mean=0,sd=2),       # Run Fumble
      rchisq(1, df=4) - 1 ,        # Run Success
      0,                                 # Pass Incomplete
      rnorm(1,mean=6.425926,sd=3.5),        # Pass Success
      0,                                 # Pass Interception
      rnorm(1, mean = 0, sd = 4), # Pass Fumble
      rnorm(1, mean = -4.833333, sd = 3), # QB Sack
      rnorm(1, mean = -7.5, sd = 1.75) # QB Sack Fumble
    )
    non_red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    non_red_zone_yards=c(
      rchisq(1, df = 4) - 2,    # Run Fumble
      rgamma(1,shape=9) - 7,    # Run Success
      0,                                 # Pass Incomplete
      rchisq(1, df = 8) - 2,             # Pass Success
      0,                                 # Pass Interception
      rexp(1, rate = 0.08) - 10,       # Pass Fumble
      rnorm(1, mean = -6.235294, sd = 2.35), # QB Sack
      rnorm(1, mean=-6.235294, sd = 3.216136)  # QB Sack Fumble
    )
  }
  
# RAINING
if (weather == 'raining'){
  rzRates <- c(
    0.0041171089, 0.4963403477, 0.2127172919, 0.2483989021,
    0.0105215005, 0.0018298262, 0.0251601098, 0.0009149131)
  rates <- c(
    0.0030792918, 0.4213730842, 0.1876268458, 0.3294142347,
    0.0130170061, 0.0048288894, 0.0380012597, 0.0026593883)
  red_zone_plays=c(
    "Run Fumble","Run Success","Pass Incomplete","Pass Success",
    "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
  red_zone_yards=c(
    rnorm(1,mean=0,sd=2),       # Run Fumble
    rchisq(1, df=4) - 1 ,        # Run Success
    0,                                 # Pass Incomplete
    rnorm(1,mean=6.425926,sd=3.5),        # Pass Success
    0,                                 # Pass Interception
    rnorm(1, mean = 0, sd = 4), # Pass Fumble
    rnorm(1, mean = -4.833333, sd = 3), # QB Sack
    rnorm(1, mean = -7.5, sd = 1.75) # QB Sack Fumble
  )
  non_red_zone_plays=c(
    "Run Fumble","Run Success","Pass Incomplete","Pass Success",
    "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
  non_red_zone_yards=c(
    rexp(1, rate = 0.3) - 0.5,    # Run Fumble
    rgamma(1,shape=9) - 4.5,    # Run Success
    0,                                 # Pass Incomplete
    rchisq(1, df = 13) - 2,             # Pass Success
    0,                                 # Pass Interception
    rnorm(1, mean = -3.4375, sd = 7.7543),       # Pass Fumble
    rnorm(1, mean = -7, sd = 1.5), # QB Sack
    rnorm(1, mean=-7.5, sd = 100)  # QB Sack Fumble
  )
}
  
  # INDOORS
  if (weather == 'indoors'){
    rzRates <- c(
      0.0024610336, 0.4536505332, 0.2145200984, 0.2908121411,
      0.0102543068, 0.0041017227, 0.0213289582, 0.0028712059)
    rates <- c(
      0.0029426911, 0.3778415361, 0.1775914073, 0.3872581476,
      0.0117707644, 0.0043404694, 0.0361951004, 0.0020598838)
    red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    red_zone_yards=c(
      runif(1, min=-2, max = 10),    # Run Fumble
      rgamma(1,shape=4) - 2.258,    # Run Success
      0,                                 # Pass Incomplete
      rgamma(1, shape=13) - 7.25,             # Pass Success
      0,                                 # Pass Interception
      rchisq(1, df=3) - 6,       # Pass Fumble
      rnorm(1, mean= -6.978723, sd = 2.35), # QB Sack
      rnorm(1, mean=-6.978723, 3.339418)  # QB Sack Fumble
    )
    non_red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    non_red_zone_yards=c(
      rnorm(1, mean = 0, sd = 1),    # Run Fumble
      rchisq(1, df = 8) - 4,    # Run Success
      0,                                 # Pass Incomplete
      rchisq(1, df = 20) - 9.25,             # Pass Success
      0,                                 # Pass Interception
      rnorm(1, mean= -1.162791, sd=10),       # Pass Fumble
      rnorm(1, mean= -7.5, sd = 2.5), # QB Sack
      rnorm(1, mean=-7.25, 2.85)  # QB Sack Fumble
    )
  }
  
  # Cold  
  if (weather == 'cold'){
    rzRates <- c(0.002, 0.453, 0.208, 0.307, 0.010, 0.001, 0.017, 0.001)
    rates <- c(0.002, 0.389, 0.213, 0.334, 0.016, 0.006, 0.036, 0.004)
    red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    red_zone_yards=c(
      rnorm(1,mean=-0.80,sd=3.80),       # Run Fumble
      rnorm(1,mean=2.78,sd=3.40),        # Run Success
      0,                                 # Pass Incomplete
      rnorm(1,mean=6.98,sd=5.29),        # Pass Success
      0,                                 # Pass Interception
      rnorm(1, mean = -2.03, sd = 3.90), # Pass Fumble
      -1*rgamma(1,shape=5.30,rate=0.38), # QB Sack
      -1*rgamma(1,shape=11.64,rate=1.42) # QB Sack Fumble
    )
    non_red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    non_red_zone_yards=c(
      rgamma(1,shape=1.27,rate=0.13),    # Run Fumble
      rgamma(1,shape=8.21,rate=0.57),    # Run Success
      0,                                 # Pass Incomplete
      rgamma(1,shape=4.80,rate=0.24),    # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean= -2.6,sd=9.94),       # Pass Fumble
      -1*rgamma(1,shape=4.20,rate=0.59), # QB Sack
      -1*rgamma(1,shape=2.25,rate=0.31)  # QB Sack Fumble
    )
  }  
  
  # Hot  
  if (weather == 'hot'){
    rzRates <- c(0.001, 0.539, 0.156, 0.243, 0.009, 0.009, 0.035, 0.009)
    rates <- c(0.005, 0.387, 0.189, 0.362, 0.011, 0.002, 0.044, 0.001)
    red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    red_zone_yards=c(
      rnorm(1,mean=1.2,sd=3.8),          # Run Fumble
      rnorm(1,mean=2.61,sd=2.91),        # Run Success 
      0,                                 # Pass Incomplete
      rnorm(1,mean=6.61,sd=3.63),        # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean= -6,sd=3.90),         # Pass Fumble
      rnorm(1,mean= -6.25,sd=3.5),       # QB Sack
      -1*rgamma(1,shape= 3.59,rate=0.41) # QB Sack Fumble
    )
    non_red_zone_plays=c(
      "Run Fumble","Run Success","Pass Incomplete","Pass Success",
      "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
    non_red_zone_yards=c(
      rnorm(1,mean=3.67,sd=3.51),        # Run Fumble
      rgamma(1,shape=5.32,rate=0.38),    # Run Success
      0,                                 # Pass Incomplete
      rgamma(1,shape=3.90,rate=0.21),    # Pass Success
      0,                                 # Pass Interception
      rnorm(1,mean= 2.73 ,sd= 11.50),    # Pass Fumble
      rnorm(1,mean= -6.93,sd=2.92),      # QB Sack
      -1*rgamma(1,shape= 11.63,rate=1.42)# QB Sack Fumble
    )
  }    
  if(fp<=20){
    play_index=sample(1:length(red_zone_plays),size=1,prob=rzRates)
    yg=red_zone_yards[play_index]
    play=red_zone_plays[play_index]
  }else{
    play_index=sample(1:length(non_red_zone_plays),size=1,prob=rates)
    yg=non_red_zone_yards[play_index]
    play <- non_red_zone_plays[play_index]}
  return(c(round(yg,0), play))
}

