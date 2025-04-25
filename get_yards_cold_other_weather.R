# FOGGY
getyg=function(fp){
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
if(fp<=20){
  play_index=sample(1:length(red_zone_plays),size=1,prob=c(
    0.0014492754, 0.2483221477, 0.0850111857, 0.1208053691,
    0.0022371365, 0.0025362319, 0.0134228188, 0.0014492754))
  yg=red_zone_yards[play_index]
  play=red_zone_plays[play_index]
}else{
  play_index=sample(1:length(non_red_zone_plays),size=1,prob=c(
    0.0028985507, 0.4355072464, 0.1681159420, 0.3376811594,
    0.0144927536, 0.0050724638, 0.0333333333, 0.0028985507))
  yg=non_red_zone_yards[play_index]
  play <- non_red_zone_plays[play_index]}
return(round(yg,0))}


# RAINING
getyg=function(fp){
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
    rnorm(mean = -3.4375, sd = 7.7543),       # Pass Fumble
    rnorm(1, mean = -7, sd = 1.5), # QB Sack
    rnorm(1, mean=-7.5, sd = 100)  # QB Sack Fumble
  )
  if(fp<=20){
    play_index=sample(1:length(red_zone_plays),size=1,prob=c(
      0.0014492754, 0.2483221477, 0.0850111857, 0.1208053691,
      0.0022371365, 0.0025362319, 0.0134228188, 0.0014492754))
    yg=red_zone_yards[play_index]
    play=red_zone_plays[play_index]
  }else{
    play_index=sample(1:length(non_red_zone_plays),size=1,prob=c(
      0.0028985507, 0.4355072464, 0.1681159420, 0.3376811594,
      0.0144927536, 0.0050724638, 0.0333333333, 0.0028985507))
    yg=non_red_zone_yards[play_index]
    play <- non_red_zone_plays[play_index]}
  return(round(yg,0))}


# RAINING
getyg=function(fp){
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
  if(fp<=20){
    play_index=sample(1:length(red_zone_plays),size=1,prob=c(
      0.0014492754, 0.2483221477, 0.0850111857, 0.1208053691,
      0.0022371365, 0.0025362319, 0.0134228188, 0.0014492754))
    yg=red_zone_yards[play_index]
    play=red_zone_plays[play_index]
  }else{
    play_index=sample(1:length(non_red_zone_plays),size=1,prob=c(
      0.0028985507, 0.4355072464, 0.1681159420, 0.3376811594,
      0.0144927536, 0.0050724638, 0.0333333333, 0.0028985507))
    yg=non_red_zone_yards[play_index]
    play <- non_red_zone_plays[play_index]}
  return(round(yg,0))}


# RAINING
getyg=function(fp){
  red_zone_plays=c(
    "Run Fumble","Run Success","Pass Incomplete","Pass Success",
    "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
  red_zone_yards=c(
    rchisq(1, df = 4) - 2,    # Run Fumble
    rgamma(1,shape=9) - 7,    # Run Success
    0,                                 # Pass Incomplete
    rchisq(1, df = 8) - 2,             # Pass Success
    0,                                 # Pass Interception
    rexp(1, rate = 0.08) - 10,       # Pass Fumble
    rnorm(1, mean = -6.235294, sd = 2.35), # QB Sack
    rnorm(1, mean=-6.235294, sd = 3.216136)  # QB Sack Fumble
  )
  non_red_zone_plays=c(
    "Run Fumble","Run Success","Pass Incomplete","Pass Success",
    "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
  non_red_zone_yards=c(
    rnorm(1, mean = 0, sd = 1.25),    # Run Fumble
    rgamma(1,shape=11) - 7,    # Run Success
    0,                                 # Pass Incomplete
    rchisq(1, df = 16) - 6,             # Pass Success
    0,                                 # Pass Interception
    rnorm(1, mean= -0.9121622, sd=5.675),       # Pass Fumble
    rnorm(1, mean= -6.058586, sd = 2.35), # QB Sack
    rnorm(1, mean=-6.25, sd = 4.373468)  # QB Sack Fumble
  )
  if(fp<=20){
    play_index=sample(1:length(red_zone_plays),size=1,prob=c(
      0.0041171089, 0.4963403477, 0.2127172919, 0.2483989021,
      0.0105215005, 0.0018298262, 0.0251601098, 0.0009149131))
    yg=red_zone_yards[play_index]
    play=red_zone_plays[play_index]
  }else{
    play_index=sample(1:length(non_red_zone_plays),size=1,prob=c(
      0.0030792918, 0.4213730842, 0.1876268458, 0.3294142347,
      0.0130170061, 0.0048288894, 0.0380012597, 0.0026593883))
    yg=non_red_zone_yards[play_index]
    play <- non_red_zone_plays[play_index]}
  return(round(yg,0))}

# INDOORS
getyg=function(fp){
  red_zone_plays=c(
    "Run Fumble","Run Success","Pass Incomplete","Pass Success",
    "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
  red_zone_yards=c(
    runif(min=-2, max = 10),    # Run Fumble
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
  if(fp<=20){
    play_index=sample(1:length(red_zone_plays),size=1,prob=c(
      0.0024610336, 0.4536505332, 0.2145200984, 0.2908121411,
      0.0102543068, 0.0041017227, 0.0213289582, 0.0028712059))
    yg=red_zone_yards[play_index]
    play=red_zone_plays[play_index]
  }else{
    play_index=sample(1:length(non_red_zone_plays),size=1,prob=c(
      0.0029426911, 0.3778415361, 0.1775914073, 0.3872581476,
      0.0117707644, 0.0043404694, 0.0361951004, 0.0020598838))
    yg=non_red_zone_yards[play_index]
    play <- non_red_zone_plays[play_index]}
  return(round(yg,0))}


