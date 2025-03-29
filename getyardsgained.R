getyg=function(fp){
  red_zone_plays=c(
    "Run Fumble","Run Success","Pass Incomplete","Pass Success",
    "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
  red_zone_yards=c(
    rnorm(1,mean=1.2,sd=3.8),          # Run Fumble
    rnorm(1,mean=2.85,sd=3.72),        # Run Success
    0,                                 # Pass Incomplete
    rnorm(1,mean=6.82,sd=4.99),        # Pass Success
    0,                                 # Pass Interception
    rnorm(1,mean=5.24,sd=5.92),        # Pass Fumble
    -1*rgamma(1,shape=2.87,rate=0.45), # QB Sack
    -1*rgamma(1,shape=2.19,rate=0.32)  # QB Sack Fumble
  )
  non_red_zone_plays=c(
    "Run Fumble","Run Success","Pass Incomplete","Pass Success",
    "Pass Interception","Pass Fumble","QB Sack","QB Sack Fumble")
  non_red_zone_yards=c(
    rnorm(1,mean=2.41,sd=6.17), # Run Fumble
    rexp(1,rate=0.21),          # Run Success
    0,                          # Pass Incomplete
    rexp(1,rate=0.08),          # Pass Success
    0,                          # Pass Interception
    rexp(1,rate=0.1),           # Pass Fumble
    -1*rexp(1,rate=0.15),       # QB Sack
    -1*rexp(1,rate=0.14)        # QB Sack Fumble
  )
  if(fp<=20){
    play_index=sample(1:length(red_zone_plays),size=1,prob=c(
      0.004, 0.461, 0.219, 0.273, 0.011, 0.002, 0.028, 0.002))
    yg=red_zone_yards[play_index]
    play=red_zone_plays[play_index]
  }else{
    play_index=sample(1:length(non_red_zone_plays),size=1,prob=c(
      0.003, 0.4,0.18,0.362,0.013,0.003,0.036,0.003))
    yg=non_red_zone_yards[play_index]
    play <- non_red_zone_plays[play_index]}
  return(round(yg,0))}
