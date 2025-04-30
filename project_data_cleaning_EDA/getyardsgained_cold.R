getyg=function(fp){
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
if(fp<=20){
  play_index=sample(1:length(red_zone_plays),size=1,prob=c(
    0.002, 0.453, 0.208, 0.307, 0.010, 0.001, 0.017, 0.001))
  yg=red_zone_yards[play_index]
  play=red_zone_plays[play_index]
}else{
  play_index=sample(1:length(non_red_zone_plays),size=1,prob=c(
    0.002, 0.389, 0.213, 0.334, 0.016, 0.006, 0.036, 0.004))
  yg=non_red_zone_yards[play_index]
  play <- non_red_zone_plays[play_index]}
return(round(yg,0))}
