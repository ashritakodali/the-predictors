getyg=function(fp){
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
if(fp<=20){
  play_index=sample(1:length(red_zone_plays),size=1,prob=c(
    0.001, 0.539, 0.156, 0.243, 0.009, 0.009, 0.035, 0.009))
  yg=red_zone_yards[play_index]
  play=red_zone_plays[play_index]
}else{
  play_index=sample(1:length(non_red_zone_plays),size=1,prob=c(
    0.005, 0.387, 0.189, 0.362, 0.011, 0.002, 0.044, 0.001))
  yg=non_red_zone_yards[play_index]
  play <- non_red_zone_plays[play_index]}
return(round(yg,0))}
