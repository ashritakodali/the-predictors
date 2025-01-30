drive=function(down,ytg,fp,n){
  while(down <= 4){#Simulate plays from the given down through 4th down
    fp=fp+sample(-120:120,1)#Random yardage gain/loss
    if(ytg<=0){#First down achieved
      down=1#Reset to first down
    }else{
      down=down+1#Advance the down count
    }
    if(down>4){#If it's 4th down and unsuccessful, the drive ends
      break
    }
  }#Determine points based on final field position
  if(fp < -10){
    points=-3
  }
  else if(fp>=-10&fp<0){
    points=-7
  }
  else if(fp>=0&fp<100){
    points=0#No score
  }
  else if(fp>=100&fp<110){
    points=7#TD
  }
  else{
    points=3#Field goal
  }
  points
}#drive function takes the state, runs a drive, and spits out how many points 
#were scored for our team of interest


