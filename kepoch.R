epoch=function(down,ytg,fp,n){   
  results=0  
  team="reference" #Start with the team of interest  
  for(i in 1:n){     
    points=drive(down,ytg,fp,n)#Flip points if it's the opposing team's drive
    if(team=="opponent"){       
      points=-points     
    }     
    results=results+points#Alternate teams after each drive
    team=ifelse(team=="reference","opponent","reference")  
  }   
  results
}#epoch function runs n drives swapping which team's drive it is


