toplevel=function(down,ytg,fp,n){
  exp=seq(1:n)
  for(i in exp){
    exp[i]=epoch(down,ytg,fp,n)
  }
  mean(exp)
}#toplevel function runs the epochs and records the mean expected point
#differential

 