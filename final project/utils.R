# helper function to compute the score.  Takes field position.

compute_score <- function(fp){
  
  if(fp <= 100) {
    return(0)
  } else if (fp <= 110) {
    return(7)
  } else {
    return(3)
  }

}
