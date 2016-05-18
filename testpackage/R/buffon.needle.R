#' @title buffons needle
#' @export
#' @author Maria 
#' @description Find pi
#' @param n  number of needles.
#' @param l length of needles 
#' @param d distance between parallel lines
#' @param dig decimal places returned 

buffon.needle<-function(n,l,d,dig){
  
  hit<-0
  for (i in 1:n){
    theta<-runif(1,min=0,max=pi/2)
    y<-runif(1,min=0,max=d)
    if(y<l*sin(theta)){
      
      hit<-hit+1
      
    }
    
  }
  return(hit)
  return(2*l*n/(d*hit))
  
  
}

buffon.needle(1000000,1,2,dig=options(digits = 6))

#plot(n,buffon.needle(n,l,d,dig=options(digits=6)))
#abline(0,0,pi)
