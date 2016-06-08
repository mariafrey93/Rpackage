#' @export
#' @title Buffon's needle experiment
#' @author Maria Frey & Cecilie Neupart
#' @return Prints P(hit) and returns an estimate of pi
#' @usage buffon(N, l = 1, d = 1)
#'
#' @keywords monte carlo simulation buffon needle experiment
#'
#' @description Simulates Buffon's experiment of throwing needles on a striped floor
#'
#' @param N number of needles to be thrown
#' @param l length of needle (default 1)
#' @param d distance between lines (default 1)
#'


buffon <- function(N,l=1,d=1){
  ### Errors and warnings:
  if(N<1){stop('N must be larger or equal 1')}
  if(length(N)!=1){stop('N must have length 1')}
  if(length(l)>1 | length(d)>1){stop('input must be single values')}
  if(l>d){warning('This function assumes l <= d')} 
  
  ### Actual function:
  U <- runif(N,0,1)            # N random numbers from uniform distr.
  V <- l*sin(pi*U)             # applying the function
  Integral <- sum(pi*V/N)      # calculating the integral
  P = Integral/(pi*d)          # calculating P(hit)
  return(pi.est <- 2*l/(P*d))  # return estimate of pi
}