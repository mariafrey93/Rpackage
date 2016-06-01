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
  U <- runif(N,0,1)
  V <- l*sin(pi*U)
  Integral <- sum(pi*V/N)
  P = Integral/(pi*d)
  print(sprintf("Probability of hit %f",P))
  return(pi.est <- 2*l/(P*d))
}
