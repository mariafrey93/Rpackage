#' @export
#' @title Goodness of fit
#' @author Maria Frey & Cecilie Neupart
#' @return Returns the chi-square goodness of fit value.
#' @usage gof(x, p=rep(1/length(x),length(x)))
#'
#' @keywords goodness of fit chi square
#'
#' @description Uses observed and expected data to calculate the goodness of fit by the chi-square test
#'
#' @param x vector of observed data
#' @param p expected probability (default uniform)
#'

gof <- function(x,p=rep(1/length(x),length(x))){
  n <- sum(x)
  
  E <- p*n
  
  print(qchisq(.95,length(x)-1))
  chi <- ((x-E)^2)/E
  return(sum(chi))
}
