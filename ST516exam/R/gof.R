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
  ### Warnings and errors:
  if(sum(p) != 1){stop('p must be a vector of probabilities, which must sum to 1')}
  if(any(p<0)){stop('negative probability')}
  if(length(x) != length(p)){
    warning('x and p must be of same length. Default probability used')
    p=rep(1/length(x),length(x))
  }
  if(any(is.na(x)) | any(is.na(p))){warning('NAs in input')}
  
  ### Actual function:
  n <- sum(x)   # total number of observations
  E <- p*n      # number of expected observations / converting probability to frequency
  
  chi <- ((x-E)^2)/E
  return(sum(chi))     # sum of ((x-E)^2)/E
}
