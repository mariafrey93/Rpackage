#' @export
#' @title Monte Carlo Chi squared distribution
#' @author Maria Frey & Cecilie Neupart
#' @return Returns the p-value, i.e. P(chi^2 > x)
#' @usage MC_chisq(x, df, n)
#'
#' @keywords Monte carlo chi square distribution p-value degrees of freedom
#'
#' @description Applies Monte Carlo to generate a chi-squared distribution with \code{d}f degrees of freedom by generating \code{n} numbers for each variable.
#'
#' @param x variable
#' @param df degrees of freedom
#' @param n number of random numbers
#'

MC_chisq <- function(x, df, n){
  ### Warnings and errors:
  if(df<1){stop('Degrees of freedom must be positive')}
  if(n<1){stop('n must be larger or equal 1')}
  if(length(n)!=1){stop('n must have length 1')}
  if(n%%1 != 0){warning('n is not integer, will be rounded down to nearest integer')}
  
  ### Actual function
  vars <- rnorm(n)        # n random normally distributed variables
  chi <- vars^2           # squared. If df = 1, this gives the chi-squared cdf
  if (df>1){              # if df > 1,
    for (i in 2:df){
      vars <- rnorm(n)    # generate new variables
      chi <- vars^2 + chi # and sum the squares
    }
  }
  cdf <- ecdf(chi)        # generate the empirical cdf
  a <- cdf(x)             # find P(chi^2 < x)
  return( p=1-a )         # return p-value / P(chi^2 > x)
}