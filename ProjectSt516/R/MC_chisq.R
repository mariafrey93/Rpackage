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
  vars <- rnorm(n)
  chi <- vars^2
  if (df>1){
    for (i in 2:df){
      vars <- rnorm(n)
      chi <- vars^2 + chi
    }
  }
  cdf <- ecdf(chi)
  a <- cdf(x)
  return(list(cdf=a, p=1-a))
}
