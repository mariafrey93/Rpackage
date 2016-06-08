#' @export
#' @title Bootstrap
#' @author Maria Frey & Cecilie Neupart
#' @return Returns the correlation between x and y, the bootstrap estimate of correlation, standard error, bias and confidence interval.
#' @usage bootstrap(n, x, y)
#'
#' @keywords bootstrap estimate correlation bias standard error confidence interval
#'
#' @description Simulates Buffon's experiment of throwing needles on a striped floor
#'
#' @param n number of bootstrap replicates
#' @param x variable
#' @param y variable
#'

bootstrap=function(n,x,y){
  ### Warnings and errors:
  if(n<1){stop('n must be larger or equal 1')}
  if(length(n)!=1){stop('n must have length 1')}
  if(!is.numeric(n)){stop('n must be numeric')}
  if(length(x)<2 | length(y)<2){stop('input must be vectors of length >1')}
  if(length(x) != length(y)){stop('x and y must have same length')}
  
  ### Actual function:
  theta.hat <- cor(x,y)       # exact correlation
  N <- NROW(x)                # sample size (number of rows)
  storage <- numeric(n)       # storage variable (for storing correlations)
  for (i in 1:n) {
    j <- sample(1:N, size = N, replace = TRUE) #random indices
    X<- x[j]
    Y <- y[j]
    storage[i] <- cor(X, Y) # correlation between random samples of X and Y
  }
  se <- sd(storage)                           # standard error
  theta.hat.boot <- mean(storage)             #
  bias <- theta.hat.boot - theta.hat          # bias: mean of sampled estimate - exact
  intstart <- theta.hat.boot-(qnorm(1-(0.05/2) )*se) # lower bound of confidence interval
  intend <- theta.hat.boot+(qnorm(1-(0.05/2) )*se)   # upper bound of confidence interval
  return(list("corr_exact"=theta.hat,"corr_boot"=theta.hat.boot,"se"=se,"bias"=bias,"interval"=c(intstart,intend)))
}