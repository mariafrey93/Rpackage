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
  theta.hat <- cor(x,y)
  # Another way to find coe
  cov(x,y)/(sd(x)*sd(y))
  #Question 2
  # Find standard divition
  set.seed(516)
  N <- NROW(x) #sample size (number of rows)
  storage <- numeric(n) #storage variable
  for (i in 1:n) {
    j <- sample(1:N, size = N, replace = TRUE) #random indices
    X1<- x[j]
    Y1 <- y[j]
    storage[i] <- cor(X1, Y1)
  }
  se=sd(storage)
  theta.hat.boot <- mean(storage)
  bias <- theta.hat.boot - theta.hat
  intstart=theta.hat.boot-(qnorm(1-(0.05/2) )*se)
  intend=theta.hat.boot+(qnorm(1-(0.05/2) )*se)
  return(c(sprintf("correlation exact,boot: %f,%f",theta.hat,theta.hat.boot),sprintf("se: %f",se),sprintf("bias: %f",bias),sprintf("interval: (%f,%f)",intstart,intend)))
}
