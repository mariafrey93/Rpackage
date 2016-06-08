#' @export
#' @title Density plot
#' @author Maria Frey & Cecilie Neupart
#' @return plots the estimated density function
#' @usage densplot(x, n=500, method="naive", from=NULL, to=NULL)
#'
#' @keywords density estimate gaussian kernel plot
#'
#' @description Uses the dens function to plot the estimated density function of the dataset.
#'
#' @param x a numeric vector
#' @param n number of points to be plotted
#' @param method the method to be used for the calculations. Must either be 'naive' (default) or the Gaussian 'kernel'.
#' @param from the point from which the plot should begin. Will default be one third of the standard deviation of x from min(x).
#' @param to the point where the plot should end. Will default be one third of the standard deviation of x from max(x).
#'

densplot <- function(x,n=500,method="naive", from=NULL, to=NULL){
  if(!is.numeric(n) | n < 1){stop('n must be positive integer')}
  if(!is.vector(x) | !is.numeric(x)){stop('x must be a numeric vector')}
  
  
  if (length(from)==0){
    from <- min(x) - sd(x)/3           # if unspecified, define from
  }
  if(length(to)==0){
    to <- max(x) + sd(x)/3             # if unspecified, define to
  }
  densi <- c()
  sequ <- seq(from,to,length.out = n)  # n points between from and to
  for(d in sequ){
    densi <- c(densi,dens(x, d=d , method = method)) # append density to vector densi
  }
  plot(sequ,densi, xlim=c(from, to),type="l") # plot density as function of the points
}