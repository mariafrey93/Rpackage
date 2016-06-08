#' @export
#' @title Density
#' @author Maria Frey & Cecilie Neupart
#' @return Returns the density in a single point d from data x using the specified method.\cr
#'  If the point d is not defined, it returns the densities at the "minimum, 1st Quantile, median, mean, 3rd Quantile and maximum of the data.
#' @usage dens(x, d=NULL, h=NULL, method="naive")
#'
#' @keywords density estimate gaussian kernel
#'
#' @description Estimates the density in a given point using the naive estimator or the Gaussian kernel.
#'
#' @param x a numeric variable
#' @param d the point to evaluate.
#' @param h the bandwidth. Will be calculated by Sturges for the naive method and Silverman's for the kernel method if not defined by user.
#' @param method the method to be used for the calculations. Must either be 'naive' (default) or the Gaussian 'kernel'.
#'
#' @examples dens(faithful$eruptions)


dens <- function(x,d=NULL,h=NULL,method="naive"){
  if(method != "naive" && method != "kernel"){stop('choose either "naive" or "kernel" as method')}
  if(!is.numeric(x)){stop('x must be a numeric vector')}
  n <- length(x)
  if(length(h)==0){
    if(method=="naive"){              # calculate h using Sturges
      n <- length(x)
      nclass <- ceiling(1+log2(n))    # Sturges
      h <- diff(range(x)/nclass)
    }else if(method=="kernel"){
      #calculate h using Silverman's suggestion for Gaussian kernel's bandwidth
      h <- 0.9*n^(-1/5)*min(sd(x),IQR(x)/1.34)
    }
  }
  if(length(d)==0){
    quantile1 <- quantile(x,names=FALSE)[2]
    quantile3 <- quantile(x,names=FALSE)[4]
    # densities in the points:
    mn <- dens(x,d=min(x),h,method)
    mx <- dens(x,d=max(x),h,method)
    md <- dens(x,d=median(x),h, method)
    me <- dens(x,d=mean(x),h,method)
    q1 <- dens(x,d=quantile1,h,method)
    q3 <- dens(x,d=quantile3,h,method)
    
    # table:
    densities <- matrix(c(min(x),mn,quantile1,q1,median(x),md,mean(x),me,quantile3,q3,max(x),mx),ncol=2,byrow=TRUE)
    colnames(densities) <- c("x","y")
    rownames(densities) <- c("Min.: ", "1st Quan.: ","Median: ","Mean: ","3rd Quan.: ","Max:")
    print(sprintf("Bandwidth 'h': %f",h))
    return(as.table(densities))
  }
  if(method=="naive"){                       # naive density estimation
    w <- function(t){if(abs(t)<1)1/2 else 0} # weight-function
    sum <- 0
    for (i in 1:n){
      sum <- sum + 1/h * w((d-x[i])/h)
    }
    f <- 1/n * sum
    return(f)
  }else if(method=="kernel"){                # kernel density estimation
    sum <- 0
    for (i in 1:n){
      sum <- sum + 1/h * dnorm((d-x[i])/h)
    }
    f <- 1/n * sum
    return(f)
  }
}