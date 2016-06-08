#' @export
#' @title Linear Regression model
#' @author Maria Frey & Cecilie Neupart
#' @return Returns summary of coefficients, standard error, t-value, p-value and R^2
#' @usage regres(formula)
#'
#' @keywords linear regression
#'
#' @description Fits linear models by regression, similar to lm.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#'
#' @examples regres(faithful$eruptions ~ faithful$waiting)

regres <- function(formula){
  # Used from lm function
  call <- match.call()
  matri <- match.call() #Use to make matrix and so the code can be run like the lm function
  matri[[1L]] <- quote(stats::model.frame)
  matri <- eval(matri, parent.frame())
  
  #Dimension of matrix
  widthmatri <- dim(matri)[2]     # Width = p+1
  heightmatri <- dim(matri)[1]    # Height = n
  y <- matri[,1]                  # Setting y
  X <- rep(1,heightmatri)         #Start X with column of 1's.
  
  for(i in 2:widthmatri){
    X <- cbind(X,matri[,i])       # Add other variables to X.
  }
  df <- heightmatri-widthmatri    #Calculate df as n-p-1
  bet <- as.vector(solve((t(X)%*%X))%*%t(X)%*%y) # Calculate beta-coefficients
  
  yhat <-  X%*% bet
  residual <- as.vector(y - yhat)
  residualsd <- sd(residual)      #Standard error of residuals
  
  RSS  <-sum(residual^2)
  SSreg <- sum((yhat-mean(y))^2)
  SST <- sum((y-mean(y))^2)
  
  Ssq <- RSS/(df)
  stderr <- sqrt(diag((Ssq*(solve(t(X)%*%X))))) # standard error
  
  rsquare <- SSreg/SST                          # Calculate r squared
  RSadj <- 1- (RSS/(df))/(SST/(heightmatri-1))  # adjusted r-squared
  F <- (SSreg/(widthmatri-1))/(RSS/(df))        # F-statistic
  Pf <- 1- pf(F,widthmatri-1,df)
  
  t <- bet/stderr             # t-value
  Pt <- 1-pt(bet/stderr,df)   # p-value of t-test
  
  # Returning results:
  att <- attr(terms(formula),"term.labels")
  # coefficient matrix:
  coeff <- matrix(cbind(bet,stderr,t,Pt),ncol=4,nrow=length(att)+1)
  colnames(coeff) <- c("Estimate","Std. Error","t value","Pr(>|t|)")
  rownames(coeff) <- c("(Intercept)",att)
  
  cat(
    "Coefficients:\n")
  
  print(as.table(coeff))
  
  cat("\n",
      "---\n","\n",
      
      sprintf("Residual standard error: %.3f on %i degrees of freedom",residualsd,df),
      "\n",
      sprintf("Multiple R-squared: %.4f, Adjusted R-squared: %.4f",rsquare,RSadj),
      "\n",
      sprintf("F-statistic: %.2f on %i and %i DF, p-value: %f",F,widthmatri-1,heightmatri-widthmatri,Pf),
      "\n"
  )
}