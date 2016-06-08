#' @export
#' @title Discrete Markov Chain
#' @author Maria Frey & Cecilie Neupart
#' @return returns the exact and simulated stationary probabilities of each state.
#' @usage MarkovChain(p, k, n)
#'
#' @keywords Markov chain monte carlo stationary probabilities discrete
#'
#' @description Simulates the stationary distribution of a discrete Markov chain.
#'
#' @param p a transition probability matrix
#' @param k integer indicating the initial state of the chain
#' @param n number of simulated steps
#'
#' @examples MarkovChain(p = matrix(c(.1,.2,.7,0,.5,.5,.1,0,.9),ncol=3, byrow=TRUE), k = 2, n = 1000)

MarkovChain <- function(p, k, n){
  # warnings and errors:
  if(!is.matrix(p)){stop('p must be a matrix of probabilities')}
  if(!is.numeric(n)){stop('n must be positive integer')}
  if(n < 1){stop('n must be positive integer')}
  if(k > dim(p)[1]){stop('k must be a possible state')}
  
  
  set.seed(12)
  N <- dim(p)[1]      # number of rows/states
  X <- 0              # initialising
  Y <- 0
  
  i <- k              # initial value
  X[1] <- i           # initial state
  
  m <- 1              # counter
  
  # Generate random variable Yi
  U <- runif(1,0,1)   # generating a random uniform variable
  for (j in 1:N){     # go through the states
    if(U <= p[i,1]){  # special case for first state
      Y[i] <- 1
    }else if ((sum(p[i,1:j-1]) < U) && (U <= sum(p[i,1:j]))){
      Y[i] <- j
    }
  }
  
  X[2] <- Y[i]        # second state
  
  while(m<n){
    i <- X[m]         # previous state
    #Generate Y[i]
    U <- runif(1,0,1)
    for (j in 1:N){
      if(U <= p[i,1]){
        Y[i] <- 1
      }else if ((sum(p[i,1:j-1]) < U) && (U <= sum(p[i,1:j]))){
        Y[i] <- j
      }
    }
    m <- m+1
    X[m] <- Y[i]      # new state
  }
  
  # simulated stationary probability:
  sim <- 0
  for(l in 1:N){
    sim[l] <- sum(X==l)/n
  }
  
  # exact stationary probability:
  p1 <- t(p)
  p1 <- p1-diag(N)                      # minus 1 in diagonal to isolate variables
  p1 <- rbind(p1[1:N-1,],rep(1,N))      # replace the last row by ones, since x1 + x2 + ... + xn = 1
  
  exact <- solve(p1, c(rep(0,N-1),1))   # solve
  
  res <- matrix(c(exact,sim),ncol=2)    # result table of exact and simulated stationary probabilities
  
  print("Stationary prob.")
  colnames(res) <- c("Exact", "Simulated")
  rownames(res) <- 1:N
  return(as.table(res))
  
}
