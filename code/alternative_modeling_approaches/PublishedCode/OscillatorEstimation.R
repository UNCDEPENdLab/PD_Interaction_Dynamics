# The following is the R~\citeyear{R} code used for analysis in the simulations 
# described. The first part of the code
# provides the code that calculates the Local Linear Approximation
# (LLA) estimates of $x$ and its derivatives. The second part
# creates the least squares function to be minimized and a function to
# call the optimization procedure several times with random restart
# values. The third section is the function the user interact with;
# This function, "TIAnalysis", requires the user to provide a time
# series ``Data'' and the maximum number of $\tau$ values to be
# examined ``MaxTau.'' Questions regarding use of this code can be 
# directed to the first author. 

### Produces LLA estimates from a time series (x).
LLA <- function(x,Tau,Embed,Delta)
{
  x <- x-mean(x)
  Elen <- length(x)-(Tau*(Embed-1))
  Ematrix <- matrix(NA,Embed,Elen)
  for(k in 1:Embed) {
    Ematrix[k,] <- x[(((k-1)*Tau)+1):(length(x)-((Embed-k)*Tau))]
  }
  
  L1 <- rep(1, Embed)
  L2 <- (seq(1,Embed,1)-mean(seq(1,Embed,1)))*Tau*Delta
  L3 <- (L2^2)/2
  L <- cbind(L1,L2,L3)
  
  W <- L%*%solve(t(L)%*%L)
  Estimates <- t(W)%*%Ematrix
  
  return(Estimates)
}

### Function Minimized by optim()
FunctionToMinimize <- function(x, EtaTau, Tau) {
  Residual <- (EtaTau*Tau*Tau)-((2*x[1]*cos(x[2]*Tau))-2)
  SSR <- sum(Residual^2)
  return(SSR)
}

### Tries to minimize "FunctionToMinimize" and returns
### the best fitting solution, the total number of
### function calls and information about how often
### the minimization function was able to converge
EstimationFunction <- function(EtaTau, Tau) {
  Repetitions <- 30
  LowestSSR <- Inf
  TotalFunctionCalls <- 0
  Convergence <- 0
  for(i in 1:Repetitions) {
    fit <- optim(c(runif(1,0,1), runif(1,0,2*pi)),
        FunctionToMinimize, Tau=Tau, EtaTau=EtaTau,
        method="L-BFGS-B", lower=c(0,-10^5),
        upper=c(1,10^5), hessian = TRUE)
    
    if(fit$value<LowestSSR && fit$convergence==0) {
      LowestSSR <- fit$value
      BestFit <- fit
    }
    TotalFunctionCalls <- TotalFunctionCalls + fit$counts[1]
    Convergence <- Convergence + fit$convergence
  }
  return(list(BFIT=BestFit,FCALLS=TotalFunctionCalls,
          CON=Convergence))
}

### This function is the user interface. Users provide a
### time series (Data) and a maximum number of tau values
### to be analyzed (MaxTau).
TIAnalysis <- function(Data, MaxTau) {
  EtaTauEstimates <- matrix(NA,2,MaxTau)
  for(Tau in 1:MaxTau) {
    Estimates <- LLA(Data,Tau,3,1)
    fit <- lm(Estimates[3,]~Estimates[1,]-1)
    EtaTauEstimates[,Tau] <- summary(fit)$coefficients[1:2]
  }
  result <- EstimationFunction(EtaTauEstimates[1,],c(1:MaxTau))
  return(result)
}
