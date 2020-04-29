# ---------------------------------------------------------------------
# Program: LDE_2ndOrder_OneIndicator_5D.R
#  Author: Steven Boker and Pascal Deboeck
#    Date: Sat Sep  5 12:23:32 EDT 2009
#
# This program runs an example single indicator 2nd order LDE model
#   with 5 time-delay columns in the embedded state space matrix
#
#
# ---------------------------------------------------------------------
# Revision History
#    -- Sat Sep  5 12:23:32 EDT 2009
#      Created untitled.
#
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Variables 
# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------

# ----------------------------------
# Read libraries and set options.

require(OpenMx)

# ----------------------------------
# Read demo data.

#data(demoOscillator)

demoOscillator <- read.table("http://openmx.psyc.virginia.edu/sites/default/files/demoOscillator.txt", col.names="x")

# ----------------------------------
# Set constants.

tau <- 1     # The lag between subsequent columns in the embedded matrix
deltaT <- .3  # The amount of time elapsed between subsequent observations
embedD <- 7  # The number of columns in the time-delay embedded matrix

# ----------------------------------
# Time delay embed the demo data.

Embed <- function(x, E, tau) {  # create a time delay matrix from x with embedding dimension E and lag tau
	len <- length(x)
	out <- x[1:(len-(E*tau)+tau)]
	for(i in 2:E) { out <- cbind(out,x[(1+((i-1)*tau)):(len-(E*tau)+(i*tau))]) }
	return(out)
}
	
embeddedOscillator <- Embed(demoOscillator$x, embedD, tau)
dimnames(embeddedOscillator) <- list(NULL, paste("x", 1:embedD, sep=""))

# ----------------------------------
# Create the fixed LDE loading matrix.

L1 <- rep(1,embedD)
L2 <- c(1:embedD)*tau*deltaT-mean(c(1:embedD)*tau*deltaT)
L3 <- (L2^2)/2
LDE.Original <- cbind(L1,L2,L3)

# ----------------------------------
# Create 2nd order LDE model.

manifestVars <- dimnames(embeddedOscillator)[[2]]

ldeModel1 <- mxModel("LDE_Model_1",
    mxMatrix("Full",  
        values=LDE.Original, 
        free=FALSE, 
        name="L", 
        byrow=TRUE
    ),
    mxMatrix("Full", 3, 3, 
        values=c(  0,  0, 0,
                   0,  0, 0,
                 -.2,-.2, 0), 
        free=c(FALSE,FALSE,FALSE,
               FALSE,FALSE,FALSE,
                TRUE, TRUE,FALSE), 
        name="A", 
        byrow=TRUE
    ),
    mxMatrix("Symm", 3, 3,
        values=c(.8, -.1, 0,
                  -.1,.8, 0,
                  0, 0,.8), 
        free=c( TRUE, TRUE, FALSE,
               TRUE, TRUE,FALSE,
               FALSE,FALSE, TRUE), 
        name="S", 
        byrow=TRUE,
        lbound=c(0.000001, -10000, 0.000001,
                 -10000, 0.000001, 0.000001,
                 0.000001, 0.000001, 0.000001)
    ),
    mxMatrix("Diag", embedD, embedD, 
        values=.8, 
        free=TRUE, 
        name="U",
        lbound=0.000001
    ),
    mxMatrix("Iden", 3, name="I"),
    mxAlgebra(L %*% solve(I-A) %*% S %*% t(solve(I-A)) %*% t(L) + U, 
        name="R", 
        dimnames = list(manifestVars, manifestVars)
    ),
    mxMLObjective("R"),
    mxData(cov(embeddedOscillator), 
        type="cov", 
        numObs=dim(embeddedOscillator)[1]
    )
)

# ----------------------------------
# Fit the LDE model and examine the summary results.

ldeModel1Fit <- mxRun(ldeModel1)

summary(ldeModel1Fit)


# ----------------------------------
# Quit here.
#
# q()

