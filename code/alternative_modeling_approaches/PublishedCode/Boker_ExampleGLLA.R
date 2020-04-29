#---------------------------------------------------------
# Example program calculates approximate derivatives and fits a 
#    damped linear oscillator model to two variables.
#
# Input:  Example.dat is a 3 column data file with equal interval occasions
#                     time-ordered with one measurement occasion per row. 
#        DeltaTime -- Interobservation interval
#           theTau -- Selected time delay 
#         theEmbed -- Selected number of embedding dimensions

deltaTime <- 1
theTau <- 1
theEmbed <- 14

tMatrix <- as.matrix(read.table("Example.dat"))
dimnames(tMatrix) <- list(NULL, c("ID", "Estradiol", "Binge"))
tFrame <- data.frame(tMatrix)

source("GLLAfunctions.R")

# Perform the state space embedding using theTau and theEmbed

eMatrix <- gllaEmbed(tFrame$Estradiol, embed=theEmbed, 
                     tau=theTau, groupby=tFrame$ID)
bMatrix <- gllaEmbed(tFrame$Binge, embed=theEmbed, 
                     tau=theTau, groupby=tFrame$ID)

# Calculate Local Linear Approximation of derivatives

wMatrix <- gllaWMatrix(embed=theEmbed, tau=theTau, 
                       deltaT=deltaTime, order=2)
idLLA <- eMatrix[,1]
eMatrixLLA <- eMatrix[,2:dim(eMatrix)[2]] %*% wMatrix
bMatrixLLA <- bMatrix[,2:dim(bMatrix)[2]] %*% wMatrix
    
# remove rows with missing data and put into a data frame

allMatrixLLA <- cbind(idLLA, eMatrixLLA[,1:3], bMatrixLLA[,1:3])
allMatrixLLA <- allMatrixLLA[!is.na(apply(allMatrixLLA, 1, sum)),]
dimnames(allMatrixLLA) <- list(NULL, c("ID","E","dE","d2E","B","dB","d2B"))
tLLAFrame <- data.frame(allMatrixLLA)
summary(tLLAFrame)

# Fit the damped linear oscillator model of estradiol

cat(paste("\n\n -------  Estradiol -----\n\n", sep=""))

treg <- lme(d2E ~  E + dE  - 1, random=list( ~ E + dE - 1 | ID), 
            data=tLLAFrame, control=lmeControl(opt="optim"))
print(summary(treg))

cat(paste("\n average R^2 = "))
print(round(1 - (var(treg$residuals[,2]) / var(tLLAFrame$d2E)),4))

cat("\n Individual Coefficients\n")
print(coef(treg))

cat("\n Individual Cycle Lengths (in Days)\n")
tEta <- coef(treg)[1]
tLambda <- 2 * pi / sqrt(- tEta)
EstradiolSoloLambdasGLLA[,theTau] <- c(tLambda[,1])
print(round(tLambda,2))

# Fit the damped linear oscillator model of Binge Eating

cat(paste("\n\n ------- Binge Eating -----\n\n", sep=""))

treg <- lme(d2E ~  E + dE  - 1, random=list( ~ E + dE - 1 | ID), 
            data=tLLAFrame, control=lmeControl(opt="optim"))
print(summary(treg))

cat(paste("\n average R^2 = "))
print(round(1 - (var(treg$residuals[,2]) / var(tLLAFrame$d2E)),4))

cat("\n Individual Coefficients\n")
print(coef(treg))

cat("\n Individual Cycle Lengths (in Days)\n")
tEta <- coef(treg)[1]
tLambda <- 2 * pi / sqrt(- tEta)
EstradiolSoloLambdasGLLA[,theTau] <- c(tLambda[,1])
print(round(tLambda,2))
