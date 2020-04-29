setwd("~/Desktop")
sfbmc <- readMat("goodEvidencebmc.mat")
#out.Ef contains estimated frequencies
#out.Vf contains the variance-covariance matrix of frequencies
#inside plotUncertainTimeSeries, which is called from VBA_groupBMC, it appears the SEs are derived by the sqrt of the diagonal of out.Vf
#somehow got mangled -- went into MATLAB and just saved these in a simpler .mat 

bestfitmodelbycouple <- as.data.frame(sfbmc$BMCposterior[,,1]$r, row.names = c("m1", "m2", "m3", "m4", "m5"))
bestfitmodelbycouple <- t(bestfitmodelbycouple)
bestfitmodelbycouple <- as.data.frame(bestfitmodelbycouple)
bestfitmodelbycouple$PTNUM <- as.vector(sfbmc$goodIDs)

model1fits <- select(bestfitmodelbycouple, m1, PTNUM)
model5fits <- select(bestfitmodelbycouple, m5, PTNUM)
model4fits <- select(bestfitmodelbycouple, m4, PTNUM)
model1best <- dplyr::filter(bestfitmodelbycouple, m1>.9)
model5best <- dplyr::filter(bestfitmodelbycouple, m5 > .9)
model4best <- dplyr::filter(bestfitmodelbycouple, m4 > .9)


