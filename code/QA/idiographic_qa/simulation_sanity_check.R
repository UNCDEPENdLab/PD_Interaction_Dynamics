library(R.matlab)
setwd("/Users/alisonmarie526/Desktop/Files_Directory/Archive_April2017/")
set1 <- readMat("logp1.mat")
set1params <- as.data.frame(set1$rawparameters)
set2 <- readMat("logp2.mat")
set2params <- as.data.frame(set2$rawparameters)
set3 <- readMat("logp3.mat")
set3params <- as.data.frame(set3$rawparameters)
set4 <- readMat("logp4.mat")
set4params <- as.data.frame(set4$rawparameters)
set5 <- readMat("logp5.mat")
set5params <- as.data.frame(set5$rawparameters)
set6 <- readMat("logp6.mat")
set6params <- as.data.frame(set6$rawparameters)
set7 <- readMat("logp7.mat")
set7params <- as.data.frame(set7$rawparameters)

totaldf <- dplyr::bind_rows(set1params, set2params, set3params, set4params, set5params, set6params, set7params)
totaldf <- dplyr::filter(totaldf, abs(V1) <.1, abs(V2) < .1, abs(V3) < .1, abs(V4) < .1)
colnames(totaldf) <- c("scpt","ccpt","scpr","ccpr") 
png("sanitycheck_scatterplotmatrix.png", width = 600, height = 600)
lattice::splom(dplyr::select(totaldf, scpt, ccpt, scpr, ccpr))
dev.off()

setwd("/Users/alisonmarie526/Documents/MATLAB/")
simulation_params <- readMat("simulation_params.mat")
simulation_params <- as.data.frame(t(simulation_params$a5))
lattice::splom(dplyr::select(simulation_params, V1, V2, V3, V4))
 
 
vanBse_params <- readMat("vanBse_modelcomparison_logevidence.mat")
vanBse_paramsm1 <- as.data.frame((vanBse_params$rawparameters[,1,]))
vanBse_paramsm2 <- as.data.frame((vanBse_params$rawparameters[,2,]))
vanBse_paramsm3 <- as.data.frame((vanBse_params$rawparameters[,3,]))
vanBse_paramsm4 <- as.data.frame((vanBse_params$rawparameters[,4,]))
vanBse_paramsm4$PTNUM <- as.numeric(vanBse_params$ids)
# ll <- data.frame()
# ll$ll <- as.data.frame((vanBse_params$logEvidence[4,]))
# ll$PTNUM <- as.numeric(vanBse_params$ids)
# vanBse_paramsm4$ll <- as.data.frame((vanBse_params$logEvidence[4,]))
vanBse_paramsm4 <- rename(vanBse_paramsm4, self_coupling_patient = V1, cross_coupling_patient = V2, self_coupling_parnter = V3, cross_coupling_partner = V4 )
vanBse_paramsm1 <- dplyr::filter(vanBse_paramsm1, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
vanBse_paramsm2 <- dplyr::filter(vanBse_paramsm2, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
vanBse_paramsm3 <- dplyr::filter(vanBse_paramsm3, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
vanBse_paramsm4 <- dplyr::filter(vanBse_paramsm4, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)




lattice::splom(dplyr::select(vanBse_paramsm1, V1, V2, V3, V4, V5, V6))
lattice::splom(dplyr::select(vanBse_paramsm3, V1, V2, V3, V4, V5, V6))
lattice::splom(dplyr::select(vanBse_paramsm2, V1, V2, V3, V4))
lattice::splom(dplyr::select(vanBse_paramsm4, V1, V2, V3, V4))


setwd("~/Desktop")
caid_params <- readMat("CAIDmodels_logevidence.mat")
caid_params_m1 <- as.data.frame(caid_params$rawparameters[,1,])
caid_params_m1 <- dplyr::filter(caid_params_m1, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
lattice::splom(dplyr::select(caid_params_m1, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10))
caid_params_m2 <- as.data.frame(caid_params$rawparameters[,2,])
caid_params_m2 <- dplyr::filter(caid_params_m2, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
caid_params_m7 <- as.data.frame(caid_params$rawparameters[,7,])
caid_params_m7 <- dplyr::filter(caid_params_m7, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
caid_params_m8 <- as.data.frame(caid_params$rawparameters[,8,])
caid_params_m7 <- dplyr::filter(caid_params_m8, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)





