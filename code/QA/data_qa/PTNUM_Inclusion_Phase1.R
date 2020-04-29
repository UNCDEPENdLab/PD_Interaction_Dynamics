# setwd("/Users/ams939/ics/pd_interaction_dynamics_analyses_feb2017")
# library(R.matlab)
# modeldata <- readMat("modelcomparison_logevidence.mat")
# R2 <- as.data.frame(modeldata$R2.evidence)
# R2$PTNUM <- as.data.frame(modeldata$ids)
# modelcomp_paramsM3 <- as.data.frame(modeldata$rawparameters[,3,])
# modelcomp_paramsM3$PTNUM <- as.vector(modeldata$ids)
# modelcomp_paramsM3$R2 <- as.vector(modeldata$R2.evidence[,3])
# modelcomp_paramsM3$SNoise_a_alpha <- as.vector(modeldata$stateNoise.a.alpha[,3])
# modelcomp_paramsM3$SNoise_b_alpha <- as.vector(modeldata$stateNoise.b.alpha[,3])
# modelcomp_paramsM3$LL <- as.vector(t(modeldata$logEvidence[3,]))


setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")
write.csv(modelcomp_paramsM3, "modelcomparison_paramsM3")
modelcomp_paramsM3 <- read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/modelcomparison_paramsM3")
crazyparams <- dplyr::filter(modelcomp_paramsM3, abs(V1) > .15| abs(V2) > .15|abs(V3) > .15| abs(V4) > .15) #8016, 8052, 8060, 8063, 8066, 8074, 8106, 8126, 8144, 8146
badr2 <- dplyr::filter(modelcomp_paramsM3, R2 < .99) #8016, 8060, 8106, 8112, 8133, 8144
badbeta <-dplyr::filter(modelcomp_paramsM3, SNoise_b_alpha > 600000) # 8035, 8127
badalpha <- dplyr::filter(modelcomp_paramsM3, SNoise_a_alpha > 7000) #8011, 8014, 8109, 8035, 8041, 8074, 8103
badLL <- dplyr::filter(modelcomp_paramsM3, LL < -60000) #8016, 8035, 8060, 8074, 8100, 8102, 8106, 8112, 8126, 8127, 8133, 8144
modelcomp_paramsM3 <- dplyr::mutate(modelcomp_paramsM3, a_b = SNoise_a_alpha/SNoise_b_alpha)
bad_snoise <- dplyr::filter(modelcomp_paramsM3, a_b > .2)

library(dplyr)
library(memisc)
setwd("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")
HRMeds <- as.data.set(spss.system.file("COUPLES_MEDICATION_BY_TIMEPOINT_20170414.sav"))
HRMeds <- as.data.frame(HRMeds)
View(HRMeds)
filter(HRMeds, usrid == 80211)