library(R.matlab)
setwd("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/")
data <- readMat("modelcomparison_logevidence.mat")
logEvidence <- t(data$logEvidence)
logEvidence <- as.data.frame(logEvidence)
#data <- dplyr::filter(logEvidence, logEvidence < -79999)
anybad <- apply(logEvidence, 1, function(row) {
  any(row < -79999)
})
idlistgood <- as.data.frame(as.vector(data$ids))
idlistgood$anybad <- as.vector(anybad) 
dt <- readMat("modelcomparison_rawparams.mat")
rawparamsModel3 <- as.data.frame(dt$sfrawparamsm3[,1,])
rawparamsModel3$PTNUM <- data$ids[1,]
logEvidence$PTNUM <- as.vector(data$ids)
logEvidenceModel3 <- dplyr::select(logEvidence, V3, as.vector(PTNUM))
rawparamsModel3 <- dplyr::inner_join(rawparamsModel3, logEvidenceModel3, by = "PTNUM")
idlistgood$PTNUM <- idlistgood$`as.vector(data$ids)`
rawparamsModel3 <- dplyr::rename(rawparamsModel3, `self coupling patient` = V1, `cross coupling patient` = V2, `self coupling partner` = V3.x, `cross coupling partner` = V4, `p1star` = V5, `p2star` = V6, `LL` = V3.y)
rawparamsModel3 <- merge(rawparamsModel3, idlistgood, by = c("PTNUM"))
rawparamsModel3 <- dplyr::select(rawparamsModel3, -`as.vector(data$ids)`)
write.csv(rawparamsModel3, "rawparamsM3.csv")
#Bad would have us exclude 8004 8008 8016 8020 8032 8049 8060 8099 8106 8112 8114 8126 8127 8133 8144 8152
rawparamsModel3BadData <- dplyr::filter(rawparamsModel3, anybad == TRUE)
#Bad M3 would have our data set exclude PTNUM: 8016, 8060, 8106, 8112, 8127, 8133, 8144
#interestingly 8016 did not need to be censored; 8016 r had a lot of large peaks but seemed to all be legit (McKayla seemed confident)
#in 8060r, a million PVCs; had to be censored a lot 
#in 8106, also had to be censored a fair bit (not as much as elsewhere)
#in 8112, a lot of jumps, but no edits needed according to McKayla
#8127 had to be censored a lot and had a lot of edits made
#8133 a lot of questionable signal
#a lot of weak signal, had to censor a fair bit but also had a lot of edits
rawparamsModel3BadM3 <- dplyr::filter(rawparamsModel3, LL < -79999)
#rawParamsModel1goodData <- dplyr::filter(rawParamsModel1, anybad == FALSE)
#rawParamsModel1goodData <- inner_join(rawParamsModel1goodData, model1fits, by = "PTNUM", copy = TRUE)
rawparamsModel3goodData <- dplyr::filter(rawparamsModel3, LL > -79999)
write.csv(rawparamsModel3goodData, file = "parameters_model3_modelcomparison_goodData.csv", row.names = TRUE)
write.csv(rawparamsModel3, file = "parameters_model3_modelcomparison_allData.csv", row.names = TRUE)
rawparamsModel3 = read.csv()






######## START HERE
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/")
dt = read.csv("parameters_model3_modelcomparison_goodData.csv", header = TRUE)
#rename ptnum to PTNUM
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")
personalitymeasures <- read.csv("couples_baseline_clinical_4April2017.csv")

dt_patient <- dplyr::select(dt, self.coupling.patient, cross.coupling.patient, PTNUM)
dt_patient <- rename(dt_patient, self_coupling = self.coupling.patient, cross_coupling = cross.coupling.patient)
dt_partner <- dplyr::select(dt, self.coupling.partner, cross.coupling.partner, PTNUM)
dt_partner <- rename(dt_partner, self_coupling = self.coupling.partner, cross_coupling = cross.coupling.partner)
# dtm4_patient$self_coupling <- dtm4_patient$self_coupling_patient
# dtm4_patient$cross_coupling <- dtm4_patient$cross_coupling_patient
# dtm4_patient <- dplyr::select(dtm4_patient, -cross_coupling_patient, -self_coupling_patient)
personalitymeasures <- rename(personalitymeasures, PTNUM = ptnum, DyadID = dyadid, UsrID = usrid)
pDatPatient <- dplyr::filter(personalitymeasures, DyadID == 1 )
pDatPatient <- dplyr::filter(pDatPatient, PTNUM != 8123, PTNUM !=8139)
pDataPatient <- inner_join(pDatPatient, dt_patient, by = "PTNUM")
excludePatient <- anti_join(pDatPatient, dt_patient, by = "PTNUM") #excludes 81441, 81331, 81271
pDatPartner <- dplyr::filter(personalitymeasures, DyadID == 0)
#not must cut from patient 8123, 8139
# dtm4_partner <- dplyr::select(dtm4, self_coupling_partner, cross_coupling_partner, PTNUM)
# dtm4_partner$self_coupling <- dtm4_partner$self_coupling_partner
# dtm4_partner$cross_coupling <- dtm4_partner$cross_coupling_partner
#dtm4_partner <- dplyr::select(dtm4_partner, -cross_coupling_partner, -self_coupling_partner)
pDataPartner <- inner_join(pDatPartner, dt_partner, by = "PTNUM")
excludePartner <-anti_join(pDatPartner, dt_partner, by = "PTNUM") #excludes 8144, 8133, 8127
names(pDataPartner) <- paste(names(pDataPartner), "partner", sep = "_")
names(pDataPatient) <- paste(names(pDataPatient), "patient", sep = "_")
personalitydata <- data.frame(pDataPatient, pDataPartner)
personalitydata$PTNUM <- personalitydata$PTNUM_patient
write.csv(personalitydata, "params_personalitydata.csv")

########## START HERE
library(lavaan)
library(dplyr)
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")
df <- read.csv("params_personalitydata.csv")

df <- df %>% mutate(iip_elcpr = iip_elevation_partner - mean(iip_elevation_partner),
                                                                    pdcountcpr = allpdCount_partner - mean(na.omit(allpdCount_partner)),
                                                                    iip_x_pdcount_partner = iip_elcpr * pdcountcpr,
                                                                    iip_elcpt = iip_elevation_patient - mean(na.omit(iip_elevation_patient)),
                                                                    pdcountcpt = allpdCount_patient - mean(na.omit(allpdCount_patient)),
                                                                    iip_x_pdcount_patient = iip_elcpt * pdcountcpt
)

tliers_vanBse <- tliers_vanBse %>% mutate(iip_elcpr = iip_elevation_partner - mean(iip_elevation_partner),
                    pdcountcpr = allpdCount_partner - mean(na.omit(allpdCount_partner)),
                    iip_x_pdcount_partner = iip_elcpr * pdcountcpr,
                    iip_elcpt = iip_elevation_patient - mean(na.omit(iip_elevation_patient)),
                    pdcountcpt = allpdCount_patient - mean(na.omit(allpdCount_patient)),
                    iip_x_pdcount_patient = iip_elcpt * pdcountcpt
)



df_params_patient <- dplyr::select(df, self_coupling_patient, cross_coupling_patient )
df_params_patient$summed_cross_coupling_patient <- with(df_params_patient, (-1)*self_coupling_patient + cross_coupling_patient)
df_params_partner <- dplyr::select(df, self_coupling_partner, cross_coupling_partner)
df_params_partner$summed_cross_coupling_partner <- with(df_params_partner, (-1)*self_coupling_partner + cross_coupling_partner)
df$summed_cross_coupling_partner <- df_params_partner$summed_cross_coupling_partner
df$summed_cross_coupling_patient <- df_params_patient$summed_cross_coupling_patient


#based off cook's D, which is calculated in parameter_clustering_march2017.R, these are the peoople that need to be excluded for no outleirs
tliers <- dplyr::filter(df, PTNUM != 8007, PTNUM != 8085, PTNUM != 8126, PTNUM != 8137, PTNUM !=8149, PTNUM!=8052, PTNUM!=8063, PTNUM!=8075, PTNUM!=8148)

setwd("~/Desktop/")
setwd("/Users/alisonmarie526/Desktop/Archive_April2017/")
setwd("/Users/ams939/Desktop/VBA_output/")
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
vanBse_paramsm1 <- dplyr::filter(vanBse_paramsm1, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
vanBse_paramsm2 <- dplyr::filter(vanBse_paramsm2, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
vanBse_paramsm3 <- dplyr::filter(vanBse_paramsm3, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
vanBse_paramsm4 <- dplyr::filter(vanBse_paramsm4, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)

vanBse_paramsm4 <- rename(vanBse_paramsm4, self_coupling_patient = V1, cross_coupling_patient = V2, self_coupling_parnter = V3, cross_coupling_partner = V4 )
vanBse_paramsm4 <- rename(vanBse_paramsm4, v_self_coupling_patient = self_coupling_patient, v_cross_coupling_patient = cross_coupling_patient, v_self_coupling_parnter= self_coupling_parnter, v_cross_coupling_partner = cross_coupling_partner  )


df_nooutliers_vanBse <- merge(df_nooutliers, vanBse_paramsm4, by = c("PTNUM"))
df_nooutliers_vanBse <- dplyr::mutate(df_nooutliers_vanBse, scpt = 1000*self_coupling_patient, scpr = 1000*self_coupling_partner, ccpt = 1000*cross_coupling_patient, ccpr = 1000*cross_coupling_partner,v_scpt = 1000*v_self_coupling_patient, v_scpr = 1000*v_self_coupling_parnter, v_ccpt = 1000*v_cross_coupling_patient, v_ccpr = 1000*v_cross_coupling_partner)
cxsetting = TRUE
#cxsetting = "default" #this will unexpectedly lead to a change in LL scaling for models 3 and 4


iipel_self <- "
self_coupling_patient ~  iip_elevation_patient
self_coupling_partner ~ iip_elevation_partner 
summed_cross_coupling_patient ~ iip_elevation_patient 
summed_cross_coupling_partner ~ iip_elevation_partner 
"
iipel_self_m <- sem(iipel_self, df, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipel_pdtot_self <- "
self_coupling_patient ~  iip_elcpt + pdcountcpt 
self_coupling_partner ~ iip_elcpr  +  pdcountcpr
summed_cross_coupling_patient ~ iip_elcpt +pdcountcpt
summed_cross_coupling_partner ~ iip_elcpr + pdcountcpr 
"
iipel_pdtot_self_m <- sem(iipel_pdtot_self, df, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)



iipel_self_nooutliers_m <- sem(iipel_self, df_nooutliers, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


#####only looking at self and cross, no summed cross
##only using nooutlier data

iipel_self_vanBse <- "
scpt ~  iip_elcpt 
scpr ~ iip_elcpr 
ccpt ~ iip_elcpt 
ccpt~~scpt
ccpr ~~scpr
ccpt ~ v_ccpt
ccpr ~v_ccpr
scpt ~ v_scpt
scpr ~ v_scpr
ccpr ~ iip_elcpr


#based on modification indices
scpr ~ v_ccpr
ccpr ~ v_scpr
ccpr ~ iip_elcpt
"
iipel_self_nooutliers_vanBse_m <- sem(iipel_self_vanBse, df_nooutliers_vanBse, missing = "ML", estimator = "ML", 
                                     mimic="Mplus", meanstructure = TRUE) #, conditional.x=cxsetting)


iipel_self_other_vanBse <- "
scpt ~  iip_elevation_patient + iip_elevation_partner 
scpr ~ iip_elevation_partner  + iip_elevation_patient
ccpt ~ iip_elevation_patient + iip_elevation_partner
ccpt~~scpt
ccpr ~~scpr
ccpt ~ v_ccpt
ccpr ~v_ccpr
scpt ~ v_scpt
scpr ~ v_scpr
ccpr ~ iip_elevation_partner + iip_elevation_patient
scpr ~ v_ccpr
ccpr ~ v_scpr

"
iipel_self_other_nooutliers_vanBse_m <- sem(iipel_self_other_vanBse, df_nooutliers_vanBse, missing = "ML", estimator = "ML", 
                                      mimic="Mplus", meanstructure = TRUE) #, conditional.x=cxsetting)




iipel_pdtot_self_nosum <- "
self_coupling_patient ~  iip_elcpt + pdcountcpt
self_coupling_partner ~ iip_elcpr  +  pdcountcpr
cross_coupling_partner ~ iip_elcpr + pdcountcpr 
cross_coupling_patient ~  iip_elcpt + pdcountcpt
"
iipel_pdtot_self_nosum_nooutliers_m <- sem(iipel_pdtot_self_nosum, df_nooutliers, missing = "listwise", estimator = "ML", 
                                           mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipel_pdtot_self_vanBse <- "
scpt ~  iip_elcpt + pdcountcpt
scpr ~ iip_elcpr  +  pdcountcpr
ccpt ~ iip_elcpr + pdcountcpr 
ccpr ~  iip_elcpt + pdcountcpt
ccpt ~ v_ccpt
ccpr ~v_ccpr
scpt ~ v_scpt
scpr ~ v_scpr
"
iipel_pdtot_self_vanBse_m <- sem(iipel_pdtot_self_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                                           mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)
 


iipel_pdtot_interaction_self_nosum <- "
self_coupling_patient ~  iip_elcpt + pdcountcpt + iip_x_pdcount_patient
self_coupling_partner ~ iip_elcpr  +  pdcountcpr + iip_x_pdcount_partner
cross_coupling_patient ~ iip_elcpt +pdcountcpt + iip_x_pdcount_patient
cross_coupling_partner ~ iip_elcpr + pdcountcpr + iip_x_pdcount_partner
"
iipel_pdtot_interaction_self_nosum_nooutliers_m <- sem(iipel_pdtot_interaction_self_nosum, df_nooutliers, missing = "listwise", estimator = "ML", 
                                                       mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)




iipel_pdtot_interaction_self_vanBse <- "
scpt ~  iip_elcpt + pdcountcpt + iip_x_pdcount_patient
scpr ~ iip_elcpr  +  pdcountcpr + iip_x_pdcount_partner
ccpt ~ iip_elcpt +pdcountcpt + iip_x_pdcount_patient
ccpr ~ iip_elcpr + pdcountcpr + iip_x_pdcount_partner
ccpt ~ v_ccpt
ccpr ~v_ccpr
scpt ~ v_scpt
scpr ~ v_scpr
"
iipel_pdtot_interaction_self_vanBse_m <- sem(iipel_pdtot_interaction_self_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                                                       mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)







iipag_self <- "

self_coupling_patient ~  iip_agency_patient 
self_coupling_partner ~ iip_agency_partner 
cross_coupling_patient ~ iip_agency_patient 
cross_coupling_partner ~ iip_agency_partner
"
iipag_self_m <- sem(iipag_self, df_nooutliers, missing = "listwise", estimator = "ML", 
                                     mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)
iipag_self_vanBse <- "

scpt ~  iip_agency_patient 
scpr ~ iip_agency_partner 
ccpt ~ iip_agency_patient 
ccpr ~ iip_agency_partner
scpt ~ v_scpt
ccpr ~ v_ccpr
scpr ~ v_scpr
ccpt ~ v_ccpt
"
iipag_self_vanBse_m <- sem(iipag_self_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)



iipcm_self <- "

self_coupling_patient ~  iip_communion_patient 
self_coupling_partner ~ iip_communion_partner 
cross_coupling_patient ~ iip_communion_patient 
cross_coupling_partner ~ iip_communion_partner
"
iipcm_self_m <- sem(iipcm_self, df_nooutliers, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipcm_self_vanBse <- "

scpt ~  iip_communion_patient 
scpr ~ iip_communion_partner 
ccpt ~ iip_communion_patient 
ccpr ~ iip_communion_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr
"
iipcm_self_vanBse_m <- sem(iipcm_self_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipcm_self_other_vanBse <- "

scpt ~  iip_communion_patient + iip_communion_partner
scpr ~ iip_communion_partner + iip_communion_patient
ccpt ~ iip_communion_patient + iip_communion_partner
ccpr ~ iip_communion_partner + iip_communion_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr
"
iipcm_self_other_vanBse_m <- sem(iipcm_self_other_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                           mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)






iipagcm_self <- "

self_coupling_patient ~  iip_communion_patient + iip_agency_patient
self_coupling_partner ~ iip_communion_partner + iip_agency_partner
cross_coupling_patient ~ iip_communion_patient + iip_agency_patient
cross_coupling_partner ~ iip_communion_partner + iip_agency_partner
"
iipagcm_self_m <- sem(iipagcm_self, df_nooutliers, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipag_self_other <- "

self_coupling_patient ~  iip_agency_patient + iip_agency_partner 
self_coupling_partner ~ iip_agency_partner + iip_agency_patient
cross_coupling_patient ~ iip_agency_patient + iip_agency_partner 
cross_coupling_partner ~ iip_agency_partner + iip_agency_patient
"
iipag_self_other_m <- sem(iipag_self_other, df_nooutliers, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipag_self_other_vanBse <- "

scpt ~  iip_agency_patient + iip_agency_partner 
scpr ~ iip_agency_partner + iip_agency_patient
ccpt ~ iip_agency_patient + iip_agency_partner 
ccpr ~ iip_agency_partner + iip_agency_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr
"
iipag_self_other_vanBse_m <- sem(iipag_self_other_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)






iipcm_self_other <- "

self_coupling_patient ~  iip_communion_patient +iip_communion_partner 
self_coupling_partner ~ iip_communion_partner + iip_communion_patient
cross_coupling_patient ~ iip_communion_patient +iip_communion_partner 
cross_coupling_partner ~ iip_communion_partner + iip_communion_patient
"
iipcm_self_other_m <- sem(iipcm_self_other, df_nooutliers, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipagcm_self_other <- "

self_coupling_patient ~  iip_communion_patient + iip_agency_patient + iip_communion_partner + iip_agency_partner
self_coupling_partner ~ iip_communion_partner + iip_agency_partner + iip_communion_patient + iip_agency_patient 
cross_coupling_patient ~ iip_communion_patient + iip_agency_patient + iip_communion_partner + iip_agency_partner
cross_coupling_partner ~ iip_communion_partner + iip_agency_partner + iip_communion_patient + iip_agency_patient 
"
iipagcm_self_other_m <- sem(iipagcm_self_other, df_nooutliers, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipelagcm_self <- "

self_coupling_patient ~  iip_communion_patient + iip_agency_patient + iip_elevation_patient 
self_coupling_partner ~ iip_communion_partner + iip_agency_partner  + iip_elevation_partner
cross_coupling_patient ~ iip_communion_patient + iip_agency_patient + iip_elevation_patient
cross_coupling_partner ~ iip_communion_partner + iip_agency_partner + iip_elevation_partner
"
iipelagcm_self_m <- sem(iipelagcm_self, df_nooutliers, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipelagcm_self_other <- "

self_coupling_patient ~  iip_communion_patient + iip_agency_patient + iip_elevation_patient + iip_communion_partner + iip_agency_partner  + iip_elevation_partner
self_coupling_partner ~ iip_communion_partner + iip_agency_partner  + iip_elevation_partner +  iip_communion_patient + iip_agency_patient + iip_elevation_patient
cross_coupling_patient ~ iip_communion_patient + iip_agency_patient + iip_elevation_patient + iip_communion_partner + iip_agency_partner  + iip_elevation_partner
cross_coupling_partner ~ iip_communion_partner + iip_agency_partner + iip_elevation_partner +  iip_communion_patient + iip_agency_patient + iip_elevation_patient
"
iipelagcm_self_other_m <- sem(iipelagcm_self_other, df_nooutliers, missing = "listwise", estimator = "ML", 
                        mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)



iipelagcm_self_other_vanBse <- "

scpt ~  iip_communion_patient + iip_agency_patient + iip_elevation_patient + iip_communion_partner + iip_agency_partner  + iip_elevation_partner
scpr ~ iip_communion_partner + iip_agency_partner  + iip_elevation_partner +  iip_communion_patient + iip_agency_patient + iip_elevation_patient
ccpt ~ iip_communion_patient + iip_agency_patient + iip_elevation_patient + iip_communion_partner + iip_agency_partner  + iip_elevation_partner
ccpr ~ iip_communion_partner + iip_agency_partner + iip_elevation_partner +  iip_communion_patient + iip_agency_patient + iip_elevation_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr

"
iipelagcm_self_other_vanBse_m <- sem(iipelagcm_self_other_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                              mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipelag_self_other_vanBse <- "

scpt ~   iip_agency_patient + iip_elevation_patient +  iip_agency_partner  + iip_elevation_partner
scpr ~ iip_agency_partner  + iip_elevation_partner +  iip_agency_patient + iip_elevation_patient
ccpt ~  iip_agency_patient + iip_elevation_patient +  iip_agency_partner  + iip_elevation_partner
ccpr ~ iip_agency_partner + iip_elevation_partner +  iip_agency_patient + iip_elevation_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr

"
iipelag_self_other_vanBse_m <- sem(iipelag_self_other_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                                     mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)




iipelag_self_vanBse <- "

scpt ~   iip_agency_patient + iip_elevation_patient 
scpr ~ iip_agency_partner  + iip_elevation_partner 
ccpt ~  iip_agency_patient + iip_elevation_patient 
ccpr ~ iip_agency_partner + iip_elevation_partner 
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr

"
iipelag_self_vanBse_m <- sem(iipelag_self_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                                   mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)







#iipel_self beats out; then iipel_pdtot_self (no interaction),the iipel_pdtot_interaction,then iipag_self, and finally iipcm_self and iipagcm_self tie (and then p pathways implicated)
#testing now with more dimension pdcount

df_nooutliers <- df_nooutliers %>% mutate(pdcpr = pdtot_partner - mean(na.omit(pdtot_partner)),
                    iip_x_pdtot_partner = iip_elcpr * pdcpr,
                    pdcpt = pdtot_patient - mean(na.omit(pdtot_patient)),
                    iip_x_pdtot_patient = iip_elcpt * pdcpt
)


df_nooutliers_vanBse <- df_nooutliers_vanBse %>% mutate(pdcpr = pdtot_partner - mean(na.omit(pdtot_partner)),
                                          iip_x_pdtot_partner = iip_elcpr * pdcpr,
                                          pdcpt = pdtot_patient - mean(na.omit(pdtot_patient)),
                                          iip_x_pdtot_patient = iip_elcpt * pdcpt
)

iipel_pdtot_dim_self_vanBse <- "
scpt ~  iip_elcpt + pdcpt
scpr ~ iip_elcpr  +  pdcpr
ccpr ~ iip_elcpr + pdcpr 
ccpt ~  iip_elcpt + pdcpt
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 
scpr ~ v_ccpr
ccpr ~ v_scpr
"
iipel_pdtot_dim_self_vanBse_m <- sem(iipel_pdtot_dim_self_vanBse, df_nooutliers_vanBse, missing = "ML", estimator = "ML", 
                                           mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


pdtot_dim_vanBse <- "
scpt ~   pdcpt + pdcpr
scpr ~  pdcpr + pdcpt
ccpr ~ pdcpr + pdcpt
ccpt ~   pdcpt + pdcpr
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 
"
pdtot_dim_vanBse_m <- sem(pdtot_dim_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                                     mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)






iipel_pdtot_interaction_dim_self<- "
self_coupling_patient ~  iip_elcpt + pdcpt + iip_x_pdtot_patient
self_coupling_partner ~ iip_elcpr  +  pdcpr + iip_x_pdtot_partner
cross_coupling_patient ~ iip_elcpt +pdcpt + iip_x_pdtot_patient
cross_coupling_partner ~ iip_elcpr + pdcpr + iip_x_pdtot_partner
"
iipel_pdtot_interaction_dim_self_m <- sem(iipel_pdtot_interaction_dim_self, df_nooutliers, missing = "listwise", estimator = "ML", 
                                                       mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipel_pdtot_interaction_dim_self_vanBse<- "
scpt ~  iip_elcpt + pdcpt + iip_x_pdtot_patient
scpr ~ iip_elcpr  +  pdcpr + iip_x_pdtot_partner
ccpt ~ iip_elcpt +pdcpt + iip_x_pdtot_patient
ccpr ~ iip_elcpr + pdcpr + iip_x_pdtot_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 
"
iipel_pdtot_interaction_dim_self_vanBse_m <- sem(iipel_pdtot_interaction_dim_self_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", 
                                          mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)



pdtot_dim_self_vanBse<- "

scpt ~    pdcpt
scpr ~   pdcpr
ccpr ~ pdcpr 
ccpt ~   pdcpt
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
pdtot_dim_self_vanBse_m <- sem(pdtot_dim_self_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)


bordl_vanBse<- "

scpt ~    bordl_sidp_patient
scpr ~   bordl_sidp_partner
ccpr ~ bordl_sidp_partner 
ccpt ~   bordl_sidp_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
bordl_vanBse_m <- sem(bordl_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)

#greater self coupling in partners, suggesting that for partners who have borderline symptoms they have this inflexilibity in HR


narci_vanBse<- "

scpt ~    narci_sidp_patient + narci_sidp_partner
scpr ~   narci_sidp_partner + narci_sidp_patient
ccpr ~ narci_sidp_partner  + narci_sidp_patient
ccpt ~   narci_sidp_patient + narci_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
narci_vanBse_m <- sem(narci_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)

#greater self coupling in partners, suggesting that for partners who have borderline symptoms they have this inflexilibity in HR



avoid_vanBse<- "

scpt ~    avoid_sidp_patient + avoid_sidp_partner
scpr ~   avoid_sidp_partner + avoid_sidp_patient
ccpr ~ avoid_sidp_partner  + avoid_sidp_patient
ccpt ~   avoid_sidp_patient + avoid_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
avoid_vanBse_m <- sem(avoid_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)
#adoption of contrarion style in partner and dependent style in patient when partner higher on avoidant sx




negtv_vanBse<- "

scpt ~    negtv_sidp_patient + negtv_sidp_partner
scpr ~   negtv_sidp_partner + negtv_sidp_patient
ccpr ~ negtv_sidp_partner  + negtv_sidp_patient
ccpt ~   negtv_sidp_patient + negtv_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
negtv_vanBse_m <- sem(negtv_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)
#adoption of contrarion style in partner and dependent style in patient when partner higher on avoidant sx




szoid_vanBse<- "

scpt ~    szoid_sidp_patient 
scpr ~   szoid_sidp_partner 
ccpr ~ szoid_sidp_partner  
ccpt ~   szoid_sidp_patient 
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
szoid_vanBse_m <- sem(szoid_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)
#trending effect such that szoid symptoms in partner associated with greater self-coupling in partner; significant if only look at self effects




histr_vanBse<- "

scpt ~    histr_sidp_patient + histr_sidp_partner
scpr ~   histr_sidp_partner + histr_sidp_patient
ccpr ~ histr_sidp_partner  + histr_sidp_patient
ccpt ~   histr_sidp_patient + histr_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
histr_vanBse_m <- sem(histr_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)
#greater histrionism in patient, associated with greater self coupling, less self-coupling in partner and trending increase in cross-coupling in patient. Additionall, greater histrionism in partner as ssociated with less self coupling in partner. 


antso_vanBse<- "

scpt ~    antso_sidp_patient + antso_sidp_partner
scpr ~   antso_sidp_partner + antso_sidp_patient
ccpr ~ antso_sidp_partner  + antso_sidp_patient
ccpt ~   antso_sidp_patient + antso_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
antso_vanBse_m <- sem(antso_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)



depen_vanBse<- "

scpt ~    depen_sidp_patient + depen_sidp_partner
scpr ~   depen_sidp_partner + depen_sidp_patient
ccpr ~ depen_sidp_partner  + depen_sidp_patient
ccpt ~   depen_sidp_patient + depen_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
depen_vanBse_m <- sem(depen_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)



obcmp_vanBse<- "

scpt ~    obcmp_sidp_patient + obcmp_sidp_partner
scpr ~   obcmp_sidp_partner + obcmp_sidp_patient
ccpr ~ obcmp_sidp_partner  + obcmp_sidp_patient
ccpt ~   obcmp_sidp_patient + obcmp_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
obcmp_vanBse_m <- sem(obcmp_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)


parnd_vanBse<- "

scpt ~    parnd_sidp_patient 
scpr ~   parnd_sidp_partner 
ccpr ~ parnd_sidp_partner  
ccpt ~   parnd_sidp_patient 
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
parnd_vanBse_m <- sem(parnd_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)
#parnd_sidp —> greater paranoid symptoms associated with greater self-coupling in partners. However, that only is the case when only looking at self




stypl_vanBse<- "

scpt ~    stypl_sidp_patient + stypl_sidp_partner
scpr ~   stypl_sidp_partner + stypl_sidp_patient
ccpr ~ stypl_sidp_partner  + stypl_sidp_patient
ccpt ~   stypl_sidp_patient + stypl_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
stypl_vanBse_m <- sem(stypl_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)
#trending effect such that greater stypl symptoms in patient associated with greater self-coupling in partner




deprs_vanBse<- "

scpt ~    deprs_sidp_patient + deprs_sidp_partner
scpr ~   deprs_sidp_partner + deprs_sidp_patient
ccpr ~ deprs_sidp_partner  + deprs_sidp_patient
ccpt ~   deprs_sidp_patient + deprs_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr 


"
deprs_vanBse_m <- sem(deprs_vanBse, df_nooutliers_vanBse, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)
#trending effect such that greater stypl symptoms in patient associated with greater self-coupling in partner









