library(R.matlab)
library(dplyr)
library(faoutlier)
library(lavaan)

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
rawparamsModel3 <- as.data.frame(dt$sfrawparamsm3[, 1, ])
rawparamsModel3$PTNUM <- data$ids[1, ]
logEvidence$PTNUM <- as.vector(data$ids)
logEvidenceModel3 <-
  dplyr::select(logEvidence, V3, as.vector(PTNUM))
rawparamsModel3 <-
  dplyr::inner_join(rawparamsModel3, logEvidenceModel3, by = "PTNUM")
idlistgood$PTNUM <- idlistgood$`as.vector(data$ids)`
rawparamsModel3 <-
  dplyr::rename(
    rawparamsModel3,
    `self coupling patient` = V1,
    `cross coupling patient` = V2,
    `self coupling partner` = V3.x,
    `cross coupling partner` = V4,
    `p1star` = V5,
    `p2star` = V6,
    `LL` = V3.y
  )
rawparamsModel3 <-
  merge(rawparamsModel3, idlistgood, by = c("PTNUM"))
rawparamsModel3 <-
  dplyr::select(rawparamsModel3,-`as.vector(data$ids)`)
write.csv(rawparamsModel3, "rawparamsM3.csv")
#Bad would have us exclude 8004 8008 8016 8020 8032 8049 8060 8099 8106 8112 8114 8126 8127 8133 8144 8152
rawparamsModel3BadData <-
  dplyr::filter(rawparamsModel3, anybad == TRUE)
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
rawparamsModel3goodData <-
  dplyr::filter(rawparamsModel3, LL > -79999)
write.csv(rawparamsModel3goodData,
          file = "parameters_model3_modelcomparison_goodData.csv",
          row.names = TRUE)
write.csv(rawparamsModel3, file = "parameters_model3_modelcomparison_allData.csv", row.names = TRUE)



###ALLOWING FOR OUTLIERS --> using faoutlier package, only get rid of outliers at model fitting stage


dt_all <-
  read.csv(file = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/parameters_model3_modelcomparison_allData.csv")
personalitymeasures <-
  read.csv(file = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/couples_baseline_clinical_4April2017.csv")
dt_all_patient <-
  dplyr::select(dt_all, self.coupling.patient, cross.coupling.patient, PTNUM)
dt_all_patient <-
  rename(dt_all_patient,
         self_coupling = self.coupling.patient,
         cross_coupling = cross.coupling.patient)
dt_all_partner <-
  dplyr::select(dt_all, self.coupling.partner, cross.coupling.partner, PTNUM)
dt_all_partner <-
  rename(dt_all_partner,
         self_coupling = self.coupling.partner,
         cross_coupling = cross.coupling.partner)
# dtm4_patient$self_coupling <- dtm4_patient$self_coupling_patient
# dtm4_patient$cross_coupling <- dtm4_patient$cross_coupling_patient
# dtm4_patient <- dplyr::select(dtm4_patient, -cross_coupling_patient, -self_coupling_patient)
personalitymeasures <-
  rename(
    personalitymeasures,
    PTNUM = ptnum,
    DyadID = dyadid,
    UsrID = usrid
  )
pDatPatient <- dplyr::filter(personalitymeasures, DyadID == 1)
pDataPatient_all <-
  inner_join(pDatPatient, dt_all_patient, by = "PTNUM")
pDatPartner <- dplyr::filter(personalitymeasures, DyadID == 0)
pDataPartner_all <-
  inner_join(pDatPartner, dt_all_partner, by = "PTNUM")
#exclude 8139 in patient because DyadID does not exist for partner. Also a ton of missing data. Maybe see if that exists somewhere else.
pDataPatient_all <- dplyr::filter(pDataPatient_all, PTNUM != 8139)
names(pDataPartner_all) <-
  paste(names(pDataPartner_all), "partner", sep = "_")
names(pDataPatient_all) <-
  paste(names(pDataPatient_all), "patient", sep = "_")
personalitydata_all <-
  data.frame(pDataPatient_all, pDataPartner_all)
personalitydata_all$PTNUM <- personalitydata_all$PTNUM_patient
write.csv(personalitydata_all, "params_personalitydata_all.csv")



















######## START HERE
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/")
dt = read.csv("parameters_model3_modelcomparison_goodData.csv", header = TRUE)
#rename ptnum to PTNUM
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")
personalitymeasures <-
  read.csv("couples_baseline_clinical_4April2017.csv")

dt_patient <-
  dplyr::select(dt, self.coupling.patient, cross.coupling.patient, PTNUM)
dt_patient <-
  rename(dt_patient,
         self_coupling = self.coupling.patient,
         cross_coupling = cross.coupling.patient)
dt_partner <-
  dplyr::select(dt, self.coupling.partner, cross.coupling.partner, PTNUM)
dt_partner <-
  rename(dt_partner,
         self_coupling = self.coupling.partner,
         cross_coupling = cross.coupling.partner)
# dtm4_patient$self_coupling <- dtm4_patient$self_coupling_patient
# dtm4_patient$cross_coupling <- dtm4_patient$cross_coupling_patient
# dtm4_patient <- dplyr::select(dtm4_patient, -cross_coupling_patient, -self_coupling_patient)
personalitymeasures <-
  rename(
    personalitymeasures,
    PTNUM = ptnum,
    DyadID = dyadid,
    UsrID = usrid
  )
pDatPatient <- dplyr::filter(personalitymeasures, DyadID == 1)
pDatPatient <-
  dplyr::filter(pDatPatient, PTNUM != 8123, PTNUM != 8139)
pDataPatient <- inner_join(pDatPatient, dt_patient, by = "PTNUM")
excludePatient <-
  anti_join(pDatPatient, dt_patient, by = "PTNUM") #excludes 81441, 81331, 81271
pDatPartner <- dplyr::filter(personalitymeasures, DyadID == 0)
#not must cut from patient 8123, 8139
# dtm4_partner <- dplyr::select(dtm4, self_coupling_partner, cross_coupling_partner, PTNUM)
# dtm4_partner$self_coupling <- dtm4_partner$self_coupling_partner
# dtm4_partner$cross_coupling <- dtm4_partner$cross_coupling_partner
#dtm4_partner <- dplyr::select(dtm4_partner, -cross_coupling_partner, -self_coupling_partner)
pDataPartner <- inner_join(pDatPartner, dt_partner, by = "PTNUM")
excludePartner <-
  anti_join(pDatPartner, dt_partner, by = "PTNUM") #excludes 8144, 8133, 8127
names(pDataPartner) <-
  paste(names(pDataPartner), "partner", sep = "_")
names(pDataPatient) <-
  paste(names(pDataPatient), "patient", sep = "_")
personalitydata <- data.frame(pDataPatient, pDataPartner)
personalitydata$PTNUM <- personalitydata$PTNUM_patient
write.csv(personalitydata, "params_personalitydata.csv")

########## START HERE
#added variable plot
#go back to the means, may it's not severity but it's style. Style variable
library(lavaan)
library(dplyr)
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")
df <-read.csv(file = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/params_personalitydata.csv")
#df <- read.csv("params_personalitydata_all.csv")
df <-
  df %>% mutate(
    iip_elcpr = iip_elevation_partner - mean(iip_elevation_partner),
    pdcountcpr = allpdCount_partner - mean(na.omit(allpdCount_partner)),
    iip_x_pdcount_partner = iip_elcpr * pdcountcpr,
    iip_elcpt = iip_elevation_patient - mean(na.omit(iip_elevation_patient)),
    pdcountcpt = allpdCount_patient - mean(na.omit(allpdCount_patient)),
    iip_x_pdcount_patient = iip_elcpt * pdcountcpt
  )

df_params_patient <-
  dplyr::select(df, self_coupling_patient, cross_coupling_patient)
df_params_patient$summed_cross_coupling_patient <-
  with(df_params_patient,
       (-1) * self_coupling_patient + cross_coupling_patient)
df_params_partner <-
  dplyr::select(df, self_coupling_partner, cross_coupling_partner)
df_params_partner$summed_cross_coupling_partner <-
  with(df_params_partner,
       (-1) * self_coupling_partner + cross_coupling_partner)
df$summed_cross_coupling_partner <-
  df_params_partner$summed_cross_coupling_partner
df$summed_cross_coupling_patient <-
  df_params_patient$summed_cross_coupling_patient

summary(ols <- lm(iip_elevation_patient ~ self_coupling_patient + cross_coupling_patient, data = df))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
d1 <- cooks.distance(ols)
r <- stdres(ols)
df_shorted <- dplyr::select(df, PTNUM, self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner, iip_elevation_patient, iip_elevation_partner)
a <- cbind(df_shorted, d1, r)
a[d1 > 4/115, ]
###note results before re-running
summary(ols <- lm(iip_elevation_partner ~ self_coupling_partner + cross_coupling_partner, data = df))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
d1 <- cooks.distance(ols)
r <- stdres(ols)
df_shorted <- dplyr::select(df, PTNUM, self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner, iip_elevation_patient, iip_elevation_partner)
a <- cbind(df_shorted, d1, r)
a[d1 > 4/115, ]


#based off cook's D, which is calculated in parameter_clustering_march2017.R, these are the peoople that need to be excluded for no outleirs
df_nooutliers <-
  dplyr::filter(
    df,
    PTNUM != 8007,
    PTNUM != 8085,
    PTNUM != 8126,
    PTNUM != 8137,
    PTNUM != 8149,
    PTNUM != 8052,
    PTNUM != 8063,
    PTNUM != 8075,
    PTNUM != 8148
  )
df_nooutliers_vanBse <-
  dplyr::filter(
    df_nooutliers_vanBse,
    PTNUM != 8007,
    PTNUM != 8085,
    PTNUM != 8126,
    PTNUM != 8137,
    PTNUM != 8149,
    PTNUM != 8052,
    PTNUM != 8063,
    PTNUM != 8075,
    PTNUM != 8148
  )

setwd("~/Desktop/Archive_April2017")
setwd("/Users/ams939/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Archive_April2017")
vanBse_params <- readMat("vanBse_modelcomparison_logevidence.mat")
vanBse_paramsm1 <- as.data.frame((vanBse_params$rawparameters[, 1, ]))
vanBse_paramsm2 <- as.data.frame((vanBse_params$rawparameters[, 2, ]))
vanBse_paramsm3 <- as.data.frame((vanBse_params$rawparameters[, 3, ]))
vanBse_paramsm4 <- as.data.frame((vanBse_params$rawparameters[, 4, ]))
vanBse_paramsm4$PTNUM <- as.numeric(vanBse_params$ids)
# ll <- data.frame()
# ll$ll <- as.data.frame((vanBse_params$logEvidence[4,]))
# ll$PTNUM <- as.numeric(vanBse_params$ids)
# vanBse_paramsm4$ll <- as.data.frame((vanBse_params$logEvidence[4,]))
vanBse_paramsm1 <-
  dplyr::filter(vanBse_paramsm1,
                abs(V1) < .3,
                abs(V2) < .3,
                abs(V3) < .3,
                abs(V4) < .3)
vanBse_paramsm2 <-
  dplyr::filter(vanBse_paramsm2,
                abs(V1) < .3,
                abs(V2) < .3,
                abs(V3) < .3,
                abs(V4) < .3)
vanBse_paramsm3 <-
  dplyr::filter(vanBse_paramsm3,
                abs(V1) < .3,
                abs(V2) < .3,
                abs(V3) < .3,
                abs(V4) < .3)
vanBse_paramsm4 <-
  dplyr::filter(vanBse_paramsm4,
                abs(V1) < .3,
                abs(V2) < .3,
                abs(V3) < .3,
                abs(V4) < .3)

vanBse_paramsm4 <-
  rename(
    vanBse_paramsm4,
    self_coupling_patient = V1,
    cross_coupling_patient = V2,
    self_coupling_parnter = V3,
    cross_coupling_partner = V4
  )
vanBse_paramsm4 <-
  rename(
    vanBse_paramsm4,
    v_self_coupling_patient = self_coupling_patient,
    v_cross_coupling_patient = cross_coupling_patient,
    v_self_coupling_parnter = self_coupling_parnter,
    v_cross_coupling_partner = cross_coupling_partner
  )


df_nooutliers_vanBse <-
  merge(df_nooutliers, vanBse_paramsm4, by = c("PTNUM"))
df_nooutliers_vanBse <-
  dplyr::filter(
    df_nooutliers_vanBse,
    abs(self_coupling_patient) < .3,
    abs(self_coupling_partner) < .3,
    abs(cross_coupling_patient) < .3,
    abs(cross_coupling_partner) < .3
  )
df_nooutliers_vanBse <-
  dplyr::filter(
    df_nooutliers_vanBse,
    abs(v_self_coupling_patient) < .3,
    abs(v_self_coupling_parnter) < .3,
    abs(v_cross_coupling_patient) < .3,
    abs(v_cross_coupling_partner) < .3
  )
df_nooutliers_vanBse <-
  dplyr::mutate(
    df_nooutliers_vanBse,
    scpt = 1000 * self_coupling_patient,
    scpr = 1000 * self_coupling_partner,
    ccpt = 1000 * cross_coupling_patient,
    ccpr = 1000 * cross_coupling_partner,
    v_scpt = 1000 * v_self_coupling_patient,
    v_scpr = 1000 * v_self_coupling_parnter,
    v_ccpt = 1000 * v_cross_coupling_patient,
    v_ccpr = 1000 * v_cross_coupling_partner
  )

dt_vanBse <- merge(df, vanBse_paramsm4, by = c("PTNUM"))
dt_vanBse <-
  dplyr::mutate(
    dt_vanBse,
    scpt = 1000 * self_coupling_patient,
    scpr = 1000 * self_coupling_partner,
    ccpt = 1000 * cross_coupling_patient,
    ccpr = 1000 * cross_coupling_partner,
    v_scpt = 1000 * v_self_coupling_patient,
    v_scpr = 1000 * v_self_coupling_parnter,
    v_ccpt = 1000 * v_cross_coupling_patient,
    v_ccpr = 1000 * v_cross_coupling_partner
  )



#length at this point is 121
#length for df is 122 --> Includes 8078 which is not included in vanBse. Seems to have been cut at some point. Check on ics once back up and running
#length for vanBse_paramsm4 is 125 --> Includes 8139, 8048, 8040, 8001 which are not included in df. 8139 had to be cut because missing personality data for one of them
#8001 should not be analyzed because of bad data
#8040 not eligible
#need to figure out about 8048??? Seems to not have personality measures. Go back to data set where merging

cxsetting = TRUE
#cxsetting = "default" #this will unexpectedly lead to a change in LL scaling for models 3 and 4

iipel_self <- "
self_coupling_patient ~  iip_elevation_patient
self_coupling_partner ~ iip_elevation_partner
summed_cross_coupling_patient ~ iip_elevation_patient
summed_cross_coupling_partner ~ iip_elevation_partner
"
iipel_self_m <-
  sem(
    iipel_self,
    df,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )

iipel_self_delta <- "
delta_scpt ~  iip_elcpt
delta_scpr ~ iip_elcpr
delta_ccpt ~ iip_elcpt
delta_ccpr ~ iip_elcpr
"
iipel_self_delta_m <-
  lavaan::sem(
    iipel_self_delta,
    dt_vanBse_simp_noOutliers,
    missing = "ML",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE) #,
    #conditional.x = cxsetting
  #)
#univariate outlier detection on physiological parameters
#box plots; how many cases are hanging off in the far tales
#rational cut of .3
#based on univariate scpt > -50, scpt < 120, scpr <150, ccpt < 70, ccpr < 60)
#to be more conservative only cut less than 300
#now cutting based on gCD from iipel_self_vanBse
#before cutting ppl at all, dt_vanBse: n = 121
dt_vanBse_simp_noOutliers <-
  dplyr::filter(dt_vanBse_simp, scpt > -50, scpt < 120, scpr < 150, ccpt < 70, ccpr < 60)
#before any more data cleaning: n = 102
dt_vanBse_simp_fewOutliers <-
  dplyr::filter(dt_vanBse_simp,
                abs(scpt) < 300,
                abs(scpr) < 300,
                abs(ccpt) < 300,
                abs(ccpr) < 300)
#before any more data cleaning: n = 116
dt_vanBse_simp_fewOutliers$id <- seq(1, 116)
#cutting gCD's less than 3.5*10^6
#dt_vanBse_simp_noOutliers <- dplyr::filter(dt_vanBse_simp_noOutliers, id != 98, id != 96, id != 73, id != 76, id != 5, id!= 49, id !=59, id!= 90, id != 18, id != 97)
#with gCD
#dt_vanBse_simp_noOutliers <- dplyr::filter(dt_vanBse_simp_noOutliers, id != 71, id != 17, id !=5, id != 90, id != 48, id!= 37, id !=56, id!= 84, id != 47, id != 33)
dt_vanBse_simp_noOutliers$id <- seq(1,102)
dt_vanBse_simp_noOutliers_iipel <-
  dplyr::filter(
    dt_vanBse_simp_noOutliers,
    id != 71,
    id != 17,
    id != 5,
    id != 90,
    id != 48,
    id != 37,
    id != 56,
    id != 84,
    id != 47,
    id != 33
  )
dt_vanBse_simp_noOutliers_pdcpt <-
  dplyr::filter(
    dt_vanBse_simp_noOutliers,
    id != 71  )

iipel_self_vanBse <- "
scpt ~  iip_elcpt
scpr ~ iip_elcpr
ccpt ~ iip_elcpt
ccpr ~ iip_elcpr
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr
scpr  ~ v_ccpr
ccpr  ~ v_scpr

"
iipel_self_vanBse_m <-
  lavaan::sem(
    iipel_self_vanBse,
    dt_vanBse_simp_noOutliers_pdcpt,
    missing = "ML",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE
  ) #,conditional.x=cxsetting)

#preferable to use dt_vanBse_simp_noOutliers because ID 71 only has undue leverage (like really undue leverage for pd count)
#elevation isn't the most homogenous group but also not terrible
#obvious outlier in pdcpt and that's 71
#trying to cut as few observations as possible
#current obs cut based on univariate outliers as identified using box plots. hopefully these will disappear when run mfx model


#based off of forward.search
dt_vanBse_simp_fewOutliers_iipel <-
  dplyr::filter(
    dt_vanBse_simp_fewOutliers,
    id != 67,
    id != 50,
    id != 111,
    id != 49,
    id != 69,
    id != 73,
    id != 27,
    id != 57,
    id != 98,
    id != 83
  )
dt_vanBse_simp_fewOutliers_iipel$ids <- seq(1, 106)
#based off of gCD
dt_vanBse_simp_fewOutliers_iipel <-
  dplyr::filter(
    dt_vanBse_simp_fewOutliers_iipel,
    id != 37,
    id != 93,
    id != 100,
    id != 53,
    id != 83,
    id != 69,
    id != 72,
    id != 91,
    id != 39,
    id != 5
  )
dt_vanBse_simp_fewOutliers_iipel_min1 <-
  dplyr::filter(dt_vanBse_simp_fewOutliers_iipel, scpr < 200)


iipel_self <- "
scpt ~  iip_elcpt
scpr ~ iip_elcpr
ccpt ~ iip_elcpt
ccpr ~ iip_elcpr



"
iipel_self_m <-
  lavaan::sem(
    iipel_self,
    df_nooutliers_vanBse,
    missing = "ML",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE
  )


#looking at the difference between df_nooutliers_vanBse and dt_vanBse_nooutliers in terms of people cut. Only differences are that dt_vanBse keep 8007, df_nooutliers keeps 8020, df keep 8035
#df keeps 8074 and 8093; df keeps 8100; df keeps 8107; dt keeps 8112 and 8115; dt keeps 8133 and f keeps 8135; df keeps 8146; dt keeps 8148

iipel_pdtot_self <- "
self_coupling_patient ~  iip_elcpt + pdcountcpt
self_coupling_partner ~ iip_elcpr  +  pdcountcpr
summed_cross_coupling_patient ~ iip_elcpt +pdcountcpt
summed_cross_coupling_partner ~ iip_elcpr + pdcountcpr
"
iipel_pdtot_self_m <-
  sem(
    iipel_pdtot_self,
    df,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )



iipel_self_nooutliers_m <-
  sem(
    iipel_self,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )


#####only looking at self and cross, no summed cross
##only using nooutlier data

iipel_self_vanBse <- "
scpt ~  iip_elevation_patient
scpr ~ iip_elevation_partner
ccpt ~ iip_elevation_patient
ccpt~~scpt
ccpr ~~scpr
ccpt ~ v_ccpt
ccpr ~v_ccpr
scpt ~ v_scpt
scpr ~ v_scpr
ccpr ~ iip_elevation_partner
scpr ~ v_ccpr
ccpr ~ v_scpr
"
iipel_self_nooutliers_vanBse_m <-
  sem(
    iipel_self_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE)
# ,
#     conditional.x = cxsetting
#   )
dt_vanBse_simp_noOutliers_iipel <-
  dplyr::filter(
    dt_vanBse_simp_noOutliers,
    id != 16,
    id != 28,
    id != 93,
    id != 27,
    id != 99,
    id != 50,
    id != 58,
    id != 82,
    id != 103,
    id != 47
  )
iipel_self_nooutliers_vanBse_m <-
  lavaan::sem(
    iipel_self_vanBse,
    dt_vanBse_simp_noOutliers_iipel,
    missing = "ML",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE
  ) #, conditional.x=cxsetting)

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
"
iipel_self_other_nooutliers_vanBse_m <-
  sem(
    iipel_self_other_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )




iipel_pdtot_self_nosum <- "
self_coupling_patient ~  iip_elcpt + pdcountcpt
self_coupling_partner ~ iip_elcpr  +  pdcountcpr
cross_coupling_partner ~ iip_elcpr + pdcountcpr
cross_coupling_patient ~  iip_elcpt + pdcountcpt
"
iipel_pdtot_self_nosum_nooutliers_m <-
  sem(
    iipel_pdtot_self_nosum,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )

iipel_pdtot_self_vanBse <- "
scpt ~  iip_elcpt + pdcpt
scpr ~ iip_elcpr  +  pdcpr
ccpt ~ iip_elcpr + pdcpr
ccpr ~  iip_elcpt + pdcpt
ccpt ~ v_ccpt
ccpr ~v_ccpr
scpt ~ v_scpt
scpr ~ v_scpr
"
iipel_pdtot_self_vanBse_m <-
  sem(
    iipel_pdtot_self_vanBse,
    dt_vanBse_simp_noOutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE)



iipel_pdtot_interaction_self_nosum <- "
self_coupling_patient ~  iip_elcpt + pdcountcpt + iip_x_pdcount_patient
self_coupling_partner ~ iip_elcpr  +  pdcountcpr + iip_x_pdcount_partner
cross_coupling_patient ~ iip_elcpt +pdcountcpt + iip_x_pdcount_patient
cross_coupling_partner ~ iip_elcpr + pdcountcpr + iip_x_pdcount_partner
"
iipel_pdtot_interaction_self_nosum_nooutliers_m <-
  sem(
    iipel_pdtot_interaction_self_nosum,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )




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
iipel_pdtot_interaction_self_vanBse_m <-
  sem(
    iipel_pdtot_interaction_self_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )







iipag_self <- "

self_coupling_patient ~  iip_agency_patient
self_coupling_partner ~ iip_agency_partner
cross_coupling_patient ~ iip_agency_patient
cross_coupling_partner ~ iip_agency_partner
"
iipag_self_m <-
  sem(
    iipag_self,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )
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
iipag_self_vanBse_m <-
  sem(
    iipag_self_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )



iipcm_self <- "

self_coupling_patient ~  iip_communion_patient
self_coupling_partner ~ iip_communion_partner
cross_coupling_patient ~ iip_communion_patient
cross_coupling_partner ~ iip_communion_partner
"
iipcm_self_m <-
  sem(
    iipcm_self,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )

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
iipcm_self_vanBse_m <-
  sem(
    iipcm_self_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )


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
iipcm_self_other_vanBse_m <-
  sem(
    iipcm_self_other_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )


dt_vanBse <-
  dplyr::mutate(
    dt_vanBse,
    d_self_coupling_partner = self_coupling_partner - v_self_coupling_parnter,
    d_self_coupling_patient = self_coupling_patient - v_self_coupling_patient,
    d_cross_coupling_partner = cross_coupling_partner - v_cross_coupling_partner,
    d_cross_coupling_patient = cross_coupling_patient - v_cross_coupling_patient
  )

dt_vanBse  <-
  dplyr::mutate(
    dt_vanBse,
    delta_scpt = d_self_coupling_patient - mean(na.omit(d_self_coupling_patient)),
    delta_scpr = d_self_coupling_partner - mean(na.omit(d_self_coupling_partner)),
    delta_ccpt = d_cross_coupling_patient - mean(na.omit(d_cross_coupling_patient)),
    delta_ccpr = d_cross_coupling_partner - mean(na.omit(d_cross_coupling_partner))
  )

iipagcm_self <- "

self_coupling_patient ~  iip_communion_patient + iip_agency_patient
self_coupling_partner ~ iip_communion_partner + iip_agency_partner
cross_coupling_patient ~ iip_communion_patient + iip_agency_patient
cross_coupling_partner ~ iip_communion_partner + iip_agency_partner
"
iipagcm_self_m <-
  sem(
    iipagcm_self,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )

iipag_self_other <- "

self_coupling_patient ~  iip_agency_patient + iip_agency_partner
self_coupling_partner ~ iip_agency_partner + iip_agency_patient
cross_coupling_patient ~ iip_agency_patient + iip_agency_partner
cross_coupling_partner ~ iip_agency_partner + iip_agency_patient
"
iipag_self_other_m <-
  sem(
    iipag_self_other,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )


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
iipag_self_other_vanBse_m <-
  sem(
    iipag_self_other_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )






iipcm_self_other <- "

self_coupling_patient ~  iip_communion_patient +iip_communion_partner
self_coupling_partner ~ iip_communion_partner + iip_communion_patient
cross_coupling_patient ~ iip_communion_patient +iip_communion_partner
cross_coupling_partner ~ iip_communion_partner + iip_communion_patient
"
iipcm_self_other_m <-
  sem(
    iipcm_self_other,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )


iipagcm_self_other <- "

self_coupling_patient ~  iip_communion_patient + iip_agency_patient + iip_communion_partner + iip_agency_partner
self_coupling_partner ~ iip_communion_partner + iip_agency_partner + iip_communion_patient + iip_agency_patient
cross_coupling_patient ~ iip_communion_patient + iip_agency_patient + iip_communion_partner + iip_agency_partner
cross_coupling_partner ~ iip_communion_partner + iip_agency_partner + iip_communion_patient + iip_agency_patient
"
iipagcm_self_other_m <-
  sem(
    iipagcm_self_other,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )

iipelagcm_self <- "

self_coupling_patient ~  iip_communion_patient + iip_agency_patient + iip_elevation_patient
self_coupling_partner ~ iip_communion_partner + iip_agency_partner  + iip_elevation_partner
cross_coupling_patient ~ iip_communion_patient + iip_agency_patient + iip_elevation_patient
cross_coupling_partner ~ iip_communion_partner + iip_agency_partner + iip_elevation_partner
"
iipelagcm_self_m <-
  sem(
    iipelagcm_self,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )

iipelagcm_self_other <- "

self_coupling_patient ~  iip_communion_patient + iip_agency_patient + iip_elevation_patient + iip_communion_partner + iip_agency_partner  + iip_elevation_partner
self_coupling_partner ~ iip_communion_partner + iip_agency_partner  + iip_elevation_partner +  iip_communion_patient + iip_agency_patient + iip_elevation_patient
cross_coupling_patient ~ iip_communion_patient + iip_agency_patient + iip_elevation_patient + iip_communion_partner + iip_agency_partner  + iip_elevation_partner
cross_coupling_partner ~ iip_communion_partner + iip_agency_partner + iip_elevation_partner +  iip_communion_patient + iip_agency_patient + iip_elevation_patient
"
iipelagcm_self_other_m <-
  sem(
    iipelagcm_self_other,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )



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
iipelagcm_self_other_vanBse_m <-
  sem(
    iipelagcm_self_other_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )

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
iipelag_self_other_vanBse_m <-
  sem(
    iipelag_self_other_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )




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
iipelag_self_vanBse_m <-
  sem(
    iipelag_self_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )







#iipel_self beats out; then iipel_pdtot_self (no interaction),the iipel_pdtot_interaction,then iipag_self, and finally iipcm_self and iipagcm_self tie (and then p pathways implicated)
#testing now with more dimension pdcount


df_nooutliers_vanBse <-
  df_nooutliers_vanBse %>% mutate(
    pdcpr = pdtot_partner - mean(na.omit(pdtot_partner)),
    iip_x_pdtot_partner = iip_elcpr * pdcpr,
    pdcpt = pdtot_patient - mean(na.omit(pdtot_patient)),
    iip_x_pdtot_patient = iip_elcpt * pdcpt
  )

df_nooutliers_vanBse <-
  df_nooutliers_vanBse %>% mutate(
    d_scpt = scpt - v_scpt,
    d_scpr = scpr - v_scpr,
    d_ccpt = ccpt - v_ccpt,
    d_ccpr = ccpr - v_ccpr
  )




dt_vanBse <-
  dt_vanBse %>% mutate(
    pdcpr = pdtot_partner - mean(na.omit(pdtot_partner)),
    iip_x_pdtot_partner = iip_elcpr * pdcpr,
    pdcpt = pdtot_patient - mean(na.omit(pdtot_patient)),
    iip_x_pdtot_patient = iip_elcpt * pdcpt
  )


iipel_pdtot_dim_self_delta <- "
d_scpt ~  iip_elcpt + pdtot_patient
d_scpr ~ iip_elcpr  +  pdtot_partner
d_ccpr ~ iip_elcpr + pdtot_partner
d_ccpt ~  iip_elcpt + pdtot_patient
"

iipel_pdtot_dim_self_delta_m <-
  lavaan::sem(
    iipel_pdtot_dim_self_delta,
    df_nooutliers_vanBse,
    missing = "ML",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE
  ) #, conditional.x=cxsetting





iipel_pdtot_dim_self_vanBse <- "
scpt ~  iip_elcpt + pdtot_patient
scpr ~ iip_elcpr  +  pdtot_partner
ccpr ~ iip_elcpr + pdtot_partner
ccpt ~  iip_elcpt + pdtot_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr

#testing from modification indices
scpr  ~ v_ccpr
ccpr  ~ v_scpr
"
iipel_pdtot_dim_self_vanBse_m <-
  lavaan::sem(
    iipel_pdtot_dim_self_vanBse,
    dt_vanBse_simp_noOutliers_pdcpt,
    missing = "ML",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE
  )  #conditional.x=cxsetting)
dt_vanBse_simp <-
  dplyr::select(
    dt_vanBse,
    scpt,
    ccpt,
    scpr,
    ccpr,
    iip_elcpt,
    iip_elcpr,
    pdtot_patient,
    pdtot_partner,
    v_scpt,
    v_scpr,
    v_ccpt,
    v_ccpr,
    delta_scpt,
    delta_ccpt,
    delta_scpr,
    delta_ccpr,
    pdcpt,
    pdcpr,
    PTNUM
  )

FS <-
  forward.search(
    dt_vanBse_simp_noOutliers,
    iipel_pdtot_dim_self_vanBse,
    missing = "ML",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE
  )
ols <- lm(scpt ~ pdcpt, data = dt_vanBse_simp_noOutliers)
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
par(opar)
d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(dt_vanBse_simp_noOutliers, d1, r)
#a <- a %>% dplyr::filter(PTNUM == 8007 | PTNUM == 8010 | PTNUM == 8017 | PTNUM == 8018 | PTNUM == 8039 | PTNUM == 8050 | PTNUM == 8057 | PTNUM == 8062 | PTNUM == 8063 | PTNUM == 8069 | PTNUM == 8074 | PTNUM == 8086 | PTNUM == 8091 |PTNUM == 8092 | PTNUM == 8094 |PTNUM == 8095 |PTNUM == 8096| PTNUM == 8105 |PTNUM == 8107 | PTNUM == 8109 |PTNUM == 8110 |PTNUM == 8128)
a[d1 > 4 / 102,]
a[d1 > 4 / 102,] %>% dplyr::select(scpt, scpr, ccpt, ccpr, PTNUM, d1)
#dt_vanBse_simp_noOutliers <- dplyr::filter(dt_vanBse_simp, PTNUM != 8016, PTNUM != 8060, PTNUM != 8106, PTNUM != 8126, PTNUM != 8144)

#dt_vanBse_noOutliers <- dplyr::filter(dt_vanBse, )

#dt_vanBse <- merge(dt_vanBse, ll, by = PTNUM)

summary(iipel_pdtot_dim_self_vanBse_m, fit.measures = TRUE)
standardizedSolution(iipel_pdtot_dim_self_vanBse_m, type = "std.all")
lavaan::modificationIndices(iipel_pdtot_dim_self_vanBse_m,
                            minimum.value = 3,
                            sort. = TRUE)
lavaan::modificationIndices(iipel_self_vanBse_m,
                            minimum.value = 3,
                            sort. = TRUE)

corwithtarget(
  df_nooutliers_vanBse,
  target = c("scpt", "scpr"),
  with = c("iip_elcpt", "iip_elcpr"),
  partial = c("pdtot_patient", "pdtot_partner")
)

summary(lm(scpr ~ iip_elcpr + pdtot_partner, df_nooutliers_vanBse))
summary(lm(scpr ~ iip_elcpr + pdtot_partner + v_scpr, df_nooutliers_vanBse))

scpr_clean <-
  residuals(lm(scpr ~ pdtot_partner + v_scpr, df_nooutliers_vanBse))
iip_clean <-
  residuals(lm(iip_elcpr ~ pdtot_partner + v_scpr, df_nooutliers_vanBse))
df <- data.frame(scpr = scpr_clean, iip = iip_clean)
#df <- data.frame(scpr=df_nooutliers_vanBse$scpr, iip=iip_clean)
ggplot(df, aes(x = scpr, y = iip)) + geom_point() + stat_smooth(method =
                                                                  "lm")
ggplot(datGraph, aes(x = scpt, y = iip_elcpt)) + geom_point() + stat_smooth(method =
                                                                              "lm")
datGraph <- as.data.frame(datGraph)

pcor_dataset <-
  dplyr::select(df_nooutliers_vanBse, scpt, scpr, v_scpt, v_scpr)

#correlate one set of variables (target) with another set (withvars)
corwithtarget <-
  function(df,
           omit = NULL,
           target,
           withvars = NULL,
           pmin = NULL,
           partial = NULL,
           absrmin = NULL,
           digits = 3,
           prewhiten = FALSE,
           orderbyr = FALSE) {
    require(forecast)
    
    if (!is.null(omit)) {
      dnames <- which(names(df) %in% omit)
      df <- df[, -1 * dnames]
    }
    
    if (is.null(withvars)) {
      withvars <- names(df)[which(!names(df) %in% target)]
    }
    
    #limit data.frame to variables in target, with, and partial
    df <- df[, c(target, withvars, partial)]
    
    if (!is.null(partial)) {
      df <- as.data.frame(lapply(df, function(col) {
        residuals(lm(col ~ as.matrix(df[, partial])))
      }))
    }
    
    
    
    res <- sapply(target, function(tv) {
      cvec <- sapply(withvars, function(wv) {
        #prewhiten?
        if (prewhiten) {
          r <- residuals(lm(df[, tv] ~ df[, wv]))
          a <- auto.arima(r)
          x <- Arima(df[, wv], model = a)$residuals
          y <- Arima(df[, tv], model = a)$residuals
        } else {
          x <- df[, wv]
          y <- df[, tv]
        }
        
        tryCatch(
          rc <-
            Hmisc::rcorr(x, y),
          error = function(e) {
            print(e)
            browser()
          }
        )
        list(r = round(rc$r[1, 2], 3), p = round(rc$P[1, 2], 3))
      })
      
      if (!is.null(pmin)) {
        sigr <- which(unlist(cvec["p", ]) <= pmin)
        if (length(sigr) == 0L) {
          cvec <- c()
        } else {
          cvec <- cvec[, sigr, drop = FALSE]
        }
      }
      
      if (!is.null(absrmin)) {
        goodr <- which(abs(unlist(cvec["r", ])) >= absrmin)
        if (length(goodr) == 0L) {
          cvec <- c()
        } else {
          cvec <- cvec[, goodr, drop = FALSE]
        }
      }
      
      #be sure that we never include the correlation of the variable with itself
      selfmatch <- dimnames(cvec)[[2]] == tv
      cvec <- cvec[, !selfmatch, drop = FALSE]
      
      #reorder by correlation size if requested
      if (orderbyr == TRUE) {
        cvec <- cvec[, order(unlist(cvec[1, ]), decreasing = TRUE)]
      }
      
      return(cvec)
      
      #print(cvec)
    }, simplify = FALSE)
    
    return(res)
  }



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
pdtot_dim_vanBse_m <-
  sem(
    pdtot_dim_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )






iipel_pdtot_interaction_dim_self <- "
self_coupling_patient ~  iip_elcpt + pdcpt + iip_x_pdtot_patient
self_coupling_partner ~ iip_elcpr  +  pdcpr + iip_x_pdtot_partner
cross_coupling_patient ~ iip_elcpt +pdcpt + iip_x_pdtot_patient
cross_coupling_partner ~ iip_elcpr + pdcpr + iip_x_pdtot_partner
"
iipel_pdtot_interaction_dim_self_m <-
  sem(
    iipel_pdtot_interaction_dim_self,
    df_nooutliers,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )

iipel_pdtot_interaction_dim_self_vanBse <- "
scpt ~  iip_elcpt + pdcpt + iip_x_pdtot_patient
scpr ~ iip_elcpr  +  pdcpr + iip_x_pdtot_partner
ccpt ~ iip_elcpt +pdcpt + iip_x_pdtot_patient
ccpr ~ iip_elcpr + pdcpr + iip_x_pdtot_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr
"
iipel_pdtot_interaction_dim_self_vanBse_m <-
  sem(
    iipel_pdtot_interaction_dim_self_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )



pdtot_dim_self_vanBse <- "

scpt ~    pdcpt
scpr ~   pdcpr
ccpr ~ pdcpr
ccpt ~   pdcpt
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
pdtot_dim_self_vanBse_m <-
  sem(
    pdtot_dim_self_vanBse,
    dt_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )


bordl_vanBse <- "

scpt ~    bordl_sidp_patient
scpr ~   bordl_sidp_partner
ccpr ~ bordl_sidp_partner
ccpt ~   bordl_sidp_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
bordl_vanBse_m <-
  sem(
    bordl_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )

#greater self coupling in partners, suggesting that for partners who have borderline symptoms they have this inflexilibity in HR


narci_vanBse <- "

scpt ~    narci_sidp_patient + narci_sidp_partner
scpr ~   narci_sidp_partner + narci_sidp_patient
ccpr ~ narci_sidp_partner  + narci_sidp_patient
ccpt ~   narci_sidp_patient + narci_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
narci_vanBse_m <-
  sem(
    narci_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )

#greater self coupling in partners, suggesting that for partners who have borderline symptoms they have this inflexilibity in HR



avoid_vanBse <- "

scpt ~    avoid_sidp_patient + avoid_sidp_partner
scpr ~   avoid_sidp_partner + avoid_sidp_patient
ccpr ~ avoid_sidp_partner  + avoid_sidp_patient
ccpt ~   avoid_sidp_patient + avoid_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
avoid_vanBse_m <-
  sem(
    avoid_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )
#adoption of contrarion style in partner and dependent style in patient when partner higher on avoidant sx




negtv_vanBse <- "

scpt ~    negtv_sidp_patient + negtv_sidp_partner
scpr ~   negtv_sidp_partner + negtv_sidp_patient
ccpr ~ negtv_sidp_partner  + negtv_sidp_patient
ccpt ~   negtv_sidp_patient + negtv_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
negtv_vanBse_m <-
  sem(
    negtv_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )
#adoption of contrarion style in partner and dependent style in patient when partner higher on avoidant sx




szoid_vanBse <- "

scpt ~    szoid_sidp_patient
scpr ~   szoid_sidp_partner
ccpr ~ szoid_sidp_partner
ccpt ~   szoid_sidp_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
szoid_vanBse_m <-
  sem(
    szoid_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )
#trending effect such that szoid symptoms in partner associated with greater self-coupling in partner; significant if only look at self effects




histr_vanBse <- "

scpt ~    histr_sidp_patient + histr_sidp_partner
scpr ~   histr_sidp_partner + histr_sidp_patient
ccpr ~ histr_sidp_partner  + histr_sidp_patient
ccpt ~   histr_sidp_patient + histr_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
histr_vanBse_m <-
  sem(
    histr_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )
#greater histrionism in patient, associated with greater self coupling, less self-coupling in partner and trending increase in cross-coupling in patient. Additionall, greater histrionism in partner as ssociated with less self coupling in partner.


antso_vanBse <- "

scpt ~    antso_sidp_patient + antso_sidp_partner
scpr ~   antso_sidp_partner + antso_sidp_patient
ccpr ~ antso_sidp_partner  + antso_sidp_patient
ccpt ~   antso_sidp_patient + antso_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
antso_vanBse_m <-
  sem(
    antso_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )



depen_vanBse <- "

scpt ~    depen_sidp_patient + depen_sidp_partner
scpr ~   depen_sidp_partner + depen_sidp_patient
ccpr ~ depen_sidp_partner  + depen_sidp_patient
ccpt ~   depen_sidp_patient + depen_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
depen_vanBse_m <-
  sem(
    depen_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )



obcmp_vanBse <- "

scpt ~    obcmp_sidp_patient + obcmp_sidp_partner
scpr ~   obcmp_sidp_partner + obcmp_sidp_patient
ccpr ~ obcmp_sidp_partner  + obcmp_sidp_patient
ccpt ~   obcmp_sidp_patient + obcmp_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
obcmp_vanBse_m <-
  sem(
    obcmp_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )


parnd_vanBse <- "

scpt ~    parnd_sidp_patient
scpr ~   parnd_sidp_partner
ccpr ~ parnd_sidp_partner
ccpt ~   parnd_sidp_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
parnd_vanBse_m <-
  sem(
    parnd_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )
#parnd_sidp —> greater paranoid symptoms associated with greater self-coupling in partners. However, that only is the case when only looking at self




stypl_vanBse <- "

scpt ~    stypl_sidp_patient + stypl_sidp_partner
scpr ~   stypl_sidp_partner + stypl_sidp_patient
ccpr ~ stypl_sidp_partner  + stypl_sidp_patient
ccpt ~   stypl_sidp_patient + stypl_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
stypl_vanBse_m <-
  sem(
    stypl_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )
#trending effect such that greater stypl symptoms in patient associated with greater self-coupling in partner




deprs_vanBse <- "

scpt ~    deprs_sidp_patient + deprs_sidp_partner
scpr ~   deprs_sidp_partner + deprs_sidp_patient
ccpr ~ deprs_sidp_partner  + deprs_sidp_patient
ccpt ~   deprs_sidp_patient + deprs_sidp_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr


"
deprs_vanBse_m <-
  sem(
    deprs_vanBse,
    df_nooutliers_vanBse,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = cxsetting
  )
#trending effect such that greater stypl symptoms in patient associated with greater self-coupling in partner
