library(R.matlab)
library(lavaan)
library(dplyr)
#########
#####now all of the APIMs and SEMs and Mediation Analyses
####need to pull in pre-post measures too for mediation
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")
personalitydata_prepost <- read.csv("pre-post_personalitydata_sfparams_outliers_included.csv")



#based on cutoffs used before
#LL v bad for 8008, 8016, 8020, 8009, 8112
#LL also bad for 8035 --> deemed ineligble, exclude from analysis
#should also exclude 8073, deemed ineligible
#Excluded 8052, 8063, 8066, 8100 --> 8052 is probably okay (just above the threshhold), 8063 has wack self coupling and cross coupling for patient
#8066 also wacky self coupling for for patient; 8078 wacky self coupling for partner

#who got excluded and why?
personalitydata_outliers <-  dplyr::filter(personalitydata_prepost, abs(self_coupling_patient) > .135 | abs(cross_coupling_patient)  > .1 | abs(self_coupling_partner) > .135 | abs(cross_coupling_partner) >.1 )

setwd("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/icsVBA/")
ll <- readMat("IBImodels_logEvidence_feb2017s.mat")
logEvidence <- t(ll$logEvidence)
logEvidence <- as.data.frame(logEvidence)
anybad <- apply(logEvidence, 1, function(row) {
  any(row < -69999)
})
logEvidence$PTNUM <- as.vector(ll$ids)
idlistgood <- as.data.frame(as.vector(ll$ids))
idlistgood$anybad <- as.vector(anybad)
logEvidenceModel4 <- select(logEvidence, V4, as.vector(PTNUM))
personalitydata_outliers <- inner_join(personalitydata_prepost, logEvidenceModel4, by = "PTNUM")
personalitydata_outliers <- rename(personalitydata_outliers, LL = V4)
dplyr::filter(personalitydata_outliers, LL < -69999) %>% select(PTNUM)


####run this to get sense of what these coupling params are for people that are being excluded as well as what their ll looks like
select(personalitydata_outliers, self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner, PTNUM, LL)



personalitydata_nooutliers <-  dplyr::filter(personalitydata_prepost, abs(self_coupling_patient) < .135 & abs(cross_coupling_patient)  < .1 & abs(self_coupling_partner) < .135 & abs(cross_coupling_partner) <.1 )

personalitydata_nooutliers <- personalitydata_nooutliers %>% mutate(iip_elcpr = iip_elevation_partner - mean(na.omit(iip_elevation_partner)),
                                                                    pdcountcpr = allpdCount_partner - mean(na.omit(allpdCount_partner)),
                                                                    iip_x_pdcount_partner = iip_elcpr * pdcountcpr,
                                                                    iip_elcpt = iip_elevation_patient - mean(na.omit(iip_elevation_patient)),
                                                                    pdcountcpt = allpdCount_patient - mean(na.omit(allpdCount_patient)),
                                                                    iip_x_pdcount_patient = iip_elcpt * pdcountcpt
)


##now for all the APIMs
##structure of analyses
##first, run each iip factor individually
##then, run each iip factor individually but for self and other
##third, run all iip factors together for self, then self and other, then other
##fourth, because p pathways less significant, test afree and indistinguishable
##finally, test combinations of agency, communion and elevation
##NOTE: feel free to compare no outliers and data including outliers BUT LL will be completely different

cxsetting = TRUE
#cxsetting = "default" #this will unexpectedly lead to a change in LL scaling for models 3 and 4

iipel_self <- "
self_coupling_patient ~  iip_elevation_patient
self_coupling_partner ~ iip_elevation_partner
cross_coupling_patient ~ iip_elevation_patient
cross_coupling_partner ~ iip_elevation_partner


"
iipel_self_m <- sem(iipel_self, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipel_self_nooutliers_m <- sem(iipel_self, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                               mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)



iipag_self <- "

self_coupling_patient ~    iip_agency_patient 
self_coupling_partner ~ iip_agency_partner
cross_coupling_patient ~ iip_agency_patient 
cross_coupling_partner ~ iip_agency_partner


"
iipag_self_m <- sem(iipag_self, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipag_self_nooutliers_m <- sem(iipag_self, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                               mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipcom_self <- "

self_coupling_patient ~    iip_communion_patient
self_coupling_partner ~ iip_communion_partner
cross_coupling_patient ~ iip_communion_patient
cross_coupling_partner ~ iip_communion_partner


"
iipcom_self_m <- sem(iipcom_self, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipcom_self_nooutliers_m <- sem(iipcom_self, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipel_self_other <- "
self_coupling_patient ~  iip_elevation_patient + iip_elevation_partner
self_coupling_partner ~ iip_elevation_patient + iip_elevation_partner
cross_coupling_patient ~ iip_elevation_patient + iip_elevation_partner
cross_coupling_partner ~ iip_elevation_patient + iip_elevation_partner
"

iipel_self_other_m <- sem(iipel_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)
iipel_self_other_nooutliers_m <- sem(iipel_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                     mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipag_self_other <- "

self_coupling_patient ~    iip_agency_patient + iip_agency_partner
self_coupling_partner ~ iip_agency_partner + iip_agency_patient
cross_coupling_patient ~ iip_agency_patient + iip_agency_partner
cross_coupling_partner ~ iip_agency_partner + iip_agency_patient


"
iipag_self_other_m <- sem(iipag_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipag_self_other_nooutliers_m <- sem(iipag_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                     mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipcom_self_other <- "

self_coupling_patient ~    iip_communion_patient + iip_communion_partner
self_coupling_partner ~ iip_communion_partner + iip_communion_patient
cross_coupling_patient ~ iip_communion_patient + iip_communion_partner
cross_coupling_partner ~ iip_communion_partner + iip_communion_patient


"
iipcom_self_other_m <- sem(iipcom_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                           mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipcom_self_other_nooutliers_m <- sem(iipcom_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                      mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)



iipall_self <- "
self_coupling_patient ~    iip_elevation_patient + iip_agency_patient + iip_communion_patient
self_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner
cross_coupling_patient ~ iip_elevation_patient + iip_agency_patient + iip_communion_patient
cross_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner
"

iipall_self_m <- sem(iipall_self, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

iipall_self_nooutliers_m <- sem(iipall_self, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
# iipall_self_goodll_m <- sem(iipall_self, personalitydata_goodll, missing = "listwise", estimator = "ML", 
#                       mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

iipall_self_other <- "
self_coupling_patient ~    iip_elevation_patient + iip_agency_patient + iip_communion_patient + iip_elevation_partner + iip_agency_partner + iip_communion_partner
self_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner + iip_elevation_patient + iip_agency_patient + iip_communion_patient
cross_coupling_patient ~ iip_elevation_patient + iip_agency_patient + iip_communion_patient + iip_elevation_partner + iip_agency_partner + iip_communion_partner
cross_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner + iip_elevation_patient + iip_agency_patient + iip_communion_patient
"

iipall_self_other_m <- sem(iipall_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                           mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


iipall_self_other_nooutliers_m <- sem(iipall_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                      mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


iipall_other <- "
self_coupling_patient ~    iip_elevation_partner + iip_agency_partner + iip_communion_partner
self_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner 
cross_coupling_patient ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner
cross_coupling_partner ~  iip_elevation_patient + iip_agency_patient + iip_communion_patient
"

iipall_other_m <- sem(iipall_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


iipall_other_nooutliers_m <- sem(iipall_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


iipall_self_other_afree <- "
self_coupling_patient ~    iip_elevation_patient + iip_agency_patient + iip_communion_patient + a1*iip_elevation_partner + a2*iip_agency_partner + a3*iip_communion_partner
self_coupling_partner ~ a1*iip_elevation_partner + a2*iip_agency_partner + a3*iip_communion_partner + iip_elevation_patient + iip_agency_patient + iip_communion_patient
cross_coupling_patient ~ iip_elevation_patient + iip_agency_patient + iip_communion_patient + b1*iip_elevation_partner + b2*iip_agency_partner + b3*iip_communion_partner
cross_coupling_partner ~ b1*iip_elevation_partner + b2*iip_agency_partner + b3*iip_communion_partner + iip_elevation_patient + iip_agency_patient + iip_communion_patient
"

iipall_self_other_afree_m <- sem(iipall_self_other_afree, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

iipall_self_other_afree_no_outliers_m <- sem(iipall_self_other_afree, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                             mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


iipall_self_other_indist <- "
self_coupling_patient ~    a1*iip_elevation_patient + a2*iip_agency_patient + a3*iip_communion_patient + a4*iip_elevation_partner + a5*iip_agency_partner + a6*iip_communion_partner
self_coupling_partner ~ a4*iip_elevation_partner + a5*iip_agency_partner + a6*iip_communion_partner + a1*iip_elevation_patient + a2*iip_agency_patient + a3*iip_communion_patient
cross_coupling_patient ~ b1*iip_elevation_patient + b2*iip_agency_patient + b3*iip_communion_patient + b4*iip_elevation_partner + b5*iip_agency_partner + b6*iip_communion_partner
cross_coupling_partner ~ b4*iip_elevation_partner + b5*iip_agency_partner + b6*iip_communion_partner + b1*iip_elevation_patient + b2*iip_agency_patient + b3*iip_communion_patient
"

iipall_self_other_indist_m <- sem(iipall_self_other_indist, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                                  mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


iipall_self_other_indist_nooutliers_m <- sem(iipall_self_other_indist, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                             mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


##elevation and communion 
iipelcm_self <- "

self_coupling_patient ~  iip_elevation_patient + iip_communion_patient
self_coupling_partner ~ iip_elevation_partner + iip_communion_partner
cross_coupling_patient ~ iip_elevation_patient + iip_communion_patient
cross_coupling_partner ~ iip_elevation_partner + iip_communion_partner


"



iipelcm_self_m <- sem(iipelcm_self, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipelcm_self_nooutliers_m <- sem(iipelcm_self, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipelcm_self_other <- "

self_coupling_patient ~  iip_elevation_patient + iip_elevation_partner +iip_communion_patient+ iip_communion_partner
self_coupling_partner ~ iip_elevation_partner + iip_elevation_patient + iip_communion_partner  +iip_communion_patient
cross_coupling_patient ~ iip_elevation_patient + iip_elevation_partner + iip_communion_patient+ iip_communion_partner
cross_coupling_partner ~ iip_elevation_partner + iip_elevation_patient + iip_communion_partner +iip_communion_patient


"



iipelcm_self_other_m <- sem(iipelcm_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                            mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipelcm_self_other_nooutliers_m <- sem(iipelcm_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                       mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


##agency and communion
iipagcm_self <- "

self_coupling_patient ~  iip_agency_patient + iip_communion_patient
self_coupling_partner ~ iip_agency_partner + iip_communion_partner
cross_coupling_patient ~ iip_agency_patient + iip_communion_patient
cross_coupling_partner ~ iip_agency_partner + iip_communion_partner


"



iipagcm_self_m <- sem(iipagcm_self, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipagcm_self_nooutliers_m <- sem(iipagcm_self, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipagcm_self_other <- "

self_coupling_patient ~  iip_agency_patient + iip_communion_patient + iip_agency_partner + iip_communion_partner
self_coupling_partner ~ iip_agency_partner + iip_communion_partner + iip_agency_patient + iip_communion_patient
cross_coupling_patient ~ iip_agency_patient + iip_communion_patient + iip_agency_partner + iip_communion_partner
cross_coupling_partner ~ iip_agency_partner + iip_communion_partner + iip_agency_patient + iip_communion_patient


"



iipagcm_self_other_m <- sem(iipagcm_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipagcm_self_other_nooutliers_m <- sem(iipagcm_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)




##elevation and agency
iipelag_self <- "

self_coupling_patient ~    iip_agency_patient + iip_elevation_patient
self_coupling_partner ~ iip_agency_partner + iip_elevation_partner
cross_coupling_patient ~ iip_agency_patient + iip_elevation_patient
cross_coupling_partner ~ iip_agency_partner + iip_elevation_partner


"
iipelag_self_m <- sem(iipelag_self, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipelag_self_nooutliers_m <- sem(iipelag_self, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipelag_self_other <- "

self_coupling_patient ~   iip_agency_patient + iip_elevation_patient +  iip_agency_partner + iip_elevation_partner
self_coupling_partner ~ iip_agency_partner + iip_elevation_partner + iip_agency_patient + iip_elevation_patient 
cross_coupling_patient ~ iip_agency_patient + iip_elevation_patient +  iip_agency_partner + iip_elevation_partner
cross_coupling_partner ~ iip_agency_partner + iip_elevation_partner + iip_agency_patient + iip_elevation_patient 


"
iipelag_self_other_m <- sem(iipelag_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipelag_self_other_nooutliers_m <- sem(iipelag_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


anova(iipel_self_nooutliers_m, iipag_self_nooutliers_m, iipcom_self_nooutliers_m, iipel_self_other_nooutliers_m, iipag_self_other_nooutliers_m, iipcom_self_other_nooutliers_m, iipall_self_nooutliers_m, iipall_self_other_nooutliers_m, iipall_self_other_afree_no_outliers_m, iipall_self_other_indist_nooutliers_m, iipelcm_self_nooutliers_m, iipagcm_self_nooutliers_m, iipelag_self_nooutliers_m, iipelag_self_other_nooutliers_m, iipelcm_self_other_nooutliers_m, iipagcm_self_other_nooutliers_m)

anova(iipelcm_self_nooutliers_m, iipel_self_nooutliers_m) #non significant difference
anova(iipall_self_other_nooutliers_m, iipall_self_other_afree_no_outliers_m) #non significant difference

mlist <- c(iipel_self_nooutliers_m, iipag_self_nooutliers_m, iipcom_self_nooutliers_m, iipel_self_other_nooutliers_m, iipag_self_other_nooutliers_m, iipcom_self_other_nooutliers_m, iipall_self_nooutliers_m, iipall_self_other_nooutliers_m, iipall_self_other_afree_no_outliers_m, iipall_self_other_indist_nooutliers_m, iipelcm_self_other_nooutliers_m, iipagcm_self_nooutliers_m, iipelag_self_nooutliers_m, iipelag_self_other_nooutliers_m, iipelcm_self_nooutliers_m, iipagcm_self_nooutliers_m)

sapply(mlist, logLik)





####now look at gender

personalitydata_gendertrue <- filter(personalitydata_nooutliers, !is.na(p_sex_patient))

iipelcm_self_gendertrue_m <- sem(iipelcm_self, personalitydata_gendertrue, missing = "listwise", estimator = "ML", 
                                       mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipel_self_gendertrue_m <- sem(iipel_self, personalitydata_gendertrue, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)
iipel_self_other_gendertrue_m <- sem(iipel_self_other, personalitydata_gendertrue, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


gender_self_other <- "

self_coupling_patient ~     p_sex_patient + p_sex_partner
self_coupling_partner ~  p_sex_partner + p_sex_patient
cross_coupling_patient ~ p_sex_patient + p_sex_partner
cross_coupling_partner ~ p_sex_partner + p_sex_patient


"
gender_self_other_m <- sem(gender_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                           mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
gender_self_other_nooutliers_m <-  sem(gender_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                       mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

gender_self_other_gendertrue_m <-  sem(gender_self_other, personalitydata_gendertrue, missing = "listwise", estimator = "ML", 
                                       mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

iipall_self_gender_self_other <- "


self_coupling_patient ~    iip_elevation_patient + iip_agency_patient + iip_communion_patient + p_sex_patient + p_sex_partner
self_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner + p_sex_patient + p_sex_partner
cross_coupling_patient ~ iip_elevation_patient + iip_agency_patient + iip_communion_patient + p_sex_patient + p_sex_partner
cross_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner + p_sex_patient + p_sex_partner




"

iipall_self_gender_self_other_m <- sem(iipall_self_gender_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                                       mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
iipall_self_gender_self_other_nooutliers_m <-  sem(iipall_self_gender_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                                   mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
iipall_self_gender_self_other_gendertrue_m <-  sem(iipall_self_gender_self_other, personalitydata_gendertrue, missing = "listwise", estimator = "ML", 
                                                   mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


iipel_self_gender_self_other <- "


self_coupling_patient ~    iip_elevation_patient + p_sex_patient + p_sex_partner
self_coupling_partner ~ iip_elevation_partner +  p_sex_patient + p_sex_partner
cross_coupling_patient ~ iip_elevation_patient +  p_sex_patient + p_sex_partner
cross_coupling_partner ~ iip_elevation_partner +  p_sex_patient + p_sex_partner




"

iipel_self_gender_self_other_m <- sem(iipel_self_gender_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                                       mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
iipel_self_gender_self_other_nooutliers_m <-  sem(iipel_self_gender_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                                   mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
iipel_self_gender_self_other_gendertrue_m <-  sem(iipel_self_gender_self_other, personalitydata_gendertrue, missing = "listwise", estimator = "ML", 
                                                  mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

iipelcm_self_gender_self <- "


self_coupling_patient ~    iip_elevation_patient + p_sex_patient + iip_communion_patient
self_coupling_partner ~ iip_elevation_partner +  p_sex_partner + iip_communion_partner
cross_coupling_patient ~ iip_elevation_patient +  p_sex_patient + iip_communion_patient
cross_coupling_partner ~ iip_elevation_partner +   p_sex_partner + iip_communion_partner




"
iipelcm_self_gender_self_gendertrue_m <-  sem(iipelcm_self_gender_self, personalitydata_gendertrue, missing = "listwise", estimator = "ML", 
                                                  mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)



##potentially issue in missingness for gender
