library(R.matlab)
library(lavaan)
library(dplyr)
#setwd("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/")
setwd("~/Desktop")
dt <- readMat("rawparams.mat")
dtm1 <- as.data.frame(dt$sfrawparamsm1, row.names = as.integer(dt$ids))
dtm2 <- as.data.frame(dt$sfrawparamsm2, row.names = as.integer(dt$ids))
dtm3 <- as.data.frame(dt$sfrawparamsm3, row.names = as.integer(dt$ids))
colnames <- c("self_coupling_patient", "cross_coupling_patient", "self_coupling_partner", "cross_coupling_partner")
dtm4 <- as.data.frame(dt$sfrawparamsm4, row.names = as.integer(dt$ids))
dtm5 <- as.data.frame(dt$sfrawparamsm5, row.names = as.integer(dt$ids))




png("self_coupling_patient.png", width = 3, height = 3, units = "in", res = 300)
a <- qplot(self_coupling_patient, data = personalitydata_nooutliers, geom = "histogram", binwidth = .025) +
  ylab("frequency") + xlab("self coupling patient") + theme_grey(base_size = 15)
a
dev.off()

png("self_coupling_partner.png", width = 3, height = 3, units = "in", res = 300)
b <- qplot(self_coupling_partner, data = personalitydata_nooutliers, geom = "histogram", binwidth = .025) +
  ylab("frequency") + xlab("self coupling partner") + theme_grey(base_size = 15)
b
dev.off()

png("cross_coupling_partner.png", width = 3, height = 3, units = "in", res = 300)
c <- qplot(cross_coupling_partner, data = personalitydata_nooutliers, geom = "histogram", binwidth = .025) +
  ylab("frequency") + xlab("cross coupling partner") + theme_grey(base_size = 15)
c
dev.off()

png("cross_coupling_patient.png", width = 3, height = 3, units = "in", res = 300)
d <- qplot(cross_coupling_patient, data = personalitydata_nooutliers, geom = "histogram", binwidth = .025) +
  ylab("frequency") + xlab("cross coupling patient")+ theme_grey(base_size = 15)
d
dev.off()



dtm4$PTNUM <- as.vector(dt$ids)

dtm4 <- dplyr::rename(dtm4,  self_coupling_patient = V1, cross_coupling_patient = V2, self_coupling_partner = V3, cross_coupling_partner = V4)
# dtm4$self_coupling_patient <- dtm4$V1
# dtm4$cross_coupling_patient <- dtm4$V2
# dtm4$self_coupling_partner <- dtm4$V3
# dtm4$cross_coupling_partner <- dtm4$V4
# dtm4 <- select(dtm4, -V1, -V2, -V3, -V4)
lattice::splom(dplyr::select(dtm4, self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner))
#dtm4nooutliers <- dplyr::filter(dtm4, abs(self_coupling_patient) < .3 & abs(cross_coupling_patient)  < .3 & abs(self_coupling_partner) < .3 & abs(cross_coupling_patient) <.3 )
lattice::splom(dplyr::select(personalitydata_prepost, self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner))



##########
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data")

# personalitymeasures <- read.csv("couples_baseline_clinical_9Oct2015.csv")
# dtm4nooutliers_patient <- dplyr::select(dtm4, self_coupling_patient, cross_coupling_patient, PTNUM)
# dtm4nooutliers_patient$self_coupling <- dtm4nooutliers_patient$self_coupling_patient
# dtm4nooutliers_patient$cross_coupling <- dtm4nooutliers_patient$cross_coupling_patient
# dtm4nooutliers_patient <- dplyr::select(dtm4nooutliers_patient, -cross_coupling_patient, -self_coupling_patient)
# pDatPatient <- subset(personalitymeasures, DyadID == 1 )
# pDataPatient <- inner_join(pDatPatient, dtm4nooutliers_patient, by = "PTNUM")
# pDatPartner <- subset(personalitymeasures, DyadID == 0)
# dtm4nooutliers_partner <- dplyr::select(dtm4nooutliers, self_coupling_partner, cross_coupling_partner, PTNUM)
# dtm4nooutliers_partner$self_coupling <- dtm4nooutliers_partner$self_coupling_partner
# dtm4nooutliers_partner$cross_coupling <- dtm4nooutliers_partner$cross_coupling_partner
# dtm4nooutliers_partner <- dplyr::select(dtm4nooutliers_partner, -cross_coupling_partner, -self_coupling_partner)
# pDataPartner <- inner_join(pDatPartner, dtm4nooutliers_partner, by = "PTNUM")
personalitymeasures <- read.csv("couples_baseline_clinical_9Oct2015.csv")
dtm4_patient <- dplyr::select(dtm4, self_coupling_patient, cross_coupling_patient, PTNUM)

dtm4_patient <- rename(dtm4_patient, self_coupling = self_coupling_patient, cross_coupling = cross_coupling_patient)
dtm4_partner <- dplyr::select(dtm4, self_coupling_partner, cross_coupling_partner, PTNUM)
dtm4_partner <- rename(dtm4_partner, self_coupling = self_coupling_partner, cross_coupling = cross_coupling_partner)

# dtm4_patient$self_coupling <- dtm4_patient$self_coupling_patient
# dtm4_patient$cross_coupling <- dtm4_patient$cross_coupling_patient
# dtm4_patient <- dplyr::select(dtm4_patient, -cross_coupling_patient, -self_coupling_patient)
pDatPatient <- subset(personalitymeasures, DyadID == 1 )
pDataPatient <- inner_join(pDatPatient, dtm4_patient, by = "PTNUM")
pDatPartner <- subset(personalitymeasures, DyadID == 0)
# dtm4_partner <- dplyr::select(dtm4, self_coupling_partner, cross_coupling_partner, PTNUM)
# dtm4_partner$self_coupling <- dtm4_partner$self_coupling_partner
# dtm4_partner$cross_coupling <- dtm4_partner$cross_coupling_partner
#dtm4_partner <- dplyr::select(dtm4_partner, -cross_coupling_partner, -self_coupling_partner)
pDataPartner <- inner_join(pDatPartner, dtm4_partner, by = "PTNUM")

names(pDataPartner) <- paste(names(pDataPartner), "partner", sep = "_")
names(pDataPatient) <- paste(names(pDataPatient), "patient", sep = "_")
personalitydata <- data.frame(pDataPatient, pDataPartner)
personalitydata$PTNUM <- personalitydata$PTNUM_patient


setwd("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports")
presaam <- read.csv("SAAMPRE.csv", header = TRUE)
presaam_partner <- subset(presaam, DyadID == 0)
presaam_patient <- subset(presaam, DyadID == 1)
postsaam <- read.csv("SAAMPOST.csv", header = TRUE)
postsaam_partner <- subset(postsaam, DyadID == 0)
postsaam_patient <- subset(postsaam, DyadID == 1)
preimic <- read.csv("IMICPRE.csv", header = TRUE)
preimic_partner <- subset(preimic, DyadID == 0)
preimic_patient <- subset(preimic, DyadID == 1)
postimic <- read.csv("IMICPOST.csv", header = TRUE)
postimic_partner <- subset(postimic, DyadID == 0)
postimic_patient <- subset(postimic, DyadID == 1)

prepostsaam_partner <- inner_join(presaam_partner, postsaam_partner, by = "PTNUM")
prepostsaam_patient <- inner_join(presaam_patient, postsaam_patient, by = "PTNUM")
prepostimic_partner <- inner_join(preimic_partner, postimic_partner, by = "PTNUM")
prepostimic_patient <- inner_join(preimic_patient, postimic_patient, by = "PTNUM")
prepostsaamimic_partner <- inner_join(prepostsaam_partner, prepostimic_partner, by = "PTNUM")
prepostsaamimic_patient <- inner_join(prepostsaam_patient, prepostimic_patient, by = "PTNUM")
names(prepostsaamimic_partner) <- paste(names(prepostsaamimic_partner), "partner", sep = "_")
prepostsaamimic_partner <- rename(prepostsaamimic_partner, PTNUM = PTNUM_partner)
names(prepostsaamimic_patient) <- paste(names(prepostsaamimic_patient), "patient", sep = "_")
prepostsaamimic_patient <- rename(prepostsaamimic_patient, PTNUM = PTNUM_patient)
prepostsaamimic <- inner_join(prepostsaamimic_patient, prepostsaamimic_partner, by = "PTNUM")
anti_join(prepostsaamimic, dtm4, by = "PTNUM")
params_prepost <- inner_join(dtm4, prepostsaamimic, by = "PTNUM")
write.csv(params_prepost, "pre-post_sfparams_outliers_included.csv", row.names = FALSE)

#write.csv(personalitydata_prepost, "pre-post_personalitydata_sfparams.csv", row.names = FALSE)

write.csv(personalitydata_prepost, "pre-post_personalitydata_sfparams_outliers_included.csv", row.names = FALSE)
personalitydata <- read.csv("pre-post_personalitydata_sfparams_outliers_included.csv")

#########
#####now all of the APIMs and SEMs and Mediation Analyses
####need to pull in pre-post measures too for mediation
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")
personalitydata_prepost <- read.csv("pre-post_personalitydata_sfparams_outliers_included.csv")

setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/icsVBA/")
ll <- readMat("IBImodels_logEvidence_feb2017s.mat")
logEvidence <- t(ll$logEvidence)
logEvidence <- as.data.frame(logEvidence)
anybad <- apply(logEvidence, 1, function(row) {
  any(row < -69999)
})
logEvidence$PTNUM <- as.vector(ll$ids)
idlistgood <- as.data.frame(as.vector(ll$ids))
idlistgood$anybad <- as.vector(anybad)
logEvidenceModel4 <- dplyr::select(logEvidence, V4, as.vector(PTNUM))
personalitydata_prepost <- inner_join(personalitydata_prepost, logEvidenceModel4, by = "PTNUM")
personalitydata_prepost <- dplyr::rename(personalitydata_prepost, LL = V4)

personalitydata_nooutliers <- personalitydata_prepost %>% filter(LL > -70000) %>% filter(PTNUM != 8075)
personalitydata_outliers <- filter(personalitydata_prepost, LL < -70000 | PTNUM == 8075)
lattice::splom(dplyr::select(personalitydata_nooutliers, self_coupling_patient, self_coupling_partner, cross_coupling_patient, cross_coupling_partner))

#personalitydata_nooutliers <-  dplyr::filter(personalitydata_prepost, abs(self_coupling_patient) < .135 & abs(cross_coupling_patient)  < .1 & abs(self_coupling_partner) < .135 & abs(cross_coupling_partner) <.1 )

personalitydata_nooutliers <- personalitydata_nooutliers %>% mutate(iip_elcpr = iip_elevation_partner - mean(na.omit(iip_elevation_partner)),
                                  pdcountcpr = allpdCount_partner - mean(na.omit(allpdCount_partner)),
                                  iip_x_pdcount_partner = iip_elcpr * pdcountcpr,
                                  iip_elcpt = iip_elevation_patient - mean(na.omit(iip_elevation_patient)),
                                  pdcountcpt = allpdCount_patient - mean(na.omit(allpdCount_patient)),
                                  iip_x_pdcount_patient = iip_elcpt * pdcountcpt
)
#based on Cook's distance
personalitydata_nooutliers <- filter(personalitydata_nooutliers, PTNUM!=8052, PTNUM !=8063) 
personalitydata_fewoutliers <- dplyr::filter(personalitydata_prepost, abs(self_coupling_patient) < 1 & abs(cross_coupling_patient)  < .5 & abs(self_coupling_partner) < 1 & abs(cross_coupling_partner) <.5 )

##other potential outliers to cut




#personalitydata_prepost <- personalitydata_prepost %>% mutate()

###check to see if people with poor parameters just have bad fit (LL less than -80000)
#ll <-read.csv("parameters_model_feb2017.csv")



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


iipall_self_iipel_other <- "

self_coupling_patient ~    iip_elevation_patient + iip_agency_patient + iip_communion_patient + iip_elevation_partner 
self_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner + iip_elevation_patient 
cross_coupling_patient ~ iip_elevation_patient + iip_agency_patient + iip_communion_patient + iip_elevation_partner 
cross_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner + iip_elevation_patient 


"

iipall_self_iipel_other_m <- sem(iipall_self_iipel_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


iipall_self_iipel_other_nooutliers_m <- sem(iipall_self_iipel_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

# iipall_self_iipel_other_oldest_m <- sem(iipall_self_iipel_other, datClean, missing = "listwise", estimator = "ML", 
#                                      mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)



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


  
  
  
  





anova(iipelcm_self_other_nooutliers_m, iipagcm_self_nooutliers_m, iipelcm_self_nooutliers_m, iipel_self_nooutliers_m, iipag_self_nooutliers_m, iipcom_self_nooutliers_m, iipel_self_other_nooutliers_m, iipag_self_other_nooutliers_m, iipcom_self_other_nooutliers_m, iipall_self_nooutliers_m, iipall_self_other_nooutliers_m, iipall_self_other_afree_no_outliers_m, iipall_self_other_indist_nooutliers_m) #simple 2 is nested in simple 1


iipelag_self <- "

self_coupling_patient ~    iip_agency_patient + iip_elevation_patient
self_coupling_partner ~ iip_agency_partner + iip_elevation_partner
cross_coupling_patient ~ iip_agency_patient + iip_elevation_patient
cross_coupling_partner ~ iip_agency_partner + iip_elevation_partner


"
iipelag_self_m <- sem(iipelag_self, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)



anova(iipel_self_m, iipag_self_m, iipcom_self_m, iipel_self_other_m, iipag_self_other_m, iipcom_self_other_m, iipall_self_m, iipall_self_other_m, iipall_self_other_afree_m, iipall_self_other_pfree_m, iipall_self_other_indist_m, iipelag_self_m) #simple 2 is nested in simple 1

mlist <- c(iipel_self_m, iipag_self_m, iipcom_self_m, iipel_self_other_m, iipag_self_other_m, iipcom_self_other_m, iipall_self_m, iipall_self_other_m, iipelag_self_m)
sapply(mlist, logLik)


iipag_gender_self <- "


self_coupling_patient ~    iip_agency_patient + p_sex_patient
self_coupling_partner ~ iip_agency_partner + p_sex_partner
cross_coupling_patient ~ iip_agency_patient + p_sex_patient
cross_coupling_partner ~ iip_agency_partner + p_sex_partner



"
iipag_gender_self_m <- sem(iipag_gender_self, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                           mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


iipag_gender_self_other <- "


self_coupling_patient ~    iip_agency_patient + p_sex_patient + p_sex_partner
self_coupling_partner ~ iip_agency_partner + p_sex_partner+ p_sex_patient
cross_coupling_patient ~ iip_agency_patient + p_sex_patient + p_sex_partner
cross_coupling_partner ~ iip_agency_partner + p_sex_partner+ p_sex_patient



"
iipag_gender_self_other_m <- sem(iipag_gender_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                           mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

anova(gender_self_m, iipag_gender_self_other_m, iipag_gender_self_m, iipall_self_m, iipag_self_m)


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

iipall_self_iipel_other_gender_self_other <- "


self_coupling_patient ~    iip_elevation_patient + iip_agency_patient + iip_communion_patient + iip_elevation_partner+ p_sex_patient + p_sex_partner
self_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner + iip_elevation_patient + p_sex_patient + p_sex_partner
cross_coupling_patient ~ iip_elevation_patient + iip_agency_patient + iip_communion_patient + iip_elevation_partner + p_sex_patient + p_sex_partner
cross_coupling_partner ~ iip_elevation_partner + iip_agency_partner + iip_communion_partner + iip_elevation_patient + p_sex_patient + p_sex_partner




"

iipall_self_iipel_other_gender_self_other_m <- sem(iipall_self_iipel_other_gender_self_other, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                                       mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
iipall_self_iipel_other_gender_self_other_nooutliers_m <-  sem(iipall_self_iipel_other_gender_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                                                   mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)




genderel_self_other <- "

self_coupling_patient ~     p_sex_patient + p_sex_partner + iip_elcpt + iip_elcpr
self_coupling_partner ~  p_sex_partner + p_sex_patient + iip_elcpt + iip_elcpr
cross_coupling_patient ~ p_sex_patient + p_sex_partner + iip_elcpt + iip_elcpr
cross_coupling_partner ~ p_sex_partner + p_sex_patient +  iip_elcpt + iip_elcpr


"
genderel_self_other_m <- sem(genderel_self_other, personalitydata_nooutliers, missing = "listwise", estimator = "ML", 
                           mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)




anova(iipag_self_m, iipag_gender_self_m, gender_self_m)
mlist2 <- c(iipag_self_m, iipag_gender_self_m, gender_self_m)
sapply(mlist2, logLik)

simple3 <- "
self_coupling_patient ~  iip_elcpr + iip_elcpt
self_coupling_partner ~ iip_elcpr + iip_elcpt 
cross_coupling_patient ~   iip_elcpr + iip_elcpt 
cross_coupling_partner ~   iip_elcpr + iip_elcpt 
"

simple3_m <- sem(simple3, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                 mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
summary(simple3_m)




simple4 <- "
self_coupling_patient ~  pdcountcpt + pdcountcpr
self_coupling_partner ~ pdcountcpt + pdcountcpr
cross_coupling_patient ~   pdcountcpt + pdcountcpr
cross_coupling_partner ~    pdcountcpt + pdcountcpr
"

simple4_m <- sem(simple4, personalitydata, missing = "listwise", estimator = "ML", 
                 mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
summary(simple4_m)


anova(simple2_m, simple3_m, simple4_m)



simple5 <- "
self_coupling_patient ~  iip_elcpr + iip_elcpt + p_sex_patient
self_coupling_partner ~ iip_elcpr + iip_elcpt + p_sex_partner
cross_coupling_patient ~   iip_elcpr + iip_elcpt + p_sex_patient
cross_coupling_partner ~   iip_elcpr + iip_elcpt + p_sex_partner
"

simple5_m <- sem(simple5, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                 mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
summary(simple5_m)

anova(simple5_m, simple1_m)


simple6 <- "
self_coupling_patient ~   p_sex_patient + p_sex_partner
self_coupling_partner ~  p_sex_partner + p_sex_patient
cross_coupling_patient ~  p_sex_patient + p_sex_partner
cross_coupling_partner ~    p_sex_partner + p_sex_patient

"
simple6_m <- sem(simple6, personalitydata_prepost, missing = "listwise", estimator = "ML", 
                 mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
summary(simple6_m)

anova(simple6_m, simple2_m)
