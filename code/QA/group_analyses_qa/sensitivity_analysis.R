#sensitivity analysis
#does model 5, which is nested within model 4 (the conceptual model)
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data")
personalitymeasuresm5 <- read.csv("couples_baseline_clinical_9Oct2015.csv")

setwd("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/")
dtm5 <- read.csv("parameters_model5_feb2017.csv")
dtm5_patient <- dplyr::select(dtm5, a1, a2, PTNUM, F)
dtm5_patient <- dplyr::rename(dtm5_patient, self_coupling = a1, cross_coupling = a2)
dtm5_partner <- dplyr::select(dtm5, b1, PTNUM)
dtm5_partner <- dplyr::rename(dtm5_partner, self_coupling = b1)
pDatPatientm5 <- subset(personalitymeasuresm5, DyadID == 1 )
pDataPatientm5 <- inner_join(pDatPatientm5, dtm5_patient, by = "PTNUM")
pDatPartnerm5 <- subset(personalitymeasuresm5, DyadID == 0)
pDataPartnerm5 <- inner_join(pDatPartnerm5, dtm5_partner, by = "PTNUM")

pDataPartnerm5 <- inner_join(pDatPartnerm5, dtm5_partner, by = "PTNUM")

names(pDataPartnerm5) <- paste(names(pDataPartnerm5), "partner", sep = "_")
names(pDataPatientm5) <- paste(names(pDataPatientm5), "patient", sep = "_")
personalitydatam5 <- data.frame(pDataPatientm5, pDataPartnerm5)
personalitydatam5$PTNUM <- personalitydatam5$PTNUM_patient
personalitydatam5 <- dplyr::rename(personalitydatam5, cross_coupling = cross_coupling_patient, F = F_patient)
personalitydatam5_nooutliers <- data.frame()
personalitydatam5_nooutliers <- dplyr::filter(personalitydatam5, F> -70000)


#checking for outliers
ols <- lm(personalitydatam5_best$self_coupling_partner ~ personalitydatam5_best$iip_elevation_partner, data = personalitydatam5_best)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
par(opar)
d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(personalitydatam5_best, d1, r)
#a <- a %>% dplyr::filter(PTNUM == 8007 | PTNUM == 8010 | PTNUM == 8017 | PTNUM == 8018 | PTNUM == 8039 | PTNUM == 8050 | PTNUM == 8057 | PTNUM == 8062 | PTNUM == 8063 | PTNUM == 8069 | PTNUM == 8074 | PTNUM == 8086 | PTNUM == 8091 |PTNUM == 8092 | PTNUM == 8094 |PTNUM == 8095 |PTNUM == 8096| PTNUM == 8105 |PTNUM == 8107 | PTNUM == 8109 |PTNUM == 8110 |PTNUM == 8128)
a[d1 > 4/21, ]
a[d1 > 4/21, ] %>% dplyr::select(iip_elevation_partner, self_coupling_partner, self_coupling_patient, cross_coupling, PTNUM, d1) 
#based off of leverage, cut 8034, 8046, 8052, 8069, 8075
personalitydatam5_nooutliers <- dplyr::filter(personalitydatam5_nooutliers, PTNUM != 8034, PTNUM!=8046, PTNUM !=8052, PTNUM!= 8069, PTNUM!=8075)
personalitydatam5_sameoutliers <- dplyr::filter(personalitydatam5, PTNUM != 8075, PTNUM!=8008, PTNUM !=8016, PTNUM!= 8020, PTNUM!=8074, PTNUM!=8052, PTNUM!=8099, PTNUM!=8100, PTNUM!=8112, PTNUM!=8063, PTNUM!=8035)
personalitydatam5_best <- dplyr::filter(personalitydatam5, PTNUM == 8007 | PTNUM == 8010 | PTNUM == 8017 | PTNUM == 8018 | PTNUM == 8039 | PTNUM == 8050 | PTNUM == 8057 | PTNUM == 8062 | PTNUM == 8063 | PTNUM == 8069 | PTNUM == 8074 | PTNUM == 8086 | PTNUM == 8091 |PTNUM == 8092 | PTNUM == 8094 |PTNUM == 8095 |PTNUM == 8096| PTNUM == 8105 |PTNUM == 8107 | PTNUM == 8109 |PTNUM == 8110 |PTNUM == 8128) %>% filter(F>-70000)
pdatm5best_nooutliers <- dplyr::filter(personalitydatam5_best, PTNUM !=8022, PTNUM !=8055, PTNUM != 8084, PTNUM != 8115)
cxsetting = TRUE
#cxsetting = "default" #this will unexpectedly lead to a change in LL scaling for models 3 and 4

iipel_self_m5 <- "
self_coupling_patient ~  iip_elevation_patient 
self_coupling_partner ~  iip_elevation_partner
cross_coupling ~ iip_elevation_patient + iip_elevation_partner


"
iipel_self_m5_m <- sem(iipel_self_m5, personalitydatam5_best, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipel_self_m5_nooutliers_m <- sem(iipel_self_m5, pdatm5best_nooutliers, missing = "listwise", estimator = "ML", 
                               mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipel_self_other_m5 <- "
self_coupling_patient ~  iip_elevation_patient + iip_elevation_partner
self_coupling_partner ~ iip_elevation_patient + iip_elevation_partner
cross_coupling ~ iip_elevation_patient + iip_elevation_partner


"
iipel_self_m5_m <- sem(iipel_self_other_m5, personalitydatam5_best, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipel_self_other_m5_nooutliers_m <- sem(iipel_self_other_m5, pdatm5best_nooutliers, missing = "listwise", estimator = "ML", 
                                  mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)


iipag_self_m5 <- "

self_coupling_patient ~    iip_agency_patient 
self_coupling_partner ~ iip_agency_partner
cross_coupling ~ iip_agency_patient + iip_agency_partner


"
iipag_self_m5_m <- sem(iipag_self_m5, personalitydatam5_best, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

iipag_self_m5_nooutliers_m <- sem(iipag_self_m5, pdatm5best_nooutliers, missing = "listwise", estimator = "ML", 
                               mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

