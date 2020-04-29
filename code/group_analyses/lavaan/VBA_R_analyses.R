
#thetas + figuring out the general patterns of data

library(dplyr)
library(tidyr)
library(lavaan)
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data")
thetas <- read.csv("VBA_thetas.csv")
thetas

setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data")


personalityDat <- read.csv("couples_baseline_clinical_9Oct2015.csv")



thetas3 <-c(thetas$self_coupling_partner, thetas$self_coupling_patient)
thetas4 <- c(thetas$cross_coupling_partner, thetas$cross_coupling_patient)

load("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/align_split_atdifferentfreqs_patpar_CAID_Feb2017.RData")
ls <- sapply(alldata$`4Hz`, function(x) { x$PTNUM[[1]] })
ls <- c(ls,ls)
theta <- data.frame(ls, thetas3, thetas4)
pDat <- filter(personalityDat, PTNUM %in% ls)
thetaFinal <- filter(theta, ls %in% pDat$PTNUM)
pDat <- arrange(pDat, DyadID)
pDat$self_coupling <- thetaFinal$thetas3
pDat$cross_coupling <- thetaFinal$thetas4
pDatPatient <- subset(pDat, DyadID == 1 )
pDatPartner <- subset(pDat, DyadID == 0)
names(pDatPartner) <- paste(names(pDatPartner), "partner", sep = "_")
names(pDatPatient) <- paste(names(pDatPatient), "patient", sep = "_")
pDatNew <- data.frame(pDatPatient, pDatPartner)
badDat <- filter(pDatNew, self_coupling_partner < -0.5) %>% dplyr::select(self_coupling_patient, self_coupling_partner, cross_coupling_partner, cross_coupling_patient, PTNUM_patient)
pDatClean <-(filter(pDatNew, PTNUM_patient !=8016 & PTNUM_patient !=8099 & PTNUM_partner != 8016 & PTNUM_partner != 8099))
pDatClean$large_self_coupling_patient <- 1000*pDatClean$self_coupling_patient
pDatClean$large_self_coupling_partner <- 1000*pDatClean$self_coupling_partner
pDatClean$large_cross_coupling_patient <- 1000*pDatClean$cross_coupling_patient
pDatClean$large_cross_coupling_partner <- 1000*pDatClean$cross_coupling_partner
filter(pDatClean, self_coupling_partner >.2) %>% dplyr::select(self_coupling_patient, self_coupling_partner, cross_coupling_partner, cross_coupling_patient, PTNUM_patient)
datClean <-(filter(pDatClean, PTNUM_patient !=8052 & PTNUM_patient !=8063 & PTNUM_partner != 8052 & PTNUM_partner != 8063))
filter(datClean, large_cross_coupling_patient < -100) %>% dplyr::select(self_coupling_patient, self_coupling_partner, cross_coupling_partner, cross_coupling_patient, PTNUM_patient)
datClean2 <- filter(datClean, PTNUM_patient != 8074 & PTNUM_partner != 8074)

datClean2 <- datClean2 %>% mutate(iip_elcpr = iip_elevation_partner - mean(iip_elevation_partner),
                                  pdcountcpr = allpdCount_partner - mean(allpdCount_partner),
                                  iip_x_pdcount_partner = iip_elcpr * pdcountcpr,
                                  iip_elcpt = iip_elevation_patient - mean(iip_elevation_patient),
                                  pdcountcpt = allpdCount_patient - mean(allpdCount_patient),
                                  iip_x_pdcount_patient = iip_elcpt * pdcountcpt
)

datClean2 <- datClean2 %>% mutate(pdtotcpt = pdtot_patient - mean(pdtot_patient), 
                                  pdtotcpr = pdtot_partner - mean(pdtot_partner),
                                  iip_x_sexcpt = iip_elcpt * pdtotcpt,
                                  iip_x_sexcpr = iip_elcpr * pdtotcpr
                                  )

simpleData <- datClean2 %>% select(one_of(c("iip_elcpr", "pdcountcpr", "iip_x_pdcount_partner", "iip_elcpt", "pdcountcpt", "iip_x_pdcount_patient", "large_self_coupling_patient", "large_self_coupling_partner", "large_cross_coupling_patient", "large_cross_coupling_partner", "p_sex_partner", "p_sex_patient")))
write.csv(simpleData, file = "simpleData.csv", row.names = TRUE)
iip_elevation_pfree <- "


large_self_coupling_patient ~  a1*iip_elevation_patient + iip_elevation_partner
large_self_coupling_partner ~ a1*iip_elevation_partner + iip_elevation_patient
large_cross_coupling_patient ~   a2*iip_elevation_patient + iip_elevation_partner
large_cross_coupling_partner ~   iip_elevation_patient + a2*iip_elevation_partner

"


iip_elevation_pfreed <- sem(iip_elevation_pfree, datClean2, missing = "ML", estimator = "MLR")
summary(iip_elevation_pfreed)


iip_pdcount_moderation_allfree <- "
large_self_coupling_patient ~  iip_elcpr + iip_elcpt + iip_x_pdcount_patient + pdcountcpt + pdcountcpr 
large_self_coupling_partner ~ iip_elcpr + iip_elcpt+ iip_x_pdcount_partner+ pdcountcpt + pdcountcpr
large_cross_coupling_patient ~   iip_elcpr + iip_elcpt + iip_x_pdcount_patient+ pdcountcpt + pdcountcpr
large_cross_coupling_partner ~   iip_elcpr + iip_elcpt + iip_x_pdcount_partner+  pdcountcpt + pdcountcpr
"

iip_pdcount_moderation_allfree_sem <- sem(iip_pdcount_moderation_allfree, datClean2, missing = "ML", estimator = "MLR", fixed.x = TRUE)

summary(iip_pdcount_moderation_allfree_sem)

iip_pdcount_moderationd <- "

large_self_coupling_patient ~  iip_elcpt  + iip_x_pdcount_patient + pdcountcpt 
large_self_coupling_partner ~ iip_elcpr +  iip_x_pdcount_partner+ pdcountcpr 
large_cross_coupling_patient ~   iip_elcpt  + iip_x_pdcount_patient + pdcountcpt 
large_cross_coupling_partner ~   iip_elcpr +  iip_x_pdcount_partner+ pdcountcpr 

"

iip_pdcount_mod <- sem(iip_pdcount_moderationd, datClean2, missing = "ML", estimator = "MLR", fixed.x = TRUE)
summary(iip_pdcount_mod)


anova(iip_pdcount_mod, iip_pdcount_moderation_allfree_sem, iip_elevation_pfreed)

#MH: track down observation differences across models

simple1 <- "
large_self_coupling_patient ~  iip_elcpt + iip_elcpr
large_self_coupling_partner ~ iip_elcpr + iip_elcpt
large_cross_coupling_patient ~ iip_elcpt + iip_elcpr
large_cross_coupling_partner ~ iip_elcpt + iip_elcpr
"

simple1_m <- sem(simple1, simpleData, missing = "ML", estimator = "MLR", meanstructure = TRUE)

#nested model where only self-effects retained
simple2 <- "
large_self_coupling_patient ~  iip_elcpt
large_self_coupling_partner ~ iip_elcpr
large_cross_coupling_patient ~ iip_elcpt
large_cross_coupling_partner ~ iip_elcpr
"

simple2_m <- sem(simple2, datClean2, missing = "listwise", estimator = "ML", mimic="Mplus", meanstructure = TRUE)

filter(parTable(simple1_m), free > 0)
filter(parTable(simple2_m), free > 0)
anova(simple1_m, simple2_m) #success (correct)

#maybe this is the problem
simple3 <- "
large_self_coupling_patient ~  iip_elcpr + iip_elcpt + pdcountcpt + pdcountcpr
large_self_coupling_partner ~ iip_elcpr + iip_elcpt + pdcountcpt + pdcountcpr
large_cross_coupling_patient ~   iip_elcpr + iip_elcpt + pdcountcpt + pdcountcpr
large_cross_coupling_partner ~   iip_elcpr + iip_elcpt + pdcountcpt + pdcountcpr
"

simple3_m <- sem(simple3, datClean2, missing = "listwise", estimator = "ML", mimic="Mplus", meanstructure=TRUE)

toanalyze <- datClean2 %>% select(large_self_coupling_patient, large_self_coupling_partner, large_cross_coupling_patient, large_cross_coupling_partner,
                                  iip_elcpr, iip_elcpt, iip_elevation_patient, iip_elevation_partner, pdcountcpt, pdcountcpr,
                                  iip_x_pdcount_patient, iip_x_pdcount_partner)

#differences in missingness?
sapply(simpleData, function(x) { sum(!is.na(x)) })

anova(simple2_m, simple3_m) #grumpy about different set of variables

toanalyze <- toanalyze %>% rename(lscpt=large_self_coupling_patient, lscpr=large_self_coupling_partner, 
                                  lccpr=large_cross_coupling_partner, lccpt=large_cross_coupling_patient,
                                  ielcpt=iip_elcpt, ielcpr=iip_elcpr, pdpr=pdcountcpr, pdpt=pdcountcpt)

library(MplusAutomation)
m2 <- mplusObject(MODEL= "lscpt ON ielcpt;
lscpr ON ielcpr;
lccpt ON ielcpt;
lccpr ON ielcpr;", TITLE="SMALL MODEL", rdata=toanalyze)#, usevariables=c("lscpt", "lscpr", "lccpt", "lccpr", "ielcpt", "ielcpr"))

res2 <- mplusModeler(m2, "outdat2.dat", modelout="model2.inp", run=1)

m3 <- mplusObject(MODEL= "lscpt ON ielcpr ielcpt pdpt pdpr;
lscpr ON ielcpr ielcpt pdpt pdpr;
lccpt ON ielcpr ielcpt pdpt pdpr;
lccpr ON ielcpr ielcpt pdpt pdpr;", TITLE="BIG MODEL", rdata=toanalyze)#, usevariables=c("lscpt", "lscpr", "lccpt", "lccpr", "ielcpt", "ielcpr"))

res3 <- mplusModeler(m3, "outdat3.dat", modelout="model3.inp", run=1)

#ugh, yes this is much more in line with my expectation (lower LL for model with more parameters)
res2$results$summaries
res3$results$summaries

###these models exist in feb2017_param_analyses.R

iipall_self_old_m <- sem(iipall_self, datClean, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

iipall_self_nooutliers_old_m <- sem(iipall_self, datClean2, missing = "listwise", estimator = "ML", 
                                    mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

iipall_self_other_old_m <- sem(iipall_self_other, datClean, missing = "listwise", estimator = "ML", 
                           mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)


iipall_self_other_nooutliers_old_m <- sem(iipall_self_other, datClean2, missing = "listwise", estimator = "ML", 
                                      mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

iipall_self_gender_self_other_nooutliers_old_m <- sem(iipall_self_gender_self_other, datClean2, missing = "listwise", estimator = "ML", 
                         mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

                                
anova(iipall_self_old_m, iipall_self_nooutliers_old_m, iipall_self_other_old_m, iipall_self_other_nooutliers_old_m)



##########################################################################################################################################





negCross <- filter(thetas, thetas$a2 <0 | thetas$b2 <0)
negCross



#capturing contraian style
contrarian <- filter(nonzero, nonzero$a2 < 0 & nonzero$b2 < 0)


#capturing cooperative style
cooperative <- filter(nonzero, nonzero$a1 > 0 & nonzero$a2 > 0 & nonzero$b1 > 0 & nonzero$b2 > 0)

#uncooperative partner/patient
uncooperative <- filter(nonzero, (nonzero$a2 <0 & nonzero$b2 >0) | (nonzero$b2 <0 & nonzero$a2 >0))

#uncooperative and noncooperative partner driving pattern
asymmmetry <- filter(uncooperative, (abs(a2) > 1.5*abs(b2) & a2 < 0) | (abs(b2) > 1.5*abs(a2) & b2 <0))
#n = 26

#independent
independent <- filter(thetas, abs(thetas$a2)  < .001 & abs(thetas$b2) < .001) #use .001 because actual value of zero v unlikely
#none are actually independent

#effectively zero --> random walk 
#not adding up to correct number
zero <- filter(thetas, abs(thetas$a1) < .000332 | abs(thetas$a2) < .000332 | abs(thetas$b1) < .000332 | abs(thetas$b2) < .000332)

nonzero <-filter(thetas, abs(thetas$a1) > .000332 & abs(thetas$a2) > .000332 & abs(thetas$b1) > .000332 & abs(thetas$b2) > .000332)


#general relation between parameters
#this firs part explore the expected relationship between parameters
a1a2 <- median(abs(thetas$a1)/abs(thetas$a2))
b1b2 <-median(abs(thetas$b1)/abs(thetas$b2))
a1b1 <- median(abs(thetas$a1)/abs(thetas$b1))
a2b2 <- median(abs(thetas$a2)/abs(thetas$b2))





#
#
#
#Now looking at APIM Models using theta structure




#ALL THE ANALYSES!!!
#looking at different personality disorders and their impact on cross coupling
runAPIM(datClean2, "large_cross_coupling", "narci_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "bordl_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "negtv_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "szoid_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "histr_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "antso_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "avoid_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "depen_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "obcmp_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "parnd_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "stypl_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "deprs_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "OPD_sidp", partners = c("_partner", "_patient"), printall = TRUE)


#looking at different personality disorder and their impact on self coupling parameters
runAPIM(datClean2, "large_self_coupling", "narci_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "bordl_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "negtv_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "szoid_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "histr_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "antso_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "avoid_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "depen_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "obcmp_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "parnd_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "stypl_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "deprs_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "OPD_sidp", partners = c("_partner", "_patient"), printall = TRUE)


#agency amd communion
runAPIM(datClean2, "large_self_coupling", "iip_agency", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "iip_communion", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "iip_agency", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "iip_communion", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "iip_elevation", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "iip_elevation", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean, "large_self_coupling", c("iip_elevation", "iip_communion", "iip_agency"), partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean, "large_cross_coupling", c("iip_elevation", "iip_communion", "iip_agency"), partners = c("_partner", "_patient"), printall = TRUE)


#Relationship satisfaction
runAPIM(datClean2, "large_self_coupling", "DASAffExp", partners = c("_partner", "_patient"), printall = TRUE) #ns
runAPIM(datClean2, "large_self_coupling", "DASCoh", partners = c("_partner", "_patient"), printall = TRUE) #ns
runAPIM(datClean2, "large_cross_coupling", "DASAffExp", partners = c("_partner", "_patient"), printall = TRUE) #ns
runAPIM(datClean2, "large_cross_coupling", "DASCoh", partners = c("_partner", "_patient"), printall = TRUE) #ns
runAPIM(datClean2, "large_cross_coupling", "DASCon", partners = c("_partner", "_patient"), printall = TRUE) #ns
runAPIM(datClean2, "large_self_coupling", "DASCon", partners = c("_partner", "_patient"), printall = TRUE) #ns
runAPIM(datClean2, "large_cross_coupling", "DASSat", partners = c("_partner", "_patient"), printall = TRUE) #ns
runAPIM(datClean2, "large_self_coupling", "DASSat", partners = c("_partner", "_patient"), printall = TRUE) #ns
runAPIM(datClean2, "large_cross_coupling", "DASrelationship", partners = c("_partner", "_patient"), printall = TRUE) #s, all free and indist good, p paths esp
runAPIM(datClean2, "large_self_coupling", "DASrelationship", partners = c("_partner", "_patient"), printall = TRUE)#s, all free and indist, p paths esp 
runAPIM(datClean2, "large_cross_coupling", "DASTotal", partners = c("_partner", "_patient"), printall = TRUE) #ns
runAPIM(datClean2, "large_self_coupling", "DASTotal", partners = c("_partner", "_patient"), printall = TRUE) #ns
runAPIM(datClean, "large_self_coupling", c("DASAffExp", "DASCoh", "DASCon", "DASSat", "DASrelationship", "DASTotal"), partners = c("_partner", "_patient"), printall = TRUE) #doesn't converge
runAPIM(datClean, "large_cross_coupling", c("DASAffExp", "DASCoh", "DASCon", "DASSat","DASrelationship" , "DASTotal"), partners = c("_partner", "_patient"), printall = TRUE) #doesn't converge

#other PD analyses
runAPIM(datClean2, "large_self_coupling", "allpdCount", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "allpdCount", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_self_coupling", "pdtot", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "pdtot", partners = c("_partner", "_patient"), printall = TRUE)




#plotting residuals
attach(datClean2)
#controlling for a path, what is the relationship between cc of patient and agency of partner
r1 <- lm(cross_coupling_patient ~ 1 + iip_agency_patient)
r1 <- resid(r1)
plot(r1, iip_agency_partner)
cor(r1, iip_agency_partner)
#controlling for a path, what is the relationships between cc of partner and agency of patient
r2 <- resid(lm(cross_coupling_partner ~ 1 + iip_agency_partner))
plot(r2, iip_agency_patient)
cor(r2, iip_agency_patient)
#controlling for a path, what is relationship between cc of patient and communion of partner
r3 <- resid(lm(cross_coupling_patient ~ 1 + iip_communion_patient))
plot(r3, iip_communion_partner)
cor(r3, iip_communion_partner)
#controlling for a path, what is relationship between cc of partner and communion of patient
r4 <- resid(lm(cross_coupling_partner ~ 1 + iip_communion_partner))
plot(r4, iip_communion_patient)
cor(r4, iip_communion_patient)
#controlling for p path, what is relationship between cc of patient and communion of patient
r5 <- resid(lm(cross_coupling_patient ~ 1 + iip_communion_partner))
plot(r5, iip_communion_patient)
cor(r5, iip_communion_patient)
#controlling for p path, what is the relationship between cc of partner and communion of partner
r6 <- resid(lm(cross_coupling_partner ~ 1 + iip_communion_patient))
plot(r6, iip_communion_partner)
cor(r6, iip_communion_partner)
#controlling for p path, what is the relationship between cc of patient and agency of patient
r7 <- resid(lm(cross_coupling_patient ~ 1 + iip_agency_partner))
plot(r7, iip_agency_patient)
cor(r7, iip_agency_patient)
#controlling for p path, what is the relationship between cc of partner and agency of partner
r8 <- resid(lm(cross_coupling_partner ~ 1 + iip_agency_patient))
plot(r8, iip_agency_partner)
cor(r8, iip_agency_partner)
#now do all the same for self coupling params
#controlling for a path, what is the relationship between sc of patient and agency of partner
r9 <- lm(self_coupling_patient ~ 1 + iip_agency_patient)
r9 <- resid(r9)
plot(r9, iip_agency_partner)
cor(r9, iip_agency_partner)
#controlling for a path, what is the relationships between sc of partner and agency of patient
r10 <- resid(lm(self_coupling_partner ~ 1 + iip_agency_partner))
plot(r10, iip_agency_patient)
cor(r10, iip_agency_patient)
#controlling for a path, what is relationship between sc of patient and communion of partner
r11 <- resid(lm(self_coupling_patient ~ 1 + iip_communion_patient))
plot(r11, iip_communion_partner)
cor(r11, iip_communion_partner)
#controlling for a path, what is relationship between sc of partner and communion of patient
r12 <- resid(lm(self_coupling_partner ~ 1 + iip_communion_partner))
plot(r12, iip_communion_patient)
cor(r12, iip_communion_patient)
#controlling for p path, what is relationship between sc of patient and communion of patient
r13 <- resid(lm(self_coupling_patient ~ 1 + iip_communion_partner))
plot(r13, iip_communion_patient)
cor(r13, iip_communion_patient)
#controlling for p path, what is the relationship between sc of partner and communion of partner
r14 <- resid(lm(self_coupling_partner ~ 1 + iip_communion_patient))
plot(r14, iip_communion_partner)
cor(r14, iip_communion_partner)
#controlling for p path, what is the relationship between sc of patient and agency of patient
r15 <- resid(lm(self_coupling_patient ~ 1 + iip_agency_partner))
plot(r15, iip_agency_patient)
cor(r15, iip_agency_patient)
#controlling for p path, what is the relationship between sc of partner and agency of partner
r16 <- resid(lm(self_coupling_partner ~ 1 + iip_agency_patient))
plot(r16, iip_agency_partner)
cor(r16, iip_agency_partner)




#self coupling and cross coupling SEM
m <- "
large_cross_coupling_patient ~ large_self_coupling_patient + large_self_coupling_partner
large_cross_coupling_partner ~ large_self_coupling_patient + large_self_coupling_partner

"

modelm <- sem(m, datClean2, missing = "ML", estimator = "MLR")
summary(modelm, fit.measures = TRUE)
cat(modelm)



#self coupling, cross coupling, agency, communion
altFree <- "
large_self_coupling_patient ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner
large_self_coupling_partner ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner
large_cross_coupling_patient ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner
large_cross_coupling_partner ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner

"

altFreed <- sem(altFree, datClean2, missing = "ML", estimator = "MLR")
summary(altFreed)


ppathsallfree <- "
large_self_coupling_patient ~ iip_agency_partner +a4* iip_agency_patient + a5*iip_communion_patient + iip_communion_partner 
large_self_coupling_partner ~ a4*iip_agency_partner + iip_agency_patient + iip_communion_patient + a5*iip_communion_partner
large_cross_coupling_patient ~ iip_agency_partner + a3*iip_agency_patient + a6*iip_communion_patient + iip_communion_partner 
large_cross_coupling_partner ~ a3*iip_agency_partner + iip_agency_patient + iip_communion_patient + a6*iip_communion_partner

"

ppathy <- sem(ppathsallfree, datClean2, missing = "ML", estimator = "MLR")
summary(ppathy)



indist <- "
large_self_coupling_patient ~ p1*iip_agency_partner +a4* iip_agency_patient + a5*iip_communion_patient + p2*iip_communion_partner 
large_self_coupling_partner ~ a4*iip_agency_partner + p1*iip_agency_patient + p2*iip_communion_patient + a5*iip_communion_partner
large_cross_coupling_patient ~ p3*iip_agency_partner + a3*iip_agency_patient + a6*iip_communion_patient + p4*iip_communion_partner 
large_cross_coupling_partner ~ a3*iip_agency_partner + p3*iip_agency_patient + p4*iip_communion_patient + a6*iip_communion_partner

"

indisti <- sem(indist, datClean2, missing = "ML", estimator = "MLR")
summary(indisti)


apathallfree <- "

large_self_coupling_patient ~ p1*iip_agency_partner +iip_agency_patient + iip_communion_patient + p2*iip_communion_partner 
large_self_coupling_partner ~ iip_agency_partner + p1*iip_agency_patient + p2*iip_communion_patient + iip_communion_partner
large_cross_coupling_patient ~ p3*iip_agency_partner + iip_agency_patient + iip_communion_patient + p4*iip_communion_partner 
large_cross_coupling_partner ~ iip_agency_partner + p3*iip_agency_patient + p4*iip_communion_patient + iip_communion_partner

"

apathy <- sem(apathallfree, datClean2, missing = "ML", estimator = "MLR")
summary(apathy)


#all the a paths held the same; estimating all p paths
ppathfree_narci_iip <- "

large_self_coupling_patient ~ iip_agency_partner +a4* iip_agency_patient + a5*iip_communion_patient + iip_communion_partner + a1*narci_sidp_patient + narci_sidp_partner
large_self_coupling_partner ~ a4*iip_agency_partner + iip_agency_patient + iip_communion_patient + a5*iip_communion_partner + narci_sidp_patient + a1*narci_sidp_partner
large_cross_coupling_patient ~ iip_agency_partner + a3*iip_agency_patient + a6*iip_communion_patient + iip_communion_partner + a2*narci_sidp_patient + narci_sidp_partner
large_cross_coupling_partner ~ a3*iip_agency_partner + iip_agency_patient + iip_communion_patient + a6*iip_communion_partner + narci_sidp_patient + a2*narci_sidp_partner


"
pfree_narci <- sem(ppathfree_narci, datClean2, missing = "ML", estimator = "MLR")
summary(pfree_narci)


narci_iip <- "

large_self_coupling_patient ~ iip_agency_partner +iip_agency_patient + iip_communion_patient + iip_communion_partner + narci_sidp_patient + narci_sidp_partner
large_self_coupling_partner ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner + narci_sidp_patient + narci_sidp_partner
large_cross_coupling_patient ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner + narci_sidp_patient + narci_sidp_partner
large_cross_coupling_partner ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner + narci_sidp_patient + narci_sidp_partner


"
narci_iip_sem <- sem(narci_iip, datClean2, missing = "ML", estimator = "MLR")
summary(narci_iip_sem)


narc_ppathfree <- "

large_self_coupling_patient ~  a2*narci_sidp_patient + narci_sidp_partner
large_self_coupling_partner ~  narci_sidp_patient + a2*narci_sidp_partner
large_cross_coupling_patient ~  a1*narci_sidp_patient + narci_sidp_partner
large_cross_coupling_partner ~  narci_sidp_patient + a1*narci_sidp_partner


"
narci_pfree <- sem(narc_ppathfree, datClean2, missing = "ML", estimator = "MLR")
summary(narci_pfree)

print(anova(ppathy, allfreedup, allnarci))


#pd count of person and elevation of both as predictors

iip_pdcount_all_free <- "


large_self_coupling_patient ~ allpdCount_patient + iip_elevation_patient + iip_elevation_partner
large_self_coupling_partner ~  allpdCount_partner + iip_elevation_partner+ iip_elevation_patient
large_cross_coupling_patient ~  allpdCount_patient + iip_elevation_patient + iip_elevation_partner
large_cross_coupling_partner ~  allpdCount_partner + iip_elevation_patient + iip_elevation_partner


"

iip_pdcount_allfree <- sem(iip_pdcount_all_free, datClean2, missing = "ML", estimator = "MLR")
summary(iip_pdcount_allfree)


#agency communion and pd count as predictors

iip_commag_pdcount <- "


large_self_coupling_patient ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner + allpdCount_patient
large_self_coupling_partner ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner + allpdCount_partner
large_cross_coupling_patient ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner + allpdCount_patient
large_cross_coupling_partner ~ iip_agency_partner + iip_agency_patient + iip_communion_patient + iip_communion_partner + allpdCount_partner


"

commag_pdcount <- sem(iip_commag_pdcount, datClean2, missing = "ML", estimator = "MLR")
summary(commag_pdcount)



#p paths free, elevation of partner and patient on sc/cc

iip_elevation_pfree <- "


large_self_coupling_patient ~  a1*iip_elevation_patient + iip_elevation_partner
large_self_coupling_partner ~ a1*iip_elevation_partner + iip_elevation_patient
large_cross_coupling_patient ~   a2*iip_elevation_patient + iip_elevation_partner
large_cross_coupling_partner ~   iip_elevation_patient + a2*iip_elevation_partner

"


iip_elevation_pfreed <- sem(iip_elevation_pfree, datClean2, missing = "ML", estimator = "MLR")
summary(iip_elevation_pfreed)





iip_elevation_afree <- "

large_self_coupling_patient ~  iip_elevation_patient + p1*iip_elevation_partner
large_self_coupling_partner ~ iip_elevation_partner + p1*iip_elevation_patient
large_cross_coupling_patient ~   iip_elevation_patient + p2*iip_elevation_partner
large_cross_coupling_partner ~   iip_elevation_partner + p2*iip_elevation_patient



"
iip_elevation_afreed <- sem(iip_elevation_afree, datClean2, missing = "listwise", estimator = "MLR")
summary(iip_elevation_afreed)

lavInspect(iip_elevation_p_n)
lavInspect(iip_pdcount_mod)



iip_elevation_aonly <- "


large_self_coupling_patient ~  iip_elevation_patient 
large_self_coupling_partner ~ iip_elevation_partner
large_cross_coupling_patient ~   iip_elevation_patient
large_cross_coupling_partner ~   iip_elevation_partner

"


iip_elevation_aonly_sem <- sem(iip_elevation_aonly, datClean2, missing = "ML", estimator = "MLR")
summary(iip_elevation_aonly_sem)


 
iip_elevation_ponly <- "

large_self_coupling_patient ~  iip_elevation_partner
large_self_coupling_partner ~ iip_elevation_patient
large_cross_coupling_patient ~   iip_elevation_partner
large_cross_coupling_partner ~   iip_elevation_patient



"

iip_elevation_ponly_sem <- sem(iip_elevation_ponly, datClean2, missing = "ML", estimator = "MLR")
summary(iip_elevation_ponly_sem)

iip_elevation_allfree <- "



large_self_coupling_patient ~  iip_elevation_patient + iip_elevation_partner
large_self_coupling_partner ~ iip_elevation_partner + iip_elevation_patient
large_cross_coupling_patient ~   iip_elevation_patient + iip_elevation_partner
large_cross_coupling_partner ~   iip_elevation_partner + iip_elevation_patient

"

iip_elevation_allfree_sem <- sem(iip_elevation_allfree, datClean2, missing = "ML", estimator = "MLR")
summary(iip_elevation_allfree_sem)


iip_elevation_indist <- "



large_self_coupling_patient ~  a1*iip_elevation_patient + p1*iip_elevation_partner
large_self_coupling_partner ~ a1*iip_elevation_partner + p1*iip_elevation_patient
large_cross_coupling_patient ~   a2*iip_elevation_patient + p2*iip_elevation_partner
large_cross_coupling_partner ~   a2*iip_elevation_partner + p2*iip_elevation_patient

"

iip_elevation_indist_sem <- sem(iip_elevation_indist, datClean2, missing = "ML", estimator = "MLR")
summary(iip_elevation_indist_sem)


anova(iip_elevation_pfreed, iip_elevation_afreed, iip_elevation_indist_sem, iip_elevation_allfree_sem, iip_elevation_ponly_sem, iip_elevation_aonly_sem)



#looking for moderation of PD count and elevation



datClean2 <- datClean2 %>% mutate(iip_elcpr = iip_elevation_partner - mean(iip_elevation_partner),
                                  pdcountcpr = allpdCount_partner - mean(allpdCount_partner),
                                  iip_x_pdcount_partner = iip_elcpr * pdcountcpr,
                                  iip_elcpt = iip_elevation_patient - mean(iip_elevation_patient),
                                  pdcountcpt = allpdCount_patient - mean(allpdCount_patient),
                                  iip_x_pdcount_patient = iip_elcpt * pdcountcpt
                                  )


#first model is all the variables

iip_pdcount_moderation_apathsfree <- "

large_self_coupling_patient ~  iip_elevation_patient + p1*iip_elevation_partner + iip_xpdcount_patient + allpdCount_patient + allpdCount_partner
large_self_coupling_partner ~ iip_elevation_partner + p1*iip_elevation_patient+ iip_x_pdcount_partner+ allpdCount_partner + allpdCount_patient
large_cross_coupling_patient ~   iip_elevation_patient + p2*iip_elevation_partner + iip_xpdcount_patient+ allpdCount_patient + allpdCount_partner
large_cross_coupling_partner ~   iip_elevation_patient + p2*iip_elevation_partner + iip_x_pdcount_partner+  allpdCount_partner + allpdCount_patient

"

iip_pdcount_moderation_apathsfreed <- sem(iip_pdcount_moderation_apathsfree, datClean2, missing = "ML", estimator = "MLR")


iip_pdcount_moderation_allfree <- "

large_self_coupling_patient ~  iip_elcpr + iip_elcpt + iip_xpdcount_patient + pdcountcpt + pdcountcpr 
large_self_coupling_partner ~ iip_elcpr + iip_elcpt+ iip_x_pdcount_partner+ pdcountcpt + pdcountcpr
large_cross_coupling_patient ~   iip_elcpr + iip_elcpt + iip_x_pdcount_patient+ pdcountcpt + pdcountcpr
large_cross_coupling_partner ~   iip_elcpr + iip_elcpt + iip_x_pdcount_partner+  pdcountcpt + pdcountcpr

"

iip_pdcount_moderation_allfree_sem <- sem(iip_pdcount_moderation_allfree, datClean2, missing = "ML", estimator = "MLR")


anova(iip_pdcount_moderation_allfree_sem, iip_elevation_pfreed)

summary(iip_pdcount_moderation_apathsfreed)
anova(iip_pdcount_mod, iip_pdcount_moderation_apathsfreed, iip_pdcount_moderation_allfree)


iip_pdcount_moderationd <- "

large_self_coupling_patient ~  iip_elcpt  + iip_x_pdcount_patient + pdcountcpt 
large_self_coupling_partner ~ iip_elcpr +  iip_x_pdcount_partner+ pdcountcpr 
large_cross_coupling_patient ~   iip_elcpt  + iip_x_pdcount_patient + pdcountcpt 
large_cross_coupling_partner ~   iip_elcpr +  iip_x_pdcount_partner+ pdcountcpr 

"

iip_pdcount_mod <- sem(iip_pdcount_moderationd, datClean2, missing = "ML", estimator = "MLR", conditional.x = TRUE)
summary(iip_pdcount_mod)


sum(is.na(datClean2$large_self_coupling_patient))
sum(is.na(datClean2$large_self_coupling_partner))
sum(is.na(datClean2$large_cross_coupling_partner))
sum(is.na(datClean2$large_cross_coupling_patient))

sum(is.na(datClean2$iip_elevation_patient))
sum(is.na(datClean2$iip_elevation_partner))
sum(is.na(datClean2$allpdCount_patient))
sum(is.na(datClean2$allpdCount_partner))
sum(is.na(datClean2$iip_xpdcount_patient))
sum(is.na(datClean2$iip_x_pdcount_partner))

cor(datClean2[,c("allpdCount_patient", "allpdCount_partner", "iip_xpdcount_patient", "iip_x_pdcount_partner", "iip_elevation_patient", "iip_elevation_part")])






iip_pdcount_messingaround <- "

large_self_coupling_patient ~  iip_elevation_patient  + 0*iip_xpdcount_patient + 0*allpdCount_patient 
large_self_coupling_partner ~ iip_elevation_partner +  0*iip_x_pdcount_partner+ 0*allpdCount_partner 
large_cross_coupling_patient ~   iip_elevation_patient +  0*iip_xpdcount_patient+ 0*allpdCount_patient 
large_cross_coupling_partner ~   iip_elevation_partner +  0*iip_x_pdcount_partner+ 0*allpdCount_partner 

"

sem(iip_pdcount_messingaround, datClean2, missing = "ML", estimator = "MLR", fixed.x = TRUE, conditional.x = TRUE)

iip_pdcount_messingaround <- "

large_self_coupling_patient ~  iip_elcpt  + iip_xpdcount_patient + pdcountcpt 
large_self_coupling_partner ~ iip_elcpr +  iip_x_pdcount_partner+ pdcountcpr 
large_cross_coupling_patient ~   iip_elcpr +  iip_xpdcount_patient+ pdcountcpt 
large_cross_coupling_partner ~   iip_elcpt +  iip_x_pdcount_partner+ pdcountcpr 

"


#iip_pdcount_mess <- sem(iip_pdcount_messingaround, datClean2, missing = "ML", estimator = "MLR", fixed.x = TRUE)
iip_pdcount_mess <- sem(iip_pdcount_messingaround, datClean2, missing = "ML", estimator = "MLR", fixed.x = TRUE)

lavInspect(iip_pdcount_mess)

summary(iip_pdcount_mess)

anova(iip_pdcount_mess, iip_pdcount_mod)
iip_pdcount_both <- "


large_self_coupling_patient ~  iip_elevation_patient + p1*iip_elevation_partner +  allpdCount_partner+ allpdCount_patient
large_self_coupling_partner ~ iip_elevation_partner + p1*iip_elevation_patient + allpdCount_patient + allpdCount_partner
large_cross_coupling_patient ~   p2*iip_elevation_patient + iip_elevation_partner  + allpdCount_partner+ allpdCount_patient
large_cross_coupling_partner ~   p2*iip_elevation_patient + iip_elevation_partner + allpdCount_patient + allpdCount_partner


"

iip_pdcount_bothd <- sem(iip_pdcount_both, datClean2, missing = "ML", estimator = "MLR")
summary(iip_pdcount_bothd)


anova(iip_pdcount_moderation_afreed, iip_pdcount_moderation_afreed_both, iip_pdcount_moderation_afreed_other, iip_elevation_p , iip_pdcount_selfd, iip_pdcount_otherd, iip_pdcount_bothd)

anova(iip_pdcount_moderation_allfree, iip_pdcount_moderation_afreed)




#check mediation

iip_pdcount_mediation_afree <- "

large_self_coupling_patient ~  iip_elevation_patient + p1*iip_elevation_partner +  allpdCount_partner+ allpdCount_patient
large_self_coupling_partner ~ iip_elevation_partner + p1*iip_elevation_patient + allpdCount_patient + allpdCount_partner
large_cross_coupling_patient ~   p2*iip_elevation_patient + iip_elevation_partner  + allpdCount_partner+ allpdCount_patient
large_cross_coupling_partner ~   p2*iip_elevation_patient + iip_elevation_partner + allpdCount_patient + allpdCount_partner
allpdCount_patient ~ iip_elevation_patient
allpdCount_partner ~ iip_elevation_partner


"

iip_pdcount_mediation_afreed <-sem(iip_pdcount_mediation_afree, datClean2, missing = "ML", estimator = "MLR")
summary(iip_pdcount_mediation_afreed)





#More systematic freeing of parameters


DV = c("large_self_coupling", "large_cross_coupling")
partners = c("_partner", "_patient")
predictors = c("iip_communion", "iip_agency")
additional = " "
indistinguishable_syn_2DV <- ""
for (p in 1:length(predictors)) {
  indistinguishable_syn_2DV <- paste0(indistinguishable_syn_2DV, DV[1], partners[1], additional, "+", additional, DV[2], partners[1], additional, " ~ a", p, "*", predictors[p], partners[1], additional,
                                      " + p", p, "*", predictors[p], partners[2], additional, "\n ",
                                      DV[1], partners[2], additional, "+ ", additional, DV[2], partners[2], additional, "~ a", p, "*", predictors[p], partners[2], additional,
                                      " + p", p, "*", predictors[p], partners[1], additional, "\n")
}


afree_syn_DV <- pfree_syn_DV <- all_free_syn_DV <- indistinguishable_syn_2DV
for (p in 1:length(predictors)) {
  afree_syn_DV <- sub(paste0("a", p, "*"), paste0("a", p, partners[1], "*"), afree_syn_DV, fixed=TRUE) #replace first instance of actor with _0
  afree_syn_DV <- sub(paste0("a", p, "*"), paste0("a", p, partners[2], "*"), afree_syn_DV, fixed=TRUE) #replace second instance of actor with _1
  pfree_syn_DV <- sub(paste0("p", p, "*"), paste0("p", p, partners[1], "*"), pfree_syn_DV, fixed=TRUE) #replace first instance of partner with _0
  pfree_syn_DV <- sub(paste0("p", p, "*"), paste0("p", p, partners[2], "*"), pfree_syn_DV, fixed=TRUE) #replace second instance of partner with _1
  
  all_free_syn_DV <- sub(paste0("a", p, "*"), paste0("a", p, partners[1], "*"), all_free_syn_DV, fixed=TRUE) #replace first instance of actor with _0
  all_free_syn_DV <- sub(paste0("a", p, "*"), paste0("a", p, partners[2], "*"), all_free_syn_DV, fixed=TRUE) #replace second instance of actor with _1
  all_free_syn_DV <- sub(paste0("p", p, "*"), paste0("p", p, partners[1], "*"), all_free_syn_DV, fixed=TRUE) #replace first instance of partner with _0
  all_free_syn_DV <- sub(paste0("p", p, "*"), paste0("p", p, partners[2], "*"), all_free_syn_DV, fixed=TRUE) #replace second instance of partner with _1
  
}

indistinguishable <- sem(indistinguishable_syn_2DV, datClean2, missing="ML", estimator="MLR")
df = datClean2
printall = TRUE
cat("\n\n-------\nIndistinguishable dyads model:\n------\n")
summary(indistinguishable, fit.measures=TRUE)
#standardizedSolution(res, type="std.all")

afree <- sem(afree_syn_DV, df, missing="ML", estimator="MLR")

pfree <- sem(pfree_syn_DV, df, missing="ML", estimator="MLR")

allfree <- sem(all_free_syn_DV, df, missing="ML", estimator="MLR")

if (printall) {
  cat("\n\n-------\nFree actor model:\n------\n")
  summary(afree, fit.measures=TRUE)
  
  cat("\n\n-------\nFree partner model:\n------\n")
  summary(pfree, fit.measures=TRUE)
  
  cat("\n\n-------\nFree actor and partner (saturated) model:\n------\n")
  summary(allfree, fit.measures=TRUE)
}

print(anova(indistinguishable, afree, pfree, allfree))
cat(indistinguishable_syn_2DV)
sem(indistinguishable_syn_2DV, datClean2, missing = "ML", estimator = "MLR")
cat("\n\n-------\nIndistiguishable dyads:\n------\n")
summary(indistinguishable_syn_2DV, fit.measures = TRUE)
#extra script
runAPIM(datClean2, "large_self_coupling", "narci_sidp", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "bordl_sidp", partners = c("_partner", "_patient"), printall = TRUE)

runAPIM(datClean2, "large_self_coupling", "large_cross_coupling", partners = c("_partner", "_patient"), printall = TRUE)
runAPIM(datClean2, "large_cross_coupling", "large_self_coupling", partners = c("_partner", "_patient"), printall = TRUE)