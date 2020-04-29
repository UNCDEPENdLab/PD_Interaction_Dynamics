#only examining iipel_self, iipel_pdtot self and pdtot self (and with vanBse for each of these separately)
#testing different data set using different criteria to cut people
dat1 = df_noOutliers_vanBse
dat2 = df_noOutliers
dat3 = dt_vanBse_simp
dat4 = dt_vanBse_simp_noOutliers_ptnum_inclusion_spreadsheet
dat5 = dt_vanBse_simp_noWackies_ptnum_inclusion_spreadsheet




#INFLUENCE FUNCTION FOR GENERALIZED COOK'S DISTANCE

#use Cook's distance to sequentially eliminate bad cases
#infl <- influence(model=bestm, group="PTNUM")
library(influence.SEM)

cd<- genCookDist(model = iipel_self, data = dt_vanBse_simp_noOutliers)
listofPTNUM <- c(as.numeric(dt_vanBse_simp_noOutliers$PTNUM))
row.names(as.dataframe(cd)) <- listofPTNUM
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:99)
dfnew$leverage = cd

dfnew$PTNUM = dt_vanBse_simp_noOutliers$PTNUM
#cthresh <- .043 #cook's distance threshold #USE THIS FOR ANALYSES
cthresh <- .5 #based off of plot, things larger than .5 seem ot really be far off
cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- dt_vanBse_simp_noOutliers #initially just the full model
allbadcases <- c()
while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(dt_vanBse_simp_noOutliers, !PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  cd <- genCookDist(iipel_self, mup) 
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}

#INFLUENCE for COOK'S DISTANCE


#load in data
library(lavaan)
library(dplyr)
library(tidyr)
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/")



setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/")
dt = read.csv("parameters_model3_modelcomparison_allData.csv", header = TRUE)
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
write.csv(personalitydata, "params_personalitydata_all.csv")

#df <-read.csv(file = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/params_personalitydata.csv")
df <- read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/params_personalitydata_all.csv")
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


#setwd("/Users/ams939/Desktop/VBA_output/")
setwd("/Users/alisonmarie526/Desktop/Archive_April2017/")
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
    v_ccpr = 1000 * v_cross_coupling_partner, 
    copt = 1000*summed_cross_coupling_patient,
    copr = 1000*summed_cross_coupling_partner
  )



dt_vanBse <-
  dt_vanBse %>% mutate(
    pdcpr = pdtot_partner - mean(na.omit(pdtot_partner)),
    iip_x_pdtot_partner = iip_elcpr * pdcpr,
    pdcpt = pdtot_patient - mean(na.omit(pdtot_patient)),
    iip_x_pdtot_patient = iip_elcpt * pdcpt
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
    iip_elevation_patient,
    iip_elevation_partner,
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
    copt,
    copr,
    iip_agency_patient,
    iip_agency_partner,
    iip_communion_patient,
    iip_communion_partner,
    PTNUM
  )



#######BASED OFF THOROUGH DATA ANALYSIS, THESE PEOPLE WERE IDENTIFIED AS BE UNSALVAGABLE
#8144, 8133, 8112, 8106, 8104, 8073, 8063, 8060, 8040, 8035, 8016
#8126, 8127, 8100, 8052, 8020
#df had included 8144, 8133, 8112, (8104 was cut), 8073, 8063, 8060, 8052, (8040 was already cut), 8035, 8020, 8016 (this helps to expalin the 122, rather than 123 but still one missing)
#df had cut 8048, which was no identified as terrible in PTNUM inclusion phase -- no personality data
#df also cut 8139 --no personality data --> IN the file, just had a ton of NAs
#dt_vanBse cut: 8144,8104 (cut at df),  8063, 8040 (that was cut on PTNUM inclusion)
#dt_vanBse did not cut: 8133, 8112, 8106, 8073, 8060, 8035, 8016, 8126, 8127, 8100, 8052, 8020  (that was cut on PTNUM inclusion)
#dt_vanBse did cut: 8139 (no personality data), 8115, 8093, 8078, 8065, 8048 (cut purposefully) (not cut on PTNUM inclusion)
dt_vanBse_simp_noOutliers_ptnum_inclusion_spreadsheet <- dplyr::filter(dt_vanBse_simp, PTNUM != 8133, PTNUM != 8127, PTNUM != 8126, PTNUM!= 8112, PTNUM !=8106, PTNUM != 8100, PTNUM != 8073, PTNUM != 8060, PTNUM != 8052, PTNUM !=8035, PTNUM != 8020, PTNUM != 8016)
#n = 104
dt_vanBse_simp_noWackies_ptnum_inclusion_spreadsheet <- dplyr::filter(dt_vanBse_simp, PTNUM != 8133, PTNUM!= 8112, PTNUM !=8106, PTNUM != 8073, PTNUM != 8063, PTNUM!= 8060, PTNUM !=8035, PTNUM != 8016)







#FIRST univariate cutting of outliers for scpt, scpr, ccpt, ccpr
#before cutting n = 116 (cut 8144, 8115, 8093, 8078, 8065, 8063) --> This is because vanBse_paramsm4 already cut outliers for people greater than 300


df_noOutliers <- dplyr::filter(df, self_coupling_patient > -.1, self_coupling_patient < .15, self_coupling_partner < .15, cross_coupling_patient < .06, cross_coupling_partner < .06) #after cuts: n = 106
df_noOutliers_vanBse <- dplyr::inner_join(df_noOutliers, vanBse_paramsm4, by = "PTNUM") #n = 102; exclude 8115, 8093. 8078, 8065
df_noOutliers_vanBse <-
  dplyr::mutate(
    df_noOutliers_vanBse,
    scpt = 1000 * self_coupling_patient,
    scpr = 1000 * self_coupling_partner,
    ccpt = 1000 * cross_coupling_patient,
    ccpr = 1000 * cross_coupling_partner,
    v_scpt = 1000 * v_self_coupling_patient,
    v_scpr = 1000 * v_self_coupling_parnter,
    v_ccpt = 1000 * v_cross_coupling_patient,
    v_ccpr = 1000 * v_cross_coupling_partner, 
    copt = 1000*summed_cross_coupling_patient,
    copr = 1000*summed_cross_coupling_partner
  )



df_noOutliers_vanBse <-
  df_noOutliers_vanBse %>% mutate(
    pdcpr = pdtot_partner - mean(na.omit(pdtot_partner)),
    iip_x_pdtot_partner = iip_elcpr * pdcpr,
    pdcpt = pdtot_patient - mean(na.omit(pdtot_patient)),
    iip_x_pdtot_patient = iip_elcpt * pdcpt
  )
df_noOutliers_vanBse <-
  dplyr::mutate(
    df_noOutliers_vanBse,
    d_self_coupling_partner = self_coupling_partner - v_self_coupling_parnter,
    d_self_coupling_patient = self_coupling_patient - v_self_coupling_patient,
    d_cross_coupling_partner = cross_coupling_partner - v_cross_coupling_partner,
    d_cross_coupling_patient = cross_coupling_patient - v_cross_coupling_patient
  )



df_noOutliers_vanBse  <-
  dplyr::mutate(
    df_noOutliers_vanBse,
    delta_scpt = d_self_coupling_patient - mean(na.omit(d_self_coupling_patient)),
    delta_scpr = d_self_coupling_partner - mean(na.omit(d_self_coupling_partner)),
    delta_ccpt = d_cross_coupling_patient - mean(na.omit(d_cross_coupling_patient)),
    delta_ccpr = d_cross_coupling_partner - mean(na.omit(d_cross_coupling_partner))
  )

df_noOutliers_vanBse_simp <-
  dplyr::select(
    df_noOutliers_vanBse,
    scpt,
    ccpt,
    scpr,
    ccpr,
    iip_elcpt,
    iip_elcpr,
    pdtot_patient,
    pdtot_partner,
    iip_elevation_patient,
    iip_elevation_partner,
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
    copt,
    copr,
    iip_communion_patient,
    iip_communion_partner,
    iip_agency_patient,
    iip_agency_partner,
    PTNUM
  )


dt_vanBse_simp_fewOutliers <-
  dplyr::filter(dt_vanBse_simp,
                abs(scpt) < 300,
                abs(scpr) < 300,
                abs(ccpt) < 300,
                abs(ccpr) < 300)
#before any more data cleaning: n = 113 (cut 8016, 8060, 8106)
dt_vanBse_simp_fewOutliers$id <- seq(1, 113)

#before any more data cleaning: n = 99 ( 8149, 8146, 8137, 8135, 8127, 8126, 8107, 8100, 8075, 8074, 8066, 8052, 8035, 8020)
dt_vanBse_simp_noOutliers <-
  dplyr::filter(dt_vanBse_simp, scpt > -60, scpt < 120, scpr < 120, ccpt < 70, ccpr < 60) #n = 99

#SECOND bivariate cutting based on cook's distance --> compute for df, dt_vanBse, dt_vanBse_fewOutliers, dt_vanBse_noOutliers
#first for df
library(foreign)
library(MASS)


##ADAPT SO THAT ITERATIVELY GETS RID OF PEOPLE UNTIL NONE ABOVE ACCEPTABLE THRESHOLD 
######FIRST WITH UNIVARIATE CUTS 


######1
df_foranlysis = df_noOutliers
numsubjs = length(df_foranlysis$PTNUM)
ols <- lm(iip_elcpt ~ self_coupling_patient + cross_coupling_patient, data = df_foranlysis)
d1 <- cooks.distance(ols)
r <- stdres(ols)
df_shorted <- dplyr::select(df_foranlysis, PTNUM, self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner, iip_elevation_patient, iip_elevation_partner)
a <- cbind(df_shorted, d1, r)
cthresh <- 4/numsubjs
a[d1 > cthresh, ] 

cat("Excluding observations with Cook's D >", cthresh, "\n")
mup <- df_foranlysis #initially just the full model
allbadcases <- c()

while (max(a$d1) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- a[which.max(a$d1),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(a$d1), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_foranlysis, !PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  ols <- lm(iip_elcpt ~ self_coupling_patient + cross_coupling_patient, data = mup)
  d1 <- cooks.distance(ols)
  r <- stdres(ols)
  df_shorted <- dplyr::select(mup, PTNUM, self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner, iip_elevation_patient, iip_elevation_partner)
  a <- cbind(mup, d1, r)
}
dplyr::anti_join(df_foranlysis, a, by = "PTNUM") %>% dplyr::select(PTNUM)
df_noOutlier_bivariate <- dplyr::filter(df_noOutliers,PTNUM != 8149, PTNUM != 8148, PTNUM != 8112, PTNUM != 8099, PTNUM != 8095, PTNUM != 8094, PTNUM != 8070, PTNUM != 8007 ) #exclude by PTNUM in that list
#n = 98

#####2
df_foranlysis = df_noOutliers_vanBse_simp
numsubjs = length(df_foranlysis$PTNUM)
ols <- lm(iip_elcpt ~ scpt + ccpt, data = df_foranlysis)
d1 <- cooks.distance(ols)
r <- stdres(ols)
df_shorted <- dplyr::select(df_foranlysis, PTNUM, scpt, ccpt, scpr, ccpr, iip_elcpt, iip_elcpr)
a <- cbind(df_shorted, d1, r)
cthresh <- 4/numsubjs
a[d1 > cthresh, ] 

cat("Excluding observations with Cook's D >", cthresh, "\n")
mup <- df_foranlysis #initially just the full model
allbadcases <- c()

while (max(a$d1) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- a[which.max(a$d1),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(a$d1), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_foranlysis, !PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  ols <- lm(iip_elcpt ~ scpt + ccpt, data = mup)
  d1 <- cooks.distance(ols)
  r <- stdres(ols)
  df_shorted <- dplyr::select(mup, PTNUM, scpt, ccpt, scpr, ccpr, iip_elcpt, iip_elcpr)
  a <- cbind(mup, d1, r)
}
dplyr::anti_join(df_foranlysis, a, by = "PTNUM") %>% dplyr::select(PTNUM)
df_noOutlier_vanBse_simp_bivariate <- dplyr::filter(df_noOutliers_vanBse_simp,PTNUM != 8149, PTNUM != 8148, PTNUM != 8112, PTNUM != 8099, PTNUM != 8095, PTNUM != 8094, PTNUM != 8070, PTNUM != 8007 ) #exclude by PTNUM in that list
#n = 94




#####3
df_foranlysis = dt_vanBse_simp_fewOutliers
numsubjs = length(df_foranlysis$PTNUM)
ols <- lm(iip_elcpt ~ scpt + ccpt, data = df_foranlysis)
d1 <- cooks.distance(ols)
r <- stdres(ols)
df_shorted <- dplyr::select(df_foranlysis, PTNUM, scpt, ccpt, scpr, ccpr, iip_elcpt, iip_elcpr)
a <- cbind(df_shorted, d1, r)
cthresh <- 4/numsubjs
a[d1 > cthresh, ] 

cat("Excluding observations with Cook's D >", cthresh, "\n")
mup <- df_foranlysis #initially just the full model
allbadcases <- c()

while (max(a$d1) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- a[which.max(a$d1),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(a$d1), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_foranlysis, !PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  ols <- lm(iip_elcpt ~ scpt + ccpt, data = mup)
  d1 <- cooks.distance(ols)
  r <- stdres(ols)
  df_shorted <- dplyr::select(mup, PTNUM, scpt, ccpt, scpr, ccpr, iip_elcpt, iip_elcpr)
  a <- cbind(mup, d1, r)
}
dplyr::anti_join(df_foranlysis, a, by = "PTNUM") %>% dplyr::select(PTNUM)
dt_fewOutlier_vanBse_simp_bivariate <- dplyr::filter(dt_vanBse_simp_fewOutliers,PTNUM != 8149, PTNUM != 8148, PTNUM != 8137, PTNUM!=8127, PTNUM != 8126, PTNUM != 8112, PTNUM!= 8100, PTNUM != 8099,  PTNUM != 8035, PTNUM != 8007 ) #exclude by PTNUM in that list
#n = 103




######4


df_foranlysis = dt_vanBse_simp_noOutliers
numsubjs = length(df_foranlysis$PTNUM)
ols <- lm(iip_elcpt ~ scpt + ccpt, data = df_foranlysis)
d1 <- cooks.distance(ols)
r <- stdres(ols)
df_shorted <- dplyr::select(df_foranlysis, PTNUM, scpt, ccpt, scpr, ccpr, iip_elcpt, iip_elcpr)
a <- cbind(df_shorted, d1, r)
cthresh <- 4/numsubjs
a[d1 > cthresh, ] 

cat("Excluding observations with Cook's D >", cthresh, "\n")
mup <- df_foranlysis #initially just the full model
allbadcases <- c()

while (max(a$d1) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- a[which.max(a$d1),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(a$d1), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_foranlysis, !PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  ols <- lm(iip_elcpt ~ scpt + ccpt, data = mup)
  d1 <- cooks.distance(ols)
  r <- stdres(ols)
  df_shorted <- dplyr::select(mup, PTNUM, scpt, ccpt, scpr, ccpr, iip_elcpt, iip_elcpr)
  a <- cbind(mup, d1, r)
}
dplyr::anti_join(df_foranlysis, a, by = "PTNUM") %>% dplyr::select(PTNUM)
dt_noOutlier_vanBse_simp_bivariate <- a
#n = 71

#THIRD multivariate cutting based on gCD but no bivariate cutting (still include univariate cutting)

#name the data sets
#from phase 1
dat1a = df_noOutliers
data1b = df_noOutliers_vanBse
data1c = dt_vanBse_simp_fewOutliers
data1d = dt_vanBse_simp_noOutliers
#from phase 2
data2a = df_noOutlier_bivariate #n = 98
data2b = df_noOutlier_vanBse_simp_bivariate #n =94
data2c = dt_fewOutlier_vanBse_simp_bivariate #n = 103
data2d = dt_noOutlier_vanBse_simp_bivariate #n = 71
gpt <- ggplot(data2c, aes(x = iip_elcpt, y = copt))
gpt + geom_point() +geom_smooth(method = "lm") + labs(x = "Patient  Interpersonal Problem Severity", y = "Coregulation Param (Contrarian to Dependent)") +ggtitle("Relationship between Patient's Interpersonal Problem Severity and Coregulation Style") + theme_classic()

gpr <- ggplot(data2c, aes(x = iip_elcpr, y = copr)) + labs(x = "Partner Interpersonal Problem Severity", y = "Coregulation Param (Contrarian to Dependent)") +ggtitle("Relationship between Partner's Interpersonal Problem Severity and Coregulation Style") + theme_classic()
gpr + geom_point() +geom_smooth(method = "lm")

##############1a
df_toanalyze = dt_vanBse_simp_noOutliers_ptnum_inclusion_spreadsheet
iipel_self <- "
self_coupling_patient ~  iip_elevation_patient
self_coupling_partner ~ iip_elevation_partner
cross_coupling_patient ~ iip_elevation_patient
cross_coupling_partner ~ iip_elevation_partner
"

library(influence.SEM)


toexclude1a<- anti_join(df_toanalyze, dfnew, by = "PTNUM") %>% dplyr::select(PTNUM)
dat3a <- dplyr::filter(dat1a, PTNUM != 8149, PTNUM!= 8148, PTNUM!=8135, PTNUM != 8112, PTNUM!= 8135, PTNUM!=8067, PTNUM != 8036, PTNUM!=8023, PTNUM!=8009)

####1b
df_toanalyze = data1b
iipel_
self <- "
scpt ~  iip_elevation_patient
scpr ~ iip_elevation_partner
ccpt ~ iip_elevation_patient
ccpr ~ iip_elevation_partner
"

cd<- genCookDist(model = iipel_self_vanBse, data = df_toanalyze)
listofPTNUM <- c(as.numeric(df_toanalyze$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(df_toanalyze$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = cd

dfnew$PTNUM = df_toanalyze$PTNUM

cthresh <- 4/nsubjs #based off of plot, things larger than .5 seem ot really be far off
cthresh = .9
cthresh = 2
cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- df_toanalyze #initially just the full model
allbadcases <- c()

library(ggplot2); library(cowplot)
while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_toanalyze, !PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <- df_toanalyze %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))
  #if 1a
  g1 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g2 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g3 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g4 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  

  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow=2)
  plot(obj)
  
  cd <- genCookDist(iipel_self_vanBse, mup) 
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
#model= iipel_self
#for dt_vanBse_simp_noOutliers_ptnum_inclusion_spreadsheet
df_tofit <- dplyr::filter(df_toanalyze, PTNUM != 8137, PTNUM != 8066, PTNUM != 8075, PTNUM != 8146, PTNUM != 8135, PTNUM != 8074)

#for dt_vanBse_simp_noWackies_ptnum_inclusion_spreadsheet thresh = .9
dat5b <- dplyr::filter(dat5, PTNUM != 8137, PTNUM != 8052, PTNUM != 8075, PTNUM != 8126, PTNUM != 8127, PTNUM != 8146, PTNUM != 8020, PTNUM != 8135, PTNUM != 8074, PTNUM != 8066, PTNUM != 8100, PTNUM != 8148, PTNUM != 8036, PTNUM != 8067, PTNUM != 8023, PTNUM != 8009, PTNUM != 8149)

#model = iipel_self_vanBse, thresh = 2 (.9 got rid of the entire data set and 2 got rid of the most egregious outliers)
dat5c <- dplyr::filter(dat5, PTNUM !=  8137, PTNUM != 8052, PTNUM != 8075, PTNUM != 8126, PTNUM != 8127, PTNUM != 8135, PTNUM != 8146, PTNUM != 8074, PTNUM !=8020, PTNUM != 8102, PTNUM != 8100)

library(influence.SEM)

cd<- genCookDist(model = iipel_self, data = df_toanalyze)
listofPTNUM <- c(as.numeric(df_toanalyze$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(df_toanalyze$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = cd

dfnew$PTNUM = df_toanalyze$PTNUM

cthresh <- 4/nsubjs #based off of plot, things larger than .5 seem ot really be far off
cthresh = .9
cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- df_toanalyze #initially just the full model
allbadcases <- c()

library(ggplot2); library(cowplot)
while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_toanalyze, !PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <- df_toanalyze %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))

  g1 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g2 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g3 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g4 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow=2)
  plot(obj)
  
  cd <- genCookDist(iipel_self, mup) 
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}


toexclude1b <- anti_join(df_toanalyze, dfnew, by = "PTNUM") %>% dplyr::select(PTNUM)
data3b <- dplyr::filter(data1b, PTNUM != 8149, PTNUM != 8148, PTNUM != 8135, PTNUM != 8112, PTNUM != 8067, PTNUM != 8036, PTNUM != 8023, PTNUM != 8009)

#######1c

df_toanalyze = data1c
iipel_self <- "
scpt ~  iip_elcpt
scpr ~ iip_elcpr
ccpt ~ iip_elcpt
ccpr ~ iip_elcpr
"
iipel_self <- "
self_coupling_patient ~ iip_elevation_patient
self_coupling_partner ~ iip_elevation_partner
cross_coupling_patient ~ iip_elevation_patient
cross_coupling_partner ~ iip_elevation_partner

"
iipel_self_novanBse <- "
scpt ~ iip_elevation_patient
scpr ~ iip_elevation_partner
ccpt ~ iip_elevation_patient
ccpr ~ iip_elevation_partner

"

library(influence.SEM)

cd<- genCookDist(model = iipel_self, data = df_toanalyze)
listofPTNUM <- c(as.numeric(df_toanalyze$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(df_toanalyze$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = cd

dfnew$PTNUM = df_toanalyze$PTNUM

cthresh <- 4/nsubjs #based off of plot, things larger than .5 seem ot really be far off
cthresh = .9
cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- df_toanalyze #initially just the full model
allbadcases <- c()


while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_toanalyze, !PTNUM %in% allbadcases)
  dftoplot <- df_toanalyze %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))
  
  g1 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g2 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g3 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g4 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow=2)
  plot(obj)
  #print(summary(mup), correlation = FALSE)
  cd <- genCookDist(iipel_self, mup) 
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
toexclude1c <- anti_join(df_toanalyze, dfnew, by = "PTNUM") %>% dplyr::select(PTNUM)
data3c <- dplyr::filter(data1c, PTNUM != 8149, PTNUM != 8148, PTNUM != 8146, PTNUM != 8137, PTNUM != 8135, PTNUM != 8127, PTNUM != 8126, PTNUM != 8112, PTNUM != 8100, PTNUM != 8075, PTNUM != 8074, PTNUM != 8067, PTNUM != 8066, PTNUM != 8052, PTNUM != 8036, PTNUM != 8035, PTNUM != 8023, PTNUM != 8020, PTNUM != 8009)


######1d

df_toanalyze = data1d
iipel_self <- "
scpt ~  iip_elcpt
scpr ~ iip_elcpr
ccpt ~ iip_elcpt
ccpr ~ iip_elcpr
"

library(influence.SEM)

cd<- genCookDist(model = iipel_self, data = df_toanalyze)
listofPTNUM <- c(as.numeric(df_toanalyze$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(df_toanalyze$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = cd

dfnew$PTNUM = df_toanalyze$PTNUM

cthresh <- 4/nsubjs #based off of plot, things larger than .5 seem ot really be far off
cthresh = .9
cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- df_toanalyze #initially just the full model
allbadcases <- c()


while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_toanalyze, !PTNUM %in% allbadcases)
  
  dftoplot <- df_toanalyze %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))
  
  g1 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g2 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g3 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g4 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  obj <- plot_grid(g1, g2, g3, g4, nrow=2)
  plot(obj)
  #print(summary(mup), correlation = FALSE)
  cd <- genCookDist(iipel_self, mup) 
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
toexclude1d <- anti_join(df_toanalyze, dfnew, by = "PTNUM") %>% dplyr::select(PTNUM)
data3d <- dplyr::filter(data1d, PTNUM != 8154, PTNUM != 8149, PTNUM != 8148, PTNUM != 8112, PTNUM != 8067, PTNUM != 8055, PTNUM != 8036, PTNUM != 8014, PTNUM != 8009)


######2a
df_toanalyze = data2a
iipel_self <- "
self_coupling_patient ~  iip_elcpt
self_coupling_partner ~ iip_elcpr
cross_coupling_patient ~ iip_elcpt
cross_coupling_partner ~ iip_elcpr
"

library(influence.SEM)

cd<- genCookDist(model = iipel_self, data = df_toanalyze)
listofPTNUM <- c(as.numeric(df_toanalyze$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(df_toanalyze$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = cd

dfnew$PTNUM = df_toanalyze$PTNUM

cthresh <- 4/nsubjs #based off of plot, things larger than .5 seem ot really be far off
cthresh = .9
cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- df_toanalyze #initially just the full model
allbadcases <- c()


while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_toanalyze, !PTNUM %in% allbadcases)
  
  dftoplot <- df_toanalyze %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))
  
  g1 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g2 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g3 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g4 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  obj <- plot_grid(g1, g2, g3, g4, nrow=2)
  plot(obj)
  #print(summary(mup), correlation = FALSE)
  cd <- genCookDist(iipel_self, mup) 
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
toexclude2a <- anti_join(df_toanalyze, dfnew, by = "PTNUM") %>% dplyr::select(PTNUM)
data4a <- dplyr::filter(data2a, PTNUM != 9154, PTNUM != 8135, PTNUM != 8107, PTNUM != 8102, PTNUM != 8091, PTNUM != 8067, PTNUM != 8061, PTNUM != 8055, PTNUM != 8036, PTNUM != 8023, PTNUM != 8014, PTNUM != 8009)


###2b

df_toanalyze = data2b
iipel_self <- "
scpt ~  iip_elcpt
scpr ~ iip_elcpr
ccpt ~ iip_elcpt
ccpr ~ iip_elcpr
"

library(influence.SEM)

cd<- genCookDist(model = iipel_self, data = df_toanalyze)
listofPTNUM <- c(as.numeric(df_toanalyze$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(df_toanalyze$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = cd

dfnew$PTNUM = df_toanalyze$PTNUM

cthresh <- 4/nsubjs #based off of plot, things larger than .5 seem ot really be far off
cthresh = .9
cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- df_toanalyze #initially just the full model
allbadcases <- c()


while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_toanalyze, !PTNUM %in% allbadcases)
  dftoplot <- df_toanalyze %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))
  
  g1 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g2 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g3 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g4 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow=2)
  plot(obj)
  #print(summary(mup), correlation = FALSE)
  cd <- genCookDist(iipel_self, mup) 
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
toexclude2b<- anti_join(df_toanalyze, dfnew, by = "PTNUM") %>% dplyr::select(PTNUM)
data4b <- dplyr::filter(data2b, PTNUM != 8135, PTNUM != 8036)

###############2c

df_toanalyze = data2c
iipel_self <- "
scpt ~  iip_elcpt
scpr ~ iip_elcpr
ccpt ~ iip_elcpt
ccpr ~ iip_elcpr
"

library(influence.SEM)

cd<- genCookDist(model = iipel_self, data = df_toanalyze)
listofPTNUM <- c(as.numeric(df_toanalyze$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(df_toanalyze$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = cd

dfnew$PTNUM = df_toanalyze$PTNUM

cthresh <- 4/nsubjs #based off of plot, things larger than .5 seem ot really be far off
cthresh = .9
cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- df_toanalyze #initially just the full model
allbadcases <- c()


while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_toanalyze, !PTNUM %in% allbadcases)
  dftoplot <- df_toanalyze %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))
  
  g1 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g2 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g3 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g4 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  obj <- plot_grid(g1, g2, g3, g4, nrow=2)
  plot(obj)
  #print(summary(mup), correlation = FALSE)
  cd <- genCookDist(iipel_self, mup) 
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
toexclude2c<- anti_join(df_toanalyze, dfnew, by = "PTNUM") %>% dplyr::select(PTNUM)
data4c <- dplyr::filter(data2c, PTNUM != 8146, PTNUM != 8135, PTNUM != 8075, PTNUM != 8074, PTNUM != 8067, PTNUM != 8066, PTNUM != 8052, PTNUM != 8036, PTNUM != 8023, PTNUM != 8020, PTNUM != 8009)


#####2d
df_toanalyze = data2d
iipel_self <- "
scpt ~  iip_elcpt
scpr ~ iip_elcpr
ccpt ~ iip_elcpt
ccpr ~ iip_elcpr
"

library(influence.SEM)

cd<- genCookDist(model = iipel_self, data = df_toanalyze)

listofPTNUM <- c(as.numeric(df_toanalyze$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(df_toanalyze$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = cd

dfnew$PTNUM = df_toanalyze$PTNUM

cthresh <- 4/nsubjs #based off of plot, things larger than .5 seem ot really be far off
cthresh = 1
cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- df_toanalyze #initially just the full model
allbadcases <- c()


while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(df_toanalyze, !PTNUM %in% allbadcases)
  dftoplot <- df_toanalyze %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))
  
  g1 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g2 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g3 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g4 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  obj <- plot_grid(g1, g2, g3, g4, nrow=2)
  plot(obj)
  #print(summary(mup), correlation = FALSE)
  cd <- genCookDist(iipel_self, mup) 
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
toexclude2d <- anti_join(df_toanalyze, dfnew, by = "PTNUM") %>% dplyr::select(PTNUM)
#because of issues for determining appropriate threshold, no cuts
data4d = data2d




#MODELS TO BE TESTED ACROSS THESE DIFFERENT DATA SETS

tofit = data2b
iipel_self_vanBse <- "
scpt ~  iip_elcpt + v_scpt + v_ccpt
scpr ~ iip_elcpr + v_scpr + v_ccpr
ccpt ~ iip_elcpt + v_scpt + v_ccpt 

ccpr ~ iip_elcpr  + v_scpr + v_ccpr


"
iipel_self_nooutliers_vanBse_m <- sem(iipel_self_vanBse, tofit, missing = "listwise", estimator = "ML", 
                                      mimic="Mplus", meanstructure = TRUE, conditional.x=TRUE)
iipel_self_vanBse <- "scpt ~  iip_elevation_patient + v_scpt
scpr ~ iip_elevation_partner + v_scpr + v_ccpr + v_ccpt
ccpt ~ iip_elevation_patient + v_ccpt

ccpr ~ iip_elevation_partner  + v_ccpr + v_scpr
"

testing1 <- sem(iipel_self_vanBse, df_tofit, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE )

pdcpt <- untouched - mean(untouched)
iipel_pdtot_self_nosum <- "
self_coupling_patient ~  iip_elcpt + pdcountcpt
self_coupling_partner ~ iip_elcpr  +  pdcountcpr
cross_coupling_partner ~ iip_elcpr + pdcountcpr 
cross_coupling_patient ~  iip_elcpt + pdcountcpt
"
iipel_pdtot_self_nosum_nooutliers_m <- sem(iipel_pdtot_self_nosum, df_nooutliers, missing = "listwise", estimator = "ML", 
                                           mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)
tofit = data2b
tofit <- dplyr::mutate(tofit, pdcptR = pdtot_patient - mean(na.omit(pdtot_patient)), 
                       pdcprR = pdtot_partner - mean(na.omit(pdtot_partner)),
                       iip_elcptR = iip_elevation_patient - mean(na.omit(iip_elevation_patient)),
                       iip_elcprR = iip_elevation_partner -mean(na.omit(iip_elevation_partner)))
iipel_pdtot_self_vanBse <- "
scpt ~  iip_elcptR + pdcptR
scpr ~ iip_elcprR  +  pdcprR
ccpt ~ iip_elcprR + pdcptR 
ccpr ~  iip_elcptR + pdcprR
ccpt ~ v_ccpt
ccpr ~v_ccpr
scpt ~ v_scpt
scpr ~ v_scpr

#testing from modification indices
scpr  ~ v_ccpr
ccpr  ~ v_scpr
"
iipel_pdtot_self_vanBse_m <- sem(iipel_pdtot_self_vanBse, tofit, missing = "listwise", estimator = "ML", 
                                 mimic="Mplus", meanstructure = TRUE, conditional.x=TRUE)


iipel_pdtot_dim_self_vanBse <- "
scpt ~  iip_elevation_patient + pdtot_patient
scpr ~ iip_elevation_partner +  pdtot_partner
ccpr ~ iip_elevation_partner + pdtot_partner
ccpt ~  iip_elevation_patient + pdtot_patient + pdtot_partner + v_ccpr
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
    tofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE, conditional.x=TRUE)


summary(lm(ccpr ~ iip_elcprR + pdcprR + v_ccpr, tofit))
summary(lm(ccpr ~ iip_elevation_partner + pdtot_partner + v_ccpr, tofit))

summary(lm(ccpr ~ iip_elcprR, tofit))
summary(lm(ccpr ~ iip_elevation_partner, tofit))

summary(lm(ccpr ~  pdcprR, tofit))
summary(lm(ccpr ~  pdtot_patient, tofit))

summary(lm(ccpr ~ iip_elcprR + pdcprR, tofit))
summary(lm(ccpr ~ iip_elevation_partner+ pdtot_partner, tofit))


round(cor(dplyr::select(tofit, ccpr, v_ccpr, iip_elcptR, iip_elevation_patient, pdcprR,pdcptR, pdtot_patient, pdtot_partner)), 3)


simple1 <- "
scpt ~  iip_elevation_patient + pdtot_patient
scpr ~ iip_elevation_partner +  pdtot_partner

ccpt ~  iip_elevation_patient + pdtot_patient
ccpr ~ iip_elevation_partner + pdtot_partner

scpt ~ v_scpt
scpr ~ v_scpr

ccpt ~ v_ccpt
ccpr ~ v_ccpr

#testing from modification indices
scpr  ~ v_ccpr
ccpr  ~ v_scpr

"
simple1out <-  lavaan::sem(simple1, tofit, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x=TRUE)

simple2 <- "
scpt ~  iip_elcptR + pdcptR
scpr ~ iip_elcprR  +  pdcprR

ccpt ~  iip_elcptR + pdcptR
ccpr ~ iip_elcprR + pdcprR

scpt ~ v_scpt
scpr ~ v_scpr

ccpt ~ v_ccpt
ccpr ~ v_ccpr

#testing from modification indices
scpr  ~ v_ccpr
ccpr  ~ v_scpr

"
simple2out <-  lavaan::sem(simple2, tofit, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE, conditional.x=TRUE)


str(tofit)


pdtot_dim_self_vanBse <- "
scpt ~  pdtot_patient
scpr ~ pdtot_partner
ccpr ~  pdtot_partner
ccpt ~   pdtot_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr

#testing from modification indices
scpr  ~ v_ccpr
#ccpr  ~ v_scpr
"
pdtot_dim_self_vanBse_m <-
  lavaan::sem(
    pdtot_dim_self_vanBse,
    tofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE, conditional.x=TRUE)








tofit = data2b
iipel_self <- "
scpt ~  iip_elcpt
scpr ~ iip_elcpr
ccpt ~ iip_elcpt 
ccpr ~ iip_elcpr 
"
iipel_self_m <- sem(iipel_self, tofit, missing = "listwise", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE, conditional.x=TRUE)

tofit = data4b
iipel_self <- "
self_coupling_patient ~  iip_elcpt
self_coupling_partner ~ iip_elcpr 
cross_coupling_patient ~ iip_elcpt 
cross_coupling_partner ~ iip_elcpr 
"
iipel_self_m <- sem(iipel_self, tofit, missing = "ML", estimator = "ML", 
                    mimic="Mplus", meanstructure = TRUE) #, conditional.x=cxsetting)



iipag_self_vanBse <- "

scpt ~  iip_agency_patient
scpr ~ iip_agency_partner
ccpt ~ iip_agency_patient
ccpr ~ iip_agency_partner
scpt ~ v_scpt
ccpr ~ v_ccpr
scpr ~ v_scpr
ccpt ~ v_ccpt
scpr ~ v_ccpr
ccpr ~ v_scpr
"
iipag_self_vanBse_m <-
  sem(
    iipag_self_vanBse,
    tofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE, conditional.x = TRUE)


iipcm_self_vanBse <- "

scpt ~  iip_communion_patient
scpr ~ iip_communion_partner
ccpt ~ iip_communion_patient
ccpr ~ iip_communion_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr
ccpr ~ v_scpr
#scpr ~ v_ccpr
"
iipcm_self_vanBse_m <-
  sem(
    iipcm_self_vanBse,
    tofit,
    missing = "ML",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE, conditional.x = TRUE)


tofit$agptresid <- residuals(lm(iip_agency_patient ~ v_scpt + v_ccpt, data = tofit))
tofit$agprresid <- residuals(lm(iip_agency_partner ~ v_scpr + v_ccpr, data = tofit))
tofit$ccptresid <- residuals(lm(ccpt ~ v_ccpt, data = tofit))
tofit$scptresid <- residuals(lm(scpt ~ v_scpt, data = tofit))
tofit$scprresid <- residuals(lm(scpr ~ v_scpr, data = tofit))
tofit$ccprresid <- residuals(lm(ccpr ~ v_ccpr, data = tofit))
tofit$coptresid <- with(tofit, -scptresid + ccptresid)
tofit$coprresid <- with(tofit, -scprresid + ccprresid)
tofit$cmptresid <- residuals(lm(iip_communion_patient ~ v_scpt + v_ccpt, data = tofit))
tofit$cmprresid <- residuals(lm(iip_communion_partner ~ v_scpr + v_ccpr, data = tofit))
tofit$elptresid <- residuals(lm(iip_elevation_patient ~ v_scpt + v_ccpt, data = tofit))
tofit$elprresid <- residuals(lm(iip_elevation_partner ~ v_scpt + v_ccpr, data = tofit))

g <- ggplot(tofit, aes(x= agprresid, y = coprresid))
g + geom_point() + geom_smooth(method = "lm") + ggtitle("The Relationship between Problems in Agency and Coregulation for Partners") + xlab("Problems in Agency") + ylab("Coregulation Style (from Contrarian to Dependent")






iipagcm_self_vanBse <- "

scpt ~  iip_agency_patient + iip_communion_patient 
scpr ~ iip_agency_partner + iip_communion_partner
ccpt ~ iip_agency_patient + iip_communion_patient
ccpr ~ iip_agency_partner + iip_communion_partner
scpt ~ v_scpt
ccpr ~ v_ccpr
scpr ~ v_scpr
ccpt ~ v_ccpt
scpr ~ v_ccpr
ccpr ~ v_scpr
"
iipagcm_self_vanBse_m <-
  sem(
    iipagcm_self_vanBse,
    tofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE, conditional.x = TRUE)


iipelagcm_self_vanBse <- "

scpt ~  iip_agency_patient + iip_communion_patient + iip_elevation_patient
scpr ~ iip_agency_partner + iip_communion_partner + iip_elevation_partner
ccpt ~ iip_agency_patient + iip_communion_patient + iip_elevation_patient
ccpr ~ iip_agency_partner + iip_communion_partner + iip_elevation_partner
scpt ~ v_scpt
ccpr ~ v_ccpr
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_scpr
"
iipelagcm_self_vanBse_m <-
  sem(
    iipelagcm_self_vanBse,
    tofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE, conditional.x = TRUE)














gCD <- genCookDist(iipel_self, dt_vanBse_simp_fewOutliers)


#BEST DATA SET, 2B
data2b$iip_elcpr_residA = residuals(lm(iip_elcpr ~ v_scpr, data = data2b)) 
data2b$iip_elcpr_residB = residuals(lm(iip_elcpr ~ v_ccpr, data = data2b))
data2b$iip_elcpr_residC = residuals(lm(iip_elcpr ~ v_scpr + v_ccpr, data = data2b))
data2b$scpr_resid = residuals(lm(scpr ~ v_scpr, data = data2b))
data2b$ccpr_resid = residuals(lm(ccpr ~ v_ccpr, data = data2b))
data2b$copr_resid = with(data2b,-scpr_resid +ccpr_resid )
data2b$iip_elcpt_residA = residuals(lm(iip_elcpt ~ v_scpt, data = data2b)) 
data2b$iip_elcpt_residB = residuals(lm(iip_elcpt ~ v_ccpt, data = data2b))
data2b$iip_elcpt_residC = residuals(lm(iip_elcpt ~ v_scpt + v_ccpt, data = data2b))
data2b$scpt_resid = residuals(lm(scpt ~ v_scpt, data = data2b))
data2b$ccpt_resid = residuals(lm(ccpt ~ v_ccpt, data = data2b))

data2b$copt_res = with(data2b, -scpt_resid + ccpt_resid)
iip_elctr_resid = dplyr::select(data2b, iip_elcpt_residA, iip_elcpt_residB, iip_elcpt_residC)
g <- ggplot(data2b, aes(x = iip_elcpt_residC, y = copt_res))
g + geom_point() + geom_smooth(method = "lm") + theme_classic() + ggtitle("The relationship between interpersonal problem severity and coregulation style for Patients") + xlab("Interpersonal Problem Severity") + ylab("Cross-Coupling (From Contrairan to Dependent")






#Cross check this data set and the R2 for each of the couples as a way to determine fit.
library(R.matlab)
setwd("~/Desktop/")
R2 <- readMat("modelcomparisonR2.mat")
fit <- as.data.frame(R2$outputsR2)
#note: since only examining M3 (which had all 6 parameters) only need to examine v3
fit$PTNUM <- as.vector(R2$ids)
baddat <- anti_join(dt, data2b, by = "PTNUM") %>% dplyr::select(PTNUM)
baddat <- inner_join(baddat, fit, by = "PTNUM")
fit <- dplyr::select(fit, V3, PTNUM)
fit <- rename(fit, fit = V3)
dplyr::inner_join(data2b, fit, by = "PTNUM") %>% dplyr::select(PTNUM, fit, scpt, ccpt, scpr, ccpr) %>% dplyr::filter(fit < .99)
#people with bad fits. 8016, 8060, 8106, 8112, 8133 (not crazy estimates so this is surprising), 8144





