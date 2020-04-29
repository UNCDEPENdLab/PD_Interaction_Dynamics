#Lavaan and Mplus APIM functions
#Lavaan is setup for conventional APIMs (4 variables)
#Mplus is multilevel APIM where alters are clustered within individuals (e.g., degree centrality or anger ratings)
#modified so that one of the APIMs tested fixed partner pathways to 0
coregAPIM <- function(df, DV1, DV2, predictors, vanBse=FALSE, modIndices, partners=c("_0", "_1"), additional="", printall=FALSE) {
  require(lavaan)
  #by default, only print indistinguishable dyads model (printall=FALSE)
  
  indistinguishable_syn <- ""
  for (p in 1:length(predictors)) {
    indistinguishable_syn <- paste0(indistinguishable_syn, DV1, partners[1], additional, " ~ a1", p, "*", predictors[p], partners[1], additional,
                                    " + p1", p, "*", predictors[p], partners[2], additional, "\n ",
                                    DV1, partners[2], additional, " ~ a1", p, "*", predictors[p], partners[2], additional,
                                    " + p1", p, "*", predictors[p], partners[1], additional, "\n", 
                                    DV2, partners[1], additional, " ~ a2", p, "*", predictors[p], partners[1], additional,
                                    " + p2", p, "*", predictors[p], partners[2], additional, "\n", 
                                    DV2, partners[2], additional, " ~ a2", p, "*", predictors[p], partners[2], additional,
                                    " + p2", p, "*", predictors[p], partners[1], additional, "\n")
  }
  
  aonly_syn <- afree_syn <- pfree_syn <- allfree_syn <- indistinguishable_syn
  for (p in 1:length(predictors)) {
    afree_syn <- sub(paste0("a1", p, "*"), paste0("a1", p, partners[1], "*"), afree_syn, fixed=TRUE) #replace first instance of actor with _0
    afree_syn <- sub(paste0("a1", p, "*"), paste0("a1", p, partners[2], "*"), afree_syn, fixed=TRUE) #replace second instance of actor with _1
    afree_syn <- sub(paste0("a2", p, "*"), paste0("a2", p, partners[1], "*"), afree_syn, fixed=TRUE) #replace first instance of actor with _0
    afree_syn <- sub(paste0("a2", p, "*"), paste0("a2", p, partners[2], "*"), afree_syn, fixed=TRUE) #replace second instance of actor with _1
    
    pfree_syn <- sub(paste0("p1", p, "*"), paste0("p1", p, partners[1], "*"), pfree_syn, fixed=TRUE) #replace first instance of partner with _0
    pfree_syn <- sub(paste0("p1", p, "*"), paste0("p1", p, partners[2], "*"), pfree_syn, fixed=TRUE) #replace second instance of partner with _1
    pfree_syn <- sub(paste0("p2", p, "*"), paste0("p2", p, partners[1], "*"), pfree_syn, fixed=TRUE) #replace first instance of partner with _0
    pfree_syn <- sub(paste0("p2", p, "*"), paste0("p2", p, partners[2], "*"), pfree_syn, fixed=TRUE) #replace second instance of partner with _1
    
    
    aonly_syn <- sub(paste0("a1", p, "*"), paste0("a1", p, partners[1], "*"), aonly_syn, fixed=TRUE) #replace first instance of actor with _0
    aonly_syn <- sub(paste0("a1", p, "*"), paste0("a1", p, partners[2], "*"), aonly_syn, fixed=TRUE) #replace second instance of actor with _1
    aonly_syn <- sub(paste0("a2", p, "*"), paste0("a2", p, partners[1], "*"), aonly_syn, fixed=TRUE) #replace first instance of actor with _0
    aonly_syn <- sub(paste0("a2", p, "*"), paste0("a2", p, partners[2], "*"), aonly_syn, fixed=TRUE) #replace second instance of actor with _1
    aonly_syn <- sub(paste0("p1", p, "*"), paste0(0, "*"), aonly_syn, fixed = TRUE)
    aonly_syn <- sub(paste0("p2", p, "*"), paste0(0, "*"), aonly_syn, fixed = TRUE)
    aonly_syn <- sub(paste0("p1", p, "*"), paste0(0, "*"), aonly_syn, fixed = TRUE)
    aonly_syn <- sub(paste0("p2", p, "*"), paste0(0, "*"), aonly_syn, fixed = TRUE)
    
    
    allfree_syn <- sub(paste0("a1", p, "*"), paste0("a1", p, partners[1], "*"), allfree_syn, fixed=TRUE) #replace first instance of actor with _0
    allfree_syn <- sub(paste0("a1", p, "*"), paste0("a1", p, partners[2], "*"), allfree_syn, fixed=TRUE) #replace second instance of actor with _1
    allfree_syn <- sub(paste0("a2", p, "*"), paste0("a2", p, partners[1], "*"), allfree_syn, fixed=TRUE) #replace first instance of actor with _0
    allfree_syn <- sub(paste0("a2", p, "*"), paste0("a2", p, partners[2], "*"), allfree_syn, fixed=TRUE) #replace second instance of actor with _1
    allfree_syn <- sub(paste0("p1", p, "*"), paste0("p1", p, partners[1], "*"), allfree_syn, fixed=TRUE) #replace first instance of actor with _0
    allfree_syn <- sub(paste0("p1", p, "*"), paste0("p1", p, partners[2], "*"), allfree_syn, fixed=TRUE) #replace second instance of actor with _1
    
    allfree_syn <- sub(paste0("p2", p, "*"), paste0("p2", p, partners[1], "*"), allfree_syn, fixed=TRUE) #replace first instance of partner with _0
    allfree_syn <- sub(paste0("p2", p, "*"), paste0("p2", p, partners[2], "*"), allfree_syn, fixed=TRUE) #replace second instance of partner with _1
    
  }
  #  syntax <- paste0(DV, partners[1], additional, " ~ asame*", predictor, partners[1], additional, " + psame*", predictor, partners[2], additional,
  #      "\n ", DV, partners[2], additional, " ~ asame*", predictor, partners[2], additional, " + psame*", predictor, partners[1], additional)
  
  indistinguishable <- sem(indistinguishable_syn, df, missing="ML", estimator="MLR")
  cat("\n\n-------\nIndistinguishable dyads model:\n------\n")
  summary(indistinguishable, fit.measures=TRUE)
  #standardizedSolution(res, type="std.all")
  
  afree <- sem(afree_syn, df, missing="ML", estimator="MLR")
  
  aonly <- sem(aonly_syn, df, missing = "ML", estimator = "MLR")
  
  pfree <- sem(pfree_syn, df, missing="ML", estimator="MLR")
  
  allfree <- sem(allfree_syn, df, missing="ML", estimator="MLR")
  
  if (printall) {
    cat("\n\n-------\nFree actor model:\n------\n")
    summary(afree, fit.measures=TRUE)
    
    cat("\n\n-------\nActor only model:\n------\n")
    summary(aonly, fit.measures = TRUE)
    
    cat("\n\n-------\nFree partner model:\n------\n")
    summary(pfree, fit.measures=TRUE)
    
    cat("\n\n-------\nFree actor and partner (saturated) model:\n------\n")
    summary(allfree, fit.measures=TRUE)
    }
  
  print(anova(indistinguishable, afree, aonly, pfree, allfree))
  cat(indistinguishable_syn)
  
  return(list(syntax=list(i=indistinguishable_syn, a=afree_syn, ao = aonly_syn, p=pfree_syn, all=allfree_syn), indistinguishable=indistinguishable, afree=afree, aonly = aonly, pfree=pfree, allfree=allfree))
}




##### EXAMPLE
library(R.matlab)
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



dat1a = df_noOutliers
data1b = df_noOutliers_vanBse
data1c = dt_vanBse_simp_fewOutliers
data1d = dt_vanBse_simp_noOutliers


#examinig elevation
coregAPIM(data1c, "sc", "cc", c("iip_elc", "v_sc"), partners = c("pt", "pr"), printall= TRUE)

