library(tidyverse)
library(haven)
library(dependlab)
setwd("/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/CombineSelfReports/INTAKE_20170223")

score_iip90 <- function(iip, item_prefix="IIP", max_impute=0.2, 
                        drop_items=FALSE, keep_reverse_codes=FALSE, max_value=4) {
  
  #iipItems <- paste0("IIP", 1:90, sep=".") #expect item names
  #stopifnot(is.data.frame(iip))
  #stopifnot(all(iipItems %in% names(iip)))
  pa_items <- sapply(c(21, 40, 57, 58, 65, 68, 76, 80), function(x) { paste0(item_prefix, x) })
  bc_items <- sapply(c(1, 26, 28, 38, 41, 50, 73, 88), function(x) { paste0(item_prefix, x) })
  de_items <- sapply(c(11, 18, 20, 24, 27,31, 46, 82), function(x) { paste0(item_prefix, x) })
  fg_items <- sapply(c(3,7,17,22,43,45,71,85), function(x) { paste0(item_prefix, x) })
  hi_items <- sapply(c(5,6,8,9,12,15,23,49), function(x) { paste0(item_prefix, x) })
  jk_items <- sapply(c(2,10,29,44,48,54,69,83), function(x) { paste0(item_prefix, x) })
  lm_items <- sapply(c(25,37,47,59,64,67,70,87), function(x) { paste0(item_prefix, x) })
  no_items <- sapply(c(4,30,39,52,56,61,62,78), function(x) { paste0(item_prefix, x) })
  
  
  c1_items <- sapply(c(2,9,16,48,59,66,72,74,75), function(x) { paste0(item_prefix, x) }) #need for social approval
  c2_items <- sapply(c(3,7,17,19,22,33,43,49,71,85), function(x) { paste0(item_prefix, x) }) #lack of sociability
  
  #other scales
  bpd_items <- sapply(c(51,53,55,66,77,80,89,90), function(x) { paste0(item_prefix, x) })#clifton bpd scale
  pd1_items <- sapply(c(1,35,36,42,51,55,60,78,79,81,86), function(x) { paste0(item_prefix, x) }) #interpersonal sensitivity
  pd2_items <- sapply(c(13,14,26,28,32,34,38,40,41,84), function(x) { paste0(item_prefix, x) }) #interpersonal ambivalence
  pd3_items <- sapply(c(50,53,58,63,77,80,88), function(x) { paste0(item_prefix, x) }) #aggression
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    iip <- mean_impute_items(iip, pa_items, thresh=max_impute)
    iip <- mean_impute_items(iip, bc_items, thresh=max_impute)
    iip <- mean_impute_items(iip, de_items, thresh=max_impute)
    iip <- mean_impute_items(iip, fg_items, thresh=max_impute)
    iip <- mean_impute_items(iip, hi_items, thresh=max_impute)
    iip <- mean_impute_items(iip, jk_items, thresh=max_impute)
    iip <- mean_impute_items(iip, lm_items, thresh=max_impute)
    iip <- mean_impute_items(iip, no_items, thresh=max_impute)
  }
  
  iip <- iip %>% mutate(IIP_bc = rowSums(select(., bc_items)),IIP_pa = rowSums(select(., pa_items)),
                      IIP_de = rowSums(select(., de_items)), IIP_fg = rowSums(select(., fg_items)),
                      IIP_hi = rowSums(select(., hi_items)),IIP_jk = rowSums(select(., jk_items)),
                      IIP_lm = rowSums(select(., lm_items)), IIP_no = rowSums(select(., no_items)),
                      IIP_bpd = rowSums(select(., bpd_items)),IIP_pd1 =rowSums(select(., pd1_items)),
                      IIP_pd2 = rowSums(select(., pd2_items)),IIP_pd3 =rowSums(select(., pd3_items)),
                      IIP_pd = mean(c(IIP_pd1, IIP_pd2, IIP_pd3)), IIP_havePD = as.numeric(IIP_pd > 1.1),
                      IIP_c1 = rowSums(select(., c1_items)),IIP_c2 = rowSums(select(., c2_items)),
                      IIP_c = mean(c(IIP_c1, IIP_c2)), 
                      IIP_agency = .25*(IIP_pa - IIP_hi + .707*(IIP_bc + IIP_no - IIP_fg - IIP_jk)),
                      IIP_communion = .25*(IIP_lm - IIP_de + .707*(IIP_no + IIP_jk - IIP_bc - IIP_fg)),
                      IIP_elevation = (IIP_pa + IIP_bc + IIP_de + IIP_fg + IIP_hi + IIP_jk + IIP_lm + IIP_no)/8)

  return(iip)
}


iip <- read_spss("IIP90.sav") #grabbing ag, com, el
iip_baseline <- dplyr::filter(iip, mth == 0)
iip_baseline_df <- score_iip90(iip_baseline) %>% dplyr::select(PTNUM, DyadID, IIP_agency, IIP_communion, IIP_elevation) %>% gather(key = "key", value = "value", -PTNUM, -DyadID) %>% mutate(key = paste0(key, "_", DyadID)) %>% dplyr::select(-DyadID) %>% spread(key = "key", value = "value")

das <- read_spss("DAS.sav") #grabbing subscales at baseline and 12 month fu
das_baseline <- dplyr::filter(das, mth == 0)
das_baseline_df <- score_das(das_baseline) %>% dplyr::select(PTNUM, DyadID, DASCon, DASSat, DASCoh, DASAffExp, DASTotal) %>% gather(key = "key", value = "value", -PTNUM, -DyadID) %>% mutate(key = paste0(key, "_", DyadID)) %>% dplyr::select(-DyadID) %>% spread(key = "key", value = "value")
das_baseline_df_wide <- score_das(das_baseline) %>% dplyr::select(PTNUM, DyadID, DASCon, DASSat, DASCoh, DASAffExp, DASTotal, UsrID)
das_fu <- dplyr::filter(das, mth == 12)

das_fu_df_wide <- score_das(das_fu) %>% dplyr::select(PTNUM, DyadID, DASCon, DASSat, DASCoh, DASAffExp, DASTotal, DASrelationship) 
das_fu_df <- score_das(das_fu) %>% dplyr::select(PTNUM, DyadID, DASCon, DASSat, DASCoh, DASAffExp, DASTotal, DASrelationship) %>% gather(key = "key", value = "value", -PTNUM, -DyadID) %>% mutate(key = paste0(key, "_fu_", DyadID)) %>% dplyr::select(-DyadID) %>% spread(key = "key", value = "value")
ecr <- read_spss("ECR.sav")
ecr_baseline <- dplyr::filter(ecr, mth == 0) 
ecr_baseline_df <- score_ecr(ecr_baseline) %>% dplyr::select(PTNUM,DyadID, ECR_anxiety, ECR_avoidance) %>% gather(key = "key", value = "value", -PTNUM, -DyadID) %>% mutate(key = paste0(key, "_", DyadID)) %>% dplyr::select(-DyadID) %>% spread(key = "key", value = "value")


fulldata_VAR <- read.csv("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv") %>% dplyr::select(-X, -starts_with("iip"))

alldata <- plyr::join_all(list(fulldata_VAR, iip_baseline_df, das_baseline_df, das_fu_df, ecr_baseline_df))
alldata_nopplcut <- alldata %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
alldata_nopplcut_badR2 <- alldata_nopplcut %>% dplyr::filter(m1_R2 > .98)
alldata_nopplcut_badR2_modna <- mutate(alldata_nopplcut_badR2, elpt.c= IIP_elevation_1 - mean(IIP_elevation_1, na.rm = TRUE),
                                 elpr.c= IIP_elevation_0 - mean(IIP_elevation_0, na.rm = TRUE),
                                 agpt.c= IIP_agency_1 - mean(IIP_agency_1, na.rm = TRUE),
                                 agpr.c= IIP_agency_0 - mean(IIP_agency_0, na.rm = TRUE),
                                 cmpt.c= IIP_communion_1 - mean(IIP_communion_1, na.rm = TRUE),
                                 cmpr.c= IIP_communion_0 - mean(IIP_communion_0, na.rm = TRUE),
                                 agcmpt = agpt.c*cmpt.c, agcmpr = agpr.c*cmpr.c,
                                 agpt = IIP_agency_1, agpr = IIP_agency_0, 
                                 elpt = IIP_elevation_1, elpr = IIP_elevation_0,
                                 cmpt = IIP_communion_1, cmpr = IIP_communion_0,
                                 lprnapt = log(na_pre_patient), lpnapt = log(na_post_patient),
                                 lprnapr = log(na_pre_partner), lpnapr = log(na_post_partner),
                                 prnapt = na_pre_patient, prnapr = na_pre_partner,
                                 pnapt = na_post_patient, pnapr = na_post_partner,
                                 DASTot1 = DASTotal_1, DASTot0 = DASTotal_0, DASrel0 = DASrelationship_fu_0, DASrel1 = DASrelationship_fu_1, DASrel = if_else(DASrel0 == 0 | DASrel1 == 0, 0, 1),
                                 prafpt = AFF_pre_patient, prafpr = AFF_post_partner,
                                 pafpt = AFF_post_patient, pafpr = AFF_post_partner)  
write.csv(alldata_nopplcut_badR2_modna,"~/Box/DEPENd/Projects/PD_Interaction_Dynamics/output/fulldata_VAR_mod10Dec2018.csv")

missingdata_DAS <- alldata_nopplcut_badR2_modna %>% filter(is.na(DASTot0) | is.na(DASTot1))
missingdata_ECR <- alldata_nopplcut_badR2_modna %>% filter(is.na(ECR_anxiety_0) | is.na(ECR_anxiety_1) | is.na(ECR_avoidance_0) | is.na(ECR_anxiety_1))
missingdata_PANAS <- alldata_nopplcut_badR2_modna %>% filter(is.na(prnapt) | is.na(prnapr) | is.na(pnapt) | is.na(pnapr))
missingdata_DASrel <- alldata_nopplcut_badR2_modna %>% filter( is.na(DASrel))
brokenup_DASrel <- alldata_nopplcut_badR2_modna %>% filter( DASrel == 0)
together_DASrel <- alldata_nopplcut_badR2_modna %>% filter( DASrel ==1)
missingdata_DASfu <- together_DASrel %>% filter(is.na(DASTotal_fu_0) | is.na(DASTotal_fu_1))
missingdata_iip <- alldata_nopplcut_badR2_modna %>% filter(is.na(elpt) | is.na(elpr) | is.na(agpt) | is.na(agpr) | is.na(cmpt) | is.na(cmpr))
View(dplyr::select(missingdata_iip, PTNUM, elpt, elpr, agpt, agpr, cmpt,cmpr))
missingdata_imic <- alldata_nopplcut_badR2_modna %>% filter(is.na(prafpt) | is.na(prafpr) | is.na(pafpt) | is.na(pafpr) )

#trying to figure out where 8022 imi 8149 imi 8153 imi 8098 panas and 8139 das went
str(alldata_nopplcut_badR2_modna)
                                                           
                                                           
                                                           
                                                           
