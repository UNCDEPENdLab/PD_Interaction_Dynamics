library(tidyverse)
params_df <- read.csv("~/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv") %>% dplyr::filter(!is.na(scpt), m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104) 
univariateoutiers_min <- function(vec) {
  min <- as.numeric(quantile(vec, .25))
  max <- as.numeric(quantile(vec, .75))
  iqr = IQR(vec)
  min_cutoff <- min - 1.5*(iqr)
  return(min_cutoff)
  
  
  
}
univariateoutiers_max <- function(vec) {
  min <- as.numeric(quantile(vec, .25))
  max <- as.numeric(quantile(vec, .75))
  iqr = IQR(vec)
  max_cutoff <- max + 1.5*(iqr)
  return(max_cutoff)
  
  
  
}
params_df <- dplyr::filter(params_df,scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) 
params_df <- dplyr::select(mutate(params_df, scpt = scpt/100, ccpt/100, scpr/100, ccpr/100), scpt, ccpt, scpr, ccpr,PTNUM)
writeMat("~/Desktop/pdid_params_98.mat", x = as.matrix(params_df))
ptpr_sim_prcrazy = readMat("~/Downloads/sep2019_pr_crazy.mat")
pt_sim_prcrazy = as.data.frame(ptpr_sim_prcrazy$pt)
pr_sim_prcrazy = as.data.frame(ptpr_sim_prcrazy$pr)
colnames(pt_sim_prcrazy) = c(paste0("t",1:800), "PTNUM")
pt_sim_prcrazy$DyadID <- 1
colnames(pr_sim_prcrazy) = c(paste0("t",1:800), "PTNUM")
pr_sim_prcrazy$DyadID <- 0
ptpr_simulations_prcrazy <- bind_rows(pt_sim_prcrazy, pr_sim_prcrazy)
ptpr_simulations_prcrazy_long <- gather(ptpr_simulations_prcrazy, key = "time", value = "value", -PTNUM, -DyadID)
ptpr_simulations_prcrazy_long$time <- as.numeric(gsub("t","", ptpr_simulations_ptcrazy_long$time))
pdf("~/Download/sep2019_ptpr_time2baseline_prcrazy.pdf", width = 8, height = 5)
ptnum = unique(ptpr_simulations_prcrazy_long$PTNUM)
for(i in 1:98) {
  tmp_df <- dplyr::filter(ptpr_simulations_prcrazy_long, PTNUM == ptnum[[i]])
  tmp_df$time <- as.numeric(gsub("t", "", tmp_df$time)) 
  tmp_df$DyadID <- factor(tmp_df$DyadID, levels = c("0", "1"), labels = c("Partner", "Patient"))
  a <- ggplot(tmp_df, aes(x =time,  y = value, color = DyadID)) + geom_line() + geom_point()+ ggtitle(ptnum[i]) 
  print(a)
}
dev.off()

ptpr_sim_ptcrazy = readMat("~/Downloads/sep2019_pt_crazy.mat")
pt_sim_ptcrazy = as.data.frame(ptpr_sim_ptcrazy$pt)
pr_sim_ptcrazy = as.data.frame(ptpr_sim_ptcrazy$pr)
colnames(pt_sim_ptcrazy) = c(paste0("t",1:800), "PTNUM")
pt_sim_ptcrazy$DyadID <- 1
colnames(pr_sim_ptcrazy) = c(paste0("t",1:800), "PTNUM")
pr_sim_ptcrazy$DyadID <- 0
ptpr_simulations_ptcrazy <- bind_rows(pt_sim_ptcrazy, pr_sim_ptcrazy)
ptpr_simulations_ptcrazy_long <- gather(ptpr_simulations_ptcrazy, key = "time", value = "value", -PTNUM, -DyadID)
ptpr_simulations_ptcrazy_long$time <- as.numeric(gsub("t","", ptpr_simulations_ptcrazy_long$time))
pdf("~/Desktop/sep2019_ptpr_time2baseline_ptcrazy.pdf", width = 8, height = 5)
ptnum = unique(ptpr_simulations_ptcrazy_long$PTNUM)
for(i in 1:98) {
  tmp_df <- dplyr::filter(ptpr_simulations_ptcrazy_long, PTNUM == ptnum[[i]])
  tmp_df$time <- as.numeric(gsub("t", "", tmp_df$time)) 
  tmp_df$DyadID <- factor(tmp_df$DyadID, levels = c("0", "1"), labels = c("Partner", "Patient"))
  a <- ggplot(tmp_df, aes(x = time, y = value, color = DyadID)) + geom_point() + geom_line() + ggtitle(ptnum[i])
  print(a)
}
dev.off()

ptpr_simulations_prcrazy_long$p_crazy <- "PR"
ptpr_simulations_ptcrazy_long$p_crazy <- "PT"
ptpr_total <- bind_rows(ptpr_simulations_prcrazy_long, ptpr_simulations_ptcrazy_long)
ptpr_total <- dplyr::mutate(ptpr_total, baseline_liberal = if_else(abs(value) < 50, 1, 0), baseline_conservative = if_else(abs(value) < .5, 1, 0))
ptpr_total_summarized_liberal <- dplyr::filter(ptpr_total, baseline_liberal == 1) %>% group_by(PTNUM, DyadID, p_crazy) %>% summarise(time_minimum = min(time)) %>% ungroup() 
ptpr_total_summarized_conservative <- dplyr::filter(ptpr_total, baseline_conservative == 1) %>% group_by(PTNUM, DyadID, p_crazy) %>% summarise(time_minimum = min(time)) %>% ungroup() 

ptpr_summarize <- dplyr::filter(ptpr_total_summarized_conservative, (DyadID == 0 & p_crazy == "PR") | (DyadID == 1 & p_crazy == "PT"))
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}
ptpr_summarize <- dplyr::mutate(ptpr_summarize, time_minimum_z = scale_this(time_minimum))
ptpr_wide <- dplyr::select(ptpr_total_summarized_conservative, PTNUM, DyadID,p_crazy, time_minimum) %>% mutate(DyadID = paste0(DyadID, p_crazy)) %>% dplyr::select(-p_crazy) %>% mutate(DyadID = case_when(DyadID == "1PR"~ "time_min_prcrazy_pt",
                                                                                                                                                                 DyadID == "1PT"~ "time_min_ptcrazy_pt",
                                                                                                                                                                 DyadID == "0PR"~ "time_min_prcrazy_pr",
                                                                                                                                                                 DyadID == "0PT"~ "time_min_ptcrazy_pr")) %>% spread(key = DyadID, value  = time_minimum)  
ptpr_wide <- ptpr_wide%>% dplyr::mutate_at(c("time_min_prcrazy_pt", "time_min_prcrazy_pr", "time_min_ptcrazy_pt", "time_min_ptcrazy_pr"), log)
dat <- left_join(params_df, ptpr_wide)
hmap <- function(df, npar) {
  rawcor_df <- matrix(ncol = npar, nrow = npar) 
  for(i in 1:npar) {
    for (j in 1:npar) {
      tmp_dv = names(df)[[i]]
      tmp_iv = names(df)[[j]]
      tmp_tribble <- tribble(
        ~v1, ~v2,
        tmp_dv, tmp_iv)
      print(tmp_iv)
      print(tmp_dv)
      if (i != j) {
        tmp_rawcor = cor(df[[tmp_iv]], df[[tmp_dv]], use = "complete.obs")
        rawcor_df[[i,j]] = tmp_rawcor
        
      } else {
        tmp_rawcor = 1
        rawcor_df[[i, j]] = tmp_rawcor
      }
    }
  }
  rawcor_hmap <- plot_ly(x= names(df), y = names(df), z =rawcor_df, type = "heatmap")
  
  return(list(rawcor = rawcor_hmap, rawcor_df = rawcor_df))
}
forheat <- dplyr::select(dat, scpt, ccpt, scpr, ccpr, starts_with("pd"), starts_with("iip"), starts_with("na_"), starts_with("pa_"), starts_with("CON_"), starts_with("AFF_"), starts_with("anx_"), starts_with("sec_"), starts_with("nasty"), starts_with("time_min")) %>% dplyr::filter_at(vars(starts_with("time")), all_vars(!is.na(.)))
heat <- heatmap(forheat,length(names(forheat)))

## 1. Do ppl that diverge cluster differently
dat <- mutate(dat, converge = if_else(is.na(time_min_ptcrazy_pt) | is.na(time_min_prcrazy_pr) | is.na(time_min_prcrazy_pt) | is.na(time_min_ptcrazy_pr), "Diverge", "Converge"))
out <- clPairs(dplyr::select(dat, scpt, scpr, ccpr, ccpt), dat$converge) # so you either see the most extreme cases or cases with very little cross coupling to have divergent dynamics
#
## 3. Time 2 Baseline 
summary(rlm(iip_elevation_patient ~ time_min_ptcrazy_pt, dat))
summary(rlm(iip_elevation_patient ~ time_min_prcrazy_pr, dat))
summary(rlm(iip_elevation_patient ~ time_min_prcrazy_pt, dat))
summary(rlm(iip_elevation_patient ~ time_min_ptcrazy_pr, dat)) 

summary(rlm(iip_elevation_partner ~ time_min_ptcrazy_pt, dat)) #2.31
summary(rlm(iip_elevation_partner ~ time_min_prcrazy_pr, dat)) # 2.18
summary(rlm(iip_elevation_partner ~ time_min_prcrazy_pt, dat)) # 1.96
summary(rlm(iip_elevation_partner ~ time_min_ptcrazy_pr, dat))
# In summary, little support for time to baseline being associated with iip elevation patient, one of the main things we discuss in the paper

summary(rlm(na_post_patient ~ time_min_ptcrazy_pt + na_pre_patient, dat))
summary(rlm(na_post_patient ~ time_min_prcrazy_pr + na_pre_patient, dat))
summary(rlm(na_post_patient ~ time_min_prcrazy_pt + na_pre_patient, dat))
summary(rlm(na_post_patient ~ time_min_ptcrazy_pr + na_pre_patient, dat)) 

summary(rlm(na_post_partner ~ time_min_ptcrazy_pt + na_pre_partner, dat)) 
summary(rlm(na_post_partner ~ time_min_prcrazy_pr  + na_pre_partner, dat))
summary(rlm(na_post_partner ~ time_min_prcrazy_pt + na_pre_partner, dat)) 
summary(rlm(na_post_partner ~ time_min_ptcrazy_pr  + na_pre_partner, dat))

summary(rlm(AFF_post_patient ~ time_min_ptcrazy_pt + AFF_pre_patient, dat))
summary(rlm(AFF_post_patient ~ time_min_prcrazy_pr + AFF_pre_patient, dat))
summary(rlm(AFF_post_patient ~ time_min_prcrazy_pt + AFF_pre_patient, dat))
summary(rlm(AFF_post_patient ~ time_min_ptcrazy_pr + AFF_pre_patient, dat)) 

summary(rlm(AFF_post_partner ~ time_min_ptcrazy_pt + AFF_post_patient, dat)) 
summary(rlm(AFF_post_partner ~ time_min_prcrazy_pr  + AFF_post_patient, dat))
summary(rlm(AFF_post_partner ~ time_min_prcrazy_pt + AFF_post_patient, dat)) 
summary(rlm(AFF_post_partner ~ time_min_ptcrazy_pr  + AFF_post_patient, dat))

# no evidence that this is an important predictor of post-interaction indices of discord

# Test for other potential personality variables
summary(rlm(iip_agency_patient ~ time_min_ptcrazy_pt, dat))
summary(rlm(iip_agency_patient ~ time_min_prcrazy_pr, dat))
summary(rlm(iip_agency_patient ~ time_min_prcrazy_pt, dat))
summary(rlm(iip_agency_patient ~ time_min_ptcrazy_pr, dat)) 

summary(rlm(iip_agency_partner ~ time_min_ptcrazy_pt, dat)) # 3.46 same for patient
p # 2.49 -- partners that are less able to come back to their own basleine report having fewer problems of agency (i.e., if you're too domineering then you tend to be able to regulate more quickly after excursion, as the partner)
summary(rlm(iip_agency_partner ~ time_min_prcrazy_pt, dat)) 
summary(rlm(iip_agency_partner ~ time_min_ptcrazy_pr, dat))

summary(rlm(iip_communion_patient ~ time_min_ptcrazy_pt, dat))
summary(rlm(iip_communion_patient ~ time_min_prcrazy_pr, dat))
summary(rlm(iip_communion_patient ~ time_min_prcrazy_pt, dat))
summary(rlm(iip_communion_patient ~ time_min_ptcrazy_pr, dat)) 

summary(rlm(iip_communion_partner ~ time_min_ptcrazy_pt, dat)) 
summary(rlm(iip_communion_partner ~ time_min_prcrazy_pr, dat)) 
summary(rlm(iip_communion_partner ~ time_min_prcrazy_pt, dat)) 
summary(rlm(iip_communion_partner ~ time_min_ptcrazy_pr, dat))

das_bu <- read.csv("~/Downloads/das_bu.csv")
das_dat <- left_join(dat, dplyr::select(das_bu, PTNUM, DAS_BL_Sat.1, DASSatMEAN12.1, DAS_BL_Sat.0, DASSatMEAN12.0, ANYBRK_CPL.0, ANYBRK_CPL.1,PTNUM))
summary(rlm(DAS_BL_Sat.1 ~ time_min_ptcrazy_pt, das_dat)) #1.92, if you as a patient took longer to come to your baseline after the conflict, you tended to be less satisfied with the relationship
summary(rlm(DAS_BL_Sat.1 ~ time_min_prcrazy_pr, das_dat))
summary(rlm(DAS_BL_Sat.1 ~ time_min_prcrazy_pt, das_dat))
summary(rlm(DAS_BL_Sat.1 ~ time_min_ptcrazy_pr, das_dat)) # 1.99, if you as a partner tend to take longer to re-regulate after the patient's excursion, you tend to be less satisfied with the relationship at baseline 

summary(rlm(DAS_BL_Sat.0 ~ time_min_ptcrazy_pt, das_dat)) 
summary(rlm(DAS_BL_Sat.0 ~ time_min_prcrazy_pr, das_dat)) 
summary(rlm(DAS_BL_Sat.0 ~ time_min_prcrazy_pt, das_dat)) 
summary(rlm(DAS_BL_Sat.0 ~ time_min_ptcrazy_pr, das_dat))

summary(rlm(DASSatMEAN12.1 ~ time_min_ptcrazy_pt, das_dat))
summary(rlm(DASSatMEAN12.1 ~ time_min_prcrazy_pr, das_dat))
summary(rlm(DASSatMEAN12.1 ~ time_min_prcrazy_pt, das_dat))
summary(rlm(DASSatMEAN12.1 ~ time_min_ptcrazy_pr, das_dat)) 

summary(rlm(DASSatMEAN12.0 ~ time_min_ptcrazy_pt, das_dat)) 
summary(rlm(DASSatMEAN12.0 ~ time_min_prcrazy_pr, das_dat)) 
summary(rlm(DASSatMEAN12.0 ~ time_min_prcrazy_pt, das_dat)) 
summary(rlm(DASSatMEAN12.0 ~ time_min_ptcrazy_pr, das_dat))

summary(rlm(DASSatMEAN12.1 ~ time_min_ptcrazy_pt, das_dat))
summary(rlm(DASSatMEAN12.1 ~ time_min_prcrazy_pr, das_dat))
summary(rlm(DASSatMEAN12.1 ~ time_min_prcrazy_pt, das_dat))
summary(rlm(DASSatMEAN12.1 ~ time_min_ptcrazy_pr, das_dat)) 

summary(rlm(DASSatMEAN12.0 ~ time_min_ptcrazy_pt, das_dat)) 
summary(rlm(DASSatMEAN12.0 ~ time_min_prcrazy_pr, das_dat)) 
summary(rlm(DASSatMEAN12.0 ~ time_min_prcrazy_pt, das_dat)) 
summary(rlm(DASSatMEAN12.0 ~ time_min_ptcrazy_pr, das_dat))

summary(rlm(time_min_ptcrazy_pt ~ ANYBRK_CPL.1, das_dat))
summary(rlm(time_min_prcrazy_pr~ ANYBRK_CPL.1, das_dat))
summary(rlm(time_min_prcrazy_pt~ ANYBRK_CPL.1, das_dat))
summary(rlm(time_min_ptcrazy_pr~ ANYBRK_CPL.1, das_dat)) 

summary(rlm(time_min_ptcrazy_pt~ ANYBRK_CPL.0, das_dat)) 
summary(rlm(time_min_prcrazy_pr~ ANYBRK_CPL.0, das_dat)) 
summary(rlm(time_min_prcrazy_pt~ ANYBRK_CPL.0, das_dat)) 
summary(rlm(time_min_ptcrazy_pr~ ANYBRK_CPL.0, das_dat))
# no evidence of time to baseline being predicted by whether there was a break in the relationship 


summary(rlm(scpt ~ time_min_ptcrazy_pt, dat))
summary(rlm(scpt ~ time_min_prcrazy_pr, dat))
summary(rlm(scpt ~ time_min_prcrazy_pt, dat))
summary(rlm(scpt ~ time_min_ptcrazy_pr, dat)) 

summary(rlm(scpr ~ time_min_ptcrazy_pt, dat)) 
summary(rlm(scpr ~ time_min_prcrazy_pr, dat)) #-3.28p
summary(rlm(scpr ~ time_min_prcrazy_pt, dat)) #1.92 negative associatoin 
summary(rlm(scpr ~ time_min_ptcrazy_pr, dat))

summary(rlm(ccpt ~ time_min_ptcrazy_pt, dat))
summary(rlm(ccpt ~ time_min_prcrazy_pr, dat))
summary(rlm(ccpt ~ time_min_prcrazy_pt, dat))
summary(rlm(ccpt ~ time_min_ptcrazy_pr, dat)) 

summary(rlm(ccpr ~ time_min_ptcrazy_pt, dat)) 
summary(rlm(ccpr ~ time_min_prcrazy_pr, dat)) 
summary(rlm(ccpr ~ time_min_prcrazy_pt, dat)) 
summary(rlm(ccpr ~ time_min_ptcrazy_pr, dat))

