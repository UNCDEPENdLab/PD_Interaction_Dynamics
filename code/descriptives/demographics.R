#examining how long couples were together, how many cohabitated, how many are same sex couples
library(tidyverse)
oralhx <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/ORALHX.csv")
datatofit <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv")
datatofit <- dplyr::filter(datatofit, PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104, m1_R2 > .98) # just get rid of all of the people that we had data on but were cut
load("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/couples_baseline_clinical_27Jul2017.RData")
#datatofit <- dplyr::filter(datatofit, PTNUM != 8000, PTNUM != 8001, PTNUM != 8021, PTNUM != 8104)
ids <- datatofit$PTNUM
baseline_clin_ids <- couples_baseline_clin$PTNUM # for some reason doesn't have 8048
oralhx_110 <- dplyr::filter(oralhx, PTNUM %in% ids)
oralhx_ids <- oralhx_110$PTNUM
couples_baseline_clin_109 <- dplyr::filter(couples_baseline_clin, PTNUM %in% oralhx_ids)
oralhx_110 <- mutate(oralhx_110, nummonths = 12*(ifelse(is.na(Dating_Yrs), 0, Dating_Yrs)) + ifelse(is.na(Dating_Mths),0, Dating_Mths))
mean(oralhx_110$nummonths)
sd(oralhx_110$nummonths)
livetogether_logical <- (oralhx_110$LiveTogether==1)
couples_baseline_clin_109 <- dplyr::filter(couples_baseline_clin, PTNUM %in% ids)
female_logical <- (couples_baseline_clin_109$p_sex=="female")
length(female_logical[female_logical == TRUE])/length(female_logical)
length(female_logical[female_logical == TRUE])
male_logical <- (couples_baseline_clin_109$p_sex=="male  ")
length(male_logical[male_logical == TRUE])
mean(couples_baseline_clin_109$p_age)
sd(couples_baseline_clin_109$p_age)
race <-table(couples_baseline_clin_109$race)
library(haven)
biq <- read_spss("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/code/CombineSelfReports/INTAKE_20170223/BIQ.sav")
load("/Users/alisonmarie526/Downloads/biq.RData")
biq <- mutate(biq, PTNUM = as.integer(as.character(PTNUM)), DyadID = as.integer(as.character(DyadID)), UsrID = as.integer(as.character(UsrID)), mth = as.integer(as.character(mth)), ScrnID =as.integer(as.character(ScrnID)))
biq <- as.data.frame(biq)

biq_126 <- dplyr::filter(biq, PTNUM %in% ids)
View(table(biq_126$Identity))
couples_baseline_clin_wide_109 <- dplyr::filter(couples_clin_wide, PTNUM %in% ids)
gay_logical <- (couples_baseline_clin_wide_109$p_female_0 == couples_baseline_clin_wide_109$p_female_1)
length(gay_logical[gay_logical == TRUE])
sex_sexuality <- dplyr::select(couples_baseline_clin_wide_109, PTNUM, p_female_0, p_female_1)
sex_sexuality <- dplyr::mutate(sex_sexuality, gay = if_else(p_female_0 == p_female_1, TRUE, FALSE))
sex_sexuality <- dplyr::mutate(sex_sexuality, lesbian = if_else(gay & p_female_0==1, TRUE, FALSE))
gayids <- dplyr::filter(couples_baseline_clin_wide_109, p_female_0 == p_female_1) %>% dplyr::select(PTNUM) %>% as.vector()


datatofit_sccc <- dplyr::select(datatofit, PTNUM, scpt, ccpt, scpr, ccpr) %>%
  dplyr::filter_all(all_vars(!is.na(.))) %>% 
  gather(key = "coupling_param", "coupling_value", -PTNUM) %>%
  separate(coupling_param, into = c("coupling_param", "ptpr"), sep = -3) %>% 
  spread(key = "coupling_param", value = "coupling_value") %>% 
  mutate(DyadID = as.integer(dplyr::recode(ptpr, "pt" = 1, "pr" = 0))) %>% dplyr::select(-ptpr)


datatofit_v_sccc <- dplyr::select(datatofit, PTNUM, v_scpt, v_ccpt, v_scpr, v_ccpr) %>%
  dplyr::filter_all(all_vars(!is.na(.))) %>% 
  gather(key = "coupling_param", "coupling_value", -PTNUM) %>%
  separate(coupling_param, into = c("coupling_param", "ptpr"), sep = -3) %>% 
  spread(key = "coupling_param", value = "coupling_value") %>% 
  mutate(DyadID = as.integer(dplyr::recode(ptpr, "pt" = 1, "pr" = 0))) %>% dplyr::select(-ptpr)

datatofit_personality <- dplyr::select(datatofit, PTNUM, ends_with("patient"), ends_with("partner")) %>%
  gather(key = "personality_param", value = "personality_value", -PTNUM) %>%
  separate(personality_param, into = c("personality_param", "ptpr"), sep = "_pa", extra = "merge") %>% spread(key = "personality_param", value = "personality_value") %>%  
  mutate(DyadID = as.integer(dplyr::recode(ptpr, "tient" = 1, "rtner" = 0))) %>%dplyr::select(-ptpr) 

datatofit$censoring_pt <- 0
datatofit$cenosring_pr = 0
#calculated using matlab script, in milliseconds
datatofit_testing <- mutate(datatofit, censoring_pt = case_when(PTNUM == 8152~110,
                                                                PTNUM == 8133 ~ 483, 
                                                                PTNUM == 8030 ~331,
                                                                PTNUM == 8032 ~ 896, 
                                                                PTNUM == 8034  ~ 84,
                                                                PTNUM == 8060 ~372,
                                                                PTNUM == 8084 ~ 21,
                                                                PTNUM == 8106 ~ 910,
                                                                PTNUM == 8126  ~ 162,
                                                                PTNUM == 8127 ~ 850,
                                                                TRUE ~ 0),
                            censoring_pr = case_when(PTNUM == 8034 ~ 31,
                                                     PTNUM == 8039 ~ 26,
                                                     PTNUM == 8048 ~ 75,
                                                     PTNUM == 8064 ~ 108,
                                                     PTNUM == 8114 ~ 57,
                                                     PTNUM == 8134 ~ 26,
                                                     PTNUM == 8144 ~ 325,
                                                     TRUE ~ 0))

censoring_df <- dplyr::select(datatofit_testing, PTNUM, censoring_pt, censoring_pr) %>% tidyr::gather(key = key, value = value, -PTNUM)
mean(censoring_df$value)
median(censoring_df$value)
range(censoring_df$value)
group_by(censoring_df, key) %>% summarise(mean(value))
group_by(censoring_df, key) %>% summarise(median(value))
ptdf <- dplyr::filter(censoring_df, key == "censoring_pt")
prdf <- dplyr::filter(censoring_df, key == "censoring_pr")
range(ptdf$value)
range(prdf$value)
labelDataset <- function(data) {
  correctLabel <- function(x) {
    
    if(!is.null(attributes(x)$labels)) {
      class(attributes(x)$labels) <- typeof(x)
    }
    return(x)
  }
  for(i in colnames(data)) {
    data[, i] <- correctLabel(data[, i])
  }
  return(data)
}

biq <- labelDataset(biq)

biq <- biq%>% do(data.frame(PTNUM = as.integer(.$PTNUM), DyadID = as.integer(.$DyadID), Identity = as.character(.$Identity)))
alldatajoined <- plyr::join_all(list(biq, couples_baseline_clin, datatofit_personality, oralhx, datatofit_sccc, datatofit_v_sccc),type = "full", by = c("PTNUM", "DyadID"))
alldatajoined_char<- mutate(alldatajoined, p_sex = as.character(p_sex),
                       p_age = as.character(p_age),
                       race = as.character(race),
                       Identity = as.character(Identity))
alldatajoined <- dplyr::mutate(alldatajoined, nummonths =  12*(ifelse(is.na(Dating_Yrs), 0, Dating_Yrs)) + ifelse(is.na(Dating_Mths),0, Dating_Mths))
alldatajoined_noOralhx <- plyr::join_all(list(biq, couples_baseline_clin, datatofit_personality, datatofit_sccc, datatofit_v_sccc),type = "full", by = c("PTNUM", "DyadID"))
psex_alldatajoined_wide <-   dplyr::filter(alldatajoined_noOralhx, !is.na(sc), !is.na(cc), !is.na(p_female)) %>% dplyr::select(PTNUM, DyadID, p_female) %>%
  mutate(sex_id = paste0("p_female", DyadID)) %>% dplyr::select(-DyadID) %>%spread(key = "sex_id", value = "p_female")
sex_participants = dplyr::filter(alldatajoined, !is.na(sc), !is.na(cc)) %>% mutate(p_sex = as.character(p_sex)) %>% summarise(males = length(which(p_sex == "male  ")), females = length(which(p_sex == "female")))
livetogether_participants = dplyr::filter(alldatajoined, !is.na(sc), !is.na(cc))%>% summarise(cohabitate = length(which(LiveTogether == 1)), notcohabitate = length(which(p_sex == "female")))
nummonths = dplyr::filter(alldatajoined, !is.na(sc), !is.na(cc), nummonths > 2) %>% summarise(min(nummonths), max(nummonths), mean(nummonths), sd(nummonths))
gay_df = dplyr::filter(alldatajoined, !is.na(sc), !is.na(cc),!is.na(p_sex)) 
View(table(gay_df$Identity))
gay_couples <-  summarise(psex_alldatajoined_wide, gay = length(which(p_female1 == p_female0)), hetero = length(which(p_female1 != p_female0)))

alldatajoined_partofstudy <- dplyr::filter(alldatajoined, !is.na(sc), !is.na(cc), !is.na(p_sex))
View(table(alldatajoined_partofstudy$race))

id_partofstudy <- data.frame(PTNUM = as.vector(unique(alldatajoined_partofstudy$PTNUM)))
write.csv(id_partofstudy,"../ids.csv")


fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
load('/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/couples_baseline_clinical_27Jul2017.RData') 
demographics <- dplyr::select(couples_clin_wide, starts_with("p_female"), starts_with("p_sex"), PTNUM)
fulldata_demographics <- full_join(fulldata, demographics)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata_demographics, m1_R2 > .98)  %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, scpt, scpr, PTNUM) %>% dplyr::filter(!is.na(scpt), !is.na(scpr)) %>% gather(key = "sc", value = "self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, sc,c("sc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
demographics_long <- dplyr::select(couples_baseline_clin, DyadID,PTNUM, starts_with("p_female"), starts_with("p_race")) %>% mutate(DyadID = paste0(PTNUM, DyadID)) %>% mutate(DyadID = as.integer(DyadID))%>% dplyr::select(-PTNUM)
fulldata_demographics_noNegIntOutliers_long <- inner_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(sc))
library(robustlmm)
p_sc_sex_out <- rlmer(self_coupling_value ~ p_female + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- dplyr::mutate(fulldata_demographics_noNegIntOutliers_long, p_female = p_female- mean(p_female, na.rm = TRUE))
afex_sc_sex_out <- afex::mixed(self_coupling_value ~ p_female + (1|PTNUM),forafex)

afex_sc_sex_df <- afex_sc_sex_out$anova_table$`den Df`[[1]]
fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpr, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, ccpt, ccpr, PTNUM) %>% dplyr::filter(!is.na(ccpt), !is.na(ccpr)) %>% gather(key = "cc", value = "cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, cc,c("cc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(cc))
p_cc_sex_out <- rlmer(cross_coupling_value ~ p_female + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- dplyr::mutate(fulldata_demographics_noNegIntOutliers_long, p_female = p_female- mean(p_female, na.rm = TRUE))
afex_cc_sex_out <- afex::mixed(cross_coupling_value ~ p_female + (1|PTNUM),forafex)

afex_cc_sex_df <- afex_cc_sex_out$anova_table$`den Df`[[1]]

fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98)  %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_scpt, v_scpr, PTNUM) %>% dplyr::filter(!is.na(v_scpt), !is.na(v_scpr)) %>% gather(key = "v_sc", value = "v_self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_sc,c("v_sc", "ptpr"), sep = -3)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_sc))
p_v_sc_sex_out <- rlmer(v_self_coupling_value ~  p_female + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- dplyr::mutate(fulldata_demographics_noNegIntOutliers_long, p_female = p_female- mean(p_female, na.rm = TRUE))
afex_v_sc_sex_out <- afex::mixed(v_self_coupling_value ~ p_female + (1|PTNUM),forafex)
afex_v_sc_sex_df <- afex_v_sc_sex_out$anova_table$`den Df`[[1]]


fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98)%>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_ccpt, v_ccpr, PTNUM) %>% dplyr::filter(!is.na(v_ccpt), !is.na(v_ccpr)) %>% gather(key = "v_cc", value = "v_cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_cc,c("v_cc", "ptpr"), sep = -3)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_cc))
forafex <- dplyr::mutate(fulldata_demographics_noNegIntOutliers_long, p_female = p_female- mean(p_female, na.rm = TRUE))

p_v_cc_sex_out <- rlmer(v_cross_coupling_value ~ p_female + (1|PTNUM), forafex)

afex_v_cc_sex_out <- afex::mixed(v_cross_coupling_value ~ p_female + (1|PTNUM),fulldata_demographics_noNegIntOutliers_long)
afex_v_cc_sex_df <- afex_v_cc_sex_out$anova_table$`den Df`[[1]]


pf(2.474^2, 1, afex_sc_sex_df, lower.tail = FALSE)
pf((-1.782)^2, 1, afex_cc_sex_df, lower.tail = FALSE)
pf(0.188^2, 1, afex_v_sc_sex_df,lower.tail = FALSE)
pf((0.160)^2, 1, afex_v_cc_sex_df, lower.tail = FALSE)


########## now calculating for race


fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
load('/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/couples_baseline_clinical_27Jul2017.RData') 
demographics <- dplyr::select(couples_clin_wide, starts_with("p_female"), starts_with("p_sex"), PTNUM)
fulldata_demographics <- full_join(fulldata, demographics)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata_demographics, m1_R2 > .98)%>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, scpt, scpr, PTNUM) %>% dplyr::filter(!is.na(scpt), !is.na(scpr)) %>% gather(key = "sc", value = "self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, sc,c("sc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
demographics_long <- dplyr::select(couples_baseline_clin, DyadID,PTNUM, starts_with("race")) %>% mutate(DyadID = paste0(PTNUM, DyadID)) %>% mutate(DyadID = as.integer(DyadID))%>% dplyr::select(-PTNUM) %>% dplyr::mutate(race = as.integer(race))
demographics_long <- dplyr::mutate(demographics_long, racecc = if_else(race == 10, 1, -1))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(sc))
library(robustlmm)
p_sc_race_out <- rlmer(self_coupling_value ~ racecc + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- fulldata_demographics_noNegIntOutliers_long#, p_female = p_female- mean(p_female, na.rm = TRUE))
afex_sc_race_out <- afex::mixed(self_coupling_value ~ racecc + (1|PTNUM),forafex)

afex_sc_race_df <- afex_sc_race_out$anova_table$`den Df`[[1]]
fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpr, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, m1_R2 > .98)%>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, ccpt, ccpr, PTNUM) %>% dplyr::filter(!is.na(ccpt), !is.na(ccpr)) %>% gather(key = "cc", value = "cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, cc,c("cc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(cc))
p_cc_race_out <- rlmer(cross_coupling_value ~ racecc + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <-fulldata_demographics_noNegIntOutliers_long
afex_cc_race_out <- afex::mixed(cross_coupling_value ~ racecc + (1|PTNUM),forafex)

afex_cc_race_df <- afex_cc_race_out$anova_table$`den Df`[[1]]

fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98)%>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_scpt, v_scpr, PTNUM) %>% dplyr::filter(!is.na(v_scpt), !is.na(v_scpr)) %>% gather(key = "v_sc", value = "v_self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_sc,c("v_sc", "ptpr"), sep = -3)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_sc))
p_v_sc_race_out <- rlmer(v_self_coupling_value ~  racecc + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- fulldata_demographics_noNegIntOutliers_long
afex_v_sc_race_out <- afex::mixed(v_self_coupling_value ~ racecc + (1|PTNUM),forafex)
afex_v_sc_race_df <- afex_v_sc_race_out$anova_table$`den Df`[[1]]


fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_ccpt, v_ccpr, PTNUM) %>% dplyr::filter(!is.na(v_ccpt), !is.na(v_ccpr)) %>% gather(key = "v_cc", value = "v_cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_cc,c("v_cc", "ptpr"), sep = -3)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_cc))
p_v_cc_race_out <- rlmer(v_cross_coupling_value ~ racecc + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)

 forafex <- fulldata_demographics_noNegIntOutliers_long#, p_female = p_female- mean(p_female, na.rm = TRUE))
 afex_v_cc_race_out <- afex::mixed(v_cross_coupling_value ~ racecc + (1|PTNUM),fulldata_demographics_noNegIntOutliers_long)
 afex_v_cc_race_df <- afex_v_cc_race_out$anova_table$`den Df`[[1]]
# 
# 
pf((-3.372)^2, 1, afex_sc_race_df, lower.tail = FALSE)
pf(0.958^2, 1, afex_cc_race_df, lower.tail = FALSE)
pf((-1.122)^2, 1, afex_v_sc_race_df,lower.tail = FALSE)
pf((-0.562)^2, 1, afex_v_cc_race_df, lower.tail = FALSE)
# 

#### now testing age

fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
load('/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/couples_baseline_clinical_27Jul2017.RData') 
demographics <- dplyr::select(couples_clin_wide, starts_with("p_female"), starts_with("p_sex"), PTNUM)
fulldata_demographics <- full_join(fulldata, demographics)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata_demographics, m1_R2 > .98)  %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, scpt, scpr, PTNUM) %>% dplyr::filter(!is.na(scpt), !is.na(scpr)) %>% gather(key = "sc", value = "self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, sc,c("sc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
demographics_long <- dplyr::select(couples_baseline_clin, DyadID,PTNUM, starts_with("p_age")) %>% mutate(DyadID = paste0(PTNUM, DyadID)) %>% mutate(DyadID = as.integer(DyadID))%>% dplyr::select(-PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(sc))
library(robustlmm)
p_sc_age_out <- rlmer(self_coupling_value ~ p_age + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- dplyr::mutate(fulldata_demographics_noNegIntOutliers_long, p_age = p_age- mean(p_age, na.rm = TRUE))
afex_sc_age_out <- afex::mixed(self_coupling_value ~ p_age + (1|PTNUM),forafex)

afex_sc_age_df <- afex_sc_age_out$anova_table$`den Df`[[1]]
fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpr, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, m1_R2 > .98)  %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, ccpt, ccpr, PTNUM) %>% dplyr::filter(!is.na(ccpt), !is.na(ccpr)) %>% gather(key = "cc", value = "cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, cc,c("cc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(cc))
p_cc_age_out <- rlmer(cross_coupling_value ~ p_age + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <-dplyr::mutate(fulldata_demographics_noNegIntOutliers_long, p_age = p_age- mean(p_age, na.rm = TRUE))
afex_cc_age_out <- afex::mixed(cross_coupling_value ~ p_age + (1|PTNUM),forafex)

afex_cc_age_df <- afex_cc_age_out$anova_table$`den Df`[[1]]

fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_scpt, v_scpr, PTNUM) %>% dplyr::filter(!is.na(v_scpt), !is.na(v_scpr)) %>% gather(key = "v_sc", value = "v_self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_sc,c("v_sc", "ptpr"), sep = -3)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_sc))
p_v_sc_age_out <- rlmer(v_self_coupling_value ~  p_age + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- dplyr::mutate(fulldata_demographics_noNegIntOutliers_long,p_age = p_age- mean(p_age, na.rm = TRUE))
afex_v_sc_age_out <- afex::mixed(v_self_coupling_value ~ p_age + (1|PTNUM),forafex)
afex_v_sc_age_df <- afex_v_sc_age_out$anova_table$`den Df`[[1]]


fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_ccpt, v_ccpr, PTNUM) %>% dplyr::filter(!is.na(v_ccpt), !is.na(v_ccpr)) %>% gather(key = "v_cc", value = "v_cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_cc,c("v_cc", "ptpr"), sep = -3)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_cc))
p_v_cc_age_out <- rlmer(v_cross_coupling_value ~ p_age + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)

forafex <- dplyr::mutate(fulldata_demographics_noNegIntOutliers_long, p_age = p_age- mean(p_age, na.rm = TRUE))
afex_v_cc_age_out <- afex::mixed(v_cross_coupling_value ~ p_age + (1|PTNUM),fulldata_demographics_noNegIntOutliers_long)
afex_v_cc_age_df <- afex_v_cc_race_out$anova_table$`den Df`[[1]]
# 
# 
pf((-2.325)^2, 1, afex_sc_age_df, lower.tail = FALSE)
pf(0.171^2, 1, afex_cc_age_df, lower.tail = FALSE)
pf((-0.378)^2, 1, afex_v_sc_age_df,lower.tail = FALSE)
pf((0.085)^2, 1, afex_v_cc_age_df, lower.tail = FALSE)

############ now testing sexuality


fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
load("/Users/alisonmarie526/Downloads/biq.RData")
biq <- mutate(biq, PTNUM = as.integer(as.character(PTNUM)), DyadID = as.integer(as.character(DyadID)), UsrID = as.integer(as.character(UsrID)), mth = as.integer(as.character(mth)), ScrnID =as.integer(as.character(ScrnID)))
biq_sexuality <- dplyr::select(biq, UsrID, mth, Identity) %>% mutate(DyadID = UsrID, Identity = as.integer(Identity)) %>% mutate(sexualitycc = if_else(Identity == 4, 1, -1))
fulldata_demographics <- full_join(fulldata, demographics)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata_demographics, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, scpt, scpr, PTNUM) %>% dplyr::filter(!is.na(scpt), !is.na(scpr)) %>% gather(key = "sc", value = "self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, sc,c("sc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
demographics_long <- biq_sexuality
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(sc))
library(robustlmm)
p_sc_sexuality_out <- rlmer(self_coupling_value ~ sexualitycc + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- fulldata_demographics_noNegIntOutliers_long #, p_age = p_age- mean(p_age, na.rm = TRUE))
afex_sc_sexuality_out <- afex::mixed(self_coupling_value ~ sexualitycc + (1|PTNUM),forafex)

afex_sc_sexuality_df <- afex_sc_sexuality_out$anova_table$`den Df`[[1]]



fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpr, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, ccpt, ccpr, PTNUM) %>% dplyr::filter(!is.na(ccpt), !is.na(ccpr)) %>% gather(key = "cc", value = "cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, cc,c("cc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(cc))
p_cc_sexuality_out <- rlmer(cross_coupling_value ~ sexualitycc + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <-fulldata_demographics_noNegIntOutliers_long #, p_age = p_age- mean(p_age, na.rm = TRUE))
afex_cc_sexuality_out <- afex::mixed(cross_coupling_value ~ sexualitycc + (1|PTNUM),forafex)

afex_cc_sexuality_df <- afex_cc_sexuality_out$anova_table$`den Df`[[1]]

fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98,m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_scpt, v_scpr, PTNUM) %>% dplyr::filter(!is.na(v_scpt), !is.na(v_scpr)) %>% gather(key = "v_sc", value = "v_self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_sc,c("v_sc", "ptpr"), sep = -3)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_sc))
p_v_sc_sexuality_out <- rlmer(v_self_coupling_value ~  sexualitycc + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- fulldata_demographics_noNegIntOutliers_long #,p_age = p_age- mean(p_age, na.rm = TRUE))
afex_v_sc_sexuality_out <- afex::mixed(v_self_coupling_value ~ sexualitycc + (1|PTNUM),forafex)
afex_v_sc_sexuality_df <- afex_v_sc_sexuality_out$anova_table$`den Df`[[1]]


fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_ccpt, v_ccpr, PTNUM) %>% dplyr::filter(!is.na(v_ccpt), !is.na(v_ccpr)) %>% gather(key = "v_cc", value = "v_cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_cc,c("v_cc", "ptpr"), sep = -3)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_cc))
p_v_cc_sexuality_out <- rlmer(v_cross_coupling_value ~ sexualitycc + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)

forafex <- fulldata_demographics_noNegIntOutliers_long #, p_age = p_age- mean(p_age, na.rm = TRUE))
afex_v_cc_sexuality_out <- afex::mixed(v_cross_coupling_value ~ sexualitycc + (1|PTNUM),fulldata_demographics_noNegIntOutliers_long)
afex_v_cc_sexuality_df <- afex_v_cc_sexuality_out$anova_table$`den Df`[[1]]
# 
# 
pf((0.123)^2, 1, afex_sc_sexuality_df, lower.tail = FALSE)
pf(0.074, 1, afex_cc_sexuality_df, lower.tail = FALSE)
pf((-0.292)^2, 1, afex_v_sc_sexuality_df,lower.tail = FALSE)
pf((8.924)^2, 1, afex_v_cc_sexuality_df, lower.tail = FALSE)



#### testing length of relationship


fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
oralhx <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/ORALHX.csv")
oralhx<- mutate(oralhx, nummonths = 12*(ifelse(is.na(Dating_Yrs), 0, Dating_Yrs)) + ifelse(is.na(Dating_Mths),0, Dating_Mths))
oralhx <- dplyr::select(oralhx,PTNUM, DyadID, nummonths) %>% dplyr::mutate(DyadID = as.integer(paste0(PTNUM, DyadID)))%>% dplyr::filter(nummonths > 1) %>% dplyr::select(-PTNUM)
fulldata_demographics <- full_join(fulldata, demographics)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata_demographics, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, scpt, scpr, PTNUM) %>% dplyr::filter(!is.na(scpt), !is.na(scpr)) %>% gather(key = "sc", value = "self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, sc,c("sc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
demographics_long <- oralhx
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(sc))
library(robustlmm)
p_sc_rellength_out <- rlmer(self_coupling_value ~ nummonths + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- mutate(fulldata_demographics_noNegIntOutliers_long, nummonths = nummonths- mean(nummonths, na.rm = TRUE))
afex_sc_rellength_out <- afex::mixed(self_coupling_value ~ nummonths + (1|PTNUM),forafex)

afex_sc_rellength_df <- afex_sc_rellength_out$anova_table$`den Df`[[1]]



fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpr, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, m1_R2 > .98)%>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, ccpt, ccpr, PTNUM) %>% dplyr::filter(!is.na(ccpt), !is.na(ccpr)) %>% gather(key = "cc", value = "cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, cc,c("cc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(cc))
p_cc_rellength_out <- rlmer(cross_coupling_value ~ nummonths + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <-mutate(fulldata_demographics_noNegIntOutliers_long, nummonths = nummonths- mean(nummonths, na.rm = TRUE))
afex_cc_rellength_out <- afex::mixed(cross_coupling_value ~ nummonths + (1|PTNUM),forafex)

afex_cc_rellength_df <- afex_cc_rellength_out$anova_table$`den Df`[[1]]

fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_scpt, v_scpr, PTNUM) %>% dplyr::filter(!is.na(v_scpt), !is.na(v_scpr)) %>% gather(key = "v_sc", value = "v_self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_sc,c("v_sc", "ptpr"), sep = -3)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_sc))
p_v_sc_rellength_out <- rlmer(v_self_coupling_value ~  nummonths + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- mutate(fulldata_demographics_noNegIntOutliers_long, nummonths = nummonths- mean(nummonths, na.rm = TRUE))
afex_v_sc_rellength_out <- afex::mixed(v_self_coupling_value ~ nummonths + (1|PTNUM),forafex)
afex_v_sc_rellength_df <- afex_v_sc_rellength_out$anova_table$`den Df`[[1]]


fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_ccpt, v_ccpr, PTNUM) %>% dplyr::filter(!is.na(v_ccpt), !is.na(v_ccpr)) %>% gather(key = "v_cc", value = "v_cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_cc,c("v_cc", "ptpr"), sep = -3)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", 1, 0)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_cc))
p_v_cc_rellength_out <- rlmer(v_cross_coupling_value ~ nummonths + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)

forafex <- mutate(fulldata_demographics_noNegIntOutliers_long, nummonths = nummonths- mean(nummonths, na.rm = TRUE))
afex_v_cc_rellength_out <- afex::mixed(v_cross_coupling_value ~ nummonths + (1|PTNUM),forafex)
afex_v_cc_rellength_df <- afex_v_cc_rellength_out$anova_table$`den Df`[[1]]
# 
# 
pf((0.942)^2, 1, afex_sc_rellength_df, lower.tail = FALSE)
pf((-0.437)^2, 1, afex_cc_rellength_df, lower.tail = FALSE)
pf((-0.615)^2, 1, afex_v_sc_rellength_df,lower.tail = FALSE)
pf((-0.777)^2, 1, afex_v_cc_rellength_df, lower.tail = FALSE)


#### RLMER for ptpr

fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, scpt, scpr, PTNUM) %>% dplyr::filter(!is.na(scpt), !is.na(scpr)) %>% gather(key = "sc", value = "self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, sc,c("sc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", .5, -.5)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(sc))
library(robustlmm)
p_sc_ptpr_out <- rlmer(self_coupling_value ~ ptprnum + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- fulldata_demographics_noNegIntOutliers_long
afex_sc_ptpr_out <- afex::mixed(self_coupling_value ~ ptprnum + (1|PTNUM),forafex)

afex_sc_ptpr_df <- afex_sc_ptpr_out$anova_table$`den Df`[[1]]



fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpr, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, ccpt, ccpr, PTNUM) %>% dplyr::filter(!is.na(ccpt), !is.na(ccpr)) %>% gather(key = "cc", value = "cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, cc,c("cc", "ptpr"), sep = 2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", .5, -.5)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(cc))
p_cc_ptpr_out <- rlmer(cross_coupling_value ~ ptprnum + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <-fulldata_demographics_noNegIntOutliers_long
afex_cc_ptpr_out <- afex::mixed(cross_coupling_value ~ ptprnum + (1|PTNUM),forafex)

afex_cc_ptpr_df <- afex_cc_ptpr_out$anova_table$`den Df`[[1]]

fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_scpt, v_scpr, PTNUM) %>% dplyr::filter(!is.na(v_scpt), !is.na(v_scpr)) %>% gather(key = "v_sc", value = "v_self_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_sc,c("v_sc", "ptpr"), sep = -2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", .5, -.5)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_sc))
p_v_sc_ptpr_out <- rlmer(v_self_coupling_value ~  ptprnum + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)
forafex <- fulldata_demographics_noNegIntOutliers_long
afex_v_sc_ptpr_out <- afex::mixed(v_self_coupling_value ~ ptprnum + (1|PTNUM),forafex)
afex_v_sc_ptpr_df <- afex_v_sc_ptpr_out$anova_table$`den Df`[[1]]


fulldata <- read.csv("../../fulldata_VAR.csv") %>% dplyr::select(scpt, ccpt, scpr, ccpt, v_scpt, v_ccpt, v_scpr, v_ccpr, PTNUM, m1_R2, v_R2)
fulldata_demographics_noNegIntOutliers <- dplyr::filter(fulldata, v_R2 > .98, m1_R2 > .98)%>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
fulldata_demographics_noNegIntOutliers_long <- dplyr::select(fulldata_demographics_noNegIntOutliers, v_ccpt, v_ccpr, PTNUM) %>% dplyr::filter(!is.na(v_ccpt), !is.na(v_ccpr)) %>% gather(key = "v_cc", value = "v_cross_coupling_value", -PTNUM) 
fulldata_demographics_noNegIntOutliers_long <- separate(fulldata_demographics_noNegIntOutliers_long, v_cc,c("v_cc", "ptpr"), sep = -2)
fulldata_demographics_noNegIntOutliers_long <- mutate(fulldata_demographics_noNegIntOutliers_long, ptprnum = if_else(ptpr == "pt", .5, -.5)) %>% mutate(DyadID = paste0(PTNUM, ptprnum)) %>% mutate(DyadID = as.integer(DyadID))
fulldata_demographics_noNegIntOutliers_long <- full_join(fulldata_demographics_noNegIntOutliers_long, demographics_long, by = "DyadID")
fulldata_demographics_noNegIntOutliers_long <- dplyr::filter(fulldata_demographics_noNegIntOutliers_long, !is.na(v_cc))
p_v_cc_ptpr_out <- rlmer(v_cross_coupling_value ~ ptprnum + (1|PTNUM), fulldata_demographics_noNegIntOutliers_long)

forafex <- fulldata_demographics_noNegIntOutliers_long
afex_v_cc_ptpr_out <- afex::mixed(v_cross_coupling_value ~ ptprnum + (1|PTNUM),forafex)
afex_v_cc_ptpr_df <- afex_v_cc_ptpr_out$anova_table$`den Df`[[1]]
# 
# 
pf(( 0.84)^2, 1, afex_sc_ptpr_df, lower.tail = FALSE)
pf((0.142)^2, 1, afex_cc_ptpr_df, lower.tail = FALSE)
pf((1.487)^2, 1, afex_v_sc_ptpr_df,lower.tail = FALSE)
pf((1.551)^2, 1, afex_v_cc_ptpr_df, lower.tail = FALSE)
