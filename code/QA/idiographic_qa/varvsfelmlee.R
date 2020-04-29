library(R.matlab)
library(lattice)
library(tidyverse)
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/sasvsVB/")
allrawmat <- readMat("paramsm9_VARCoreg.mat")
ids <- as.vector(allrawmat$ids)
allrawparamsm9 <- as.data.frame(allrawmat$rawparamsm9)
colnames(allrawparamsm9) <- c("scpt", "ccpt", "scpr", "ccpr")
allrawparamsm9$ids <- ids

allrawparamsm4 <- as.data.frame(allrawmat$rawparamsm4)
colnames(allrawparamsm4) <- c("scpt", "ccpt", "scpr", "ccpr")
allrawparamsm4$ids <- ids
dplyr::filter(allrawparamsm4, abs(scpt) < 1, abs(scpr) <1, abs(ccpt) <1,abs(ccpr) <1 ) %>%dplyr::select(scpt, ccpt, scpr, ccpr) %>%  lattice::splom()
dplyr::filter(allrawparamsm9, abs(scpt) < .5, abs(scpr) <.5, abs(ccpt) <.5,abs(ccpr) <.5 ) %>%dplyr::select(scpt, ccpt, scpr, ccpr) %>%  lattice::splom()
dplyr::select(allrawparamsm4, scpt, ccpt, scpr, ccpr) %>%  cor()
dplyr::select(allrawparamsm9, scpt, ccpt, scpr, ccpr) %>%  cor()
dplyr::filter(allrawparamsm4, abs(scpt) < 1, abs(scpr) <1, abs(ccpt) <1,abs(ccpr) <1 ) %>%dplyr::select(scpt, ccpt, scpr, ccpr) %>%  cor()
dplyr::filter(allrawparamsm9, abs(scpt) < .5, abs(scpr) <.5, abs(ccpt) <.5,abs(ccpr) <.5 ) %>%dplyr::select(scpt, ccpt, scpr, ccpr) %>% cor()
nounivariateoutliers_rawparamsm9 <- dplyr::filter(allrawparamsm9, abs(scpt) < .5, abs(scpr) <.5, abs(ccpt) <.5,abs(ccpr) <.5 ) 
nounivariateoutliers_rawparamsm4 <-dplyr::filter(allrawparamsm4, abs(scpt) < 1, abs(scpr) <1, abs(ccpt) <1,abs(ccpr) <1 )

length(nounivariateoutliers_rawparamsm4$scpt) #119
length(nounivariateoutliers_rawparamsm9$scpt) #121

sas_params <- read.csv("sas_params.csv")
sas_params_long <- dplyr::select(sas_params, ptnum, a1, a2, b1, b2) %>% mutate(PTNUM = ptnum, scpt = a1, ccpt = a2, scpr = b1,ccpr = b2) %>% gather(key = "coupling_param", value = "coupling_value_sas", -PTNUM, -ptnum, -a1, -a2, -b1, -b2) %>% dplyr::select(-ptnum, -a1, -a2, -b1, -b2)
allrawparamsm4_long <- dplyr::select(allrawparamsm4,ids, scpt, ccpt, scpr, ccpr) %>% mutate(PTNUM = ids) %>% gather(key = "coupling_param", value = "coupling_value_m4", -PTNUM, -ids) %>% dplyr::select(-ids)
allrawparamsm9_long <- dplyr::select(allrawparamsm9,ids, scpt, ccpt, scpr, ccpr) %>% mutate(PTNUM = ids) %>% gather(key = "coupling_param", value = "coupling_value_m9", -PTNUM, -ids) %>% dplyr::select(-ids)
tocor <- plyr::join_all(list(sas_params_long, allrawparamsm4_long, allrawparamsm9_long), type = "full")

cor(dplyr::select(tocor, coupling_value_sas, coupling_value_m4, coupling_value_m9))
tocor_shortened <-dplyr::filter(tocor, abs(coupling_value_m4) <1) 
cor(dplyr::select(tocor_shortened, coupling_value_sas, coupling_value_m4, coupling_value_m9))

ggplot(tocor_shortened, aes(x = coupling_value_sas, y = coupling_value_m4, color = coupling_param)) + geom_point()


sas_var_params <- read.csv("sas_params_var.csv")
sas_var_params_long <- dplyr::select(sas_var_params, ptnum, a1, a2, b1, b2) %>% mutate(PTNUM = ptnum, scpt = a1, ccpt = a2, scpr = b1,ccpr = b2) %>% gather(key = "coupling_param", value = "coupling_value_sas_var", -PTNUM, -ptnum, -a1, -a2, -b1, -b2) %>% dplyr::select(-ptnum, -a1, -a2, -b1, -b2)
tocor <- plyr::join_all(list(sas_params_long, allrawparamsm4_long, allrawparamsm9_long, sas_var_params_long), type = "full")
tocor_shortened <-dplyr::filter(tocor, abs(coupling_value_m4) <1) 
ggplot(tocor_shortened, aes(x= coupling_value_sas_var, y = coupling_value_m9, color = coupling_param)) + geom_point()
cor(dplyr::select(tocor_shortened, coupling_value_sas, coupling_value_m4, coupling_value_m9, coupling_value_sas_var))
dplyr::filter(tocor_shortened, coupling_param == "scpt") %>% dplyr::select(coupling_value_sas, coupling_value_m4, coupling_value_m9, coupling_value_sas_var) %>% cor()
dplyr::filter(tocor_shortened, coupling_param == "scpr") %>% dplyr::select(coupling_value_sas, coupling_value_m4, coupling_value_m9, coupling_value_sas_var) %>% cor()
dplyr::filter(tocor_shortened, coupling_param == "ccpt") %>% dplyr::select(coupling_value_sas, coupling_value_m4, coupling_value_m9, coupling_value_sas_var) %>% cor()
dplyr::filter(tocor_shortened, coupling_param == "ccpr") %>% dplyr::select(coupling_value_sas, coupling_value_m4, coupling_value_m9, coupling_value_sas_var) %>% cor()
dplyr::filter(tocor, coupling_param == "scpt") %>% dplyr::select(coupling_value_sas, coupling_value_m4, coupling_value_m9, coupling_value_sas_var) %>% cor()
dplyr::filter(tocor, coupling_param == "scpr") %>% dplyr::select(coupling_value_sas, coupling_value_m4, coupling_value_m9, coupling_value_sas_var) %>% cor()
dplyr::filter(tocor, coupling_param == "ccpt") %>% dplyr::select(coupling_value_sas, coupling_value_m4, coupling_value_m9, coupling_value_sas_var) %>% cor()
dplyr::filter(tocor, coupling_param == "ccpr") %>% dplyr::select(coupling_value_sas, coupling_value_m4, coupling_value_m9, coupling_value_sas_var) %>% cor()
ggplot(tocor, aes(x= coupling_value_sas_var, y = coupling_value_m9, color = coupling_param)) + geom_point()



rawparamsm1 <- readMat("feb2018_params_VARCoreg_m1.mat")
rawparamsm1_df <- as.data.frame(rawparamsm1$rawparamsm1)
colnames(rawparamsm1_df) <- c("scpt", "ccpt", "scpr", "ccpr")
rawparamsm1_df$PTNUM <- as.vector(rawparamsm1$ids)
rawparamsm1_df$ll <- as.vector(rawparamsm1$logEvidence[,1])

dplyr::filter(rawparamsm1_df, abs(scpt) < .5, abs(scpr) < .5, abs(ccpt) < .5, abs(ccpr) < .5) %>% dplyr::select(-ll, -PTNUM) %>% lattice::splom()
sas_var_params <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/sasvsVB/sas_params_var.csv")
sas_var_params <- dplyr::select(sas_var_params, ptnum, a1, a2, b1, b2) %>% dplyr::mutate(PTNUM = ptnum) %>% dplyr::select(-ptnum)
sasraw_wide <- full_join(rawparamsm1_df, sas_var_params)

sas_var_params_long <- dplyr::select(sas_var_params, PTNUM, a1, a2, b1, b2) %>% mutate(scpt = a1, ccpt = a2, scpr = b1,ccpr = b2) %>% gather(key = "coupling_param", value = "coupling_value_sas_var", -PTNUM, -a1, -a2, -b1, -b2) %>% dplyr::select(-a1, -a2, -b1, -b2)
rawparamsm1_df_long <- dplyr::select(rawparamsm1_df_noUnivariate_outliers, PTNUM, scpt, ccpt, scpr, ccpr) %>% gather(key = "coupling_param", value = "coupling_value_VBA_var", -PTNUM) 
sasraw_long <- inner_join(sas_var_params_long, rawparamsm1_df_long)
ggplot(sasraw_long_noUnivariate_outliers, aes(x = coupling_value_sas_var, y = coupling_value_VBA_var, color = coupling_param))+ geom_point() + geom_smooth(method = "lm", se = FALSE)

rawparamsm1_df_noUnivariate_outliers <- dplyr::filter(rawparamsm1_df, abs(scpt) < .5, abs(scpr) < .5, abs(ccpt) < .5, abs(ccpr) < .5) #only gets rid of 3 people, also people with really bad LL
sasraw_wide_noUnivariateOutliers <- inner_join(rawparamsm1_df_noUnivariate_outliers, sas_var_params)
cor(dplyr::select(sasraw_wide_noUnivariateOutliers, a1, scpt))
cor(dplyr::select(sasraw_wide_noUnivariateOutliers, a2, ccpt))

cor(dplyr::select(sasraw_wide_noUnivariateOutliers, b1, scpr))
cor(dplyr::select(sasraw_wide_noUnivariateOutliers, b2, ccpr))


df_params <- readMat("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/modelcomparison_logevidence.mat")
df_params_m1 <- as.data.frame(df_params$rawparameters[,1,])
colnames(df_params_m1) <- c("felm_scpt", "felm_ccpt", "felm_scpr", "felm_ccpr", "p1star","p2star")
df_params_m1$PTNUM <- as.vector(df_params$ids)

library(car)
source('http://psych.colorado.edu/~jclab/R/mcSummaryLM.R')


df_wide_m1 <- inner_join(rawparamsm1_df, df_params_m1)  
str(df_wide)
df_wide_m1_noUnivariateOutliers <- dplyr::filter(df_wide_m1, abs(scpt) <.5, abs(scpr) < .5, abs(ccpt) < .5, abs(ccpr) < .5)
df_wide_m1_noUnivariateOutliers_noWackies <- dplyr::filter(df_wide_m1_noUnivariateOutliers, PTNUM != 8133, PTNUM!= 8112, PTNUM !=8106, PTNUM != 8073, PTNUM != 8063, PTNUM!= 8060, PTNUM !=8035, PTNUM != 8016)
length(df_wide_m1_noUnivariateOutliers_noWackies$scpt)
summary(lm(scpt ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_m1_noUnivariateOutliers_noWackies))
summary(lm(ccpt ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_m1_noUnivariateOutliers_noWackies))
summary(lm(scpr ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_m1_noUnivariateOutliers_noWackies))
summary(lm(ccpr ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_m1_noUnivariateOutliers_noWackies))
mcSummary(lm(scpt ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_m1_noUnivariateOutliers_noWackies))
mcSummary(lm(ccpt ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_m1_noUnivariateOutliers_noWackies))
mcSummary(lm(scpr ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_m1_noUnivariateOutliers_noWackies))
mcSummary(lm(ccpr ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_m1_noUnivariateOutliers_noWackies))



df_params <- readMat("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/modelcomparison_logevidence.mat")
df_params_m3 <- as.data.frame(df_params$rawparameters[,3,])
colnames(df_params_m3) <- c("felm_scpt", "felm_ccpt", "felm_scpr", "felm_ccpr", "p1star","p2star")
df_params_m3$PTNUM <- as.vector(df_params$ids)

df_wide <- inner_join(rawparamsm1_df, df_params_m3)  
str(df_wide)
df_wide_noUnivariateOutliers <- dplyr::filter(df_wide, abs(scpt) <.5, abs(scpr) < .5, abs(ccpt) < .5, abs(ccpr) < .5)
df_wide_noUnivariateOutliers_noWackies <- dplyr::filter(df_wide_noUnivariateOutliers, PTNUM != 8133, PTNUM!= 8112, PTNUM !=8106, PTNUM != 8073, PTNUM != 8063, PTNUM!= 8060, PTNUM !=8035, PTNUM != 8016)
length(df_wide_noUnivariateOutliers_noWackies$scpt)
summary(lm(scpt ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_noUnivariateOutliers_noWackies))
summary(lm(ccpt ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_noUnivariateOutliers_noWackies))
summary(lm(scpr ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_noUnivariateOutliers_noWackies))
summary(lm(ccpr ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_noUnivariateOutliers_noWackies))
mcSummary(lm(scpt ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_noUnivariateOutliers_noWackies))
mcSummary(lm(ccpt ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_noUnivariateOutliers_noWackies))
mcSummary(lm(scpr ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_noUnivariateOutliers_noWackies))
mcSummary(lm(ccpr ~ felm_scpt + felm_ccpt+ felm_scpr + felm_ccpr + p1star + p2star, df_wide_noUnivariateOutliers_noWackies))

################### Updated parameters
vanBse_varparams <- readMat("/Users/alisonmarie526/Documents/MATLAB/vanbse_rawparamsm1.mat")
vanBse_varparams_df <- as.data.frame(vanBse_varparams$vanbse.rawparamsm1)
colnames(vanBse_varparams_df) <- c("v_scpt", "v_ccpt", "v_scpr", "v_ccpr", "v_ll", "v_R2")
vanBse_varparams_df$PTNUM <- as.vector(vanBse_varparams$ids)


setwd("~/Desktop/VAR_output/")
library(R.matlab)
varparms <- readMat("VAR_rawparamsm1m2.mat")
varparmsm1_df <- as.data.frame(varparms$VAR.rawparamsm1)
colnames(varparmsm1_df) <- c("m1_scpt", "m1_ccpt", "m1_scpr", "m1_ccpr", "m1_ll", "m1_R2")
varparmsm1_df$PTNUM <- as.vector(varparms$ids)
varparmsm2_df <- as.data.frame(varparms$VAR.rawparamsm2)
colnames(varparmsm2_df) <- c("m2_scpt", "m2_ccpt", "m2_scpr", "m2_ccpr", "p1star", "p2star","m2_ll", "m2_R2")
varparmsm2_df$PTNUM <- as.vector(varparms$ids)


varparmsm1_df <- dplyr::rename(varparmsm1_df, "scpt" = "m1_scpt", "scpr" = "m1_scpr", "ccpt" = "m1_ccpt", "ccpr" = "m1_ccpr" )
fulldata <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata.csv") %>% dplyr::select(-scpt, -ccpt, -scpr, -ccpr, -v_scpt, -v_ccpt, -v_scpr, -v_ccpr, -starts_with("X"))
fulldata_var_negint <- plyr::join_all(list(varparmsm1_df, fulldata, vanBse_varparams_df), type = "full") %>% dplyr::mutate(scpt = 1000*scpt, ccpt = 1000*ccpt, scpr = 1000*scpr,ccpr = 1000*ccpr, v_scpt = 1000*v_scpt, v_ccpt = 1000*v_ccpt, v_scpr = 1000*v_scpr, v_ccpr = 1000*v_ccpr)
resid_fulldata_var_negint_resids <- dplyr::filter(fulldata_var_negint, !is.na(scpt), !is.na(scpr), !is.na(ccpt), !is.na(ccpr), !is.na(v_scpt), !is.na(v_ccpt), !is.na(v_scpr), !is.na(v_ccpr)) %>% do(data.frame(resid_scpt = resid(lm(scpt~v_scpt + v_ccpt,.)),
                                                                           resid_ccpt = resid(lm(ccpt~v_scpt + v_ccpt,.)),
                                                                           resid_scpr = resid(lm(scpr~v_scpr + v_ccpr, .)),
                                                                           resid_ccpr = resid(lm(ccpr~v_scpr + v_ccpr, .)),
                                                                          PTNUM = .$PTNUM))
resid_fulldata_var_negint <- full_join(resid_fulldata_var_negint_resids, fulldata_var_negint) %>%dplyr::filter(!is.na(scpt), !is.na(scpr), !is.na(ccpt), !is.na(ccpr), !is.na(v_scpt), !is.na(v_ccpt), !is.na(v_scpr), !is.na(v_ccpr))

write.csv(fulldata_var_negint, "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv")
write.csv(resid_fulldata_var_negint, "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/results/resid_df_VAR.csv")

########### GRAPHING


dplyr::filter(vanBse_varparams_df, R2 > .90, abs(scpr) < .5, abs(scpt) < .5, abs(ccpt) < .5, abs(ccpr) < .5) %>% dplyr::select(scpt, ccpt, scpr, ccpr) %>% lattice::splom() #119
dplyr::filter(vanBse_varparams_df, R2 > .90, abs(scpr) < .5, abs(scpt) < .5, abs(ccpt) < .5, abs(ccpr) < .5) %>% dplyr::select(scpt, ccpt, scpr, ccpr) %>% cor()


dplyr::filter(varparmsm1_df, abs(m1_scpt) < .5, abs(m1_scpr) < .5, abs(m1_ccpr) < .5, abs(m1_ccpt) < .5) %>% dplyr::select(m1_scpt, m1_ccpt, m1_scpr, m1_ccpr) %>% lattice::splom()
dplyr::filter(varparmsm1_df, abs(m1_scpt) < .5, abs(m1_scpr) < .5, abs(m1_ccpr) < .5, abs(m1_ccpt) < .5) %>% dplyr::select(m1_scpt, m1_ccpt, m1_scpr, m1_ccpr) %>% cor()

dplyr::filter(varparmsm2_df, abs(m2_scpt) < .5, abs(m2_scpr) < .5, abs(m2_ccpr) < .5, abs(m2_ccpt) < .5) %>% dplyr::select(m2_scpt, m2_ccpt, m2_scpr, m2_ccpr) %>% lattice::splom()
dplyr::filter(varparmsm2_df, abs(m2_scpt) < .5, abs(m2_scpr) < .5, abs(m2_ccpr) < .5, abs(m2_ccpt) < .5) %>% dplyr::select(m2_scpt, m2_ccpt, m2_scpr, m2_ccpr, p1star, p2star) %>% cor()

varparmsm1m2_df <- full_join(varparmsm1_df, varparmsm2_df)

df <- dplyr::mutate(varparmsm1m2_df, ll_diff = m1_ll - m2_ll) 
ggplot(df, aes(x = m1_ll)) + geom_density()
ggplot(df, aes(x = m2_ll)) + geom_density()
ggplot(df, aes(x = p1star)) + geom_histogram()
ggplot(df, aes(x = p2star)) + geom_histogram()
ggplot(df, aes(x = ll_diff, y = p1star)) + geom_jitter()
ggplot(df, aes(x = ll_diff, y = p2star)) + geom_jitter()


df <- dplyr::filter(varparmsm1m2_df, m1_ll > -75000, m2_ll > -75000) %>% dplyr::mutate(ll_diff = m1_ll - m2_ll) 
ggplot(df, aes(x = m1_ll)) + geom_density()
ggplot(df, aes(x = m2_ll)) + geom_density()
ggplot(df, aes(x = p1star)) + geom_histogram()
ggplot(df, aes(x = p2star)) + geom_histogram()
ggplot(df, aes(x = ll_diff, y = p1star)) + geom_jitter()
ggplot(df, aes(x = ll_diff, y = p2star)) + geom_jitter()
ggplot(df, aes(x = (m1_scpt - m2_scpt), y= ll_diff)) + geom_point()
ggplot(df, aes(x = (m1_scpr - m2_scpr), y= ll_diff)) + geom_point()
ggplot(df, aes(x = (m1_ccpt - m2_ccpt), y= ll_diff)) + geom_point()
ggplot(df, aes(x = (m1_ccpr - m2_ccpr), y= ll_diff)) + geom_point()

dplyr::filter(varparmsm1m2_df, abs(m2_scpt) < .5, abs(m2_scpr) < .5, abs(m2_ccpr) < .5, abs(m2_ccpt) < .5, abs(m1_scpt) < .5, abs(m1_scpr) < .5, abs(m1_ccpr) < .5, abs(m1_ccpt) < .5) %>% dplyr::select(m1_scpt, m1_ccpt, m1_scpr, m1_ccpr, m2_scpt, m2_ccpt, m2_scpr, m2_ccpr, p1star, p2star) %>% cor()

toplot_df <- dplyr::filter(varparmsm1m2_df, abs(m2_scpt) < .5, abs(m2_scpr) < .5, abs(m2_ccpr) < .5, abs(m2_ccpt) < .5, abs(m1_scpt) < .5, abs(m1_scpr) < .5, abs(m1_ccpr) < .5, abs(m1_ccpt) < .5) %>% dplyr::select(m1_scpt, m1_ccpt, m1_scpr, m1_ccpr, m2_scpt, m2_ccpt, m2_scpr, m2_ccpr, PTNUM)
toplot_df_long <- gather(toplot_df, key = "key", value = "value", -PTNUM) %>% separate(key,into=c("model", "key"), sep = "_") %>% spread(model, value)
ggplot(toplot_df_long, aes(x = m1, y = m2)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~key)


png("felm_collinearity.png", width = 500, height = 500)
dplyr::filter(df_params_m1, abs(felm_scpt) <.5, abs(felm_scpr) <.5, abs(felm_ccpt) <.5, abs(felm_ccpr) <.5) %>% dplyr::select(felm_scpt, felm_ccpt, felm_scpr, felm_ccpr) %>% splom()
dev.off()
dplyr::filter(df_params_m1, abs(felm_scpt) <.5, abs(felm_scpr) <.5, abs(felm_ccpt) <.5, abs(felm_ccpr) <.5) %>% dplyr::select(felm_scpt, felm_ccpt, felm_scpr, felm_ccpr) %>% cor()
fulldata <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata.csv") 
png("mod_sde_collinearity.png", width = 750, height = 750)
dplyr::filter(fulldata, abs(scpt) <100, abs(scpr) <100, abs(ccpt) <60, abs(ccpr) <60) %>% dplyr::select(scpt, ccpt, scpr, ccpr) %>% splom()
dev.off()
dplyr::filter(fulldata, abs(scpt) <100, abs(scpr) <100, abs(ccpt) <60, abs(ccpr) <60) %>% dplyr::select(scpt, ccpt, scpr, ccpr) %>% cor()


