#first load in full dataset. Want to be looking at parameters, m1_R2 and ll
library(tidyverse)
library(R.matlab)
#ffx_df <-  read.csv("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv")
ffx_df <-  read.csv("~/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv")

#rawparamsm1 <- readMat("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/VAR_output/VAR_rawparamsm1m2.mat")
rawparamsm1 <- readMat("~/Box/DEPENd/Projects/PD_Interaction_Dynamics/data/VAR_output/VAR_rawparamsm1m2.mat")

#rawparamsm1 <- readMat("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/scratch/feb2018_params_VARCoreg_m1.mat")
rawparamsm1_df <- as.data.frame(rawparamsm1$VAR.rawparamsm1)
colnames(rawparamsm1_df) <- c("rscpt", "rccpt", "rscpr", "rccpr", "ll", "R2")
rawparamsm1_df$PTNUM <- as.vector(rawparamsm1$ids)
#rawparamsm1_df$ll <- as.vector(rawparamsm1$logEvidence[,1])


#mfx_ll <- readMat("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/nowackies_M1_modelcomparison_logevidence.mat")
mfx_ll <- readMat("~/Desktop/weakpriors_nowackies_M1_modelcomparison_logevidence.mat")

mfx_df <- data.frame(mscpt = 1:115, mccpt = 1:115, mscpr = 1:115, mccpr = 1:115, mll = 1:115, mR2 = 1:115)
mfx_scpt <- as.vector(mfx_ll$rawparameters[,,1])
mfx_ccpt <- as.vector(mfx_ll$rawparameters[,,2])
mfx_scpr <- as.vector(mfx_ll$rawparameters[,,3])
mfx_ccpr <- as.vector(mfx_ll$rawparameters[,,4])
mfx_df <- mutate(mfx_df, mscpt = mfx_scpt, mccpt = mfx_ccpt, mscpr = mfx_scpr , mccpr = mfx_ccpr, mll = as.vector(mfx_ll$logEvidence), mR2 = as.vector(mfx_ll$R2.evidence), PTNUM = as.vector(mfx_ll$ids))

ffmfx_df <- plyr::join_all(list(ffx_df, mfx_df, rawparamsm1_df))
ffmfx_df_115 <- dplyr::filter(ffmfx_df, !is.na(mscpt) , m1_R2 > .98)
lattice::splom(dplyr::select(ffmfx_df_115, scpt, ccpt, scpr, ccpr, mscpt, mccpt, mscpr, mccpr))

ffmfx_df_long <- dplyr::select(ffmfx_df, mscpt, mccpt, mscpr,mccpr, PTNUM) %>% gather(key = "key", value = "value", -PTNUM) 
ffmfx_df_long2 <- dplyr::select(ffmfx_df, PTNUM, mll, m1_ll) %>% gather(key = "key", value = "value", -PTNUM )  

ffmfx_df_all115 <- dplyr::filter(ffmfx_df, !is.na(mscpt))
a <- ggplot(ffmfx_df_all115, aes(x = scpt)) + geom_histogram()
b <- ggplot(ffmfx_df_all115, aes(x = scpr)) + geom_histogram()
c <- ggplot(ffmfx_df_all115, aes(x = ccpt)) + geom_histogram()
d <- ggplot(ffmfx_df_all115, aes(x = ccpr)) + geom_histogram()
aa <- ggplot(ffmfx_df_all115, aes(x = mscpt)) + geom_histogram()
bb <- ggplot(ffmfx_df_all115, aes(x = mscpr)) + geom_histogram()
cc<- ggplot(ffmfx_df_all115, aes(x = mccpt)) + geom_histogram()
dd <- ggplot(ffmfx_df_all115, aes(x = mccpr)) + geom_histogram()
cowplot::plot_grid(a, aa, b, bb, c, cc, d, dd, ncol = 2)

 
cutdf <- dplyr::filter(ffmfx_df,abs(scpt) <100, abs(ccpt) <100, abs(ccpr) <100, abs(scpr) <100, abs(mscpt) <1, abs(mccpt) <1, abs(mscpr)<1, abs(mccpr))

summary(lm(mscpt ~ scpt, cutdf))
summary(lm(mscpr ~ scpr, cutdf))
summary(lm(mccpt ~ ccpt, cutdf))
summary(lm(mccpr ~ ccpr, cutdf))

cor(dplyr::select(cutdf, mscpt, scpt, mscpr, scpr, mccpt, ccpt, mccpr, ccpr))

ggplot(dplyr::filter(ffmfx_df, !is.na(mscpt)), aes(x = m1_R2, y = mR2)) + geom_jitter()  

ffmfx_df_all115_towrite <- dplyr::select(ffmfx_df_all115, m1_ll, mll)
write.csv(ffmfx_df_all115_towrite, "~/Desktop/ffxmfx_df_only115.csv")


ffmfx_df_all115 <- mutate(ffmfx_df_all115, diffscpt = 1000*mscpt - scpt, diffccpt = 1000*mccpt - ccpt, diffscpr = 1000*mscpr - scpr, diffccpr = 1000*mccpr - ccpr)
