library(R.matlab)
setwd("~/Desktop/")
library(tidyverse)
df <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv") 
sigma_thetas <- readMat("feb2018_VAR_SigmaThetas.mat")
sigma_thetas_df <- as.data.frame(sigma_thetas$SigmaThetas[1:500,])
colnames(sigma_thetas_df)<- c("PTNUM", "scpt", "ccpt", "scpr", "ccpr")
sigma_thetas_df$byVar<- c("scpt", "ccpt","scpr","ccpr")
vardf <- data.frame(PTNUM = unique(sigma_thetas_df$PTNUM), scpt = 1:length(unique(sigma_thetas_df$PTNUM)), ccpt = 1:length(unique(sigma_thetas_df$PTNUM)), scpr = 1:length(unique(sigma_thetas_df$PTNUM)), ccpr = 1:length(unique(sigma_thetas_df$PTNUM)))
for(i in 1:length(unique(sigma_thetas_df$PTNUM))) {
  out <- unique(sigma_thetas_df$PTNUM)
  tempdf <- dplyr::filter(sigma_thetas_df, PTNUM == out[[i]])
  
  vardf[[i, 2]] = tempdf[[1, 2]]
  vardf[[i, 3]] = tempdf[[2, 3]]
  vardf[[i, 4]] = tempdf[[3, 4]]
  vardf[[i, 5]] = tempdf[[4, 5]]
}
  

sd <- vardf %>% transmute(sdscpt= sqrt(scpt), sdscpr = sqrt(scpr), sdccpt = sqrt(ccpt), sdccpr= sqrt(ccpr), PTNUM= PTNUM)
vardf <- transmute(vardf, sdscpt = scpt, sdscpr = scpr, sdccpt = ccpt, sdccpr = ccpr, PTNUM = PTNUM ) 
df <- dplyr::filter(df,  m1_R2 > .98,PTNUM != 8035, PTNUM != 8040, PTNUM!= 8073) %>% transmute(PTNUM = PTNUM, scpt = scpt/1000, ccpt = ccpt/1000, scpr = scpr/1000, ccpr = ccpr/1000) #rescaled to original value
totaldf <- inner_join(vardf, df) 
totaldf <- mutate(totaldf, lowerciscpt = scpt - 2*sdscpt, upperciscpt = scpt + 2*sdscpt, lowerciscpr = scpr - 2*sdscpr, upperciscpr = scpr + 2*sdscpr, lowerciccpt = ccpt - 2*sdccpt, upperciccpt = ccpt + 2*sdccpt, lowerciccpr = ccpr - 2*sdccpr, upperciccpr = ccpr + 2*sdccpr)
totaldf <- mutate(totaldf, sigscpt = if_else(((lowerciscpt & upperciscpt > 0) | (lowerciscpt & upperciscpt < 0)), TRUE, FALSE),
                  sigscpr = if_else(((lowerciscpr & upperciscpr > 0) | (lowerciscpr & upperciscpr < 0)), TRUE, FALSE),
                  sigccpt = if_else(((lowerciccpt & upperciccpt > 0) | (lowerciccpt & upperciccpt < 0)), TRUE, FALSE),
                  sigccpr = if_else(((lowerciccpr & upperciccpr > 0) | (lowerciccpr & upperciccpr < 0)), TRUE, FALSE))
totaldf_nonsig <- dplyr::filter(totaldf, sigscpt == FALSE |sigscpr == FALSE |sigccpt == FALSE|sigccpr == FALSE)
str(totaldf_nonsig) # also no nonsig
#no non sig results using vardf
#now double check with sddf
totaldf <- inner_join(sd, df) 
totaldf <- mutate(totaldf, lowerciscpt = scpt - 2*sdscpt, upperciscpt = scpt + 2*sdscpt, lowerciscpr = scpr - 2*sdscpr, upperciscpr = scpr + 2*sdscpr, lowerciccpt = ccpt - 2*sdccpt, upperciccpt = ccpt + 2*sdccpt, lowerciccpr = ccpr - 2*sdccpr, upperciccpr = ccpr + 2*sdccpr)
totaldf <- mutate(totaldf, sigscpt = if_else(((lowerciscpt & upperciscpt > 0) | (lowerciscpt & upperciscpt < 0)), TRUE, FALSE),
                  sigscpr = if_else(((lowerciscpr & upperciscpr > 0) | (lowerciscpr & upperciscpr < 0)), TRUE, FALSE),
                  sigccpt = if_else(((lowerciccpt & upperciccpt > 0) | (lowerciccpt & upperciccpt < 0)), TRUE, FALSE),
                  sigccpr = if_else(((lowerciccpr & upperciccpr > 0) | (lowerciccpr & upperciccpr < 0)), TRUE, FALSE))

totaldf_nonsig <- dplyr::filter(totaldf, sigscpt == FALSE |sigscpr == FALSE |sigccpt == FALSE|sigccpr == FALSE)
str(totaldf_nonsig) # also no nonsig
