library(dplyr)
par_varbase <- read.csv("/Users/mnh5174/Data_Analysis/psychopathology_network_sim/data/sas_params_pstar.csv")
round(cor(dplyr::select(par_varbase, a1, a2, b1, b2, p1star, p2star)), 3)

hist(par_varbase$p1star)
hist(par_varbase$p2star)
hist(par_varbase$a1)
hist(par_varbase$a2)
hist(par_varbase$b1)
hist(par_varbase$b2)


par_var <- read.csv("/Users/mnh5174/Data_Analysis/psychopathology_network_sim/data/sas_params_var.csv")
cor(dplyr::select(par_var, a1, a2, b1, b2))

par_felmlee <- read.csv("/Users/mnh5174/Data_Analysis/psychopathology_network_sim/data/sas_params_felmlee.csv")
cor(dplyr::select(par_felmlee, a1, a2, b1, b2))

par_mstar <- read.csv("/Users/mnh5174/Data_Analysis/psychopathology_network_sim/data/sas_params_mstar.csv")
cor(dplyr::select(par_mstar, a1, a2, b1, b2, mstar))

#fit comparison of var with and without p1star and p2star
fit_varbase <- read.csv("/Users/mnh5174/Data_Analysis/psychopathology_network_sim/data/sas_fitstats_pstar.csv")
fit_var <- read.csv("/Users/mnh5174/Data_Analysis/psychopathology_network_sim/data/sas_fitstats_var.csv")

fit_varbase <- fit_varbase %>% select(ptnum, Equation, RMSE, RSquare) %>% rename(BaseR2=RSquare, BaseRMSE=RMSE)
fit_var <- fit_var %>% select(ptnum, Equation, RMSE, RSquare) %>% rename(NobaseR2=RSquare, NobaseRMSE=RMSE)

fit_both <- inner_join(fit_varbase, fit_var)
hist(fit_both$BaseR2 - fit_both$NobaseR2)
hist(fit_both$BaseRMSE - fit_both$NobaseRMSE)

ll_varbase <- read.csv("/Users/mnh5174/Data_Analysis/psychopathology_network_sim/data/sas_convergence_pstar.csv")
vv <- ll_varbase %>% filter(Label1=="Log likelihood") %>% 
  mutate(cValue1=as.numeric(as.character(cValue1)), nValue1=as.numeric(as.character(nValue1)))

ll_varnobase <- read.csv("/Users/mnh5174/Data_Analysis/psychopathology_network_sim/data/sas_convergence_var.csv")
vv2 <- ll_varnobase %>% filter(Label1=="Log likelihood") %>% 
  mutate(cValue1=as.numeric(as.character(cValue1)), nValue1=as.numeric(as.character(nValue1)))

vv$cValue1 - vv2$cValue1
