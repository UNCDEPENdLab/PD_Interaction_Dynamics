#packages needed for this script
library(ggplot2)
library(dynr)
library(dplyr)
library(tidyr)



#set working directory (may need to change for your computer)

basedir <- "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data"
#basedir <- "/Users/quc16/Box Sync/PD_Interaction_Dynamics/data"

setwd(basedir)

#This file contains physio data for 71 couples in list alldata.
#There are 5 elements where each element is a list of data.frames for couples.
#The data are available at 5 frequencies: 1, 1.5, 2, 4, and 10 Hz.
load(file="align_split_atdifferentfreqs_patpar_Nov2016.RData")


#example
#str(alldata[["1Hz"]][[1]])
#str(alldata[["4Hz"]][[1]])

Hz <- 4 #frequency used in fitting all subjects (1, 1.5, 2, 4, 10)

#for testing multi-subject fits, these four couples have clean data
dat4couples <- c()
dat4couples <- c(
    alldata[[paste0(Hz, "Hz")]]$`8017`,
    alldata[[paste0(Hz, "Hz")]]$`8062`,
    alldata[[paste0(Hz, "Hz")]]$`8069`,
    alldata[[paste0(Hz, "Hz")]]$`8086`
)

#plot of data for 4 couples prior to standardization
pos <- which(names(alldata$`4Hz`) %in% c("8017", "8062", "8069", "8086"))
dat4couples_df <- alldata$`4Hz`[pos]
toplot <- do.call(rbind, dat4couples_df) %>% 
  select(PTNUM, ms, ibi_interp_detrend_partner, ibi_interp_detrend_patient) %>%
  mutate(time=ms/1000) %>% select(-ms) %>%
  gather(key=signal, value=value, ibi_interp_detrend_partner, ibi_interp_detrend_patient)

ggplot(toplot, aes(x=time, y=value, color=signal)) + geom_line() + facet_wrap(~PTNUM, ncol=1)

#if you want just one person's data
#datsingle <-  dat4couples #alldata[[paste0(Hz, "Hz")]]$`8069`

#if you want all of the data --  some which is still undergoing quality checks
#dfall <- do.call("rbind", alldata[[paste0(Hz, "Hz")]])

#specify which data to fit
tofit <- dat4couples #single subject fits (change to dat4couples for 4-subject fit)
data4Hz <- alldata$`4Hz`
#this will add a time column by couple
Hz = 4

#Alison: to lapply over a subset of a list, you need single brackets []
#The double brackets attempts to extract objects within a single element of the list,
#thereby losig the list structure

tofit <- lapply(data4Hz[1:5], function(x) {
  names(x) <- gsub("_", ".", names(x), fixed = TRUE)
  df <- x %>% mutate_at(vars(ibi.interp.detrend.patient, ibi.interp.detrend.partner, 
                             dom.interp.detrend.patient, dom.interp.detrend.partner, 
                             aff.interp.detrend.patient, aff.interp.detrend.partner), 
                        funs(as.vector(scale(.)))) %>% mutate(time=seq(1/Hz, n()/Hz, by=1/Hz))
  return(df)
} )
#substitute periods for underscores in variable names
# lapply(tofit, function(tofit) {names(tofit) <- gsub("_", ".", names(tofit), fixed = TRUE)}) #for consistency with below

#z-score all detrended time series by couple
#MNH: Not sure why this is duplicated from above
# tofit <- lapply(tofit, function(x) {x %>%
#   mutate_at(vars(ibi.interp.detrend.patient, ibi.interp.detrend.partner, 
#                  dom.interp.detrend.patient, dom.interp.detrend.partner, 
#                  aff.interp.detrend.patient, aff.interp.detrend.partner), 
#             funs(as.vector(scale(.)))) %>% as.data.frame() #explicitly convert to data.frame
# })
toplot <- do.call(rbind, tofit) %>% select(PTNUM, ms, ibi.interp.detrend.partner, ibi.interp.detrend.patient) %>%
  mutate(time=ms/1000) %>% select(-ms) %>%
  gather(key=signal, value=value, ibi.interp.detrend.partner, ibi.interp.detrend.patient)

ggplot(toplot, aes(x=time, y=value, color=signal)) + geom_line() + facet_wrap(~PTNUM, ncol=1)

tofit_diff <- do.call(rbind, tofit) %>% group_by(PTNUM) %>% mutate(difftime.ibi.partner = c(0, diff(ibi.interp.detrend.partner)), difftime.ibi.patient = c(0,diff(ibi.interp.detrend.patient))) %>% ungroup() %>%  as.data.frame()

# tofit_notinitial <- filter(tofit_diff, time!=0)
# tofit_notinitial$time <- tofit_notinitial$time-.25
# tofit <- lapply(tofit, function(x) {
#   x$time <- x$time - .25
#   x
# })

#BEGIN DYNR MODELING USING TOFIT
dynr_info_fx <- function(coupledf) { 


data <- dynr.data(
  coupledf,
  id = "PTNUM",
  time = "time",
  observed=c("ibi.interp.detrend.patient", "ibi.interp.detrend.partner")
  #observed = c("difftime.ibi.patient", "difftime.ibi.partner")
) 

#zero-order correlations
#cor(tofit_notinitial[, c("ibi.interp.detrend.patient", "ibi.interp.detrend.partner")])
cor(coupledf[, c("ibi.interp.detrend.patient", "ibi.interp.detrend.partner")])

#setting measurement model (single indicator latent variable with loading 1.0)
meas <- prep.measurement(
  values.load = diag(1, 2),
  params.load = diag(rep("fixed", 2), 2),
  state.names = c("patient_ibi", "partner_ibi"),
  obs.names = c("ibi.interp.detrend.patient", "ibi.interp.detrend.partner")
)

#setup initial conditions for states
initial <- prep.initial(
  values.inistate = c(0, 0), #zero starting value for states
  params.inistate = c("fixed", "fixed"),
  values.inicov = diag(1, 2), #starting value of 1 for signal covariance
  params.inicov = diag(rep("fixed", 2), 2)
  #params.inicov = diag(c("P11","P22"),2)
)

#Only allowing for state noise.
#In our tests, allowing for measurement noise, and both measurement and state noise did not provide better fits
mdcov_sonly <- prep.noise(
  values.latent = diag(.1, 2),
  params.latent = diag(c("sn1", "sn2"), 2),
  values.observed = diag(rep(0, 2)),
  params.observed = diag("fixed", 2)
)

# dynamics (evolution functions)
# starting out with only ibi-ibi coupling
# Time series have been detrended, so probably don't need a homeostasis point
plstar = 0 #homeostasis point for left
prstar = 0 #homeostasis point for right
fcoupled = list(list(
  patient_ibi ~ p_patientibiself * patient_ibi + p_partpatibi * partner_ibi,
  partner_ibi ~ p_partneribiself * partner_ibi + p_patpartibi * patient_ibi
  
  #using Steele and Ferrer parameterization
  # patient_ibi ~ p_patientibiself * (0 - patient_ibi) + p_partpatibi * (partner_ibi - patient_ibi),
  # partner_ibi ~  p_partneribiself * (0 - partner_ibi) + p_patpartibi * (patient_ibi - partner_ibi)
))

#starting parameter values: moderate self-coupling, weak cross-coupling
svals <- c(rep(.5, 2), rep(0.05, 2))
names(svals) <- c("p_patientibiself", "p_partneribiself", "p_partpatibi", "p_patpartibi")

dynm  <- prep.formulaDynamics(formula = fcoupled,
                              startval = svals,
                              isContinuousTime = FALSE) #,jacobian=jacob

#Setup logit transformation of self-coupling to constrain 0..1 (not used at the moment)
#This did help with some subjects to avoid coupling above 1.0 (unstable)
#NB. This transformation does not work for Steele and Ferrer variant where self coupling should be negative
# trans <-
#   prep.tfun(
#     formula.trans = list(
#       p_patientibiself ~  exp(p_patientibiself) / (1 + exp(p_patientibiself)),
#       p_partneribiself ~  exp(p_partneribiself) / (1 + exp(p_partneribiself))
#     ),
#     formula.inv = list(
#       p_patientibiself ~ log(p_patientibiself / (1 - p_patientibiself)),
#       p_partneribiself ~ log(p_partneribiself / (1 - p_partneribiself))
#     )
#   )

#create dynr model
model_snoise <- dynr.model(
  dynamics = dynm,
  measurement = meas,
  noise = mdcov_sonly,
  initial = initial,
  data = data,
  outfile = "VAR_snoise.c"
)

model_snoise$param.names
#set upper and lower bounds for parameters
model_snoise@ub[model_snoise$param.names %in% 
                  c("p_patientibiself", "p_partneribiself", 
                    "p_partpatibi",     "p_patpartibi")] = rep(5, 4)
model_snoise@lb[model_snoise$param.names %in% 
                  c("p_patientibiself", "p_partneribiself", 
                    "p_partpatibi",     "p_patpartibi")] = rep(-5, 4)
#Extract parameter names to set ub and lb (optional)
#model_snoise$param.names

#Need to set path explicitly in unattended R session --> 
#using local bin because gsl couldn't be downloaded the conventional way due to IT constraints
#copied from interactive R session
#Sys.setenv(PATH = "/Users/ams939/bin/homebrew/bin/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")
#Sys.setenv(PATH = "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")

#results of state noise model
res_sonly <-
  tryCatch(
     #print new line print ID as cook
    dynr.cook(model_snoise),
    error = function(e) {
      print(e)
      return(NULL)
    }
  )

res_sonly_failed <- ifelse(is.null(res_sonly), TRUE, FALSE)

options(scipen = 999)
sum <- summary(res_sonly)
coef <- coef(res_sonly)

# get the log likelihood, AIC, and BIC from a cooked model/data combo
loglik <- logLik(res_sonly)
aic <- AIC(res_sonly)
bic <- BIC(res_sonly)

return(res_sonly)

}




str(dat4couples)

tofit_info <- lapply(tofit, dynr_info_fx)

parameter_estimates <- lapply(tofit_info, function(x) {
  d <- c(x@fitted.parameters, x@exitflag)
  return(d)
})


# p1 = dynr.ggplot(dynrModel = model_snoise, res = res_sonly, numSubjDemo=1,
#     names.state=c("Patient","Partner"),
#     title="Results from RS-linear ODE model")
