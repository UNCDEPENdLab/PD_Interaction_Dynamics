#packages needed for this script
library(ggplot2)
library(dynr)
library(dplyr)
library(tidyr)

#set working directory (may need to change for your computer)
basedir <- "~/Box_Sync/DEPENd/Couples Data/data"
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
dat4couples <-
  rbind(
    alldata[[paste0(Hz, "Hz")]]$`8017`,
    alldata[[paste0(Hz, "Hz")]]$`8062`,
    alldata[[paste0(Hz, "Hz")]]$`8069`,
    alldata[[paste0(Hz, "Hz")]]$`8086`
  )

toplot <- dat4couples %>% select(PTNUM, ms, ibi_interp_detrend_partner, ibi_interp_detrend_patient) %>%
  mutate(time=ms/1000) %>% select(-ms) %>%
  gather(key=signal, value=value, ibi_interp_detrend_partner, ibi_interp_detrend_patient)

ggplot(toplot, aes(x=time, y=value, color=signal)) + geom_line() + facet_wrap(~PTNUM, ncol=1)

#if you want just one person's data
datsingle <- alldata[[paste0(Hz, "Hz")]]$`8069`

#if you want all of the data --  some which is still undergoing quality checks
#dfall <- do.call("rbind", alldata[[paste0(Hz, "Hz")]])

#specify which data to fit
tofit <- datsingle #single subject fits (change to dat4couples for 4-subject fit)

#this will add a time column by couple
tofit <- tofit %>% group_by(PTNUM) %>% mutate(time=seq(1/Hz, n()/Hz, by=1/Hz)) %>% ungroup()

#substitute periods for underscores in variable names
names(tofit) <- gsub("_", ".", names(tofit), fixed = TRUE) #for consistency with below

#z-score all detrended time series by couple
tofit <- tofit %>% group_by(PTNUM) %>%
  mutate_at(vars(ibi.interp.detrend.patient, ibi.interp.detrend.partner, 
                 dom.interp.detrend.patient, dom.interp.detrend.partner, 
                 aff.interp.detrend.patient, aff.interp.detrend.partner), 
            funs(as.vector(scale(.)))) %>% ungroup() %>% as.data.frame() #explicitly convert to data.frame

#BEGIN DYNR MODELING USING TOFIT
data <- dynr.data(
  tofit,
  id = "PTNUM",
  time = "time",
  observed = c("ibi.interp.detrend.patient", "ibi.interp.detrend.partner")
)

#zero-order correlations
cor(tofit[, c("ibi.interp.detrend.patient", "ibi.interp.detrend.partner")])

#setting measurement model (single indicator latent variable with loading 1.0)
meas <- prep.measurement(
  values.load = diag(1, 2),
  params.load = diag(rep("fixed", 2), 2),
  state.names = c("patient_ibi", "partner_ibi"),
  obs.names = c("ibi.interp.detrend.patient",
                "ibi.interp.detrend.partner")
)

#setup initial conditions for states
initial <- prep.initial(
  values.inistate = c(0, 0), #zero starting value for states
  params.inistate = c("fixed", "fixed"),
  values.inicov = diag(1, 2), #starting value of 1 for signal covariance
  params.inicov = diag(rep("fixed", 2), 2)
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
  partner_ibi ~  p_partneribiself * partner_ibi + p_patpartibi * patient_ibi
  
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

#set upper and lower bounds for parameters
model_snoise@ub = c(rep(25, 4), rep(999, 2))
model_snoise@lb = c(rep(-25, 4), rep(-999, 2))

#Extract parameter names to set ub and lb (optional)
#model_snoise$param.names

#Need to set path explicitly in unattended R session --> 
#using local bin because gsl couldn't be downloaded the conventional way due to IT constraints
#copied from interactive R session
#Sys.setenv(PATH = "/Users/ams939/bin/homebrew/bin/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")
Sys.setenv(PATH = "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")

#results of state noise model
res_sonly <-
  tryCatch(
    dynr.cook(model_snoise),
    error = function(e) {
      print(e)
      return(NULL)
    }
  )

res_sonly_failed <- ifelse(is.null(res_sonly), TRUE, FALSE)

summary(res_sonly)
coef(res_sonly)

# get the log likelihood, AIC, and BIC from a cooked model/data combo
logLik(res_sonly)
AIC(res_sonly)
BIC(res_sonly)

# p1 = dynr.ggplot(dynrModel = model_snoise, res = res_sonly, numSubjDemo=1,
#     names.state=c("Patient","Partner"),
#     title="Results from RS-linear ODE model")
