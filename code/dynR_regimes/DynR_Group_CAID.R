#extend prototype to group fitting of CAID
library(dynr)
library(reshape2)

basedir <- "/Users/michael/Tresors/Couples_Interaction"
#basedir <- "/Users/mnh5174/Couples_Interaction"

setwd(basedir)
load(file.path(basedir, "data", "align_split_atdifferentfreqs.RData")) #contains align_split list at 4, 2, 1.5, and 1Hz

f_centerPredictors <- function(df, predictors, scale=FALSE, addsuffix=NULL) {
  for (predictor in predictors) {
    if (!is.null(addsuffix)) { predout <- paste0(predictor, addsuffix) } else { predout <- predictor }
    if (!grepl(":", predictor, fixed=TRUE) && is.numeric(df[[predictor]])) df[[predout]] <- as.vector(scale(df[[predictor]], center=TRUE, scale=scale))
  }
  return(df)
}

#add simple time column
align_split_1Hz <- lapply(align_split_1Hz, function(subject) { subject$time <- seq(0, nrow(subject)-1, by=1); return(subject) }) 
df <- do.call(rbind, align_split_1Hz)

couple1 <- align_split_1Hz[[1]]
mdf <- melt(couple1[,c("ms", "l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")], id.vars="ms")

ggplot(mdf, aes(x=ms, y=value)) + geom_line() + facet_wrap(~variable, ncol=1, scales="free_y")

caidvars <- c("l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")
df <- f_centerPredictors(df, caidvars, scale=TRUE) #z-score CAID variables

data <- dynr.data(df, id="PTNUM", time="time", 
    observed=caidvars)

#zero-order correlations
cor(df[,caidvars])

#setup measurement model (factor loadings)
#because we don't have a measurement model per se, just use fixed 1.0 state -> item loadings
meas <- prep.measurement(
    values.load=diag(1,4), # starting values and fixed values
    params.load=diag(rep("fixed",4),4),
    state.names=paste0("eta",1:4),
    obs.names=caidvars)

#initial values of hidden states (signals)
#should we be estimating these?
initial <- prep.initial(
    values.inistate=c(0, 0, 0, 0),
    params.inistate=c("fixed", "fixed", "fixed", "fixed"),
    #params.inistate=c("X0_1", "X0_2", "X0_3", "X0_4"),
    #values.inicov=diag(1, 4),
    #params.inicov=diag(c("vv1", "vv2", "vv3", "vv4"), 4)
    values.inicov=diag(1, 4),
    params.inicov=diag(rep("fixed",4),4)
    #values.regimep=c(1, 0), #two regimes -- forced to start out in first
    #params.regimep=c("fixed", "fixed")
)

#measurement and dynamics covariances
mdcov <- prep.noise(
    values.latent=diag(1, 4), #noise in hidden states
    params.latent=diag(c("sigmasq_e1","sigmasq_e2","sigmasq_e3","sigmasq_e4"),4),
    values.observed=diag(rep(0, 4)), #no measurement noise in signals
    params.observed=diag("fixed", 4))

# dynamics (evolution functions)
# starting out with only dom-dom and aff-aff coupling
# Time series have been detrended, so probably don't need a homeostasis point for the process at the moment
fcoupled=list(
    list(eta1 ~ r1*eta1 + r5*eta2,
        eta2 ~  r2*eta2 + r6*eta1,
        eta3 ~  r3*eta3 + r7*eta4,
        eta4 ~  r4*eta4 + r8*eta3))

#start with negative prior on dominance coupling and positive for affiliation coupling
svals <- c(rep(.5, 4),rep(-.1,2), rep(.1,2))
names(svals) <- paste0("r", 1:8)
dynm  <- prep.formulaDynamics(formula=fcoupled,
    startval=svals, isContinuousTime=FALSE)

#try the 0..1 logit transform for autoregressive terms
#this assumes there is always positive autocorrelation
trans<-prep.tfun(formula.trans=list(r1 ~  exp(r1)/(1+exp(r1)), 
        r2 ~  exp(r2)/(1+exp(r2)),
        r3 ~  exp(r3)/(1+exp(r3)),
        r4 ~  exp(r4)/(1+exp(r4))),
    formula.inv=list(r1 ~ log(r1/(1-r1)),
        r2 ~ log(r2/(1-r2)),
        r3 ~ log(r3/(1-r3)),
        r4 ~ log(r4/(1-r4)))
)

model <- dynr.model(dynamics=dynm, measurement=meas,
    noise=mdcov, initial=initial, data=data, transform=trans,
    outfile="VAR.c")

model$param.names

#Use the `@' sign to set upper and lower boundaries for the parameters

#-25 -- 25 for logit-transformed parameters
model@ub=c(rep(25, 4), rep(2.5, 4), rep(999,4)) #rather than allow broad constraints, can use variance of observed data as bound on error variance
model@lb=c(rep(-25, 4), rep(-2.5, 4), rep(-999,4))

#@ applies constraint to untransformed parameter
#$ applies constraint to transformed parameter

#Need to set path explicitly in unattended R session
#copied from interactive R session
Sys.setenv(PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")
res <- dynr.cook(model)

summary(res)

coef(res)

# get the log likelihood, AIC, and BIC from a cooked model/data combo
logLik(res)
AIC(res)
BIC(res)
