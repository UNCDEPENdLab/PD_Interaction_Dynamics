
require(gsl)
library(ggplot2)
library(dynr)
library(dplyr)
library(tidyr)
basedir <- "~/Box Sync/DEPENd/Couples Data/data"
setwd(basedir)
load("align_split_atdifferentfreqs.RData")
# ?read.table
# str(dat)
# str(align_split_2Hz)
# ?cor
# ?data.frame
# smallDf <- data.frame(align_split_4Hz$l_ibi_interp_detrend, align_split_4Hz$r_ibi_interp_detrend) 
# ?subset
# #dataFile <- read.table(file.path(basedir, align_split_2Hz),
#                col.names=c(strsplit("l.ibi.interp.detrend r.ibi.interp.detrend", "\\s+")[[1L]]))
#load("~/Box Sync/DEPENd/Couples Data/data/psychophys_log_7May2015.RData")
#str()

dyn_bivariate <- function(df)  {
  numVars = 2
  df$time <- seq(1,nrow(df)/1, by =1 )
  names(df) <- gsub("_", ".", names(df), fixed = TRUE)
  df$id <- "Example"
  #plot once %>% figured out
  mdf <- df %>% gather(key="variable", value="value", -time, -id)
  ggplot(mdf, aes(x=time, y=value)) + facet_grid(variable~.,scales = "free_y") + geom_line() + theme_bw(base_size=20)
  
  df[,c("l.ibi.interp.detrend", "r.ibi.interp.detrend")] <-
    lapply(df[,c("l.ibi.interp.detrend", "r.ibi.interp.detrend")], scale)
  
  
  time2 = rep(1:(round(length(df[[1]])/10),each=10)

   agg = function(x){x=aggregate(x,by=list(time2),mean);return(x[,2])}
   tmp = agg(df[,1])
   tmp = sapply(df[,1:2],agg)
   df=data.frame(tmp)
   df$time=unique(time2)
   df$id = rep(1,length(df$time))

  
  data <- dynr.data(df, id="id", time="time",
                    observed=c("l_ibi_interp_detrend", "r_ibi_interp_detrend"))
  
  cor(df[,c("l_ibi_interp_detrend", "r_ibi_interp_detrend")])
  meas <- prep.measurement(
    values.load=diag(1,numVars), # starting values and fixed values
    params.load=diag(rep("fixed",numVars),numVars),
    state.names=c("libi", "ribi"),
    obs.names=c("l_ibi_interp_detrend","r_ibi_interp_detrend")) # parameter numbers or indication that parameter is fixed
  
  initial <- prep.initial(
    values.inistate=c(0, 0),
    params.inistate=c("fixed", "fixed", "fixed", "fixed"),
    #values.inicov=diag(diag(cov(df[,c("l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend")])), 4),
    #params.inicov=diag("fixed",4) #does this not allow for covariance in the signals?
    values.inicov=diag(1, 2),
    params.inicov=diag(rep("fixed",2),2)
    #params.inicov=diag(c("vv1", "vv2", "vv3", "vv4"),4) #free initial variances for hidden states
    #values.regimep=c(1, 0), #two regimes -- forced to start out in first
    #params.regimep=c("fixed", "fixed")
  )
  
  mdcov_monly <- prep.noise(
    values.latent=diag(0, numVars), #no noise in latent states (since there is a direct mapping from observed)
    params.latent=diag("fixed",numVars),
    values.observed=diag(rep(0.1, numVars)), #measurement noise in signals
    params.observed=diag(c("mn1", "mn2"), numVars))
  
  mdcov_sonly <- prep.noise(
    values.latent=diag(1, numVars), #process noise only
    params.latent=diag(c("sn1","sn2"), numVars),
    values.observed=diag(rep(0, numVars)), #no measurement noise in signals
    params.observed=diag("fixed", numVars))
  
  #alternative with process and measurement noise
  mdcov_both <- prep.noise(
    values.latent=diag(1, 2*numVars), #allow for noisy states
    params.latent=diag(c("sn1", "sn2"),numVars),
    values.observed=diag(rep(0.1, numVars)), #measurement noise in signals
    params.observed=diag(c("mn1", "mn2"),numVars))
  
  fcoupled=list(
    list(libi ~ p_libiself*libi + p_rlibi*ribi,
         ribi ~  p_ribiself*ribi + p_lribi*libi))
  
  svals <- c(rep(.5, 4), rep(-0.1,2), rep(0.1,2))
  names(svals) <- c("p_libiself", "p_ribiself", "p_rlibi", "p_lribi")
  dynm  <- prep.formulaDynamics(formula=fcoupled,
                                startval=svals, isContinuousTime=FALSE)
  
  trans<-prep.tfun(formula.trans=list(p_libiself ~  exp(p_libiself)/(1+exp(p_libiself)),
                                      p_ribiself ~  exp(p_ribiself)/(1+exp(p_ribiself))),
                   formula.inv=list(p_libiself ~ log(p_libiself/(1-p_libiself)),
                                    p_ribiself ~ log(p_ribiself/(1-p_ribiself)))
  )
  
  
  noise_model <- "ms_noise"
  
  
  if (noise_model=="mnoise_only") {
    mdcov = mdcov_monly
    
    #noise upper and lower bound in optimization
    noiselb <- rep(-999,numVars)
    noiseub <- rep(999,numVars) 
  } else if (noise_model == "snoise_only") {
    mdcov = mdcov_sonly
    
    #noise upper and lower bound in optimization
    noiselb <- rep(-999,numVars)
    noiseub <- rep(999,numVars) 
  } else if (noise_model == "ms_noise") {
    mdcov = mdcov_both
    #noise upper and lower bound in optimization
    noiselb <- rep(-999,2*numVars)
    noiseub <- rep(999,2*numVars) 
    
  }
  
  model_mnoise <- dynr.model(dynamics=dynm, measurement=meas,
                             noise=mdcov_monly, initial=initial, data=data, transform=trans,
                             outfile="VAR_mnoise.c")
  
  model_mnoise@ub=c(rep(25, 2*numVars), rep(999, numVars))
  model_mnoise@lb=c(rep(-25, 2*numVars), rep(-999, numVars))
  
  
  model_snoise <- dynr.model(dynamics=dynm, measurement=meas,
                             noise=mdcov_sonly, initial=initial, data=data, transform=trans,
                             outfile="VAR_snoise.c")
  
  model_snoise@ub=c(rep(25, 2*numVals), rep(999, numVals))
  model_snoise@lb=c(rep(-25, 2*numVals), rep(-999, numVals))
  
  model_bothnoise <- dynr.model(dynamics=dynm, measurement=meas,
                                noise=mdcov_both, initial=initial, data=data, transform=trans,
                                outfile="VAR_bothnoise.c")
  
  model_bothnoise@ub=c(rep(25, 2*numVals), rep(999, 2*numVals))
  model_bothnoise@lb=c(rep(-25, 2*numVals), rep(-999, 2*numVals))
  
  
  #Extract parameter names to set ub and lb (optional)
  model_snoise$param.names
  
  Sys.setenv(PATH="/Users/ams939/bin/homebrew/bin/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")
  res_monly <- dynr.cook(model_mnoise)
  res_sonly <- dynr.cook(model_snoise)
  res_bothnoise <- dynr.cook(model_bothnoise)
  
  AIC(res_monly)
  AIC(res_sonly)
  AIC(res_bothnoise)
  
  summary(res_monly)
  summary(res_sonly)
  summary(res_bothnoise) #doesn't add beyond state only
  
  coef(res_sonly)
  
  # get the log likelihood, AIC, and BIC from a cooked model/data combo
  logLik(res_sonly)
  AIC(res_sonly)
  BIC(res_sonly)
  
  plot(res_sonly,dynrModel=model_snoise,names.state=meas$state.names)
  
}
example <- read.table(file.path(basedir, "example_dyad_ibis.txt"), col.names = c(strsplit("l.ibi.interp.detrend r.ibi.interp.detrend", "\\s+") [[1L]]))
str(example)
 (dyn_bivariate(example))


