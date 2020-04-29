require(gsl)
library(ggplot2)
library(dynr)
library(dplyr)
library(tidyr)

#loop scaffolding
# for (i in 1:length(align_split_1Hz)) {
#   df <- align_split_1Hz[[i]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend")]
#   fitdynrCouple(df)
#   if (i > 10) break
# }


basedir <- "~/Box Sync/DEPENd/Couples Data/data"
setwd(basedir)
#basedir <- "/Users/mnh5174/Couples_Interaction"
#load("~/Desktop/ams939/Couples Project/R/data/align_split_atdifferentfreqs.RData")
#
df <- read.table(file.path(basedir, "example_dyad_ibis_caid.txt"),
                 col.names=c(strsplit("l.ibi.interp.detrend r.ibi.interp.detrend l.dom.interp.detrend r.dom.interp.detrend l.aff.interp.detrend r.aff.interp.detrend", "\\s+")[[1L]]))

foo_real_data <- function (alpha) {

#have 4Hz in memory at the moment (trying to see whether we get convergence)

#10Hz timing
#df$time <- seq(0.1, nrow(df)/10, by=0.1) #The time index doesn't affect discrete-time models so any scaling is fine.

#df <- align_split_4Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
#df$time <- seq(0.25, nrow(df)/4, by=0.25)

#df <- align_split_2Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
#df$time <- seq(0.5, nrow(df)/2, by=0.5)

#df <- align_split_1Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
alpha$time <- seq(1, nrow(alpha)/1, by=1)

#this still doesn't work... doesn't converge, even with a lock on 0..1 autocorrelation
#df <- align_split_1.5Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
#df$time <- seq(1/1.5, nrow(df)/1.5, by=1/1.5)

names(alpha) <- gsub("_",".", names(alpha), fixed=TRUE) #for consistency with below
alpha$id <- "Couple1" #just a single subject for now

#visualize this couple
# mdf <- df %>% gather(key="variable", value="value", -time, -id)
# ggplot(mdf, aes(x=time, y=value)) + facet_grid(variable~.,scales = "free_y") + geom_line() + theme_bw(base_size=20)
# mdf <- melt(df, id.vars=c("id", "time"))
# dev.new(); ggplot(mdf, aes(x=time, y=value)) + facet_grid(variable~.,scales = "free_y") + geom_line() + theme_bw(base_size=20)

#z-score series
alpha[,c("l.ibi.interp.detrend", "r.ibi.interp.detrend","l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend")] <-
  lapply(alpha[,c("l.ibi.interp.detrend", "r.ibi.interp.detrend","l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend")], scale)

#mdf <- melt(df, id.vars=c("id", "time"))
#ggplot(mdf, aes(x=time, y=value)) + facet_grid(.~variable,scales = "free_y") + geom_line() + theme_bw(base_size=20)

#downsampling by a factor of 10 (Sy-Miin's modification to get convergence). 10Hz -> 1Hz
# timeNum <- round(length(alpha[[1]])/10)
# time2 = rep(1:timeNum,each=10)
# 
# agg = function(x){x=aggregate(x,by=list(time2),mean);return(x[,2])}
# tmp = agg(alpha[,1])
# tmp = sapply(alpha[,1:6],agg)
# alpha=data.frame(tmp)
# alpha$time=unique(time2)
# alpha$id = rep(1,length(alpha$time))

data <- dynr.data(alpha, id="id", time="time",
                  observed=c("l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend"))

#zero-order correlations
cor(alpha[,c("l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend")])

# Measurement (factor loadings)
# No measurement model for now (just a fixed 1.0 loading)
#meas <- prep.loadings(
#    map=list(
#        ldom="l.dom.interp.detrend",
#        rdom="r.dom.interp.detrend",
#        laff="l.aff.interp.detrend",
#        raff="r.aff.interp.detrend"),
#    params = NULL)

#This has more functionalities
meas <- prep.measurement(
  values.load=diag(1,4), # starting values and fixed values
  params.load=diag(rep("fixed",4),4),
  state.names=c("ldom", "rdom", "laff", "raff"),
  obs.names=c("l.dom.interp.detrend",
              "r.dom.interp.detrend",
              "l.aff.interp.detrend",
              "r.aff.interp.detrend")) # parameter numbers or indication that parameter is fixed


#initial conditions for states (dom + aff) and covariance
#just using true initial conditions at the moment

#c(df$l.dom.interp.detrend[1],
#df$r.dom.interp.detrend[1],
#df$l.aff.interp.detrend[1],
#df$r.aff.interp.detrend[1])

initial <- prep.initial(
  values.inistate=c(0, 0, 0, 0),
  params.inistate=c("fixed", "fixed", "fixed", "fixed"),
  #values.inicov=diag(diag(cov(df[,c("l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend")])), 4),
  #params.inicov=diag("fixed",4) #does this not allow for covariance in the signals?
  values.inicov=diag(1, 4),
  params.inicov=diag(rep("fixed",4),4)
  #params.inicov=diag(c("vv1", "vv2", "vv3", "vv4"),4) #free initial variances for hidden states
  #values.regimep=c(1, 0), #two regimes -- forced to start out in first
  #params.regimep=c("fixed", "fixed")
)

#I don't think the initial condition variances are identifiable with single-subject data

#measurement and dynamics covariances

#alternative with measurement noise only
mdcov_monly <- prep.noise(
  values.latent=diag(0, 4), #no noise in latent states (since there is a direct mapping from observed)
  params.latent=diag("fixed",4),
  values.observed=diag(rep(0.1, 4)), #measurement noise in signals
  params.observed=diag(c("mn1", "mn2", "mn3", "mn4"), 4))

mdcov_sonly <- prep.noise(
  values.latent=diag(1, 4), #process noise only
  params.latent=diag(c("sn1","sn2","sn3","sn4"),4),
  values.observed=diag(rep(0, 4)), #no measurement noise in signals
  params.observed=diag("fixed", 4))

#alternative with process and measurement noise
mdcov_both <- prep.noise(
  values.latent=diag(1, 4), #allow for noisy states
  params.latent=diag(c("sn1", "sn2", "sn3","sn4"),4),
  values.observed=diag(rep(0.1, 4)), #measurement noise in signals
  params.observed=diag(c("mn1", "mn2", "mn3", "mn4"), 4))


#omitting regime-switching piece for now

# dynamics (evolution functions)
# starting out with only dom-dom and aff-aff coupling
# Time series have been detrended, so probable don't need a homeostasis point for the process at the moment
fcoupled=list(
  list(ldom ~ p_ldomself*ldom + p_rldom*rdom,
       rdom ~  p_rdomself*rdom + p_lrdom*ldom,
       laff ~  p_laffself*laff + p_rlaff*raff,
       raff ~  p_raffself*raff + p_lraff*laff))

# fcoupled=list(
#   list(ldom ~ p_ldomself*ldom + p_rldom*rdom + r9*laff (coupling of my dominance with my affiliation)+ p_ldomself0*raff (my dominance with your affiliation),
#        rdom ~  p_rdomself*rdom + p_lrdom*ldom,
#        laff ~  p_laffself*laff + p_rlaff*raff,
#        raff ~  p_raffself*raff + p_lraff*laff))

#start values: strong self-coupling, mild anticorrelation for dom and positive correlation for aff
svals <- c(rep(.5, 4), rep(-0.1,2), rep(0.1,2))
names(svals) <- c("p_ldomself", "p_rdomself", "p_laffself", "p_raffself", "p_rldom", "p_lrdom", "p_rlaff", "p_lraff")
dynm  <- prep.formulaDynamics(formula=fcoupled,
                              startval=svals, isContinuousTime=FALSE) #,jacobian=jacob


#not using transformation of parameters at the moment (unbounded, untransformed)
#parameters should be -1 -- 1, but probably small and close to zero.
#trans<-prep.tfun(formula.trans=list(p_ldomself~exp(p_ldomself),
#        p_rdomself~exp(p_rdomself),
#        a12~exp(a12),
#        a21~exp(a21)),
#    formula.inv=list(p_ldomself~log(p_ldomself),
#        p_rdomself~log(p_rdomself),
#        a12~log(a12),
#        a21~log(a21)))

#try the 0..1 logit transform for autoregressive terms
#this assumes there is always positive autocorrelation
trans<-prep.tfun(formula.trans=list(p_ldomself ~  exp(p_ldomself)/(1+exp(p_ldomself)),
                                    p_rdomself ~  exp(p_rdomself)/(1+exp(p_rdomself)),
                                    p_laffself ~  exp(p_laffself)/(1+exp(p_laffself)),
                                    p_raffself ~  exp(p_raffself)/(1+exp(p_raffself))),
                 formula.inv=list(p_ldomself ~ log(p_ldomself/(1-p_ldomself)),
                                  p_rdomself ~ log(p_rdomself/(1-p_rdomself)),
                                  p_laffself ~ log(p_laffself/(1-p_laffself)),
                                  p_raffself ~ log(p_raffself/(1-p_raffself)))
)


noise_model <- "ms_noise"

if (noise_model=="mnoise_only") {
  mdcov = mdcov_monly
  
  #noise upper and lower bound in optimization
  noiselb <- rep(-999,4)
  noiseub <- rep(999,4) 
} else if (noise_model == "snoise_only") {
  mdcov = mdcov_sonly
  
  #noise upper and lower bound in optimization
  noiselb <- rep(-999,4)
  noiseub <- rep(999,4) 
} else if (noise_model == "ms_noise") {
  mdcov = mdcov_both
  #noise upper and lower bound in optimization
  noiselb <- rep(-999,8)
  noiseub <- rep(999,8) 
  
}

model_mnoise <- dynr.model(dynamics=dynm, measurement=meas,
                           noise=mdcov_monly, initial=initial, data=data, transform=trans,
                           outfile="VAR_mnoise.c")

model_mnoise@ub=c(rep(25, 8), rep(999, 4))
model_mnoise@lb=c(rep(-25, 8), rep(-999, 4))


model_snoise <- dynr.model(dynamics=dynm, measurement=meas,
                           noise=mdcov_sonly, initial=initial, data=data, transform=trans,
                           outfile="VAR_snoise.c")

model_snoise@ub=c(rep(25, 8), rep(999, 4))
model_snoise@lb=c(rep(-25, 8), rep(-999, 4))

model_bothnoise <- dynr.model(dynamics=dynm, measurement=meas,
                              noise=mdcov_both, initial=initial, data=data, transform=trans,
                              outfile="VAR_bothnoise.c")

model_bothnoise@ub=c(rep(25, 8), rep(999, 8))
model_bothnoise@lb=c(rep(-25, 8), rep(-999, 8))


#Extract parameter names to set ub and lb (optional)
model_snoise$param.names
model_mnoise$param.names
model_bothnoise$param.names

#Use the `@' sign to set upper and lower boundaries for the parameters
#model@ub=c(rep(2.5, 8), rep(999,4))
#model@lb=c(rep(-2.5, 8), rep(-999,4))

#model@ub=c(rep(25, 8), noiseub)
#model@lb=c(rep(-25, 8), noiselb)

#printex(model,ParameterAs=model$param.names,show=FALSE,printInit=TRUE,
#        outFile=file.path(basedir, "LinearTime.tex"))
#tools::texi2pdf(file.path(basedir, "LinearTime.tex"))
#system(paste(getOption("pdfviewer"), file.path(basedir,"LinearTime.pdf")))

#cat(writeCcode(model$dynamics)$c.string)

# Estimate free parameters

#Need to set path explicitly in unattended R session
#copied from interactive R session
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

#res different --> to attempt continuity between files, I've used the res_sonly because that's what was used in previous analyses
coef(res_sonly)

# get the log likelihood, AIC, and BIC from a cooked model/data combo
logLik(res_sonly)
AIC(res_sonly)
BIC(res_sonly)

#---- Dynr decor ----
#p1 = dynr.ggplot(res, model, idtoPlot=1)

#, states=c(1:4),
#    names.regime=c("Exploration","Proximity-seeking"),
#    names.state=c("l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend"),
#    title="Results from CAID Prototype", idtoPlot = 1)#, numSubjDemo=1) #,idtoPlot=c(1,2),
#    shape.values = c(1,2),
#    text=element_text(size=16))
#
# print(p1)

plot(res_sonly,dynrModel=model_snoise,names.state=meas$state.names)

#observed versus predicted
#plot(res,dynrModel=model,names.state=meas$state.names, style=2)

#manual plot of observed versus expected
observed <- alpha[,c("l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend")]
predicted <- data.frame(t(res_sonly@eta_smooth_final))
names(predicted) <- names(observed)
predicted$type=factor("predicted")
observed$type=factor("observed")
predicted$time <- 1:nrow(predicted)
observed$time <- 1:nrow(observed)

both <- rbind(predicted, observed)

forplotting <- both %>% gather(key="signal", value="caidval", -type, -time)

g <- ggplot(forplotting, aes(x=time, y=caidval, color=type)) + geom_line() + 
  facet_wrap(~signal, ncol=1, scales="free_y") + ggtitle("Observed versus expected")
plot(g)

str(res_sonly)
str(res_monly)
str(res_bothnoise)

}
#preprocessing 1Hz data so that just the last 6 columns
align_split_1Hz_preprocess <- lapply(align_split_1Hz, function(x) {
  select(x, ends_with("detrend"))
}) 
str(align_split_1Hz_preprocess)

foo_real_data(df)


foo_real_data(align_split_1Hz_preprocess[[1]])
for (i in 1:length(align_split_1Hz_preprocess)) {
    df <- align_split_1Hz_preprocess[[i]]
    foo_real_data(df)
   if (i > 10) break
 }


(initialAnalyses <- lapply(align_split_1Hz_preprocess, foo_real_data))
