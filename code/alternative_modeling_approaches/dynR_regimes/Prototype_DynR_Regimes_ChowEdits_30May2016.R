

library(ggplot2)
library(dynr)
library(reshape2)
#setwd("~/Desktop/ams939/Couples Project/R")
#df <- read.table("/Users/michael/Tresors/Couples_Interaction/example_dyad_ibis_caid.txt",
#    col.names=c(strsplit("l.ibi.interp.detrend r.ibi.interp.detrend l.dom.interp.detrend r.dom.interp.detrend l.aff.interp.detrend r.aff.interp.detrend", "\\s+")[[1L]]))

basedir <- "~/Box_Sync/DEPENd/Couples Data/data"
setwd(basedir)
#basedir <- "/Users/mnh5174/Couples_Interaction"
#load("~/Desktop/ams939/Couples Project/R/data/align_split_atdifferentfreqs.RData")
#
df <- read.table(file.path(basedir, "example_dyad_ibis_caid.txt"),
                 col.names = c(
                   strsplit(
                     "l.ibi.interp.detrend r.ibi.interp.detrend l.dom.interp.detrend r.dom.interp.detrend l.aff.interp.detrend r.aff.interp.detrend",
                     "\\s+"
                   )[[1L]]
                 ))
# df <- align_split_4Hz[[1]]
#have 4Hz in memory at the moment (trying to see whether we get convergence)

#10Hz timing
#df$time <- seq(0.1, nrow(df)/10, by=0.1) #The time index doesn't affect discrete-time models so any scaling is fine.

#df <- align_split_4Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
#df$time <- seq(0.25, nrow(df)/4, by=0.25)

#df <- align_split_2Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
#df$time <- seq(0.5, nrow(df)/2, by=0.5)

#df <- align_split_1Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
df$time <- seq(1, nrow(df) / 1, by = 1)

#this still doesn't work... doesn't converge, even with a lock on 0..1 autocorrelation
#df <- align_split_1.5Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
#df$time <- seq(1/1.5, nrow(df)/1.5, by=1/1.5)

names(df) <-
  gsub("_", ".", names(df), fixed = TRUE) #for consistency with below
df$id <- "Couple1" #just a single subject for now

#visualize this couple

#visualize all couples
library(tidyr)
library(dplyr)
mas4hz <- lapply(align_split_4Hz, function(couple) {
  couple %>% select(ms, l_ibi_interp_detrend, r_ibi_interp_detrend) %>% mutate(time =
                                                                                 ms / 1000) %>% select(-ms) %>% gather(key = "signal", value = "value",-time)
})


pdf("plots.pdf")
lapply(1: length(mas4hz), function(e) {
  ggplot(mas4hz[[e]], aes (x = time, y = value)) + facet_grid(signal~., scales = "free_y") + geom_line() + theme_bw(base_size = 20) + ggtitle(names(mas4hz)[e])

})
dev.off()


#z-score series
df[, c(
  "l.ibi.interp.detrend",
  "r.ibi.interp.detrend",
  "l.dom.interp.detrend",
  "r.dom.interp.detrend",
  "l.aff.interp.detrend",
  "r.aff.interp.detrend"
)] <-
  lapply(df[, c(
    "l.ibi.interp.detrend",
    "r.ibi.interp.detrend",
    "l.dom.interp.detrend",
    "r.dom.interp.detrend",
    "l.aff.interp.detrend",
    "r.aff.interp.detrend"
  )], scale)

mdf <- melt(df, id.vars = c("id", "time"))
ggplot(mdf, aes(x = time, y = value)) + facet_grid(. ~ variable, scales = "free_y") + geom_line() + theme_bw(base_size =
                                                                                                               20)

#downsampling by a factor of 10 (Sy-Miin's modification to get convergence). 10Hz -> 1Hz
time2 = rep(1:602, each = 10)

agg = function(x) {
  x = aggregate(x, by = list(time2), mean)
  return(x[, 2])
}
tmp = agg(df[, 1])
tmp = sapply(df[, 1:6], agg)
df = data.frame(tmp)
df$time = unique(time2)
df$id = rep(1, length(df$time))

data <- dynr.data(
  df,
  id = "id",
  time = "time",
  observed = c(
    "l.dom.interp.detrend",
    "r.dom.interp.detrend",
    "l.aff.interp.detrend",
    "r.aff.interp.detrend"
  )
)

#zero-order correlations
cor(df[, c(
  "l.dom.interp.detrend",
  "r.dom.interp.detrend",
  "l.aff.interp.detrend",
  "r.aff.interp.detrend"
)])

# Measurement (factor loadings)
# No measurement model for now (just a fixed 1.0 loading)
#meas <- prep.loadings(
#    map=list(
#        eta1="l.dom.interp.detrend",
#        eta2="r.dom.interp.detrend",
#        eta3="l.aff.interp.detrend",
#        eta4="r.aff.interp.detrend"),
#    params = NULL)

#This has more functionalities
meas <- prep.measurement(
  values.load = diag(1, 4),
  # starting values and fixed values
  params.load = diag(rep("fixed", 4), 4),
  state.names = paste0("eta", 1:4),
  obs.names = c(
    "l.dom.interp.detrend",
    "r.dom.interp.detrend",
    "l.aff.interp.detrend",
    "r.aff.interp.detrend"
  )
) # parameter numbers or indication that parameter is fixed
# Look

#initial conditions for states (dom + aff) and covariance
#just using true initial conditions at the moment

#c(df$l.dom.interp.detrend[1],
#df$r.dom.interp.detrend[1],
#df$l.aff.interp.detrend[1],
#df$r.aff.interp.detrend[1])

initial <- prep.initial(
  values.inistate = c(0, 0, 0, 0),
  params.inistate = c("fixed", "fixed", "fixed", "fixed"),
  #values.inicov=diag(diag(cov(df[,c("l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend")])), 4),
  #params.inicov=diag("fixed",4) #does this not allow for covariance in the signals?
  values.inicov = diag(1, 4),
  params.inicov = diag(rep("fixed", 4), 4)
  #params.inicov=diag(c("vv1", "vv2", "vv3", "vv4"),4) #free initial variances for hidden states
  #values.regimep=c(1, 0), #two regimes -- forced to start out in first
  #params.regimep=c("fixed", "fixed")
)

#I don't think the initial condition variances are identifiable with single-subject data

#measurement and dynamics covariances
mdcov <- prep.noise(
  values.latent = diag(1, 4),
  #no noise in latent states (since there is a direct mapping from observed)
  params.latent = diag(c(
    "sigmasq_e1", "sigmasq_e2", "sigmasq_e3", "sigmasq_e4"
  ), 4),
  values.observed = diag(rep(0, 4)),
  #measurement noise in signals
  params.observed = diag("fixed", 4)
)

#omitting regime-switching piece for now

# dynamics (evolution functions)
# starting out with only dom-dom and aff-aff coupling
# Time series have been detrended, so probable don't need a homeostasis point for the process at the moment
fcoupled = list(
  list(
    eta1 ~ r1 * eta1 + r5 * eta2,
    eta2 ~  r2 * eta2 + r6 * eta1,
    eta3 ~  r3 * eta3 + r7 * eta4,
    eta4 ~  r4 * eta4 + r8 * eta3
  )
)

svals <- c(rep(.5, 4), rep(-.1, 2), rep(0.1, 2))
names(svals) <- paste0("r", 1:8)
dynm  <- prep.formulaDynamics(formula = fcoupled,
                              startval = svals,
                              isContinuousTime = FALSE) #,jacobian=jacob


#not using transformation of parameters at the moment (unbounded, untransformed)
#parameters should be -1 -- 1, but probably small and close to zero.
#trans<-prep.tfun(formula.trans=list(r1~exp(r1),
#        r2~exp(r2),
#        a12~exp(a12),
#        a21~exp(a21)),
#    formula.inv=list(r1~log(r1),
#        r2~log(r2),
#        a12~log(a12),
#        a21~log(a21)))

#try the 0..1 logit transform for autoregressive terms
#this assumes there is always positive autocorrelation
trans <- prep.tfun(
  formula.trans = list(
    r1 ~  exp(r1) / (1 + exp(r1)),
    r2 ~  exp(r2) / (1 + exp(r2)),
    r3 ~  exp(r3) / (1 + exp(r3)),
    r4 ~  exp(r4) / (1 + exp(r4))
  ),
  formula.inv = list(r1 ~ log(r1 / (1 - r1)),
                     r2 ~ log(r2 / (1 - r2)),
                     r3 ~ log(r3 / (1 - r3)),
                     r4 ~ log(r4 / (1 - r4)))
)



model <- dynr.model(
  dynamics = dynm,
  measurement = meas,
  noise = mdcov,
  initial = initial,
  data = data,
  transform = trans,
  outfile = "VAR.c"
)

#printex(model,ParameterAs=model$param.names,show=FALSE,printInit=TRUE,
#        outFile=file.path(basedir, "LinearTime.tex"))
#tools::texi2pdf(file.path(basedir, "LinearTime.tex"))
#system(paste(getOption("pdfviewer"), file.path(basedir,"LinearTime.pdf")))

#Extract parameter names to set ub and lb (optional)
model$param.names

#Use the `@' sign to set upper and lower boundaries for the parameters
#model@ub=c(rep(2.5, 8), rep(999,4))
#model@lb=c(rep(-2.5, 8), rep(-999,4))

model@ub = c(rep(25, 8), rep(999, 4))
model@lb = c(rep(-25, 8), rep(-999, 4))


#cat(writeCcode(model$dynamics)$c.string)

# Estimate free parameters

#Need to set path explicitly in unattended R session
#copied from interactive R session
Sys.setenv(PATH = "/Users/ams939/bin/homebrew/bin/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")
res <- dynr.cook(model)

summary(res)
model
coef(res)

# get the log likelihood, AIC, and BIC from a cooked model/data combo
logLik(res)
AIC(res)
BIC(res)

#---- Dynr decor ----
#p1 = dynr.ggplot(res, data.dynr=data, states=c(1:2),
#    names.regime=c("Exploration","Proximity-seeking"),
#    names.state=c("Mom","Infant"),
#    title="Results from RS-linear ODE model", numSubjDemo=2,idtoPlot=c(1,2),
#    shape.values = c(1,2),
#    text=element_text(size=16))
#
#print(p1)

#plot(res,dynrModel=model,names.state=meas$state.names)
