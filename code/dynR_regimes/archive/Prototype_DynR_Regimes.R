library(dynr)
library(reshape2)
library(ggplot2)
setwd("/Users/michael/Tresors/Couples_Interaction")

df <- read.table("/Users/michael/Tresors/Couples_Interaction/example_dyad_ibis_caid.txt",
    col.names=c(strsplit("l_ibi_interp_detrend r_ibi_interp_detrend l_dom_interp_detrend r_dom_interp_detrend l_aff_interp_detrend r_aff_interp_detrend", "\\s+")[[1L]]))

df$id <- "Couple1" #just a single subject for now
df$time <- seq(0.1, nrow(df)/10, by=0.1)

#visualize this couple
mdf <- melt(df, id.vars=c("id", "time"))
ggplot(mdf, aes(x=time, y=value)) + facet_grid(variable~., scales="free_y") + geom_line() + theme_bw(base_size=20)


data <- dynr.data(df, id="id", time="time", 
    observed=c("l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend"))

#zero-order correlations
cor(df[,c("l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")])

#standardize time series over time to reduce variance
df[,c("l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")] <- 
    lapply(df[,c("l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")], scale)

# Measurement (factor loadings)
# No measurement model for now (just a fixed 1.0 loading)
meas <- prep.loadings(
    map=list(
        eta1="l_dom_interp_detrend",
        eta2="r_dom_interp_detrend",
        eta3="l_aff_interp_detrend",
        eta4="r_aff_interp_detrend"),
    params = NULL)


#initial conditions for states (dom + aff) and covariance
#just using true initial conditions at the moment
initial <- prep.initial(
    #values.inistate=c(0, 0, 0, 0),
    values.inistate=c(df$l_dom_interp_detrend[1], 
        df$r_dom_interp_detrend[1], 
        df$l_aff_interp_detrend[1], 
        df$r_aff_interp_detrend[1]),
    params.inistate=c("fixed", "fixed", "fixed", "fixed"),
    #values.inicov=diag(diag(cov(df[,c("l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")])), 4),
    #params.inicov=diag("fixed",4) #does this not allow for covariance in the signals?
    #values.inicov=diag(10, 4),
    values.inicov=diag(1, 4),
    params.inicov=diag(c("vv1", "vv2", "vv3", "vv4"),4) #free initial variances for hidden states
    #values.regimep=c(1, 0), #two regimes -- forced to start out in first
    #params.regimep=c("fixed", "fixed")
)

#measurement and dynamics covariances
mdcov <- prep.noise(
    values.latent=diag(.01, 4), #no noise in latent states (since there is a direct mapping from observed)
    #params.latent=diag(rep("fixed", 4), 4),
    params.latent=diag(c("ss1", "ss2", "ss3", "ss4"), 4),
    #values.observed=diag(rep(10, 4)), #measurement noise in signals
    values.observed=diag(rep(1, 4)), #measurement noise in signals
    params.observed=diag(c("sigmasq_e1","sigmasq_e2","sigmasq_e3","sigmasq_e4"),4))

#omitting regime-switching piece for now

# dynamics (evolution functions)
# starting out with only dom-dom and aff-aff coupling
# Time series have been detrended, so probable don't need a homeostasis point for the process at the moment
fcoupled=list(
    list(eta1 ~ -r1*eta1 + r2*eta2,
        eta2 ~ -r3*eta2 + r4*eta1,
        eta3 ~ -r5*eta3 + r6*eta4,
        eta4 ~ -r7*eta4 + r8*eta3))

svals <- rep(0.01, 8)
names(svals) <- paste0("r", 1:8)
dynm  <- prep.formulaDynamics(formula=fcoupled,
    startval=svals, isContinuousTime=FALSE) #,jacobian=jacob


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

model <- dynr.model(dynamics=dynm, measurement=meas,
    noise=mdcov, initial=initial, 
    outfile="RSODEmodelRecipe.c")

#Extract parameter names to set ub and lb (optional)
model$param.names

#Use the `@' sign to set upper and lower boundaries for the parameters
#model@ub=c(rep(1.5, 4), 200, 5, 5, rep(30, 6))
#model@lb=c(rep(-20, 4), 50, -10, -10, rep(-30, 6))

#cat(writeCcode(model$dynamics)$c.string)

# Estimate free parameters

#Need to set path explicitly in unattended R session
#copied from interactive R session
Sys.setenv(PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")
res <- dynr.cook(model, data=data)

summary(res)

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
#plot(res,data.dynr = data,model=model)
