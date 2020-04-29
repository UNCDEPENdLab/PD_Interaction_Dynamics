#packages needed for this script
library(ggplot2)
library(dynr)
library(dplyr)
library(tidyr)

#set working directory
basedir <- "~/Box Sync/DEPENd/Couples Data/data"
setwd(basedir)
load("align_split_atdifferentfreqs.RData")

#this function will use the dynr package for the multilevel modeling of "goodDat"
dynHRPrototype <- function (dataset, hz) {
  numVars = 2

  #this will specify the time
  dataset$time <- seq(1 / hz, nrow(dataset) / hz, by = 1 / hz)

  #substitute periods for underscores in variable names
  names(dataset) <- gsub("_", ".", names(dataset), fixed = TRUE) #for consistency with below

  #z-score all time detrended series
  dataset <- dataset %>% mutate_at(vars(l.ibi.interp.detrend, r.ibi.interp.detrend, 
                                    l.dom.interp.detrend, r.dom.interp.detrend, 
                                    l.aff.interp.detrend, r.aff.interp.detrend), 
                               funs(scale))

  data <- dynr.data(
    dataset,
    id = "PTNUM",
    time = "time",
    observed = c("l.ibi.interp.detrend", "r.ibi.interp.detrend")
  )

  #zero-order correlations
  cor(dataset[, c("l.ibi.interp.detrend", "r.ibi.interp.detrend")])

  #setting measurement model (single indicator latent variable with loading 1.0)
  meas <- prep.measurement(
    values.load = diag(1, 2),
    params.load = diag(rep("fixed", 2), 2),
    state.names = c("libi", "ribi"),
    obs.names = c("l.ibi.interp.detrend",
                  "r.ibi.interp.detrend")
  )

  initial <- prep.initial(
    values.inistate = c(0, 0),
    #zero starting value for states
    params.inistate = c("fixed", "fixed"),
    values.inicov = diag(0, 2),
    #zero covariance between starting values for states
    params.inicov = diag(rep("fixed", 2), 2)
  )

  #only looked at state noise; measurement noise and both measurement and state noise did not provide beter fit typically
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
  #using Steele and Ferrer parameterization
  fcoupled = list(list(
    libi ~ p_libiself * (0 - libi) + p_rlibi * (ribi - libi),
    ribi ~  p_ribiself * (0 - ribi) + p_lribi * (libi - ribi)
  ))

  #start values: strong self-coupling, mild anticorrelation for dom and positive correlation for aff
  svals <- c(rep(.5, 2), rep(-0.1, 1), rep(0.1, 1))
  names(svals) <-
    c("p_libiself", "p_ribiself", "p_rlibi", "p_lribi")
  dynm  <- prep.formulaDynamics(formula = fcoupled,
                                startval = svals,
                                isContinuousTime = FALSE) #,jacobian=jacob

  #This assumes there is always positive autocorrelation (not used at the moment)
  #This does not work for Steele and Ferrer variant where self coupling should be negative
  trans <-
    prep.tfun(
      formula.trans = list(
        p_libiself ~  exp(p_libiself) / (1 + exp(p_libiself)),
        p_ribiself ~  exp(p_ribiself) / (1 + exp(p_ribiself))
      ),
      formula.inv = list(
        p_libiself ~ log(p_libiself / (1 - p_libiself)),
        p_ribiself ~ log(p_ribiself / (1 - p_ribiself))
      )
    )

  noise_model <- "snoise_only"

  if (noise_model == "snoise_only") {
    mdcov = mdcov_sonly
    #noise upper and lower bound in optimization
    noiselb <- rep(-999, 2)
    noiseub <- rep(999, 2)
  }

  model_snoise <- dynr.model(
    dynamics = dynm,
    measurement = meas,
    noise = mdcov_sonly,
    initial = initial,
    data = data,
    outfile = "VAR_snoise.c"
  )

  model_snoise@ub = c(rep(25, 4), rep(999, 2))
  model_snoise@lb = c(rep(-25, 4), rep(-999, 2))

  #Extract parameter names to set ub and lb (optional)
  model_snoise$param.names

  #Need to set path explicitly in unattended R session --> 
  #using local bin because gsl couldn't be downloaded the conventional way due to IT constraints
  #copied from interactive R session
  Sys.setenv(PATH = "/Users/ams939/bin/homebrew/bin/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")

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

  if (res_sonly_failed) {
    plot_res_sonly <- ggplot()
  }
  else {
    plot(res_sonly,
         dynrModel = model_snoise,
         names.state = meas$state.names)

    for (res in c("res_sonly")) {
      observed <- dataset[, c("l.ibi.interp.detrend", "r.ibi.interp.detrend")]
      predicted <- data.frame(t(eval(parse(text = res))@eta_smooth_final))
      names(predicted) <- names(observed)
      predicted$type = factor("predicted")
      observed$type = factor("observed")
      predicted$time <- 1:nrow(predicted)
      observed$time <- 1:nrow(observed)
    }

    both <- rbind(predicted, observed)

    forplotting <- both %>% gather(key = "signal", value = "hrval", -type, -time)

    g <- ggplot(forplotting, aes(x = time, y = hrval, color = type)) + geom_line() +
      facet_wrap(~ signal, ncol = 1, scales = "free_y") + ggtitle(paste("Observed versus expected", res))
    assign(paste0("plot_", res), g)
  }
  return(list(
    s_only = list(
      model = model_snoise,
      result = res_sonly,
      plot = plot_res_sonly
    )
  ))
}


#is a subset of the total data, the couples including are relatively normal and representative
goodDat <-
  rbind(
    align_split_2Hz$`8017`,
    align_split_2Hz$`8059`,
    align_split_2Hz$`8069`,
    align_split_2Hz$`8086`,
    align_split_2Hz$`8099`
  )
#goodDat <- rbind(align_split_10Hz$`8017`, align_split_10Hz$`8059`, align_split_10Hz$`8069`, align_split_10Hz$`8086`, align_split_10Hz$`8099`)

#provides results for this subset of the Data
resSimple <- dynHRPrototype(goodDat, 2)

#gives summary of these results
(summary(resSimple$s_only$result))
resSimple$s_only$model@dynamics

