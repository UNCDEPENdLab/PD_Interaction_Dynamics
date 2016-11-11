#packages needed for this script
library(ggplot2)
library(dynr)
library(dplyr)
library(tidyr)


##MH Nov2016: code to convert the align_split structure to patient partner

#convert all l/r to patient partner
biglist <- list(align_split_1Hz, align_split_1.5Hz, align_split_2Hz, align_split_4Hz, align_split_10Hz)
alldata <- lapply(biglist, function(couplelist) {
  df <- do.call(rbind, couplelist)
  dfconvert <- alignLR_PatientPartner(df)
  split(dfconvert, dfconvert$PTNUM) #convert back to list for consistency
})
names(alldata) <- c("1Hz", "1.5Hz", "2Hz", "4Hz", "10Hz")
save(alldata, file="align_split_atdifferentfreqs_patpar_Nov2016.RData")


#In these analyses, running dynr package on detrended data that has been transformed from the Left-Right comparison to the Patient-Partner comparison
#advantages of Patietn-Partner is that (a) the results are more interpretable and (b) any differences between these groups might be oscuring trends otherwise
#Note that recently re-ran data at 10Hz with continuous time; while it works both ways, it really depends on how much we want to assume continuous time in this model
#to change back to discontinuous time, make sure to set isContinuousTime = FALSE


#set working directory
basedir <- "~/Box Sync/DEPENd/Couples Data/data"
#basedir <- "~/Box Sync/Couples Data/data"
setwd(basedir)
load("align_split_atdifferentfreqs.RData")

#this function will use the dynr package for the multilevel modeling of "goodDat"
#takes in Database that has already had the Patient Partner funciton ran on the data
dynHRPrototypePatPart <- function (dataset, hz) {
  numVars = 2
  hz = 2
  #this will specify the time
  dataset$time <- seq(1 / hz, nrow(dataset) / hz, by = 1 / hz)
  
  #substitute periods for underscores in variable names
  names(dataset) <- gsub("_", ".", names(dataset), fixed = TRUE) #for consistency with below
  
  #z-score all time detrended series
  dataset <- dataset %>% mutate_at(vars(ibi.interp.detrend.patient, ibi.interp.detrend.partner, 
                                        dom.interp.detrend.patient, dom.interp.detrend.partner, 
                                        aff.interp.detrend.patient, aff.interp.detrend.partner), 
                                   funs(scale))
  
  data <- dynr.data(
    dataset,
    id = "PTNUM",
    time = "time",
    observed = c("ibi.interp.detrend.patient", "ibi.interp.detrend.partner")
  )
  
  #zero-order correlations
  cor(dataset[, c("ibi.interp.detrend.patient", "ibi.interp.detrend.partner")])
  
  #setting measurement model (single indicator latent variable with loading 1.0)
  meas <- prep.measurement(
    values.load = diag(1, 2),
    params.load = diag(rep("fixed", 2), 2),
    state.names = c("patient_ibi", "partner_ibi"),
    obs.names = c("ibi.interp.detrend.patient",
                  "ibi.interp.detrend.partner")
  )
  
  initial <- prep.initial(
    values.inistate = c(0, 0),
    #zero starting value for states
    params.inistate = c("fixed", "fixed"),
    values.inicov = diag(1, 2),
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
    patient_ibi ~ p_patientibiself * patient_ibi + p_partpatibi * partner_ibi,
    partner_ibi ~  p_partneribiself * partner_ibi + p_patpartibi * patient_ibi
    # patient_ibi ~ p_patientibiself * (0 - patient_ibi) + p_partpatibi * (partner_ibi - patient_ibi),
    # partner_ibi ~  p_partneribiself * (0 - partner_ibi) + p_patpartibi * (patient_ibi - partner_ibi)
  ))
  
  #start values: strong self-coupling, mild anticorrelation for dom and positive correlation for aff
  svals <- c(rep(.5, 2), rep(-0.1, 1), rep(0.1, 1))
  names(svals) <-
    c("p_patientibiself", "p_partneribiself", "p_partpatibi", "p_patpartibi")
  dynm  <- prep.formulaDynamics(formula = fcoupled,
                                startval = svals,
                                isContinuousTime = FALSE) #,jacobian=jacob
  
  #This assumes there is always positive autocorrelation (not used at the moment)
  #This does not work for Steele and Ferrer variant where self coupling should be negative
  trans <-
    prep.tfun(
      formula.trans = list(
        p_patientibiself ~  exp(p_patientibiself) / (1 + exp(p_patientibiself)),
        p_partneribiself ~  exp(p_partneribiself) / (1 + exp(p_partneribiself))
      ),
      formula.inv = list(
        p_patientibiself ~ log(p_patientibiself / (1 - p_patientibiself)),
        p_partneribiself ~ log(p_partneribiself / (1 - p_partneribiself))
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
      observed <- dataset[, c("ibi.interp.detrend.patient", "ibi.interp.detrend.partner")]
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
goodDatBind <-
  rbind(
    align_split_2Hz$`8017`,
    align_split_2Hz$`8059`,
    align_split_2Hz$`8069`,
    align_split_2Hz$`8086`,
    align_split_2Hz$`8099`
  )


goodDatPatPart <- alignLR_PatientPartner(goodDatBind)

df <- do.call("rbind", align_split_2Hz)
dfPatPart <- alignLR_PatientPartner(df)
#goodDat <- rbind(align_split_10Hz$`8017`, align_split_10Hz$`8059`, align_split_10Hz$`8069`, align_split_10Hz$`8086`, align_split_10Hz$`8099`)

#provides results for this subset of the Data
res <- dynHRPrototypePatPart(goodDatPatPart, 2)
resDf <- dynHRPrototypePatPart(dfPatPart, 2)
capture.output(summary(resDf$s_only$result))
#gives summary of these results
(summary(res$s_only$result))
resDf$s_only$model@dynamics


resDf_rerun <- dynHRPrototypePatPart(dfPatPart, 2)
summary(resDf_rerun$s_only$result)

goodDatPatPart <- alignLR_PatientPartner(align_split_2Hz$`8017`)

resDf_rerun <- dynHRPrototypePatPart(goodDatPatPart, 2)

summary(resDf_rerun$s_only$result)

#10 Hz data, continuous time
df2 <-do.call("rbind", align_split_10Hz)
df2PatPart <- alignLR_PatientPartner(df2)
resdf2 <- dynHRPrototypePatPart(df2PatPart, 10)
save