require(gsl)
library(ggplot2)
library(dynr)
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#library(tidyr)
#setwd("~/Desktop/ams939/Couples Project/R")
#df <- read.table("/Users/michael/Tresors/Couples_Interaction/example_dyad_ibis_caid.txt",
#    col.names=c(strsplit("l.ibi.interp.detrend r.ibi.interp.detrend l.dom.interp.detrend r.dom.interp.detrend l.aff.interp.detrend r.aff.interp.detrend", "\\s+")[[1L]]))

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
load("align_split_atdifferentfreqs.RData")
nf <- read.table(file.path(basedir, "example_dyad_ibis.txt"),
                 col.names = c(
                   strsplit("l.ibi.interp.detrend r.ibi.interp.detrend ", "\\s+")[[1L]]
                 ))


goodDatHR <- alignLR_PatientPartner(goodDat)
fooHR <- function (alpha, hz, addParameters = NULL) {

  numVars = 2 + length(addParameters)
  # alpha = align_split_4Hz[[1]]
  # hz = 4
  #have 4Hz in memory at the moment (trying to see whether we get convergence)
  
  #10Hz timing
  #df$time <- seq(0.1, nrow(df)/10, by=0.1) #The time index doesn't affect discrete-time models so any scaling is fine.
  
  #df <- align_split_4Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
  #df$time <- seq(0.25, nrow(df)/4, by=0.25)
  
  #df <- align_split_2Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
  #df$time <- seq(0.5, nrow(df)/2, by=0.5)
  
  #df <- align_split_1Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
  alpha$time <- seq(1 / hz, nrow(alpha) / hz, by = 1 / hz)
  
  #this still doesn't work... doesn't converge, even with a lock on 0..1 autocorrelation
  #df <- align_split_1.5Hz[["8005"]][,c("l_ibi_interp_detrend", "r_ibi_interp_detrend", "l_dom_interp_detrend", "r_dom_interp_detrend", "l_aff_interp_detrend", "r_aff_interp_detrend")]
  #df$time <- seq(1/1.5, nrow(df)/1.5, by=1/1.5)
  
  names(alpha) <-
    gsub("_", ".", names(alpha), fixed = TRUE) #for consistency with below
  alpha$id <- "prototype" #just a single subject for now
  
  #visualize this couple
  # mdf <- df %>% gather(key="variable", value="value", -time, -id)
  # ggplot(mdf, aes(x=time, y=value)) + facet_grid(variable~.,scales = "free_y") + geom_line() + theme_bw(base_size=20)
  # mdf <- melt(df, id.vars=c("id", "time"))
  # dev.new(); ggplot(mdf, aes(x=time, y=value)) + facet_grid(variable~.,scales = "free_y") + geom_line() + theme_bw(base_size=20)
  
  #z-score series
  alpha[, c("l.ibi.interp.detrend", "r.ibi.interp.detrend", "l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend")] <-
    lapply(alpha[, c("l.ibi.interp.detrend", "r.ibi.interp.detrend", "l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend")], scale)
  
  #mdf <- melt(df, id.vars=c("id", "time"))
  #ggplot(mdf, aes(x=time, y=value)) + facet_grid(.~variable,scales = "free_y") + geom_line() + theme_bw(base_size=20)
  
  #downsampling by a factor of 10 (Sy-Miin's modification to get convergence). 10Hz -> 1Hz
  # timeNum <- round(length(alpha[[1]])/10)
  # time2 = rep(1:timeNum,each=10)
  #
  # agg = function(x){x=aggregate(x,by=list(time2),mean);return(x[,2])}
  # tmp = agg(alpha[,1])
  # tmp = sapply(alpha[,1:2],agg)
  # alpha=data.frame(tmp)
  # alpha$time=unique(time2)
  # alpha$id = rep(1,length(alpha$time))

  data <- dynr.data(
    alpha,
    id = "id",
    time = "time",
    observed = c("l.ibi.interp.detrend", "r.ibi.interp.detrend")
  )
  
  #zero-order correlations
  cor(alpha[, c("l.ibi.interp.detrend", "r.ibi.interp.detrend")])
  
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
    values.load = diag(1, 2),
    # starting values and fixed values
    params.load = diag(rep("fixed", 2), 2),
    state.names = c("libi", "ribi"),
    obs.names = c("l.ibi.interp.detrend",
                  "r.ibi.interp.detrend")
  ) # parameter numbers or indication that parameter is fixed
  
  
  #initial conditions for states (dom + aff) and covariance
  #just using true initial conditions at the moment
  
  #c(df$l.dom.interp.detrend[1],
  #df$r.dom.interp.detrend[1],
  #df$l.aff.interp.detrend[1],
  #df$r.aff.interp.detrend[1])
  
  initial <- prep.initial(
    values.inistate = c(0, 0),
    params.inistate = c("fixed", "fixed"),
    #values.inicov=diag(diag(cov(df[,c("l.dom.interp.detrend", "r.dom.interp.detrend", "l.aff.interp.detrend", "r.aff.interp.detrend")])), 4),
    #params.inicov=diag("fixed",4) #does this not allow for covariance in the signals?
    values.inicov = diag(1, 2),
    params.inicov = diag(rep("fixed", 2), 2)
    #params.inicov=diag(c("vv1", "vv2", "vv3", "vv4"),4) #free initial variances for hidden states
    #values.regimep=c(1, 0), #two regimes -- forced to start out in first
    #params.regimep=c("fixed", "fixed")
  )
  
  #I don't think the initial condition variances are identifiable with single-subject data
  
  #measurement and dynamics covariances
  
  #alternative with measurement noise only
  # mdcov_monly <- prep.noise(
  #   values.latent = diag(0, 2),
  #   #no noise in latent states (since there is a direct mapping from observed)
  #   params.latent = diag("fixed", 2),
  #   values.observed = diag(rep(0.1, 2)),
  #   #measurement noise in signals
  #   params.observed = diag(c("mn1", "mn2"), 2)
  # )
  
  mdcov_sonly <- prep.noise(
    values.latent = diag(.01, 2),
    #process noise only
    params.latent = diag(c("sn1", "sn2"), 2),
    values.observed = diag(rep(0, 2)),
    #no measurement noise in signals
    params.observed = diag("fixed", 2)
  )
  
  #alternative with process and measurement noise
  # mdcov_both <- prep.noise(
  #   values.latent = diag(1, 2),
  #   #allow for noisy states
  #   params.latent = diag(c("sn1", "sn2"), 2),
  #   values.observed = diag(rep(0.1, 2)),
  #   #measurement noise in signals
  #   params.observed = diag(c("mn1", "mn2"), 2)
  # )
  
  
  #omitting regime-switching piece for now
  
  # dynamics (evolution functions)
  # starting out with only dom-dom and aff-aff coupling
  # Time series have been detrended, so probable don't need a homeostasis point for the process at the moment
  fcoupled = list(list(
    libi ~ p_libiself * libi + p_rlibi * ribi,
    ribi ~  p_ribiself * ribi + p_lribi * libi
  ))
  
  # fcoupled2 = list(list(
  #   libi ~ p_libiself * libi + p_rlibi * ribi + p_ldomself*ldom,
  #   ribi ~  p_ribiself * ribi + p_lribi * libi + p_rdomself*rdom
  # ))
  # 
  # fcoupled2 = list(list(
  #   libi ~ p_libiself * libi + p_rlibi * ribi + p_ldomself*ldom +,
  #   ribi ~  p_ribiself * ribi + p_lribi * libi
  # ))
  # fcoupled=list(
  #   list(ldom ~ p_ldomself*ldom + p_rldom*rdom + r9*laff (coupling of my dominance with my affiliation)+ p_ldomself0*raff (my dominance with your affiliation),
  #        rdom ~  p_rdomself*rdom + p_lrdom*ldom,
  #        laff ~  p_laffself*laff + p_rlaff*raff,
  #        raff ~  p_raffself*raff + p_lraff*laff))
  
  #start values: strong self-coupling, mild anticorrelation for dom and positive correlation for aff
  svals <- c(rep(.5, 2), rep(-0.1, 1), rep(0.1, 1))
  names(svals) <-
    c("p_libiself", "p_ribiself", "p_rlibi", "p_lribi")
  dynm  <- prep.formulaDynamics(formula = fcoupled,
                                startval = svals,
                                isContinuousTime = FALSE) #,jacobian=jacob
  
  
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
  #
  # if (noise_model == "mnoise_only") {
  #   mdcov = mdcov_monly
  #   
  #   #noise upper and lower bound in optimization
  #   noiselb <- rep(-999, 2)
  #   noiseub <- rep(999, 2)
  # } else 
    if (noise_model == "snoise_only") {
    mdcov = mdcov_sonly
    
    #noise upper and lower bound in optimization
    noiselb <- rep(-999, 2)
    noiseub <- rep(999, 2)
    
  } 
  # else if (noise_model == "ms_noise") {
  #   mdcov = mdcov_both
  #   #noise upper and lower bound in optimization
  #   noiselb <- rep(-999, 4)
  #   noiseub <- rep(999, 4)
  #   
  # }
  
  # model_mnoise <- dynr.model(
  #   dynamics = dynm,
  #   measurement = meas,
  #   noise = mdcov_monly,
  #   initial = initial,
  #   data = data,
  #   transform = trans,
  #   outfile = "VAR_mnoise.c"
  # )
  # 
  # model_mnoise@ub = c(rep(25, 4), rep(999, 2))
  # model_mnoise@lb = c(rep(-25, 4), rep(-999, 2))
  
  
  model_snoise <- dynr.model(
    dynamics = dynm,
    measurement = meas,
    noise = mdcov_sonly,
    initial = initial,
    data = data,
    transform = trans,
    outfile = "VAR_snoise.c"
  )
  
  model_snoise@ub = c(rep(25, 4), rep(999, 2))
  model_snoise@lb = c(rep(-25, 4), rep(-999, 2))
  
  # model_bothnoise <- dynr.model(
  #   dynamics = dynm,
  #   measurement = meas,
  #   noise = mdcov_both,
  #   initial = initial,
  #   data = data,
  #   transform = trans,
  #   outfile = "VAR_bothnoise.c"
  # )
  # 
  # model_bothnoise@ub = c(rep(25, 4), rep(999, 4))
  # model_bothnoise@lb = c(rep(-25, 4), rep(-999, 4))
  # 
  
  #Extract parameter names to set ub and lb (optional)
  model_snoise$param.names
  # model_mnoise$param.names
  # model_bothnoise$param.names
  #
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
  Sys.setenv(PATH = "/Users/ams939/bin/homebrew/bin/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")
  # res_monly <-
  #   tryCatch(
  #     dynr.cook(model_mnoise),
  #     error = function(e) {
  #       print(e)
  #       return(NULL)
  #     }
  #   )
  res_sonly <-
    tryCatch(
      dynr.cook(model_snoise),
      error = function(e) {
        print(e)
        return(NULL)
      }
    )
  # res_bothnoise <-
  #   tryCatch(
  #     dynr.cook(model_bothnoise),
  #     error = function(e) {
  #       print(e)
  #       return(NULL)
  #     }
  #   )
  
  # res_monly_failed <- ifelse(is.null(res_monly), TRUE, FALSE)
  res_sonly_failed <- ifelse(is.null(res_sonly), TRUE, FALSE)
  # res_bothnoise_failed <- ifelse(is.null(res_bothnoise), TRUE, FALSE)
  
  # allmodelnames <- c("res_sonly", "res_monly", "res_bothnoise")
  # goodmodels <-
  #   allmodelnames[c(!res_sonly_failed,!res_monly_failed,!res_bothnoise_failed)]
  
  if (res_sonly_failed) {
    plot_res_sonly <- ggplot()
  }
  else {
    plot(res_sonly,
         dynrModel = model_snoise,
         names.state = meas$state.names)
    for (res in c("res_sonly")) {
      observed <-
        alpha[, c("l.ibi.interp.detrend", "r.ibi.interp.detrend")]
      predicted <-
        data.frame(t(eval(parse(text = res))@eta_smooth_final))
      names(predicted) <- names(observed)
      predicted$type = factor("predicted")
      observed$type = factor("observed")
      predicted$time <- 1:nrow(predicted)
      observed$time <- 1:nrow(observed)
    }
    #
    both <- rbind(predicted, observed)
    
    forplotting <-
      both %>% gather(key = "signal", value = "hrval",-type,-time)
    
    g <-
      ggplot(forplotting, aes(x = time, y = hrval, color = type)) + geom_line() +
      facet_wrap( ~ signal, ncol = 1, scales = "free_y") + ggtitle(paste("Observed versus expected", res))
    #plot(g)
    assign(paste0("plot_", res), g)
   }
  # if (res_monly_failed) {
  #   plot_res_monly <- ggplot()
  # }
  # else {
  #   plot(res_monly,
  #        dynrModel = model_snoise,
  #        names.state = meas$state.names)
  #   for (res in c("res_monly")) {
  #     observed <-
  #       alpha[, c("l.ibi.interp.detrend", "r.ibi.interp.detrend")]
  #     predicted <-
  #       data.frame(t(eval(parse(text = res))@eta_smooth_final))
  #     names(predicted) <- names(observed)
  #     predicted$type = factor("predicted")
  #     observed$type = factor("observed")
  #     predicted$time <- 1:nrow(predicted)
  #     observed$time <- 1:nrow(observed)
  #   }
  #   #
  #   both <- rbind(predicted, observed)
  #   
  #   forplotting <-
  #     both %>% gather(key = "signal", value = "hrval", -type, -time)
  #   
  #   g <-
  #     ggplot(forplotting, aes(x = time, y = hrval, color = type)) + geom_line() +
  #     facet_wrap(~ signal, ncol = 1, scales = "free_y") + ggtitle(paste("Observed versus expected", res))
  #   #plot(g)
  #   assign(paste0("plot_", res), g)
  # }
  
  # if (res_bothnoise_failed) {
  #   plot_res_bothnoise <- ggplot()
  # }
  # else {
  #   plot(res_bothnoise,
  #        dynrModel = model_bothnoise,
  #        names.state = meas$state.names)
  #   for (res in c("res_bothnoise")) {
  #     observed <-
  #       alpha[, c("l.ibi.interp.detrend", "r.ibi.interp.detrend")]
  #     predicted <-
  #       data.frame(t(eval(parse(text = res))@eta_smooth_final))
  #     names(predicted) <- names(observed)
  #     predicted$type = factor("predicted")
  #     observed$type = factor("observed")
  #     predicted$time <- 1:nrow(predicted)
  #     observed$time <- 1:nrow(observed)
  #   }
  #   #
  #   both <- rbind(predicted, observed)
  #   
  #   forplotting <-
  #     both %>% gather(key = "signal", value = "hrval", -type, -time)
  #   
  #   g <-
  #     ggplot(forplotting, aes(x = time, y = hrval, color = type)) + geom_line() +
  #     facet_wrap(~ signal, ncol = 1, scales = "free_y") + ggtitle(paste("Observed versus expected", res))
  #   #plot(g)
  #   assign(paste0("plot_", res), g)
  # }
  
  # if(res_sonly_failed) {
  #   plot_res_sonly <- NULL
  # }
  #
  # if (res_bothnoise_failed) {
  #   plot_res_bothnoise <- NULL
  # }
  
  # #if they all work
  # if (!(res_monly_failed&res_bothnoise_failed&res_sonly_failed)) {
  #   AIC(res_monly)
  #   AIC(res_sonly)
  #   AIC(res_bothnoise)
  #   summary(res_sonly)
  #   summary(res_bothnoise)
  #   summary(res_monly)
  #   coef(res_sonly)
  #
  #   # get the log likelihood, AIC, and BIC from a cooked model/data combo
  #   logLik(res_sonly)
  #   AIC(res_sonly)
  #   BIC(res_sonly)
  #
  #
  
  #
  #   str(res_sonly)
  #   str(res_monly)
  #   str(res_bothnoise)
  #
  #   return(list(
  #     m_only=list(model=model_mnoise, result=res_monly, plot=plot_res_monly),
  #     s_only=list(model=model_snoise, result=res_sonly, plot=plot_res_sonly),
  #     ms_both=list(model=model_bothnoise, result=res_bothnoise, plot=plot_res_bothnoise)
  #   ))
  # }
  #
  
  return(list(
    # m_only = list(
    #   model = model_mnoise,
    #   result = res_monly,
    #   plot = plot_res_monly
    # ),
    s_only = list(
      model = model_snoise,
      result = res_sonly,
      plot = plot_res_sonly
     )
    # ms_both = list(
    #   model = model_bothnoise,
    #   result = res_bothnoise,
    #   plot = plot_res_bothnois
    # )
  ))
# sonly <- list(model = model_snoise,
#                result = res_sonly,
#                plot = plot_res_sonly)
# return sonly
}

goodDat <- rbind(align_split_2Hz$`8017`, align_split_2Hz$`8059`, align_split_2Hz$`8069`, align_split_2Hz$`8086`, align_split_2Hz$`8099`)

# align_split_1Hz_HR_preprocess <-
#   lapply(align_split_1Hz, function(x) {
#     select(x, ends_with("ibi_interp_detrend"))
#   })
# 
# align_split_2Hz_HR_preprocess <-
#   lapply(align_split_2Hz, function(x) {
#     select(x, ends_with("ibi_interp_detrend"))
#   })
# 
# align_split_4Hz_HR_preprocess <-
#   lapply(align_split_4Hz, function(x) {
#     select(x, ends_with("ibi_interp_detrend"))
#   })

(result <- fooHR(align_split_2Hz[[1]], 2))


allresults1Hz <- list()
for (i in 1:length(align_split_1Hz)) {
  df <- align_split_1Hz_HR_preprocess[[i]]
  allresults1Hz[[names(align_split_1Hz)[i]]] <-
    fooHR(df, 1)
}

allresults4Hz <- list()
for (i in 1:length(align_split_4Hz_HR_preprocess)) {
  df <- align_split_4Hz_HR_preprocess[[i]]
  allresults4Hz[[names(align_split_4Hz)[i]]] <-
    fooHR(df, 4)
  if (i>10)
    break
    }


sonlyAICs4Hz <-
  lapply(allresults4Hz, function(element) {
    AIC(element$s_only$result)
  })

sonlyAICs1Hz <-  lapply(allresults1Hz, function(element) {
  AIC(element$s_only$result)
})
#don't have multiple AICs because mnoise and bothnoise unreliable and consistently worst
# allAICs4Hz <- sapply(allresults4Hz, function(couple) {
#   sapply(couple, function(model) {
#     AIC(model$result)
#   })
# })
# allAICs1Hz <- sapply(allresults1Hz, function(couple) {
#   sapply(couple, function(model) {
#     AIC(model$result)
#   })
# })
# diffVBest4Hz <- apply(allAICs4Hz, 2, function(x) {
#   x - min(x)
# })
# 
# diffVBest1Hz <- apply(allAICs1Hz, 2, function(x) {
#   x - min(x)
# })
(summaries1Hz <-
  lapply(allresults1Hz, function(couple) {
    capture.output(summary(couple$s_only$result))
  }))
(summaries4Hz <-
  lapply(allresults4Hz, function(couple) {
    capture.output(summary(couple$s_only$result))
  }))
str(allresults1Hz[[1]]$s_only$result@transformed.parameters[[5]])
statenoise <- lapply(allresults4Hz, function(couple) {couple$s_only$result@transformed.parameters[[5]] })
sn2 <- lapply(allresults4Hz, function(e) {e$s_only$result@transformed.parameters[[6]]})

