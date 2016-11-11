

#CCF Participants
setwd("~/Box Sync/DEPENd/Couples Data/code/Descriptives")
library(parallel)
load('./Data/align_split_atdifferentfreqs.RData')
list.files()
ailgn_split_4Hz_PatientPartner <-
  lapply(align_split_4Hz, alignLR_PatientPartner)

pdf("CCF_Patient_to_Partner.pdf",
    width = 11,
    height = 8.5)
cross <- lapply(ailgn_split_4Hz_PatientPartner, function(x) {
  ccf(
    x$ibi_interp_detrend_patient,
    x$ibi_interp_detrend_partner,
    main = x$PTNUM[[1]],
    lag.max = 100
  )
})
dev.off()

#ACF of Left Participants
pdf("ACF_Patient.pdf", width = 11, height = 8.5)
interPatient <-
  lapply(ailgn_split_4Hz_PatientPartner, function(x) {
    acf(x$ibi_interp_detrend_patient,
        main = x$PTNUM[[1]],
        lag.max = 100)
  })
dev.off()


#ACF of Right Participants
pdf("ACF_Partner.pdf", width = 11, height = 8.5)
interPartner <-
  lapply(ailgn_split_4Hz_PatientPartner, function(x) {
    acf(x$ibi_interp_detrend_partner,
        main = x$PTNUM[[1]],
        lag.max = 100)
  })
dev.off()

#helper functions
signal_motion_spec <-
  function(x) {
    spectrum(
      x,
      kernel("modified.daniell", c(8, 20)),
      plot = FALSE,
      detrend = TRUE,
      pad = 0.5
    )
  }
library(pracma)
library(multitaper)
mtapPlot_multitaper <-
  function(y,
           dt,
           subnum = NULL,
           nFFT = NULL,
           k = 5,
           nw = 3,
           plot = FALSE) {
    require(pracma)
    require(multitaper)
    
    #stopifnot(class(y) == "numeric") #this function only supports univariate time series
    if (!class(y) == "numeric") {
      stop("Time series y does not appear to be a numeric vector")
    }
    # y <- y - mean(y)
    
    #y <- centre(y, nw=3, k=5, deltaT=dt)
    if (is.null(nFFT))
      nFFT = 2 ^ nextpow2(length(y))
    
    #spec.mtm does not support multivariate time series or computation of coherence
    mspec <-
      spec.mtm(
        y,
        k = k,
        nw = nw,
        nFFT = nFFT,
        plot = FALSE,
        centreWithSlepians = TRUE,
        jackknife = FALSE,
        returnZeroFreq = TRUE,
        deltat = dt,
        returnInternals = TRUE
      )
    #plot(mspec, jackknife=FALSE)
    
    #freq is returned in 0 - 0.5 where 0.5 is nyquist freq. Divide by sampling freq to get into Hz
    #spec is the power estimate in that bin (not log transformed)
    if (plot)
      plot(
        mspec$freq / dt,
        10 * log10(mspec$spec),
        type = "l",
        ylab = "10*log10(power)",
        xlab = "Frequency (Hz)",
        main = paste("Multitaper spectrum estimate for subject:", subnum)
      )
    
    #return(invisible(data.frame(f=mspec$freq/dt, power=10*log10(mspec$spec))))
    return(list(
      df = data.frame(f = mspec$freq / dt, power = 10 * log10(mspec$spec)),
      mspec = mspec
    ))
  }
multicoh <- function(ts1, ts2, dt) {
  mtm1 <- mtapPlot_multitaper(ts1, dt, k = 31, nw = 16)$mspec
  mtm2 <- mtapPlot_multitaper(ts2, dt, k = 31, nw = 16)$mspec
  
  coh <- mtm.coh(mtm1, mtm2, plot = FALSE)
  #plot(coh, main = ts1)
  
}


fun1 <- function(x) {
  spec <-
    multicoh(x$ibi_interp_detrend_patient,
             x$ibi_interp_detrend_partner)
}
.llftr7
.lftr3p <- function(x, xv, nhi, nehl, nehc, slo, shi) {
  ip <- 2
  
  xp <- matrix(NA, nhi, 3)
  
  xp[, 3] <- .llftr7(xv, nhi, "hi", "even", "extend", nehl, ip)
  xp[, 2] <- .llftr7(x, nhi, "-", slo, shi, nehc, ip)
  
  xp[, 1] <- xp[, 2] - sqrt(xp[, 3])
  xp[, 3] <- xp[, 2] + sqrt(xp[, 3])
  
  return(xp)
}

pdf(
  "Frequency_Domain_Patient_Predicting_Partner.pdf",
  width = 11,
  height = 8.5
)
fourierCCFRawORIGINAL <-
  lapply(ailgn_split_4Hz_PatientPartner, function (x) {
    #spec <- mtapPlot_multitaper(x[,c("l_ibi_interp_detrend", "r_ibi_interp_detrend")], dt = .25)
    spec <-
      multicoh(x$ibi_interp_detrend_patient,
               x$ibi_interp_detrend_partner,
               dt = .25)
    #percent <- .8*max(spec$msc)
    
    plot.mtm.coh(spec)
    mtext(x$PTNUM[[1]], side = 3)
    #text(x=1, y = percent, label = x$PTNUM[[1]])
    #plot(fHz, spec$coh, type="l", main=x[,'PTNUM'], ylab = "Coherence of L and R")
  })

dev.off()


pdf(
  "Frequency_Domain_Autocorrelation_Patient.pdf",
  width = 11,
  height = 8.5
)
fourierR <- lapply(ailgn_split_4Hz_PatientPartner, function(x) {
  powerR <- signal_motion_spec(x$ibi_interp_detrend_patient)
  powerRdB = 10 * log10(powerR$spec)
  freq <- powerR$freq * 4
  plot(
    freq,
    powerRdB,
    type = "l",
    main = x$PTNUM[[1]],
    ylab = "Db",
    xlab = "Freq (Hz)"
  )
  mtext("Patient", side = 3)
  
})

dev.off()
pdf("Frequency_Domain_Autocorrelation_Partner.pdf")
fourierL <- lapply(ailgn_split_4Hz_PatientPartner, function(x) {
  powerL <- signal_motion_spec(x$ibi_interp_detrend_partner)
  powerLdB = 10 * log10(powerL$spec)
  plot(
    powerL$freq,
    powerLdB,
    type = "l",
    main = x$PTNUM[[1]],
    xlab = "Freq",
    ylab = "Db"
  )
  mtext("Partner", side = 3)
  
})

dev.off()

#10 Hz data --> see if other dominant frequencies
pdf(
  "Frequency_Domain_Spectral_Coherence10Hz.pdf",
  width = 11,
  height = 8.5
)
fourier10Hz <- lapply(align_split_10Hz_PatientPartner, function(x) {
  #spec <- mtapPlot_multitaper(x[,c("l_ibi_interp_detrend", "r_ibi_interp_detrend")], dt = .25)
  spec <-
    multicoh(x$ibi_interp_detrend_patient,
             x$ibi_interp_detrend_partner,
             dt = .1)
  #percent <- .8*max(spec$msc)
  
  plot.mtm.coh(spec)
  mtext(x$PTNUM[[1]], side = 3)
  #text(x=1, y = percent, label = x$PTNUM[[1]])
  #plot(fHz, spec$coh, type="l", main=x[,'PTNUM'], ylab = "Coherence of L and R")
})

dev.off()

pdf("CCF_10Hz_Patient_Partner.pdf",
    width = 11,
    height = 8.5)
ccf10Hz <- lapply(align_split_10Hz_PatientPartner, function(x) {
  ccf(
    x$ibi_interp_detrend_patient,
    x$ibi_interp_detrend_partner,
    main = x$PTNUM[[1]],
    lag.max = 200
  )
})
dev.off()