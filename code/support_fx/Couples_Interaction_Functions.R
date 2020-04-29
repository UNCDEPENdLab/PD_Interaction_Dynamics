#estimate derivatives from two time series that are already aligned and upsampled to 10Hz
#uses a fixed estimation here with embedding dimension 5 and tau 3.
runCLO <- function(id, ts1, ts2) {
  #need the workding dir to be right already
  source("PublishedCode/OscillatorEstimation.R")
  source("PublishedCode/Boker_GLLAfunctions.R")
  
  #inputs should be detrended time series aligned onto the same grid (10Hz)
  ts1_ibimat <- gllaEmbed(ts1, embed=5, tau=3)
  ts2_ibimat <- gllaEmbed(ts2, embed=5, tau=3)
  wMatrix <- gllaWMatrix(embed=5, tau=3, deltaT=1, order=2)
  
  #gives you variable, first derivative, and second derivative
  ts1_ibilla <- as.data.frame(ts1_ibimat[,2:dim(ts1_ibimat)[2]] %*% wMatrix)
  ts2_ibilla <- as.data.frame(ts2_ibimat[,2:dim(ts2_ibimat)[2]] %*% wMatrix)
  names(ts1_ibilla) <- c("ts1_ibi", "ts1_dibi", "ts1_d2ibi")
  names(ts2_ibilla) <- c("ts2_ibi", "ts2_dibi", "ts2_d2ibi")
  
  df <- cbind(PTNUM=id, ts1_ibilla, ts2_ibilla)
  return(df)
}

#spline interpolate both behavior and ibi onto same 10Hz grid.
#options(error=recover)
interpolateCouple <- function(ibi_couple, joystick_couple, freqHz=10) {
  #figure min and max times for ibi and behavior to create common time grid
  l_ibi <- subset(ibi_couple, position=="l")
  r_ibi <- subset(ibi_couple, position=="r")
  
  #ibi streams are unique per person, whereas joystick timing is shared by couple
  xoutmin <- max(min(l_ibi$time), min(r_ibi$time), min(joystick_couple$ms_wrtlight))
  xoutmax <- min(max(l_ibi$time), max(r_ibi$time), max(joystick_couple$ms_wrtlight))
  
  gridstep <- 1/freqHz * 1000
  interp_time_grid <- seq(xoutmin, xoutmax, by=gridstep) #100ms increment
  
  #have 6 time series to resample onto grid (could probably functionalize this, but happy just to repeat...)
  l_ibi_interp <- spline(x=l_ibi$time, y=l_ibi$ibi, xout=interp_time_grid, method="fmm")$y
  r_ibi_interp <- spline(x=r_ibi$time, y=r_ibi$ibi, xout=interp_time_grid, method="fmm")$y
  l_dom_interp <- spline(x=joystick_couple$ms_wrtlight, y=joystick_couple$l_dom, xout=interp_time_grid, method="fmm")$y #Y is dominance
  l_aff_interp <- spline(x=joystick_couple$ms_wrtlight, y=joystick_couple$l_aff, xout=interp_time_grid, method="fmm")$y #X is affiliation
  r_dom_interp <- spline(x=joystick_couple$ms_wrtlight, y=joystick_couple$r_dom, xout=interp_time_grid, method="fmm")$y #Y is dominance
  r_aff_interp <- spline(x=joystick_couple$ms_wrtlight, y=joystick_couple$r_aff, xout=interp_time_grid, method="fmm")$y #X is affiliation
  
  interpdf <- data.frame(PTNUM=joystick_couple$PTNUM[1], ms=interp_time_grid, l_ibi_interp, r_ibi_interp, l_dom_interp, l_aff_interp, r_dom_interp, r_aff_interp)
  
  #linear detrend of all time series
  detrend <- as.data.frame(lapply(interpdf[,grep("interp", names(interpdf), value=TRUE)], function(x) {
    lin <- 1:length(x)
    residuals(lm(x ~ 1 + lin))
  } ))
  names(detrend) <- paste(names(detrend), "detrend", sep="_")
  interpdf <- cbind(interpdf, detrend)
  return(interpdf)
}

interpolateIBI <- function(ibi_couple, freqHz=10) {
  #figure min and max times for ibi and behavior to create common time grid
  l_ibi <- subset(ibi_couple, position=="l")
  r_ibi <- subset(ibi_couple, position=="r")
  
  #ibi streams are unique per person, whereas joystick timing is shared by couple
  xoutmin <- max(min(l_ibi$time), min(r_ibi$time))
  xoutmax <- min(max(l_ibi$time), max(r_ibi$time))
  
  gridstep <- 1/freqHz *  1000
  interp_time_grid <- seq(xoutmin, xoutmax, by=gridstep) #100ms increment
  
  #have 6 time series to resample onto grid (could probably functionalize this, but happy just to repeat...)
  l_ibi_interp <- spline(x=l_ibi$time, y=l_ibi$ibi, xout=interp_time_grid, method="fmm")$y
  r_ibi_interp <- spline(x=r_ibi$time, y=r_ibi$ibi, xout=interp_time_grid, method="fmm")$y
  
  interpdf <- data.frame(PTNUM=ibi_couple$PTNUM[1], ms=interp_time_grid, l_ibi_interp, r_ibi_interp)
  
  #linear detrend of all time series
  detrend <- as.data.frame(lapply(interpdf[,grep("interp", names(interpdf), value=TRUE)], function(x) {
    lin <- 1:length(x)
    residuals(lm(x ~ 1 + lin))
  } ))
  names(detrend) <- paste(names(detrend), "detrend", sep="_")
  interpdf <- cbind(interpdf, detrend)
  return(interpdf)
}



#looks for all variables with prefix l_ or r_ in data.frame and renames them with suffix _patient or _partner depending on lookup in psychophys log.
alignLR_PatientPartner <- function(df) {
  #	require(xlsx)
  #	logfile <- read.xlsx("~/Tresors/Couples_Interaction/data/psychophys_log_7May2015.xlsx", sheetIndex=1, as.data.frame=TRUE)[,c("PTNum", "DyadID", "DyadText", "L.R", "Kaeleen.P1.P2", "PInitials")]
  #	logfile <- plyr::rename(logfile, c(PTNum="PTNUM"))
  #save(logfile, file="~/Tresors/Couples_Interaction/data/psychophys_log_7May2015.RData")
  #load(file="~/Desktop/ams939/Couples Project/R/Data/psychophys_log_7May2015.RData")
  #load(file="/Users/michael/Box_Sync/DEPENd/Couples Data/data/psychophys_log_7May2015.RData")
  #logfile <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/PSYCHOPHYS_LOG_20170118.csv")
  #logfile <- select(logfile, one_of(c("PTNUM", "Dyad.ID", "Dyad.Text", "Left.or.Right.Seat", "Kaeleen.P1.P2", "Particpant.s.Initials")))
  #save(logfile, file="~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/PSYCHOPHYS_LOG_20170118.RData")
  load(file = "/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/data/PSYCHOPHYS_LOG_20170118.RData")
  df_split <- split(df, df$PTNUM)
  
  recode <- do.call(rbind, lapply(df_split, function(coupledf) {
    print(coupledf)
    lookup <- subset(logfile, PTNUM==coupledf$PTNUM[1] & Left.or.Right.Seat == "L")
    if (nrow(lookup) != 1L) { stop("Unable to find lookup for ID: ", coupledf$PTNUM[1])}
    if (lookup$Dyad.Text=="PARTNER") { #partner is on left, patient on right
      names(coupledf) <- sub("^l_(.*)", "\\1_partner", names(coupledf), perl=TRUE)
      names(coupledf) <- sub("^r_(.*)", "\\1_patient", names(coupledf), perl=TRUE)
    } else if (lookup$Dyad.Text=="PATIENT") { #patient on left, partner on right
      names(coupledf) <- sub("^l_(.*)", "\\1_patient", names(coupledf), perl=TRUE)
      names(coupledf) <- sub("^r_(.*)", "\\1_partner", names(coupledf), perl=TRUE)
    } else { stop("Cannot match dyad text: ", lookup$Dyad.Text) }
    
    coupledf
  }))
  return(recode)
}

#example IBI series
#p1 <- read.table("data/ecg_proc/8024_p1_negint_ecgraw_ibis.txt", col.names=c("time", "ibi"))
#p2 <- read.table("data/ecg_proc/8024_p2_negint_ecgraw_ibis.txt", col.names=c("time", "ibi"))
#p2_matlabspline <- read.table("data/ecg_proc/8024_p2_negint_ecgraw_ibi10Hz.txt", col.names=c("time", "ibi"))
#
##check the bounds of the IBI series and use only times when both members of the couple have observed data (i.e., don't interpolate beyond bounds)
#xoutmin <- max(min(p1$time), min(p2$time))
#xoutmax <- min(max(p1$time), max(p2$time))
#
##interpolate p1 and p2 ibi series onto 10Hz grid
#xout <- seq(xoutmin, xoutmax, by=100) #100ms increment
#p1interp <- as.data.frame(spline(x=p1$time, y=p1$ibi, xout=xout, method="fmm"))
#names(p1interp) <- c("time", "ibi")
#p1interp$role <- factor("patient")
#
##demean and detrend ibi
#lin <- 1:length(p1interp$ibi)
#p1interp$ibidetrend <- residuals(lm(p1interp$ibi ~ 1 + lin))
#
#computeDLOR2 <- function(ts, embedVals=3:14, tauVals=1:20) {
#  r2vals <- matrix(NA, nrow=length(embedVals), ncol=length(tauVals), dimnames=list(embed=embedVals, tau=tauVals))
#  for (e in 1:length(embedVals)) {
#    for (t in 1:length(tauVals)) {
#      ibimat <- gllaEmbed(ts, embed=embedVals[e], tau=tauVals[t])
#      wMatrix <- gllaWMatrix(embed=embedVals[e], tau=tauVals[t], deltaT=1, order=2)
#      
#      #gives you variable, first derivative, and second derivative
#      ibilla <- as.data.frame(ibimat[,2:dim(ibimat)[2]] %*% wMatrix)
#      names(ibilla) <- c("ibi", "dibi", "d2ibi")
#      
#      test <- lm(d2ibi ~ ibi + dibi - 1, ibilla)      
#      r2vals[e,t] <- summary(test)$r.squared
#    }
#  }
#  return(r2vals)
#}
#
#r2mat <- computeDLOR2(p1interp$ibidetrend)
#
##shuffle data ala helm to find null distribution of R2 values
#shuf <- replicate(500, p1interp$ibidetrend[sample(1:length(p1interp$ibidetrend))])
#
#setDefaultClusterOptions(master="localhost")
#clusterobj <- makeSOCKcluster(8)
#registerDoSNOW(clusterobj)
#
#comb3d <- function(...) { abind(..., along=3) }
#res <- foreach(i=iter(shuf, by="col"), .inorder=TRUE, .noexport="shuf", .combine=comb3d) %dopar% {
#  computeDLOR2(i)
#}
#
#stopCluster(clusterobj)
#
#
#
##try out estimation of tau smoothing parameter for derivative estimation
##apparently this doesn't help estimate tau... 
#bestTau <- TIAnalysis(p1interp$ibidetrend, 20) #taus 1-20
#
##following the Reed paper (and Helm et al.), try out different levels of embedding and tau smoothing to maximize R^2 for a given univariate stream
#
#
#
##look for lowest numbered row (embed) and column (tau) that approaches 95% of max (asymptote); see Reed, Butler 2015
#gt95 <- which(r2mat >= .95*max(r2mat), arr.ind=TRUE)
#parsimonious <- gt95[which.min(apply(gt95, 1, sum)),] #embed=11, tau=9
