#import joystick data
#align onto same grid as physio
library(R.matlab)
library(reshape2)
library(RHRV)

setwd("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/code")

#setwd("/Users/michael/Tresors/Couples_Interaction")
source("Couples_Interaction_Functions.R")
setwd("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics")
#here, the timing is from the beginning of the video = 0.0s, not light on
#joystick <- gdata::read.xls("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/joystick codes wright 28Apr2015.xlsx")
joystick <- gdata::read.xls("data/Couples Study Joystick 5 Best Reliability Raw 21Jul2015.xlsx")

joystick <- plyr::rename(joystick, c(L_X_Mean="l_aff", L_Y_Mean="l_dom", R_X_Mean="r_aff", R_Y_Mean="r_dom"))
joystick <- dplyr::filter(joystick, PTNUM != 8021)
#for MATLAB Granger, create a dataset where columns are arranged by patient and partner
joystick_recode <- alignLR_PatientPartner(joystick) #assumes columns have have prefix l_|r_


writeMat("data/joystick_bestreliability_arranged_2Oct2015.mat", joystick=split(joystick_recode, joystick_recode$PTNUM))

#use these offsets to realign joystick times to light on = 0.0s
lightdiff <- gdata::read.xls("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/Joystick PD Video Start_End Times June2015.xlsx")
#lightdiff <- gdata::read.xls("~/Tresors/Couples_Interaction/data/Joystick PD Video Start_End Times June2015.xlsx")
lightdiff <- plyr::rename(lightdiff, c(Video.Name="PTNUM"))

#updated format ends up as a funky string 00:00:04:80. Just parse it and glue together as decimal
lightdiff$start_sec <- sapply(lightdiff$Hundreths.Start, function(x) {
      xs <- strsplit(as.character(x), ":", fixed=TRUE)[[1]]
      as.numeric(paste0(xs[3], ".", xs[4]))
    })

joystick <- merge(joystick, lightdiff[,c("PTNUM", "start_sec")], by="PTNUM", all.x=TRUE)
joystick$ms_wrtlight <- (joystick$SEC - joystick$start_sec)*1000
joystick_split <- split(joystick, joystick$PTNUM)

#######
#read in IBI series for all couples
ibifiles  <- list.files(path="/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/negint_edited_ibis", pattern=".*negint.*\\.txt", full.names=TRUE)

ids <- as.numeric(sub("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/negint_edited_ibis/(8[0-9]{3}).*", "\\1", ibifiles, perl=TRUE))
position <- sub("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/negint_edited_ibis/8[0-9]{3}_([lr])_.*", "\\1", ibifiles, perl=TRUE)

#only retain ids where both partners have data
tb <- as.data.frame(table(ids))
#missingPartner <- grepl(as.character(paste(tb$ids[tb$Freq < 2], collapse="|")), ids, perl=TRUE)
#ibifiles <- ibifiles[!missingPartner]
#ids <- ids[!missingPartner]
#position <- position[!missingPartner]

allibi <- c()
for (i in 1:length(ibifiles)) {
  f <- read.table(ibifiles[i], col.names=c("time", "ibi"))
  f$PTNUM <- factor(ids[i])
  f$position <- factor(position[i])
  allibi <- rbind(allibi, f)
}

#all times in these data frames are with respect to light on = 0.0s
physio_split <- split(allibi, allibi$PTNUM)

#######
#synthesize physio and joystick data
haveboth <- intersect(names(physio_split), names(joystick_split))
missingJoystick <- setdiff(names(physio_split), names(joystick_split))
missingPhysio <- setdiff(names(joystick_split), names(physio_split))

align_split_10Hz <- lapply(haveboth, function(id) {
      interpolateCouple(physio_split[[id]], joystick_split[[id]])
    })
names(align_split_10Hz) <- haveboth

#4Hz version
align_split_4Hz <- lapply(haveboth, function(id) {
      interpolateCouple(physio_split[[id]], joystick_split[[id]], freqHz=4)
    })
names(align_split_4Hz) <- haveboth

#2Hz version
align_split_2Hz <- lapply(haveboth, function(id) {
      interpolateCouple(physio_split[[id]], joystick_split[[id]], freqHz=2)
    })
names(align_split_2Hz) <- haveboth

#1Hz version
align_split_1Hz <- lapply(haveboth, function(id) {
      interpolateCouple(physio_split[[id]], joystick_split[[id]], freqHz=1)
    })
names(align_split_1Hz) <- haveboth

align_split_1.5Hz <- lapply(haveboth, function(id) {
      interpolateCouple(physio_split[[id]], joystick_split[[id]], freqHz=1.5)
    })
names(align_split_1.5Hz) <- haveboth

save(align_split_10Hz, align_split_4Hz, align_split_2Hz, align_split_1Hz, align_split_1.5Hz, file="data/align_split_caid_atdifferentfreqs_april2017.RData")


biglist <- list(align_split_1Hz, align_split_1.5Hz, align_split_2Hz, align_split_4Hz, align_split_10Hz)
alldata <- lapply(biglist, function(couplelist) {
  df <- do.call(rbind, couplelist)
  dfconvert <- alignLR_PatientPartner(df)
  split(dfconvert, dfconvert$PTNUM) #convert back to list for consistency
})
names(alldata) <- c("1Hz", "1.5Hz", "2Hz", "4Hz", "10Hz")
save(alldata, file="align_split_atdifferentfreqs_patpar_CAID_april2017.RData")
length(alldata)
align_split_patientpartner_10Hz <- alldata[[5]]
align_split_patientpartner_4Hz <- alldata[[4]]
#10Hz version

basedir = "~/Desktop/negint_caid_dyad_txt"

lapply(align_split_patientpartner_10Hz, function(couple) {
  couple$time <- couple$ms / 1000
  write.table(file=file.path(basedir, paste0(couple$PTNUM[1], "_caid_ibis.txt")), couple[,c("PTNUM", "ms", "ibi_interp_detrend_patient", "ibi_interp_detrend_partner", "dom_interp_detrend_patient", "dom_interp_detrend_partner", "aff_interp_detrend_patient", "aff_interp_detrend_partner")],
              row.names=FALSE, col.names=FALSE, quote = FALSE)
})













#######
#compute summary statistics on joystick data
joystick_summaries <- do.call(rbind, lapply(align_split, function(x) {
          #save some basic stats about joystick
          l_dom_m <- mean(x$l_dom_interp, na.rm=T)
          r_dom_m <- mean(x$r_dom_interp, na.rm=T)
          l_dom_sd <- sd(x$l_dom_interp, na.rm=T)
          r_dom_sd <- sd(x$r_dom_interp, na.rm=T)
          l_dom_mssd <- psych::mssd(x$l_dom_interp, na.rm=T)
          r_dom_mssd <- psych::mssd(x$r_dom_interp, na.rm=T)
          
          l_aff_m <- mean(x$l_aff_interp, na.rm=T)
          r_aff_m <- mean(x$r_aff_interp, na.rm=T)
          l_aff_sd <- sd(x$l_aff_interp, na.rm=T)
          r_aff_sd <- sd(x$r_aff_interp, na.rm=T)
          l_aff_mssd <- psych::mssd(x$l_aff_interp, na.rm=T)
          r_aff_mssd <- psych::mssd(x$r_aff_interp, na.rm=T)
          
          data.frame(PTNUM=x$PTNUM[1], l_dom_m, r_dom_m, l_dom_sd, r_dom_sd, l_dom_mssd, r_dom_mssd,
              l_aff_m, r_aff_m, l_aff_sd, r_aff_sd, l_aff_mssd, r_aff_mssd)
        }))

joystick_summaries <- alignLR_PatientPartner(joystick_summaries) #combine into data.frame

#####
#Now compute measures of RSA

#pull in resting RSA from vanilla baseline
#compute both the time-varying and whole-period RSA
#for time-varying use, use the adapted RSAseconds function from MATLAB 
#loop over vanilla baseline files

vanillaibifiles <- list.files(path="/Users/michael/ics/couples_stage/ecg_raw", pattern="*vanilla_ecgraw_ibis.txt", full.names=TRUE)
beatdir <- "/Users/michael/ics/couples_stage/ecg_raw/vanillabeat_temp"
dir.create(beatdir, showWarnings=FALSE)
restingrsa <- c()
#setwd(beatdir) #for CMETX to work
for (f in vanillaibifiles) {
  id <- sub(".*ecg_raw/(\\d+)_(?:negative_)*[lr]_.*", "\\1", f, perl=TRUE)
  position <- tolower(sub(".*ecg_raw/\\d+_(?:negative_)*([lr])_.*", "\\1", f, perl=TRUE))
  x <- read.table(f)
  beatfile <- paste0(id, position, "_beattimes.txt")
  ibidosfile <- paste0(id, position, "_ibidos.txt")
  write.table(x[,1], file=file.path(beatdir, beatfile), row.names=FALSE, col.names=FALSE)
  write.table(x[,2], file=file.path(beatdir, ibidosfile), row.names=FALSE, col.names=FALSE, eol="\r\n")
  hrv.data  = CreateHRVData(Verbose=TRUE)
  hrv.data = LoadBeatAscii(hrv.data, beatfile, RecordPath = beatdir, scale=.001) #in ms
  hrv.data = BuildNIHR(hrv.data)
  hrv.data = FilterNIHR(hrv.data, minbpm=35, maxbpm=180)
  hrv.data = InterpolateNIHR (hrv.data, freqhr = 10)
  #PlotNIHR(hrv.data)
  #PlotHR(hrv.data)
  hrv.data = CreateFreqAnalysis(hrv.data)
  #get overall RSA during vanilla baseline by creating a single window of the length of the time series
  
  #new tack: use wavelet analysis to get overall power (this matches above very closely, but avoids the window size problem)
  hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis=1,
      type = "wavelet", wavelet = "la8", bandtolerance = 0.01, relative = FALSE,
      ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
      LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4)
  
  #hrv.data = CalculatePowerBand(hrv.data , indexFreqAnalysis=1,
  #    size = floor(max(hrv.data$Beat$Time)) -3, shift = floor(max(hrv.data$Beat$Time)) -3, type = "fourier",
  #    ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
  #    LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4)
  
  #~60-second bins with ~30-second windows
  hrv.data = CreateFreqAnalysis(hrv.data)
  #get overall RSA during vanilla baseline by creating a single window of the length of the time series
  hrv.data = CalculatePowerBand( hrv.data , indexFreqAnalysis=2,
      size = 54, shift = 24, type = "fourier",
      ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
      LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4)
  
  moving_rsa <- log(2*hrv.data$FreqAnalysis[[2]]$HF)
  
  restingrsa <- rbind(restingrsa, data.frame(PTNUM=id, position=position, restingrsa_all=mean(log(2*hrv.data$FreqAnalysis[[1]]$HF)), restingrsa_tmean=mean(moving_rsa), restingrsa_tsd=sd(moving_rsa)))
  #system(paste("/usr/local/bin/wine ~/CMETX.EXE ", tools::file_path_sans_ext(ibidosfile), " -o CMHRV", sep="\t"))
}

#reform dataframe into the l_ and r_ convention (rather than long format)
#remove 8104 and 8120
restingrsa <- subset(restingrsa, !PTNUM %in% c("8104", "8120"))
m <- melt(restingrsa, id.variable=c("PTNUM", "position"))
restingrsa_wide <- dcast(m, PTNUM ~ position + variable, value.var="value")

#rough comparison to cmetx
#cmetx <- read.csv("/Users/michael/ics/couples_stage/ecg_raw/vanillabeat_temp/CMHRV.CSV", header=TRUE)
#cmetx$id <- sub("(\\d+)[LR]_ibidos.*", "\\1", cmetx$ID, perl=TRUE)
#cmetx$position <- sub("\\d+([LR])_ibidos.*", "\\1", cmetx$ID, perl=TRUE)
#both <- merge(restingrsa, cmetx, by=c("id", "position"))
#levels(cmetx$Arts.Found) <- c("TRUE", "FALSE")
#cmetx[cmetx$Arts.Found=="TRUE",]

#8001 R is quite suspicious -- I believe this was after the electrodes were yanked by 8000.
#this couple was already excluded from the interaction data anyhow
#cor(subset(both, !id=="8001", select=c(rsa, LogRSA)))
#plot(subset(both, !id=="8001", select=c(rsa, LogRSA)))
#[1] 0.9874755

#hack together hrv.data objects for aligned interpolated interaction IBIs
#this will give us RSA estimates during the interaction task outside of the RSAseconds data
rsareactivity <- do.call(rbind, lapply(haveboth, function(id) {
          lphysio <- subset(physio_split[[id]], position=="l")
          rphysio <- subset(physio_split[[id]], position=="r")
          
          lmintime <- min(lphysio$time)
          lmaxtime <- max(lphysio$time)
          rmintime <- min(rphysio$time)
          rmaxtime <- max(rphysio$time)
          
          jmin <- min(joystick_split[[id]]$ms_wrtlight)
          jmax <- max(joystick_split[[id]]$ms_wrtlight)
          
          lhrv <- CreateHRVData(Verbose=TRUE)
          lhrv$Beat <- data.frame(Time=lphysio$time/1000)
          lhrv <- BuildNIHR(lhrv)
          lhrv = FilterNIHR(lhrv, minbpm=35, maxbpm=180)
          lhrv = InterpolateNIHR (lhrv, freqhr = 10)
          
          #trim interpolated heart rate to match the aligned time bounds of the couple (physio + joystick)
          ltrimmin <- ceiling((max(lmintime, rmintime, jmin) - lmintime)/100)
          ltrimmax <- ceiling((lmaxtime - min(lmaxtime, rmaxtime, jmax))/100)
          lhrv$HR <- lhrv$HR[(1+ltrimmin):(length(lhrv$HR) - ltrimmax)]
          
          lhrv = CreateFreqAnalysis(lhrv)
          #get overall RSA during vanilla baseline by creating a single window of the length of the time series
          #lhrv = CalculatePowerBand(lhrv, indexFreqAnalysis=1,
          #		size = floor(max(lhrv$Beat$Time)) -30, shift = floor(max(lhrv$Beat$Time)), type = "fourier",
          #		ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
          #		LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4)
          
          #new tack: use wavelet analysis to get overall power (this matches above very closely, but avoids the window size problem)
          lhrv = CalculatePowerBand(lhrv, indexFreqAnalysis=1,
              type = "wavelet", wavelet = "la8", bandtolerance = 0.01, relative = FALSE,
              ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
              LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4)
          
          #~60-second bins with ~30-second windows
          lhrv = CreateFreqAnalysis(lhrv)
          #get overall RSA during vanilla baseline by creating a single window of the length of the time series
          lhrv = CalculatePowerBand(lhrv , indexFreqAnalysis=2,
              size = 54, shift = 24, type = "fourier",
              ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
              LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4)
          
          lmoving_rsa <- log(2*lhrv$FreqAnalysis[[2]]$HF)
          
          #right participant
          rhrv <- CreateHRVData(Verbose=TRUE)
          rhrv$Beat <- data.frame(Time=rphysio$time/1000)
          rhrv <- BuildNIHR(rhrv)
          rhrv = FilterNIHR(rhrv, minbpm=35, maxbpm=180)
          rhrv = InterpolateNIHR(rhrv, freqhr = 10)
          
          #trim interpolated heart rate to match the aligned time bounds of the couple (physio + joystick)
          rtrimmin <- ceiling((max(lmintime, rmintime, jmin) - rmintime)/100)
          rtrimmax <- ceiling((rmaxtime - min(lmaxtime, rmaxtime, jmax))/100)
          rhrv$HR <- rhrv$HR[(1+rtrimmin):(length(rhrv$HR) - rtrimmax)]
          
          rhrv = CreateFreqAnalysis(rhrv)
          #get overall RSA during vanilla baseline by creating a single window of the length of the time series
          #rhrv = CalculatePowerBand(rhrv, indexFreqAnalysis=1,
          #		size = floor(max(rhrv$Beat$Time)) -30, shift = floor(max(rhrv$Beat$Time)), type = "fourier",
          #		ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
          #		LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4)
          
          rhrv = CalculatePowerBand(rhrv, indexFreqAnalysis=1,
              type = "wavelet", wavelet = "la8", bandtolerance = 0.01, relative = FALSE,
              ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
              LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4)
          
          #~60-second bins with ~30-second windows
          rhrv = CreateFreqAnalysis(rhrv)
          #get overall RSA during vanilla baseline by creating a single window of the length of the time series
          rhrv = CalculatePowerBand(rhrv , indexFreqAnalysis=2,
              size = 54, shift = 24, type = "fourier",
              ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
              LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4)
          
          rmoving_rsa <- log(2*rhrv$FreqAnalysis[[2]]$HF)
          
          df <- data.frame(PTNUM=id, l_negrsa_all=mean(log(2*lhrv$FreqAnalysis[[1]]$HF)), l_negrsa_tmean=mean(lmoving_rsa), l_negrsa_tsd=sd(lmoving_rsa),
              r_negrsa_all=mean(log(2*rhrv$FreqAnalysis[[1]]$HF)), r_negrsa_tmean=mean(rmoving_rsa), r_negrsa_tsd=sd(rmoving_rsa))
          
        }))

#bring resting rsa data into the dataset        
#NB: 8078 is missing the baseline. I have an email from July 21 2015 to Kelly and Nate about this, but don't have a resolution
allrsa <- droplevels(merge(rsareactivity, restingrsa_wide, by="PTNUM"))
allrsa$l_negrsa_m_baseline <- allrsa$l_negrsa_all - allrsa$l_restingrsa_all
allrsa$r_negrsa_m_baseline <- allrsa$r_negrsa_all - allrsa$r_restingrsa_all

allrsa <- alignLR_PatientPartner(allrsa) #combine into data.frame
hist(allrsa$negrsa_m_baseline_patient)
hist(allrsa$negrsa_m_baseline_partner)
#couple of huge swings that look like outliers
#probably need to winsorize these for now
allrsa$PTNUM[abs(allrsa$negrsa_m_baseline_partner) - 2 > 0]
allrsa$PTNUM[abs(allrsa$negrsa_m_baseline_patient) - 2 > 0]

#compute time-varying RSA using the Gates RSAseconds approach
library(R.matlab)
Sys.setenv(PATH=paste0(Sys.getenv("PATH"), ":/Applications/MATLAB_R2015a.app/bin"))
Matlab$startServer()
matlab <- Matlab()
isOpen <- open(matlab)
if (!isOpen) { throw("MATLAB server is not running: waited 30 seconds.") }
print(matlab)

#create a time-varying RSA dataset
align_rsaseconds <- lapply(align_split, function(couple) {
      setVariable(matlab, ibi=couple$l_ibi_interp_detrend)
      evaluate(matlab, "rsaseries=DynRSAOneFile(ibi, .12, .40, 0, 10)")
      l_rsa <- getVariable(matlab, "rsaseries")[[1L]]
      
      setVariable(matlab, ibi=couple$r_ibi_interp_detrend)
      evaluate(matlab, "rsaseries=DynRSAOneFile(ibi, .12, .40, 0, 10)")
      r_rsa <- getVariable(matlab, "rsaseries")[[1L]]
      
      #verify identical timescale
      stopifnot(all(l_rsa[,1] == r_rsa[,1]))
      df <- data.frame(time=l_rsa[,1], l_rsa=l_rsa[,2], r_rsa=r_rsa[,2])
      df <- subset(df, time < 610)
      if (max(df$time) < 500) { df <- NULL }
      
      return(df)
    })

close(matlab)

#some couples have fewer than 500 seconds of data due to an aborted interaction
#remove these (n=2 at the moment)
align_rsaseconds <- align_rsaseconds[!sapply(align_rsaseconds, is.null)]

align_rsaseconds <- do.call(rbind,lapply(1:length(align_rsaseconds), function(x) { 
      df <- align_rsaseconds[[x]]
      if (!is.null(df)) { df$PTNUM <- names(align_rsaseconds)[x] }
      
      #ptnum <- as.character(coupledf$PTNUM[1L])
      interpLookup <- align_split[[names(align_rsaseconds)[x]]] #pull the original 10Hz aligned interpolated data for this couple
      
      #spline interpolate the joystick data back onto the RSA seconds grid
      df$l_dom_interp_detrend <- spline(x=interpLookup$ms/1000, y=interpLookup$l_dom_interp_detrend, xout=df$time, method="fmm")$y
      df$l_aff_interp_detrend <- spline(x=interpLookup$ms/1000, y=interpLookup$l_aff_interp_detrend, xout=df$time, method="fmm")$y
      df$r_dom_interp_detrend <- spline(x=interpLookup$ms/1000, y=interpLookup$r_dom_interp_detrend, xout=df$time, method="fmm")$y
      df$r_aff_interp_detrend <- spline(x=interpLookup$ms/1000, y=interpLookup$r_aff_interp_detrend, xout=df$time, method="fmm")$y
      return(df)
    }))

align_rsaseconds <- alignLR_PatientPartner(align_rsaseconds) #convert to _patient/_partner; assumes columns have have prefix l_|r_
align_rsaseconds$PTNUM <- as.numeric(as.character(align_rsaseconds$PTNUM)) #recode PTNUM as numberic
#reorder columns for ease of use
align_rsaseconds <- align_rsaseconds[,c("PTNUM", "time", "rsa_partner", "rsa_patient", "aff_interp_detrend_partner", "dom_interp_detrend_partner", "aff_interp_detrend_patient", "dom_interp_detrend_patient")]

#this gets used in granger causality estimation
writeMat("data/couplersa_aligned_1Mar2016.mat", rsa=split(align_rsaseconds, align_rsaseconds$PTNUM))

save(physio_split, joystick_split, joystick, joystick_summaries, align_split, align_rsaseconds, allrsa, file="data/joystick_physio_aligned_7Mar2016.RData")

load(file="data/joystick_physio_aligned_7Mar2016.RData")
#quick granger test in 
library(urca)
library(vars)


#for the cointegration statistics, the nested hypothesis is:

#here, we reject the bottom 2 rows, but not the 3rd, meaning r = 2.
#          test 10pct  5pct  1pct
#r <= 3 |  3.11  6.50  8.18 11.65
#r <= 2 |  7.89 12.91 14.90 19.19
#r <= 1 | 26.64 18.90 21.07 25.75
#r = 0  | 38.49 24.78 27.14 32.14

xx <- split(align_rsaseconds, align_rsaseconds$PTNUM)
fd <- xx[[1]]
#divide the joystick variables by 100 to get the variances similar to RSA
fd[,5:8] <- fd[,5:8]/100 

library(reshape2)
library(ggplot2)
melty <- melt(fd[,2:8], id.vars="time")
ggplot(melty, aes(x=time, y=value)) + geom_line() + facet_wrap(~variable, scales="free", ncol=1)

m <- ca.jo(scale(fd[,8:3]), K=5, type="eigen")
summary(m)
plotres(m)
vecm.level <- vec2var(m, r=4) #five of 6 cointegrated
v <- VAR(fd[,3:8])

#estimate order of VAR
infocrit <- VARselect(fd[,3:8], lag.max = 10, type = "const") #order 5
v <- VAR(fd[,3:8], p=5, type="none")
causality(v, cause="rsa_partner") #simple GC test

#looks like there are 5 (of 6) integrated vectors in the full model
model1 <- cajorls(m,r=1) #I think it's actually r = 4 to indicate 5 integrated series?! Otherwise we get a 1.0 loading on specific variables and that's it
summary(model1$rlm) #seems to show which cointegration vector goes with which process

round(model1$beta, 3)

nvec <- as.matrix(fd[,3:8]) %*% model1$beta
cor(fd[,6], nvec[,1]) #not sure if this projection using beta matrix is correct...
cor(fd[,4], nvec[,2])

#pull ect1 from model
#seems like this is just pulling the variable with itself?
ccf(model1$rlm$model$ect1, fd[,3], lag.max=15)

cor(model1$rlm$model[,c(paste0("ect", 1:4))])

round(model1$beta, 5)



#simpler test of RSA only
m2 <- ca.jo(scale(fd[,3:4]), K=5)
summary(m2)

model1 <- cajorls(m2,r=1)
summary(model1$rlm) #seems to show which cointegration vector goes with which process
round(model1$beta, 3)


ccf(model1$rlm$model$ect1, fd[,3], lag.max=15)

infocrit <- VARselect(fd[,3:4], lag.max = 10, type = "const") #order 5
v <- VAR(fd[,3:4], p=8, type="none")
causality(v, cause="rsa_partner") #simple GC test
causality(v, cause="rsa_patient") #simple GC test



#try to predict
vecm.level <- vec2var(m2, r = 1)
vecm.irf <- irf(vecm.level, impulse = 'rsa_partner',
    response = 'rsa_patient', boot = FALSE)
plot(vecm.irf)

vecm.irf <- irf(vecm.level, impulse = 'rsa_patient',
    response = 'rsa_partner', boot = FALSE)
plot(vecm.irf)

#aff interp patient -> rsa_partner
m2 <- ca.jo(fd[,c("rsa_partner", "aff_interp_detrend_patient")], K=5)
summary(m2)

#try to predict
vecm.level <- vec2var(m2, r = 1)
vecm.irf <- irf(vecm.level, impulse = 'rsa_partner',
    response = 'aff_interp_detrend_patient', boot = TRUE, n.ahead=5)
plot(vecm.irf)

vecm.irf <- irf(vecm.level, impulse = 'aff_interp_detrend_patient',
    response = 'rsa_partner', boot = TRUE, n.ahead=5)
plot(vecm.irf)


vecm.irf <- irf(vecm.level, impulse = 'aff_interp_detrend_patient',
    response = 'rsa_partner', boot = TRUE, n.ahead=5, cumulative=FALSE)


v1 <- irf(vecm.level, impulse = 'aff_interp_detrend_patient',
    response = 'rsa_partner', boot = TRUE, n.ahead=5, cumulative=FALSE)

v2 <- irf(vecm.level, impulse = 'aff_interp_detrend_patient',
    response = 'rsa_partner', boot = TRUE, n.ahead=5, cumulative=TRUE)





summary(m2)


model1 <- cajorls(m2,r=1)
print(model1)
summary(model1$rlm)

vecm.fevd <- fevd(vecm.level)
vecm.norm <- normality.test(vecm.level)
vecm.arch <- arch.test(vecm.level)
vecm.serial <- serial.test(vecm.level)



data(finland)
sjf <- finland
sjf.vecm <- ca.jo(sjf, ecdet = "none", type = "eigen", K = 2,
    spec = "longrun", season = 4)
sjf.vecm.rls <- cajorls(sjf.vecm, r = 2)
summary(sjf.vecm.rls$rlm)
sjf.vecm.rls$beta

###LEFTOVERS
#resting RSA prototype
#hrv.data  = CreateHRVData()
#hrv.data = SetVerbose(hrv.data, TRUE)

#our IBI files have beats in the first column
#hrv.data = LoadBeatAscii(hrv.data, "8039_l_vanilla_ecgraw_beattimes.txt", RecordPath = "/Users/michael/ics/couples_stage/ecg_raw",
#    scale=.001) #in ms
#
#hrv.data = BuildNIHR(hrv.data)
#hrv.data = FilterNIHR(hrv.data, minbpm=30, maxbpm=190)
#hrv.data = InterpolateNIHR (hrv.data, freqhr = 10)
#PlotNIHR(hrv.data)
#PlotHR(hrv.data)
#hrv.data = CreateFreqAnalysis(hrv.data)
##overall RSA
#hrv.data = CalculatePowerBand( hrv.data , indexFreqAnalysis=1,
#    size = floor(max(hrv.data$Beat$Time)) -1, shift = floor(max(hrv.data$Beat$Time)) -1, type = "fourier",
#    ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
#    LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4 )
#hrv.data = CreateFreqAnalysis(hrv.data) #roughly by minute
#hrv.data = CalculatePowerBand( hrv.data , indexFreqAnalysis=2,
#    size = 72, shift = 54, type = "fourier",
#    ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
#    LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4 )
#hrv.data = CreateFreqAnalysis(hrv.data) #roughly by minute
#hrv.data = CalculatePowerBand( hrv.data , indexFreqAnalysis=3,
#    size = 32, shift = 1, type = "fourier",
#    ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
#    LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4 )
#PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 200, ymaxratio = 2.5)

#I think this is RSA that matches CMETx
#log(2*hrv.data$FreqAnalysis[[1]]$HF)
#log(2*hrv.data$FreqAnalysis[[2]]$HF)
#length(log(2*hrv.data$FreqAnalysis[[3]]$HF)) #close to RSAseconds approach
