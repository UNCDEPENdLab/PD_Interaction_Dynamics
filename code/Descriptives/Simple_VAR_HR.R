library(vars)
setwd("~/Desktop/ams939/Couples Project/R")
getwd()
list.files('./Code')
 

 source('./Functions/Couples_Interaction_Functions.R')

load('./Data/align_split_atdifferentfreqs.RData')


align_split_4Hz <- lapply(align_split_4Hz, function(df) {
      df$l_hr_interp <- (1/(df$l_ibi_interp/1000))*60 #estimate heart rate (BPM)
      df$r_hr_interp <- (1/(df$r_ibi_interp/1000))*60 #estimate heart rate (BPM)
      df <- as.data.frame(lapply(df, function(col) {
                #coldec <- decimate(col, 5, 30, ftype="fir")
                #coldec <- decimate(col, 5, 30, ftype="fir")
                #coldec <- resample(col, 2, 10) #weird at the beginning of the series
                #how about simple subsampling?!
                coldec <- col[seq(1, length(col), 5)] #right, this works to get to 2Hz...
                return(coldec)
              }))
      df <- alignLR_PatientPartner(df) #inefficient since it reads the xlsx file for each couple
    })

pdf("VAR of Couples Data.pdf")
#op = par(ask=FALSE)
VAR4Hz <- sapply(align_split_4Hz, function(couple) {
      require(vars)
      
      coupleordered <- couple[,c("hr_interp_partner", "hr_interp_patient")]
      
      #detrend inputs to VAR, then use type="none" to avoid "const" and "both" (for linear)
      coupleordered <- as.data.frame(lapply(coupleordered, function(x) {
                lin <- 1:length(x)
                residuals(lm(x ~ 1 + lin))
              } ))
      
      #estimate model order using AIC (looks like 8-13 lags (4-7 seconds)
      vv <- VARselect(coupleordered, lag.max = 20, type = c("none"))$selection["AIC(n)"]      
      v <- vars::VAR(coupleordered, p=vv, type="none")
     par(ask= FALSE)
     plot(v)
     mtext(couple$PTNUM[[1]], 3)
      normality.test(v)
      
      r <- resid(v)
      #return(vv)      
      return(vv)
     # oldpar <-  par(ask = FALSE)
      #on.exit(par(oldpar))
    })
#str(VAR4Hz)
#par(op)
dev.off()

VAR4Hz_Mean <- mean(VAR4Hz)
VAR4Hz_Mean
VAR4Hz_StandardDev <- sd(VAR4Hz)
VAR4Hz_StandardDev

