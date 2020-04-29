#setwd(file.path(getMainDir(), "Psychophys_Pilot", "Couples_Psychophys"))
#setwd("X:/Couples Study/Couple Interaction/Couples_Psychophys")
#setwd("C:/Users/Psychophys/Documents/Couples_Psychophys")
#setwd("/Volumes/HALLQUIST/Couples_Psychophys")
#setwd("/Volumes/personality/Couples Study/Couple Interaction/Couples_Psychophys")
#setwd("/Users/michael/Tresors/Couples_Interaction")
setwd("/Users/alisonmarie526/Desktop/")

#source("Signal_Utilities.R")
source("/Users/alisonmarie526/Desktop/8009_proc/Signal_Utilities.R")
procList <- list.files("8009_proc", pattern="*.RData")
options(warn=2) #stop on any error

#here's an example of getting HRV during the interaction
for (f in procList) {
  
  #check that all required files are present:
  id <- gsub("(\\w+)\\.RData", "\\1", f, perl=TRUE)
  
  searchNegative <- TRUE #whether to search for negative interaction
  searchPositive <- TRUE #whether to search for positive interaction

  #expected files for negative only
   negFilesCheck <- file.exists(paste0("8009_proc/", id, "_r_negint_ecgraw.txt")) && 
       file.exists(paste0("8009_proc/", id, "_l_negint_ecgraw.txt")) &&
       file.exists(paste0("8009_proc/", id, "_l_vanilla_ecgraw.txt")) &&
       file.exists(paste0("8009_proc/", id, "_r_vanilla_ecgraw.txt")) &&
       file.exists(paste0("8009_proc/", id, "_r_negint_gsrraw.txt")) && 
       file.exists(paste0("8009_proc/", id, "_l_negint_gsrraw.txt")) &&
       file.exists(paste0("8009_proc/", id, "_r_vanilla_gsrraw.txt")) &&
       file.exists(paste0("8009_proc/", id, "_l_vanilla_gsrraw.txt"))
  # 
  #expected files for positive only
   posFilesCheck <- file.exists(paste0("8009_proc/", id, "_r_posint_ecgraw.txt")) &&
       file.exists(paste0("8009_proc/gsr_raw/", id, "_l_posint_ecgraw.txt")) &&
       file.exists(paste0("8009_proc/gsr_raw/", id, "_r_posint_gsrraw.txt")) &&
       file.exists(paste0("8009_proc/gsr_raw/", id, "_l_posint_gsrraw.txt"))  
  # 
  
   if (grepl("negative", id)) {
     searchPositive <- FALSE #no positive expected
     #for split files, don't expect positive files
     posFilesCheck <- TRUE
   }
  # 
   if (grepl("positive", id)) {
     searchNegative <- FALSE #no negative expected
     negFilesCheck <- TRUE
   }
  # 
   if (negFilesCheck && posFilesCheck) {
     cat("Skipping", id, "\n")
     next #skip this couple -- ECG already output
   }
  cat("Processing id: ", id, "\n")
  
  load(paste0("8009_proc/", f))
  
  ledOn <- which(pp$LED == 5) #voltage=5 when on
  
  #255 is supposed to denote the beginning of vanilla baseline, but somehow the code gets stuck on for a long time
  #the min 255 should be the onset. But here, we validate that the 1-minute mark is about 60 seconds after the earliest 255 code
  vanillaStart <- min(which(pp$TrialCodes==255))
  vanilla1 <- which(pp$TrialCodes==1) #60-second mark
  vanilla2 <- which(pp$TrialCodes==2) #120-second mark
  vanilla3 <- which(pp$TrialCodes==3) #180-second mark
  vanilla4 <- which(pp$TrialCodes==4) #240-second mark
  vanillaEnd <- which(pp$TrialCodes==254) #end of task
  #for some subjects, the end code is 126. This appears to be a faulty pin connection on the box, since 2^7 is 128, so the box is getting 01111110 instead of 11111110
  if (length(vanillaEnd) == 0L && length(which(pp$TrialCodes==126)) > 0L ) {
    message("Cannot identify vanilla baseline end marker using code 254. Falling back to code 126.")
    vanillaEnd <- which(pp$TrialCodes==126)
  }
  
  #For couple 8124, the first observation has TrialCode 4. Must have been some problem in resetting the pins
  #at the beginning of the experiment. Use the second element of vanilla4, which aligns in time.
  if (id == "8124") { vanilla4 <- vanilla4[2] }
  
  #For couple 8128, there are two 1-minute TrialCodes (1). The second one matches in time with vanilla2, 3, 4.
  #There is also an anomaly with vanilla start. Combining these, it looks like vanilla was run once, stopped
  #around 1.5 minutes and then restarted. Use the second one.
  if (id == "8128") { vanilla1 <- vanilla1[2]; vanillaStart = which(pp$TrialCodes==255)[2] }
  
  #For couple 8017, the ECG lead became detached during vanilla baseline, so the task was re-run. Thus, use the second set of codes
  if (id == "8017") { vanilla1 <- vanilla1[2]; vanilla2 <- vanilla2[2]; vanilla3 <- vanilla3[2]; vanilla4 <- vanilla4[2]; vanillaEnd <- vanillaEnd[2] }
  
  #For couple 8052, somehow the 4 minute marker misfired around minute 0.25, so get two code==4 matches. Use the second (which matches other times)
  #For couple 8057, minute 4 marker misfired at time=0, but also as expected at 5.41 mins (matching other markers)
  if (id == "8052" || id == "8057") { vanilla4 <- vanilla4[2] }
  
  numCodes <- sapply(list(v1=vanilla1, v2=vanilla2, v3=vanilla3, v4=vanilla4, ve=vanillaEnd), length)
  extractVanilla <- TRUE
  
  if (any(numCodes > 1L)) { 
    message("More than one time code found for vanilla baseline, id: ", id, ". Cannot resolve what to extract." )
    extractVanilla <- FALSE
  }
  if (any(numCodes == 0L)) { 
    message("Some time markers missing for vanilla baseline. Cannot resolve what to extract." )
    print(names(numCodes)[which(numCodes == 0L)]) #try and figure out what is missing.
    print(table(pp$TrialCodes))
    extractVanilla <- FALSE
  } 
    
  #check that minute boundary codes are within 2 seconds of 60 seconds
  if(extractVanilla && (abs((vanilla2 - vanilla1)/1000 - 60) > 2 ||
        abs((vanilla3 - vanilla2)/1000 - 60) > 2 ||
        abs((vanilla4 - vanilla3)/1000 - 60) > 2)) {
    
    extractVanilla <- FALSE
    message("Minute boundary markers for vanilla baseline inaccurate. Skipping participant.")
    
    message("start-min1: ", round(abs((vanilla1 - vanillaStart)/1000 - 60), 2))
    message("min1-min2: ", round(abs((vanilla2 - vanilla1)/1000 - 60), 2))
    message("min2-min3: ", round(abs((vanilla3 - vanilla2)/1000 - 60), 2))
    message("min3-min4: ", round(abs((vanilla4 - vanilla3)/1000 - 60), 2))
  } 

  if (extractVanilla && abs((vanillaEnd - vanilla4)/1000 - 60) > 2) {
    message("min4-end:", round(abs((vanillaEnd - vanilla4)/1000 - 60), 2))
    message("Given that timing of other intervals is correct, use 60 seconds after minute 4 as end marker.")
    vanillaEnd <- vanilla4 + 60979 #about 1s of lag in markers/task, so not quite 60,000
  }
  
  if (extractVanilla && abs((vanilla1 - vanillaStart)/1000 - 60) > 2) {
    message("start-min1: ", round(abs((vanilla1 - vanillaStart)/1000 - 60), 2))
    message("Given that timing of other intervals is correct, use 60 seconds back from minute 1 as start marker.")
    vanillaStart <- vanilla1 - 60979 #about 1s of lag in markers/task, so not quite 60,000
  }  
  
  vanillaLength <- (vanillaEnd - vanillaStart)/1000
  if (extractVanilla && (vanillaLength < 290 || vanillaLength > 310)) {
    message("Length of vanilla baseline not within 290-310 seconds: ", round(vanillaLength, 3))
    extractVanilla <- FALSE
  }
  
  if (extractVanilla) {
    van.ecg.r <- pp$R.ECG[vanillaStart:vanillaEnd]
    van.ecg.l <- pp$L.ECG[vanillaStart:vanillaEnd]
    van.gsr.r <- pp$R.GSR[vanillaStart:vanillaEnd]
    van.gsr.l <- pp$L.GSR[vanillaStart:vanillaEnd]
    
    #write data
    write.table(van.ecg.r, file=paste0("8009_proc/ecg_raw/", id, "_r_vanilla_ecgraw.txt"), row.names=FALSE, col.names=FALSE)
    write.table(van.ecg.l, file=paste0("8009_proc/ecg_raw/", id, "_l_vanilla_ecgraw.txt"), row.names=FALSE, col.names=FALSE)
    write.table(data.frame(time=seq(0, length(van.gsr.r)- 1L)/1000, gsr=van.gsr.r), file=paste0("8009_proc/gsr_raw/", id, "_r_vanilla_gsrraw.txt"), row.names=FALSE, col.names=FALSE)
    write.table(data.frame(time=seq(0, length(van.gsr.l)- 1L)/1000, gsr=van.gsr.l), file=paste0("8009_proc/gsr_raw/", id, "_l_vanilla_gsrraw.txt"), row.names=FALSE, col.names=FALSE) 
  }
  
  ##verify that both positive and negative are present
  ##negative interaction comes first
  if (searchNegative) {
    if (id == "8141") {
      #this couple was stopped mid-interaction because they were off topic, then restarted.
      #this code debugs and identifies all change events
      #ledevts <- pp$LED[1]
      #ledpos <- 1
      #while (TRUE) {
      #  nextevt <- which(pp$LED != ledevts[length(ledevts)] & 1:length(pp$LED) > ledpos[length(ledpos)])[1]
      #  if (is.na(nextevt)) { #which(x)[1] returns NA when the which is integer(0)
      #    message("Events exhausted. Dropping out of loop")
      #    break
      #  }
      #  ledevts <- c(ledevts, pp$LED[nextevt])
      #  ledpos <- c(ledpos, nextevt)
      #}
      #
      #output:
      #      ledpos ledevts
      #[1,]       1       0
      #[2,] 2812869       5
      #[3,] 3089332       0
      #[4,] 3090186       5
      #[5,] 3696258       0
      #[6,] 4314630       5
      #[7,] 4616672       0
      #
      #conclusion: light for resumed interaction starts at 3090186. Negative continues until 3696258.
      negintStart <- 3090186
      negintEnd <- which(pp$LED == 0 & 1:length(pp$LED) > negintStart)[1L] - 1L      
    } else if (id %in% c("8029", "8057", "8063", "8092")) {
      #8029 had the light on from the beginning of the session
      #Same issue for 8057, 8092
      #But the offset time of the light is informative.
      #So use negintEnd - 10 minutes as the negintStart
      message("Light was on from the beginning: backtracking 10 minutes from light off for negative interaction")
      negintEnd <- which(pp$LED == 0)[1L] - 1L #first time light is off is end of negint
      negintStart <- negintEnd - 1000*10*60
    } else {
      negintStart <- head(ledOn, n=1)
      negintEnd <- which(pp$LED == 0 & 1:length(pp$LED) > negintStart)[1L] - 1L  
    }
    
    if (is.na(negintStart) || is.na(negintEnd)) {
      cat("Unable to identify negative interaction start/end for id:", id, "\n")
      cat("  Skipping to next participant\n")
      next
    }
    negintLength <- (negintEnd - negintStart)/1000/60 #in minutes
    if (negintLength < 9 || negintLength > 15) {
      cat("Negative interaction not approximately 10 minutes:", negintLength, "\n")
      cat("Skipping to next participant\n")
      next
    }    
    
    cat("Negative interaction length:", round(negintLength,2), "minutes\n")
    
    neg.ecg.r <- pp$R.ECG[negintStart:negintEnd]
    neg.ecg.l <- pp$L.ECG[negintStart:negintEnd]
    neg.gsr.r <- pp$R.GSR[negintStart:negintEnd]
    neg.gsr.l <- pp$L.GSR[negintStart:negintEnd]
    
    write.table(neg.ecg.r, file=paste0("8009_proc/ecg_raw/", id, "_r_negint_ecgraw.txt"), row.names=FALSE, col.names=FALSE)
    write.table(neg.ecg.l, file=paste0("8009_proc/ecg_raw/", id, "_l_negint_ecgraw.txt"), row.names=FALSE, col.names=FALSE)
    write.table(data.frame(time=seq(0, length(neg.gsr.r)- 1L)/1000, gsr=neg.gsr.r), file=paste0("8009_proc/gsr_raw/", id, "_r_negint_gsrraw.txt"), row.names=FALSE, col.names=FALSE)
    write.table(data.frame(time=seq(0, length(neg.gsr.l)- 1L)/1000, gsr=neg.gsr.l), file=paste0("8009_proc/gsr_raw/", id, "_l_negint_gsrraw.txt"), row.names=FALSE, col.names=FALSE)
    
    ##looks for positive interaction after negative
    posintStart <- ledOn[which(ledOn > negintEnd)[1L]]    
  } else {
    posintStart <- head(ledOn, n=1)
  }
  
  if (searchPositive) {
    posintEnd <- which(pp$LED == 0 & 1:length(pp$LED) > posintStart)[1L] - 1L
    if (is.na(posintStart) || is.na(posintEnd)) {
      cat("Unable to identify positive interaction start/end for id: ", id, "\n")
      cat("  Skipping to next participant\n")
      next
    }
    posintLength <- (posintEnd - posintStart)/1000/60 #in minutes
    if (posintLength < 4 || posintLength > 7) {
      cat("Positive interaction not approximately 5 minutes:", posintLength, "\n")
      cat("Skipping to next participant\n")
      next
    }
    
    cat("Positive interaction length:", round(posintLength,2), "minutes\n")
    
    pos.ecg.r <- pp$R.ECG[posintStart:posintEnd]
    pos.ecg.l <- pp$L.ECG[posintStart:posintEnd]
    pos.gsr.r <- pp$R.GSR[posintStart:posintEnd]
    pos.gsr.l <- pp$L.GSR[posintStart:posintEnd]
    
    write.table(pos.ecg.r, file=paste0("8009_proc/ecg_raw/", id, "_r_posint_ecgraw.txt"), row.names=FALSE, col.names=FALSE)
    write.table(pos.ecg.l, file=paste0("8009_proc/ecg_raw/", id, "_l_posint_ecgraw.txt"), row.names=FALSE, col.names=FALSE)
    write.table(data.frame(time=seq(0, length(pos.gsr.r)- 1L)/1000, gsr=pos.gsr.r), file=paste0("8009_proc/gsr_raw/", id, "_r_posint_gsrraw.txt"), row.names=FALSE, col.names=FALSE)
    write.table(data.frame(time=seq(0, length(pos.gsr.l)- 1L)/1000, gsr=pos.gsr.l), file=paste0("8009_proc/gsr_raw/", id, "_l_posint_gsrraw.txt"), row.names=FALSE, col.names=FALSE)
    
  }
  
  
}



#
##process using Cmetx
#setwd("~/BPD_Psychophys/Baseline_RSA")
#for (f in ibiList) {
#	f <- sub("\\.txt", "", f)
#	system(paste("wine CMETX.EXE ", f, " -o BASELINEHRV", sep=""))
#}
#
##read Cmetx output
#BaselineHRV <- read.csv("BASELINEHRV.CSV", header=TRUE)
##convert IDs from full pathnames to numbers. Character class matches any non-digit charater
#BaselineHRV$ID <- as.numeric(sub("^[^\\d]*(\\d+).*$", "\\1", BaselineHRV$ID, perl=TRUE)) 
