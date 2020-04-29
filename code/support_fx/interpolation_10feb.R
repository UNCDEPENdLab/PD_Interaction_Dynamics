setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/code")




##is there a better way to represent the data than what I've done here?
updated_all_ibis <- list.files(path="/Users/alisonmarie526/Desktop/ecg_raw_and_edited", pattern=".*negint.*\\.txt", full.names=TRUE)
updated_ids <- as.numeric(sub("/Users/alisonmarie526/Desktop/ecg_raw_and_edited/(8[0-9]{3}).*", "\\1", updated_all_ibis, perl=TRUE))

#ask michael how to fix updated position, cuz man those are some iregular expressions
#updated_position <- sub("/Users/alisonmarie526/Desktop/ecg_raw_and_edited/8[0-9]{3}.*([lr])*", "\\2", updated_all_ibis, perl=TRUE)
updated_position <- sub("/Users/alisonmarie526/Desktop/ecg_raw_and_edited/8[0-9]{3}_([lr])_.*", "\\1", updated_all_ibis, perl=TRUE)


df <- as.data.frame(table(updated_ids))
allibis <- c()
for (i in 1:length(updated_all_ibis)) {
  f <- read.table(updated_all_ibis[i], col.names=c("time", "ibi"))
  f$PTNUM_fromfname = as.numeric(sub("/Users/alisonmarie526/Desktop/ecg_raw_and_edited/(8[0-9]{3}).*", "\\1", updated_all_ibis[i], perl=TRUE))
  f$PTNUM <- as.numeric(updated_ids[i])
  f$position <- factor(updated_position[i])
  allibis <- rbind(allibis, f)
}
physio_split <- split(allibis, allibis$PTNUM)
stopifnot(length(unique(sapply(list(updated_all_ibis, updated_ids,updated_position), length))) == 1)

#edited version of interpolate couple, only ibi streams
#something is not working here, getting 10 times the number of time points as before
#certainly not decreasing Hz as it should....
interpolateIBI <- function(ibi_couple, freqHz=10) {
  #figure min and max times for ibi and behavior to create common time grid
  l_ibi <- subset(ibi_couple, position=="l")
  r_ibi <- subset(ibi_couple, position=="r")
  
  #ibi streams are unique per person, whereas joystick timing is shared by couple
  xoutmin <- max(min(l_ibi$time), min(r_ibi$time))
  xoutmax <- min(max(l_ibi$time), max(r_ibi$time))
  
  gridstep <- 1/freqHz * 1000
  interp_time_grid <- seq(xoutmin, xoutmax, by=gridstep) #100ms increment
  
  #have 2 time series to resample onto grid (could probably functionalize this, but happy just to repeat...)
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






#also has to be better way to save data
#ideally have txt files (for VBA) and also a large data frame that can be used for analyses in dynR
interpolated_physio_split <- lapply(physio_split, interpolateIBI)
interpolated_aligned_physio_split <- lapply(interpolated_physio_split, alignLR_PatientPartner)
