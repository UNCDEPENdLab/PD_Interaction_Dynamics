library(tidyverse)
library(lavaan)
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/")

#1.Negative Interaction
dat4 <- read.csv("data/SITAR_Analyses/dat4.csv")
dat4_sccc <- dplyr::select(dat4, scpt, scpr, ccpt, ccpr, PTNUM)
dat4_sccc_long <- gather(dat4_sccc, key = "Coupling_Param", value = "Value", -PTNUM)
t.test(dat4$scpt, dat4$scpr)
t.test(dat4$ccpt, dat4$ccpr)
mean(dat4$scpt/1000)
sd(dat4$scpt/1000)
mean(dat4$scpr/1000)
sd(dat4$scpr/1000)
mean(dat4$ccpt/1000)
sd(dat4$ccpt/1000)
mean(dat4$ccpr/1000)
sd(dat4$ccpr/1000)
g <- ggplot(dat4_sccc_long, aes(x = Value))  + geom_histogram(bins = 15)  + facet_wrap(~ as.factor(Coupling_Param), nrow = 1, ncol = 4) + theme_bw(base_size = 10) + ylab("Frequency") + xlab("Coupling Parameter") 
#2. Baseline
dat4_v_sccc <- dplyr::select(dat4, v_scpt, v_scpr, v_ccpt, v_ccpr, PTNUM)
dat4_v_sccc_long <- gather(dat4_v_sccc, key = "Coupling_Param", value = "Value", -PTNUM)
#pdf("Histograms of Coupling.pdf", width = 20, height = 10)
ggplot(dat4_v_sccc_long, aes(x = Value))  + geom_histogram(bins = 15)  + facet_wrap(~ as.factor(Coupling_Param), nrow = 1, ncol = 4) + theme_bw(base_size = 10) + ylab("Frequency") + xlab("Coupling Parameter") 
#dev.off()
t.test(dat4$v_scpt, dat4$v_scpr)
t.test(dat4$v_ccpt, dat4$v_ccpr)
mean(dat4$v_scpt/1000)
sd(dat4$v_scpt/1000)
mean(dat4$v_scpr/1000)
sd(dat4$v_scpr/1000)
mean(dat4$v_ccpt/1000)
sd(dat4$v_ccpt/1000)
mean(dat4$v_ccpr/1000)
sd(dat4$v_ccpr/1000)


#3. Basic correlation between personality traits of interest
m2dat4 <- read.csv("data/SITAR_Analyses/m2dat4.csv")
cor(dplyr::select(m2dat4, iip_elevation_patient, iip_elevation_partner, pdtot_patient, pdtot_partner))
    
#4. Traits + Physio
el <- "
    scpt ~ iip_elevation_patient + v_scpt
    scpr ~ iip_elevation_partner + v_scpr
    ccpt ~ iip_elevation_patient + v_ccpt
    ccpr ~ iip_elevation_partner+ v_ccpr
     scpr ~ v_ccpr
     ccpr ~ v_scpr
     ccpr ~ v_scpt
     ccpr ~ v_ccpt
    "
el_m <- sem(el, m2dat4, estimator = "MLR", missing = "ML",meanstructure = TRUE, mimic = "Mplus")
parTable(el_m)
summary(el_m, fit.measures = TRUE)
el_a <- sem(el, m2dat4, estimator = "ML", missing = "listwise",meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)
    

pd <- "
scpt ~ pdtot_patient + v_scpt
scpr ~ pdtot_partner + v_scpr
ccpt ~ pdtot_patient +pdtot_partner +  v_ccpt
ccpr ~ pdtot_partner+ v_ccpr
scpr ~ v_ccpr
ccpr ~ v_scpr
ccpt ~ v_ccpr
ccpt ~ v_scpr
ccpr ~ v_scpt
ccpr ~ v_ccpt
"
pd_m <- sem(pd, m2dat4, estimator = "MLR", missing = "ML",meanstructure = TRUE, mimic = "Mplus")
parTable(pd_m)
summary(pd_m, fit.measures = TRUE)
pd_a <- sem(pd, m2dat4, estimator = "ML", missing = "listwise",meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)

m14dat6 <- read.csv("data/SITAR_Analyses/m14dat6.csv")

elpd <- "
scpt ~ pdtot_patient + iip_elevation_patient + v_scpt
scpr ~ pdtot_partner + iip_elevation_partner + v_scpr
ccpt ~ pdtot_patient+  pdtot_partner + iip_elevation_patient + v_ccpt
ccpr ~   iip_elevation_patient + v_ccpr
scpr ~ v_ccpr
ccpr ~ v_scpr + v_scpt + v_ccpt
"
elpd_m <- sem(elpd, m14dat6, estimator = "MLR", missing = "ML",meanstructure = TRUE, mimic = "Mplus")
parTable(elpd_m)
summary(elpd_m, fit.measures = TRUE)
elpd_a <- sem(elpd, m2dat4, estimator = "ML", missing = "listwise",meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)


#5. SPAFF + Physio
readSPAFFFile <- function(fname) {
  stopifnot(file.exists(fname))
  
  #Handle three possible file formats, one uses semicolons and has slightly different field names (newer), the other uses commas (older).
  #The newest has a separate set of fields
  head <- scan(fname, what="character", n=10, sep = "\n") #, fileEncoding="UTF-16LE") #NEWEST FILES HAVE UTF 16 encoding, which throws things off
  hlineRow <- grep("Header Lines:", head, fixed=TRUE) #this line always comes just before field names
  if (length(hlineRow) == 0L) {
    #newest format does not have the Header Lines: row in the text file, just begins with field names on first row
    hlineRow <- 1
    fieldRow <- head[1L]
    skip <- 1
  } else {	
    fieldRow <- scan(fname, what="character", n=1, skip=hlineRow, sep="\n")
    skip <- hlineRow + 1
  }
  
  #expect a bunch of semicolons or commas in this row
  ncommas <- sapply(regmatches(fieldRow, gregexpr(",", fieldRow)), length)
  nsemicolons <- sapply(regmatches(fieldRow, gregexpr(";", fieldRow)), length)
  
  if (ncommas >= 5 && ncommas > nsemicolons) {
    sep <- ","
  } else if (nsemicolons >= 5 && nsemicolons > ncommas) {
    sep <- ";"
  } else { warning("Unable to determine field separator.") }
  
  # old field names format (e.g., 8003_P1.txt)
  # [1] "Relative Time (seconds)" "Observation Name"        "Event Log File Name"     "Subject"                
  # [5] "Behavior"                "Event Type"              "Comment" 
  
  # new field names format (e.g., 8015_P1.txt)
  # [1] "Time"                "ObservationName"     "Event Log File Name"      "Subject"
  # [5] "Behavior"            "State Event"         "Comment" 
  
  fieldNames <- gsub("\"", "", strsplit(fieldRow, sep)[[1]])
  behav <- read.table(fname, skip=skip, header=FALSE, sep=sep)
  #for almost all files, there is a trailing comma/semicolon in file format, so drop last column
  if (ncol(behav) - length(fieldNames) == 1) { behav[[ncol(behav)]] <- NULL }
  names(behav) <- make.names(fieldNames)
  
  if (identical(names(behav), c("Time", "ObservationName", "Event.Log.File.Name", "Subject", "Behavior", "State.Event", "Comment"))) {
    #rename to old-style names for consistency
    names(behav) <- c("Relative.Time..seconds.", "Observation.Name", "Event.Log.File.Name", "Subject", "Behavior", "Event.Type", "Comment")
  } else if (identical(names(behav),  c("Relative.Time..seconds.", "Observation.Name", "Event.Log.File.Name", "Subject", "Behavior", "Event.Type", "Comment"))) {
    
    names(behav) <- c("Relative.Time..seconds.", "Observation.Name", "Event.Log.File.Name", "Subject", "Behavior", "Event.Type", "Comment")
    
  } else if (identical(names(behav), c("Date_Time_Absolute_dmy_hmsf", "Date_dmy", "Time_Absolute_hms", "Time_Absolute_f", "Time_Relative_hmsf", "Time_Relative_hms",
                                       "Time_Relative_f", "Time_Relative_sf", "Duration_sf", "Observation", "Event_Log", "Behavior", "Event_Type"))) {
    #newest format: drop irrelevant columns
    behav <- behav[,c("Time_Relative_sf", "Observation", "Behavior", "Event_Type")]
    names(behav) <- c("Relative.Time..seconds.", "Observation.Name", "Behavior", "Event.Type")
  } else {
    print(names(behav))
    stop("Unable to identify column names in SPAFF file: ", fname)
  }
  
  return(behav[,c("Relative.Time..seconds.", "Observation.Name", "Behavior", "Event.Type")])
} 

idfolders <- list.files(path="/Users/alisonmarie526/Box Sync/DEPENd/Projects/GSR/data/renamed_files", full.names=TRUE, recursive = FALSE)
idlist <- as.numeric(sub("/Users/alisonmarie526/Box Sync/DEPENd/Projects/GSR/data/renamed_files/(8[0-9]{3}).*", "\\1", idfolders, perl = TRUE))

tb <- as.data.frame(table(idlist))
allSPAFFfiles <- c()
for (i in 1:length(idfolders)) {
  filename = idfolders[i]
  f <- readSPAFFFile(filename)
  f <- dplyr::rename(f, time = Relative.Time..seconds.)
  f <- dplyr::filter(f, Event.Type == "State stop")
  f <- dplyr::mutate(f, diff_event = c(f$time[[1]], diff(time)))
  f$PTNUM <- factor(idlist[i])
  f$K <- sub("/Users/alisonmarie526/Box Sync/DEPENd/Projects/GSR/data/renamed_files/8[0-9]{3}.*(P[1-2]{1}).*", "\\1",idfolders[i],perl= TRUE)
  allSPAFFfiles <-rbind(allSPAFFfiles, f)
}
physLog <- read.csv(file = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/GSR/data/physLog.csv")
physLog <- dplyr::select(physLog, PTNUM, DyadID, UsrID, K)
physLog$PTNUM <- as.character(physLog$PTNUM)
physLog$K <- as.character(physLog$K)
allSPAFFfiles$K <- as.character(allSPAFFfiles$K)
allSPAFFfiles$PTNUM <- as.character(allSPAFFfiles$PTNUM)
spaff_patpar <- dplyr::inner_join(allSPAFFfiles, physLog, by = c("PTNUM", "K")) 
spaff_patpar$Behavior <- as.character(spaff_patpar$Behavior)
spaff_patpar$valence <- sapply(spaff_patpar$Behavior, function(x) {
  x <- tolower(as.character(x))
  
  if (x=="disgust") { -3
  } else if (x=="contempt") { -4
  } else if (x=="belligerence") { -2
  } else if (x=="low domineering") { -1
  } else if (x=="high domineering") { -1
  } else if (x=="criticism") { -2              
  } else if (x=="anger") { -1
  } else if (x=="tension") { 0              
  } else if (x=="tense humor" || x=="tension/humor") { 2              
  } else if (x=="defensive") { -2
  } else if (x=="whining") { -1
  } else if (x=="sadness") { -1
  } else if (x=="stonewalling" || x=="stonewall") { -2
  } else if (x=="neutral") { 0.1
  } else if (x=="interest") { 2
  } else if (x=="low validation") { 4
  } else if (x=="high validation") { 4
  } else if (x=="affection") { 4
  } else if (x=="humor") { 4
  } else if (x=="surprise/joy") { 4              
  } else if (x=="physical affection") { 4              
  } else { stop("cannot match code: ", x)}            
})

spaff_patpar$n3 <- factor(sapply(spaff_patpar$Behavior, function(x) {
  x <- tolower(as.character(x))
  
  if (x=="disgust") { "nasty"
  } else if (x=="contempt") { "nasty"
  } else if (x=="belligerence") { "nasty"
  } else if (x=="low domineering") { "nasty"
  } else if (x=="high domineering") { "nasty"
  } else if (x=="criticism") { "nasty"   
  } else if (x=="anger") { "nasty"
  } else if (x=="tension") { "nasty"              
  } else if (x=="tense humor" || x=="tension/humor") { "nice"              
  } else if (x=="defensive") { "nasty"
  } else if (x=="whining") { "nasty"
  } else if (x=="sadness") { "nasty"
  } else if (x=="stonewalling" || x=="stonewall") { "nasty"
  } else if (x=="neutral") { "neutral"
  } else if (x=="interest") { "nice"
  } else if (x=="low validation") { "nice"
  } else if (x=="high validation") { "nice"
  } else if (x=="affection") { "nice"
  } else if (x=="humor") { "nice"
  } else if (x=="surprise/joy") { "nice"              
  } else if (x=="physical affection") { "nice"              
  } else { stop("cannot match code: ", x)}            
}))

df <- read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/params_personalitydata_all.csv")

df_personalitydata <- dplyr::select(df, PTNUM, iip_elevation_patient, iip_elevation_partner, pdtot_patient, pdtot_partner, self_coupling_patient, self_coupling_partner, cross_coupling_patient, cross_coupling_partner)
df_personalitydata_patient <- dplyr::select(df_personalitydata, iip_elevation_patient, pdtot_patient, self_coupling_patient, cross_coupling_patient, PTNUM)
df_personalitydata_partner <- dplyr::select(df_personalitydata, iip_elevation_partner, pdtot_partner, self_coupling_partner, cross_coupling_partner, PTNUM)
df_personalitydata_patient$DyadID <- paste0(df_personalitydata_patient$PTNUM, "_1")
df_personalitydata_patient$Patient <- 1
df_personalitydata_partner$DyadID <- paste0(df_personalitydata_partner$PTNUM, "_0")
df_personalitydata_partner$Patient <- 0
df_personalitydata_patient <-dplyr::rename(df_personalitydata_patient, iip_elevation = iip_elevation_patient, pdtot = pdtot_patient, self_coupling = self_coupling_patient, cross_coupling = cross_coupling_patient)
df_personalitydata_partner <-dplyr::rename(df_personalitydata_partner, iip_elevation = iip_elevation_partner, pdtot = pdtot_partner, self_coupling = self_coupling_partner, cross_coupling = cross_coupling_partner)
df_personaitydata_long <- dplyr::bind_rows(df_personalitydata_patient, df_personalitydata_partner)


#df_personalitydata <- dplyr::select(m13dat4, PTNUM, iip_elevation_patient, iip_elevation_partner, pdcpt, pdcpr, scpr, scpt, ccpt, ccpr)
df_personalitydata_patient <- dplyr::select(df_personalitydata, iip_elevation_patient, pdtot_patient, self_coupling_patient, cross_coupling_patient, PTNUM)
df_personalitydata_partner <- dplyr::select(df_personalitydata, iip_elevation_partner, pdtot_partner, self_coupling_partner, cross_coupling_partner, PTNUM)
df_personalitydata_patient$DyadID <- paste0(df_personalitydata_patient$PTNUM, "_1")
df_personalitydata_patient$Patient <- 1
df_personalitydata_partner$DyadID <- paste0(df_personalitydata_partner$PTNUM, "_0")
df_personalitydata_partner$Patient <- 0
df_personalitydata_patient <-dplyr::rename(df_personalitydata_patient, iip_elevation = iip_elevation_patient, pdtot = pdtot_patient, self_coupling = self_coupling_patient, cross_coupling = cross_coupling_patient)
df_personalitydata_partner <-dplyr::rename(df_personalitydata_partner, iip_elevation = iip_elevation_partner, pdtot = pdtot_partner, self_coupling = self_coupling_partner, cross_coupling = cross_coupling_partner)
df_personaitydata_long <- bind_rows(df_personalitydata_patient, df_personalitydata_partner)

#merge personality data with SPAFF

df_personaitydata_long$UsrID <- paste0(df_personaitydata_long$PTNUM, df_personaitydata_long$Patient)
spaff_simp <- dplyr::select(df_personaitydata_long, DyadID, DyadID, iip_elevation, pdtot, Patient, UsrID, PTNUM) 
spaff_patpar_UsrIDs <- dplyr::select(spaff_patpar, UsrID) %>% unique()
spaff_patpar_UsrIDs <- as.vector(spaff_patpar_UsrIDs[["UsrID"]])
spaff_simp <- dplyr::filter(spaff_simp, UsrID %in% spaff_patpar_UsrIDs) 
spaff_simp_split <- split(spaff_simp, spaff_simp$UsrID)
spaff_patpar <- dplyr::filter(spaff_patpar, UsrID %in% as.vector(spaff_simp$UsrID))
spaff_patpar$n3 <- as.character(spaff_patpar$n3)
spaff_split <- split(spaff_patpar, spaff_patpar$UsrID)
# spaff_split <- lapply(spaff_split, function(x) {
#   x$n3 <- as.character(x$n3)
# })
for (i in 1:length(unique(spaff_patpar$UsrID))) {
  #num_nasty <- sum(as.character(x$n3) =="nasty")
  # num_nice <- sum(as.character(x$n3) == "nice")
  # num_neutral <- sum(as.character(x$n3) == "neutral")
  # time_nasty <- dplyr::filter(x, n3 == "nasty") %>% summarise(total_time= sum(diff_event)
  # time_neutral <- dplyr::filter(x,n3 == "neutral") %>% summarise(total_time = sum(diff_event)
  # time_nice <- dplyr::filter(x, n3 == "nice") %>% summarise(total_time = sum(diff_event)
  time_nasty = dplyr::filter(spaff_split[[i]],n3 =="nasty") %>% summarise(sum(diff_event))
  time_nice = dplyr::filter(spaff_split[[i]],n3 =="nice") %>% summarise(sum(diff_event))
  time_neutral = dplyr::filter(spaff_split[[i]],n3 == "neutral") %>% summarise(sum(diff_event))
  spaff_simp_split[[i]] <- dplyr::mutate(spaff_simp_split[[i]], 
                                         num_nasty = sum(as.character(spaff_split[[i]]$n3) == "nasty"),
                                         num_nice = sum(as.character(spaff_split[[i]]$n3) == "nice"),
                                         num_neutral = sum(as.character(spaff_split[[i]]$n3) == "neutral"),
                                         nastytime = time_nasty[[1]],
                                         neutraltime = time_neutral[[1]],
                                         nicetime = time_nice[[1]]
  )
}
#cuts out 8093, because got cut from personality data
library(plyr)
spaff_summarized <- ldply(spaff_simp_split, data.frame)
spaff_summarized_patient <- dplyr::filter(spaff_summarized, Patient == 1)
spaff_summarized_partner <- dplyr::filter(spaff_summarized, Patient == 0)
spaff_summarized_patient <- dplyr::rename(spaff_summarized_patient, pdtot_patient = pdtot, iip_elevation_patient = iip_elevation, num_nasty_patient = num_nasty, num_neutral_patient = num_neutral, num_nice_patient = num_nice, nastytime_patient = nastytime, neutraltime_patient = neutraltime, nicetime_patient = nicetime)
spaff_summarized_partner <- dplyr::rename(spaff_summarized_partner, pdtot_partner = pdtot, iip_elevation_partner = iip_elevation, num_nasty_partner = num_nasty, num_neutral_partner = num_neutral, num_nice_partner = num_nice, nastytime_partner = nastytime, neutraltime_partner = neutraltime, nicetime_partner= nicetime)
spaff_summarized_wide <- merge(spaff_summarized_patient, spaff_summarized_partner, by = c("PTNUM")) #117
spaff_summarized_wide <- spaff_summarized_wide %>% mutate(nastyratio_patient = nastytime_patient/(nicetime_patient + neutraltime_patient),
                                                          niceratio_patient = 100*(nicetime_patient/(nastytime_patient + neutraltime_patient)),
                                                          neutralratio_patient = neutraltime_patient/(nastytime_patient + nicetime_patient),
                                                          nastyratio_partner = nastytime_partner/(nicetime_partner + neutraltime_partner),
                                                          niceratio_partner = 100*(nicetime_partner/(nastytime_partner + neutraltime_partner)),
                                                          neutralratio_partner = neutraltime_partner/(nastytime_partner + nicetime_partner),
                                                          nastynice_patient = nastytime_patient/(1+nicetime_patient),
                                                          nastynice_partner = nastytime_partner/(1+nicetime_partner),
                                                          neutralnice_patient = neutraltime_patient/(1+nicetime_patient),
                                                          neutralnice_partner= neutraltime_partner/(1+nicetime_partner))

spaff_summarized_wide_sccc <- inner_join(spaff_summarized_wide, dat4) # n = 101
tocor <- dplyr::mutate(spaff_summarized_wide_sccc, resid_scpt = resid(lm(scpt ~ v_scpt)),
                       resid_scpr = resid(lm(scpr ~ v_scpr)),
                       resid_ccpt = resid(lm(ccpt ~ v_ccpt)),
                       resid_ccpr = resid(lm(ccpr ~ v_ccpr)))
cor(dplyr::select(tocor, resid_scpt, resid_scpr, resid_ccpt, resid_ccpr, nastynice_patient, nastynice_partner, neutralnice_patient, neutralnice_partner))


#SPAFF+Physio by Personality Varaibles
el_physio_spaff<- "
scpt ~ v_scpt  + nastynice_patient + neutralnice_patient + iip_elevation_patient
ccpt ~ v_ccpt  + nastynice_patient + neutralnice_patient + iip_elevation_patient #+ v_ccpr + v_scpr
scpr ~ v_scpr  + nastynice_partner + neutralnice_partner + iip_elevation_partner + v_ccpr + v_scpt
ccpr ~ v_ccpr  + nastynice_partner + neutralnice_partner + iip_elevation_partner + v_scpr + iip_elevation_patient + v_scpt

"

el_physio_spaff_m <- sem(el_physio_spaff, spaff_summarized_wide_sccc, estimator= "MLR", missing = "ML", meanstructure = TRUE, mimic = "Mplus")
summary(el_physio_spaff_m, fit.measures = TRUE)

el_physio_spaff_a <-sem(el_physio_spaff, spaff_summarized_wide_sccc, estimator= "ML", missing = "listwise", meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)


pd_physio_spaff<- "
scpt ~ v_scpt  + nastynice_patient + neutralnice_patient + pdtot_patient
ccpt ~ v_ccpt  + nastynice_patient + neutralnice_patient + pdtot_patient+pdtot_partner+ v_ccpr #+ v_scpr
scpr ~ v_scpr  + nastynice_partner + neutralnice_partner + pdtot_partner + v_ccpr #+ v_scpt
ccpr ~ v_ccpr  + nastynice_partner + neutralnice_partner + pdtot_partner + v_scpr #+ iip_elevation_patient + v_scpt

"

pd_physio_spaff_m <- sem(pd_physio_spaff, spaff_summarized_wide_sccc, estimator= "MLR", missing = "ML", meanstructure = TRUE, mimic = "Mplus")
summary(pd_physio_spaff_m, fit.measures = TRUE)
pd_physio_spaff_a <- sem(pd_physio_spaff, spaff_summarized_wide_sccc, estimator= "ML", missing = "listwise", meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)

elpd_physio_spaff<- "
scpt ~ v_scpt  + nastynice_patient + neutralnice_patient + iip_elevation_patient + pdtot_patient
ccpt ~ v_ccpt  + nastynice_patient + neutralnice_patient + iip_elevation_patient + pdtot_patient +pdtot_partner #+ v_ccpr #+ v_scpr
scpr ~ v_scpr  + nastynice_partner + neutralnice_partner + iip_elevation_partner + pdtot_partner + v_ccpr #+ v_scpt
ccpr ~ v_ccpr  + nastynice_partner + neutralnice_partner + iip_elevation_partner +pdtot_partner + v_scpr + iip_elevation_patient + v_scpt

"

elpd_physio_spaff_m <- sem(elpd_physio_spaff, spaff_summarized_wide_sccc, estimator= "MLR", missing = "ML", meanstructure = TRUE, mimic = "Mplus")
summary(elpd_physio_spaff_m, fit.measures = TRUE)
elpd_physio_spaff_a <- sem(elpd_physio_spaff, spaff_summarized_wide_sccc, estimator= "ML", missing = "listwise", meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)


#now just nastynice
el_physio_spaff_nast<- "
scpt ~ v_scpt  + nastynice_patient  + iip_elevation_patient
ccpt ~ v_ccpt  + nastynice_patient  + iip_elevation_patient #+ v_ccpr + v_scpr
scpr ~ v_scpr  + nastynice_partner  + iip_elevation_partner + v_ccpr + v_scpt
ccpr ~ v_ccpr  + nastynice_partner  + iip_elevation_partner + v_scpr + iip_elevation_patient + v_scpt

"

el_physio_spaff_nast_m <- sem(el_physio_spaff_nast, spaff_summarized_wide_sccc, estimator= "MLR", missing = "ML", meanstructure = TRUE, mimic = "Mplus")
summary(el_physio_spaff_m, fit.measures = TRUE)

el_physio_spaff_nast_a <- sem(el_physio_spaff_nast, spaff_summarized_wide_sccc, estimator= "ML", missing = "listwise", meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)



pd_physio_spaff_nast<- "
scpt ~ v_scpt  + nastynice_patient  + pdtot_patient
ccpt ~ v_ccpt  + nastynice_patient  + pdtot_patient+pdtot_partner+ v_ccpr #+ v_scpr
scpr ~ v_scpr  + nastynice_partner  + pdtot_partner + v_ccpr #+ v_scpt
ccpr ~ v_ccpr  + nastynice_partner  + pdtot_partner + v_scpr #+ iip_elevation_patient + v_scpt

"

pd_physio_spaff_nast_m <- sem(pd_physio_spaff_nast, spaff_summarized_wide_sccc, estimator= "MLR", missing = "ML", meanstructure = TRUE, mimic = "Mplus")
summary(pd_physio_spaff_nast_m, fit.measures = TRUE)
pd_physio_spaff_nast_a <-  sem(pd_physio_spaff_nast, spaff_summarized_wide_sccc, estimator= "ML", missing = "listwise", meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)


elpd_physio_spaff_nast<- "
scpt ~ v_scpt  + nastynice_patient  + iip_elevation_patient + pdtot_patient
ccpt ~ v_ccpt  + nastynice_patient  + iip_elevation_patient + pdtot_patient +pdtot_partner #+ v_ccpr #+ v_scpr
scpr ~ v_scpr  + nastynice_partner  + iip_elevation_partner + pdtot_partner + v_ccpr #+ v_scpt
ccpr ~ v_ccpr  + nastynice_partner  + iip_elevation_partner +pdtot_partner + v_scpr + iip_elevation_patient + v_scpt + v_ccpt

"

elpd_physio_spaff_nast_m <- sem(elpd_physio_spaff_nast, spaff_summarized_wide_sccc, estimator= "MLR", missing = "ML", meanstructure = TRUE, mimic = "Mplus")
summary(elpd_physio_spaff_nast_m, fit.measures = TRUE)
elpd_physio_spaff_nast_a <-  sem(elpd_physio_spaff_nast, spaff_summarized_wide_sccc, estimator= "ML", missing = "listwise", meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)

el_physio_spaff_neut<- "
scpt ~ v_scpt  + neutralnice_patient  + iip_elevation_patient
ccpt ~ v_ccpt  + neutralnice_patient  + iip_elevation_patient #+ v_ccpr + v_scpr
scpr ~ v_scpr  + neutralnice_partner  + iip_elevation_partner + v_ccpr + v_scpt
ccpr ~ v_ccpr  + neutralnice_partner  + iip_elevation_partner + v_scpr + iip_elevation_patient + v_scpt

"

el_physio_spaff_neut_m <- sem(el_physio_spaff_neut, spaff_summarized_wide_sccc, estimator= "MLR", missing = "ML", meanstructure = TRUE, mimic = "Mplus")
summary(el_physio_spaff_m, fit.measures = TRUE)
el_physio_spaff_neut_a <- sem(el_physio_spaff_neut, spaff_summarized_wide_sccc, estimator= "ML", missing = "listwise", meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)

pd_physio_spaff_neut<- "
scpt ~ v_scpt  + neutralnice_patient  + pdtot_patient
ccpt ~ v_ccpt  + neutralnice_patient  + pdtot_patient+pdtot_partner+ v_ccpr #+ v_scpr
scpr ~ v_scpr  + neutralnice_partner  + pdtot_partner + v_ccpr #+ v_scpt
ccpr ~ v_ccpr  + neutralnice_partner  + pdtot_partner + v_scpr #+ iip_elevation_patient + v_scpt

"

pd_physio_spaff_neut_m <- sem(pd_physio_spaff_neut, spaff_summarized_wide_sccc, estimator= "MLR", missing = "ML", meanstructure = TRUE, mimic = "Mplus")
summary(pd_physio_spaff_nast_m, fit.measures = TRUE)
pd_physio_spaff_neut_a <-  sem(pd_physio_spaff_neut, spaff_summarized_wide_sccc, estimator= "ML", missing = "listwise", meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)


elpd_physio_spaff_neut<- "
scpt ~ v_scpt  + neutralnice_patient  + iip_elevation_patient + pdtot_patient
ccpt ~ v_ccpt  + neutralnice_patient  + iip_elevation_patient + pdtot_patient +pdtot_partner #+ v_ccpr #+ v_scpr
scpr ~ v_scpr  + neutralnice_partner  + iip_elevation_partner + pdtot_partner + v_ccpr #+ v_scpt
ccpr ~ v_ccpr  + neutralnice_partner  + iip_elevation_partner +pdtot_partner + v_scpr + iip_elevation_patient + v_scpt

"

elpd_physio_spaff_neut_m <- sem(elpd_physio_spaff_neut, spaff_summarized_wide_sccc, estimator= "MLR", missing = "ML", meanstructure = TRUE, mimic = "Mplus")
summary(elpd_physio_spaff_neut_m, fit.measures = TRUE)
elpd_physio_spaff_neut_a <-  sem(elpd_physio_spaff_neut, spaff_summarized_wide_sccc, estimator= "ML", missing = "listwise", meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)
anova(el_physio_spaff_a, el_physio_spaff_nast_a, el_physio_spaff_neut_a, pd_physio_spaff_a, pd_physio_spaff_nast_a, pd_physio_spaff_neut_a,elpd_physio_spaff_a, elpd_physio_spaff_nast_a, elpd_physio_spaff_neut_a)

elpd_physio_spaff_nast_pruned<- "
scpt ~ v_scpt  + nastynice_patient  + iip_elevation_patient 
ccpt ~ v_ccpt  + nastynice_patient  + iip_elevation_patient + pdtot_patient +pdtot_partner #+ v_ccpr #+ v_scpr
scpr ~ v_scpr   + iip_elevation_partner + pdtot_partner + v_ccpr #+ v_scpt
ccpr ~ v_ccpr    + iip_elevation_partner + v_scpr + iip_elevation_patient + v_scpt

"
spaff_summarized_wide_sccc <- dplyr::filter(spaff_summarized_wide_sccc, PTNUM %in% m14dat6$PTNUM)
elpd_physio_spaff_nast_pruned_m <- sem(elpd_physio_spaff_nast_pruned, spaff_summarized_wide_sccc, estimator= "MLR", missing = "ML", meanstructure = TRUE, mimic = "Mplus")
summary(elpd_physio_spaff_nast_pruned_m, fit.measures = TRUE)

elpd_physio_spaff_nast_pruned_a <- sem(elpd_physio_spaff_nast_pruned, spaff_summarized_wide_sccc, estimator= "ML", missing = "listwise", meanstructure = TRUE, mimic = "Mplus", conditional.x = TRUE)
#of the non-pruned models, elpd_physio_spaff_nast_a works the best (slightly)
#pruned pathways outperforms non-pruned

#6 Mediation Models
## First load in data
saamprepost <- read.csv("data/SITAR_Analyses/dat7.csv")
saamprepost_spaff_personalitydata <- inner_join(saamprepost, spaff_summarized_wide, by = "PTNUM")
saamprepost_spaff_personalitydata_rescaled<- mutate(saamprepost_spaff_personalitydata,
                                                    nastyratio_patient = nastytime_patient/20,
                                                    nastytime_partner = nastytime_partner/20,
                                                    nicetime_patient = nicetime_patient/20,
                                                    nicetime_partner = nicetime_partner/20,
                                                    neutraltime_patient = neutraltime_patient/20,
                                                    neutraltime_partner = neutraltime_partner/20)
saamprepost_spaff_personalitydata_rescaled_noOutliers <- dplyr::filter(saamprepost_spaff_personalitydata_rescaled, neutraltime_patient > 0, nastytime_patient > 0, nicetime_patient >0, nastytime_partner > 0, nicetime_partner >0, neutraltime_partner >0)

saam_avo_m1 <- "
#direct effect
avo_post_partner ~ e*avo_pre_partner
#mediators
avo_post_partner ~  b4*nastynice_partner 
nastynice_patient ~ d1*avo_pre_patient
nastynice_partner~a4*avo_pre_partner 
avo_post_patient ~ f*avo_pre_patient 
avo_post_patient ~ c1*nastynice_patient
nastynice_patient ~~ nastynice_partner
b4a4 := a4*b4
"

saam_avo_m1_m <- sem(saam_avo_m1, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")



saam_avo_m2 <- "
#direct effect
avo_post_partner ~ e*avo_pre_partner
#mediators
avo_post_partner ~  b4*nastynice_partner + scpt + ccpt + scpr + ccpr
nastynice_patient ~ d1*avo_pre_patient
scpt ~ avo_pre_patient + avo_pre_partner
scpr ~ avo_pre_partner + avo_pre_patient
ccpt ~ avo_pre_patient + avo_pre_partner
ccpr ~ avo_pre_partner + avo_pre_patient
nastynice_partner~a4*avo_pre_partner
avo_post_patient ~ f*avo_pre_patient 
avo_post_patient ~ c1*nastynice_patient + scpt + ccpt + scpr + ccpr
nastynice_patient ~~ nastynice_partner
b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_avo_m2 <- sem(saam_avo_m2, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")
summary(saam_avo_m2, fit.measures = TRUE)

#getting rid of partner paths except for ccpt 

saam_avo_m3 <- "
#direct effect
avo_post_partner ~ e*avo_pre_partner
#mediators
avo_post_partner ~  b4*nastynice_partner + scpr + ccpr 
nastynice_patient ~ d1*avo_pre_patient
scpt ~ avo_pre_patient
scpr ~ avo_pre_partner
ccpt ~ avo_pre_patient + avo_pre_partner
ccpr ~ avo_pre_partner
nastynice_partner~a4*avo_pre_partner
avo_post_patient ~ f*avo_pre_patient 
avo_post_patient ~ c1*nastynice_patient + scpt + ccpt
nastynice_patient ~~ nastynice_partner 
b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_avo_m3_m <- sem(saam_avo_m3, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")


summary(saam_avo_m3_m, fit.measures = TRUE)



saam_avo_m3 <- "
#direct effect
avo_post_partner ~ e*avo_pre_partner
#mediators
avo_post_partner ~  scpt + ccpt+  scpr + ccpr 
#nastynice_patient ~ d1*avo_pre_patient
scpt ~ avo_pre_patient
scpr ~ avo_pre_partner
ccpt ~ avo_pre_patient
ccpr ~ avo_pre_partner
nastynice_partner~a4*avo_pre_partner
avo_post_patient ~ f*avo_pre_patient 
avo_post_patient ~ scpr + ccpr +  scpt + ccpt
#nastynice_patient ~~ nastynice_partner 
#b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_avo_m3_m <- sem(saam_avo_m3, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")


summary(saam_avo_m3, fit.measures = TRUE)

saam_avo_m4 <- "
#direct effect
avo_post_partner ~ e*avo_pre_partner
#mediators
avo_post_partner ~  b4*nastynice_partner + scpr + ccpr 
nastynice_patient ~ d1*avo_pre_patient
scpt ~ avo_pre_patient + v_scpt
scpr ~ avo_pre_partner + v_scpr
ccpt ~ avo_pre_patient + avo_pre_partner + v_ccpt
ccpr ~ avo_pre_partner + v_ccpr
nastynice_partner~a4*avo_pre_partner
avo_post_patient ~ f*avo_pre_patient 
avo_post_patient ~ c1*nastynice_patient + scpt + ccpt
nastynice_patient ~~ nastynice_partner 
b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_avo_m4_m <- sem(saam_avo_m4, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")


summary(saam_avo_m4_m, fit.measures = TRUE)



saam_avo_m5 <- "
#direct effect
avo_post_partner ~ e*avo_pre_partner
#mediators
avo_post_partner ~  b4*nastynice_partner + scpr + ccpr + ccpt + scpt
nastynice_patient ~ d1*avo_pre_patient
scpt ~ avo_pre_patient + v_scpt
scpr ~ avo_pre_partner + v_scpr
ccpt ~ avo_pre_patient + avo_pre_partner + v_ccpt
ccpr ~ avo_pre_partner + v_ccpr
nastynice_partner~a4*avo_pre_partner
avo_post_patient ~ f*avo_pre_patient 
avo_post_patient ~ c1*nastynice_patient + scpt + ccpt + ccpr + scpr
nastynice_patient ~~ nastynice_partner 
b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_avo_m5_m <- sem(saam_avo_m5, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")


summary(saam_avo_m5_m, fit.measures = TRUE)


saam_avo_m6 <- "
#direct effect
avo_post_partner ~ e*avo_pre_partner
#mediators
avo_post_partner ~  b4*nastynice_partner + scpr + ccpr + ccpt + scpt
nastynice_patient ~ d1*avo_pre_patient
scpt ~ avo_pre_patient + v_scpt
scpr ~ avo_pre_partner + v_scpr
ccpt ~ avo_pre_patient + avo_pre_partner + v_ccpt
ccpr ~ avo_pre_partner + v_ccpr
nastynice_partner~a4*avo_pre_partner
avo_post_patient ~ f*avo_pre_patient 
avo_post_patient ~ c1*nastynice_patient + scpt + ccpt + ccpr + scpr
nastynice_patient ~~ nastynice_partner 
b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_avo_m6_m <- sem(saam_avo_m6, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")

cor(dplyr::select(saamprepost_spaff_personalitydata_rescaled_noOutliers, scpt, ccpt, scpr, ccpr, nastynice_patient, nastynice_partner))

summary(saam_avo_m6_m, fit.measures = TRUE)

saam_anx_m1 <- "
#direct effect
anx_post_partner ~ e*anx_pre_partner
#mediators
anx_post_partner ~  b4*nastynice_partner 
nastynice_patient ~ d1*anx_pre_patient
nastynice_partner~a4*anx_pre_partner 
anx_post_patient ~ f*anx_pre_patient 
anx_post_patient ~ c1*nastynice_patient
nastynice_patient ~~ nastynice_partner
b4a4 := a4*b4
"

saam_anx_m1_m <- sem(saam_anx_m1, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")



saam_anx_m2 <- "
#direct effect
anx_post_partner ~ e*anx_pre_partner
#mediators
anx_post_partner ~  b4*nastynice_partner + scpt + ccpt + scpr + ccpr
nastynice_patient ~ d1*anx_pre_patient
scpt ~ anx_pre_patient + anx_pre_partner
scpr ~ anx_pre_partner + anx_pre_patient
ccpt ~ anx_pre_patient + anx_pre_partner
ccpr ~ anx_pre_partner + anx_pre_patient
nastynice_partner~a4*anx_pre_partner
anx_post_patient ~ f*anx_pre_patient 
anx_post_patient ~ c1*nastynice_patient + scpt + ccpt + scpr + ccpr
nastynice_patient ~~ nastynice_partner
b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_anx_m2_m <- sem(saam_anx_m2, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")


summary(saam_anx_m2, fit.measures = TRUE)
#getting rid of partner paths except for ccpt 

saam_anx_m3 <- "
#direct effect
anx_post_partner ~ e*anx_pre_partner
#mediators
anx_post_partner ~  b4*nastynice_partner + scpr + ccpr 
nastynice_patient ~ d1*anx_pre_patient
scpt ~ anx_pre_patient
scpr ~ anx_pre_partner
ccpt ~ anx_pre_patient + anx_pre_partner
ccpr ~ anx_pre_partner
nastynice_partner~a4*anx_pre_partner
anx_post_patient ~ f*anx_pre_patient 
anx_post_patient ~ c1*nastynice_patient + scpt + ccpt
nastynice_patient ~~ nastynice_partner 
b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_anx_m3_m <- sem(saam_anx_m3, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")


summary(saam_anx_m3_m, fit.measures = TRUE)



saam_anx_m4 <- "
#direct effect
anx_post_partner ~ e*anx_pre_partner
#mediators
anx_post_partner ~  scpt + ccpt+  scpr + ccpr 
#nastynice_patient ~ d1*anx_pre_patient
scpt ~ anx_pre_patient
scpr ~ anx_pre_partner
ccpt ~ anx_pre_patient
ccpr ~ anx_pre_partner
nastynice_partner~a4*anx_pre_partner
anx_post_patient ~ f*anx_pre_patient 
anx_post_patient ~ scpr + ccpr +  scpt + ccpt
#nastynice_patient ~~ nastynice_partner 
#b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_anx_m4_m <- sem(saam_anx_m4, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")


summary(saam_anx_m4, fit.measures = TRUE)

saam_anx_m5 <- "
#direct effect
anx_post_partner ~ e*anx_pre_partner
#mediators
anx_post_partner ~  b4*nastynice_partner + scpr + ccpr 
nastynice_patient ~ d1*anx_pre_patient
scpt ~ anx_pre_patient + v_scpt
scpr ~ anx_pre_partner + v_scpr
ccpt ~ anx_pre_patient + anx_pre_partner + v_ccpt
ccpr ~ anx_pre_partner + v_ccpr
nastynice_partner~a4*anx_pre_partner
anx_post_patient ~ f*anx_pre_patient 
anx_post_patient ~ c1*nastynice_patient + scpt + ccpt
nastynice_patient ~~ nastynice_partner 
b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_anx_m5_m <- sem(saam_anx_m5, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")


summary(saam_anx_m5_m, fit.measures = TRUE)



saam_anx_m6 <- "
#direct effect
anx_post_partner ~ e*anx_pre_partner
#mediators
anx_post_partner ~  b4*nastynice_partner + scpr + ccpr + ccpt + scpt
nastynice_patient ~ d1*anx_pre_patient
scpt ~ anx_pre_patient + v_scpt
scpr ~ anx_pre_partner + v_scpr
ccpt ~ anx_pre_patient + anx_pre_partner + v_ccpt
ccpr ~ anx_pre_partner + v_ccpr
nastynice_partner~a4*anx_pre_partner
anx_post_patient ~ f*anx_pre_patient 
anx_post_patient ~ c1*nastynice_patient + scpt + ccpt + ccpr + scpr
nastynice_patient ~~ nastynice_partner 
b4a4 := a4*b4
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_anx_m6_m <- sem(saam_anx_m6, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")


summary(saam_anx_m6_m, fit.measures = TRUE)





saam_anx_m7 <- "
#direct effect
anx_post_partner ~ anx_pre_partner
#mediators
anx_post_partner ~  nastynice_partner + scpr+  scpt + ccpt + ccpr
nastynice_patient ~ anx_pre_patient
scpt ~ anx_pre_patient + anx_pre_partner + v_scpt
scpr ~ anx_pre_partner + v_scpr
ccpt ~ anx_pre_patient + anx_pre_partner + v_ccpt
ccpr ~ anx_pre_partner + v_ccpr
nastynice_partner~anx_pre_partner
anx_post_patient ~ anx_pre_patient 
anx_post_patient ~ nastynice_patient + ccpr
#residual covariances
nastynice_patient ~~ nastynice_partner 
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_anx_m7_m <- sem(saam_anx_m7, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")
summary(saam_anx_m7_m, fit.measures = TRUE)
saamprepost_spaff_personalitydata_rescaled_noOutliers <- dplyr::mutate(saamprepost_spaff_personalitydata_rescaled_noOutliers, nastynice_patient = nastytime_patient/5*nicetime_patient, nastynice_partner = nastytime_partner/nicetime_partner,  nastyneutral_patient = nastytime_patient/nicetime_patient, 
                                                                       nastyneutral_partner = nastytime_partner/neutraltime_partner)


saam_anx_m8 <- "
#direct effect
anx_post_partner ~ anx_pre_partner
#mediators
anx_post_partner ~  neutralratio_partner + scpr+  scpt + ccpt + ccpr
neutralratio_patient ~ anx_pre_patient
scpt ~ anx_pre_patient + anx_pre_partner + v_scpt
scpr ~ anx_pre_partner + v_scpr
ccpt ~ anx_pre_patient + anx_pre_partner + v_ccpt 
ccpr ~ anx_pre_partner + v_ccpr
neutralratio_partner~anx_pre_partner
anx_post_patient ~ anx_pre_patient 
anx_post_patient ~ neutralratio_patient + ccpr
#residual covariances
neutralratio_patient ~~ neutralratio_partner 
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr

"

saam_anx_m8_m <- sem(saam_anx_m8, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")


summary(saam_anx_m8_m, fit.measures = TRUE)


saam_anx_m9 <- "
#direct effect
anx_post_partner ~ anx_pre_partner
#mediators
anx_post_partner ~  nastynice_partner + scpr+  scpt + ccpt + ccpr
nastynice_patient ~ anx_pre_patient
scpt ~ anx_pre_patient + anx_pre_partner + v_scpt 
scpr ~ anx_pre_partner + v_scpr + v_ccpr
ccpt ~ anx_pre_patient + anx_pre_partner + v_ccpt  + v_ccpr
ccpr ~ anx_pre_partner + v_ccpr + v_scpr
nastynice_partner~anx_pre_partner
anx_post_patient ~ anx_pre_patient 
anx_post_patient ~ nastynice_patient + ccpr
#residual covariances
nastynice_patient ~~ nastynice_partner 
scpt ~~ scpr
scpt~~ ccpt
scpr ~~ ccpr
nastynice_patient ~~ ccpt

"

saam_anx_m9_m <- sem(saam_anx_m9, saamprepost_spaff_personalitydata_rescaled_noOutliers, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")
summary(saam_anx_m9_m, fit.measures = TRUE)


anova(saam_anx_m1_m, saam_anx_m2_m, saam_anx_m3_m, saam_anx_m4_m, saam_anx_m5_m, saam_anx_m6_m)





