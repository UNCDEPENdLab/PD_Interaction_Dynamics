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

# df <- read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/params_personalitydata_all.csv")
# 
# df_personalitydata <- dplyr::select(df, PTNUM, iip_elevation_patient, iip_elevation_partner, pdtot_patient, pdtot_partner, self_coupling_patient, self_coupling_partner, cross_coupling_patient, cross_coupling_partner)
# df_personalitydata_patient <- dplyr::select(df_personalitydata, iip_elevation_patient, pdtot_patient, self_coupling_patient, cross_coupling_patient, PTNUM)
# df_personalitydata_partner <- dplyr::select(df_personalitydata, iip_elevation_partner, pdtot_partner, self_coupling_partner, cross_coupling_partner, PTNUM)
# df_personalitydata_patient$DyadID <- paste0(df_personalitydata_patient$PTNUM, "_1")
# df_personalitydata_patient$Patient <- 1
# df_personalitydata_partner$DyadID <- paste0(df_personalitydata_partner$PTNUM, "_0")
# df_personalitydata_partner$Patient <- 0
# df_personalitydata_patient <-dplyr::rename(df_personalitydata_patient, iip_elevation = iip_elevation_patient, pdtot = pdtot_patient, self_coupling = self_coupling_patient, cross_coupling = cross_coupling_patient)
# df_personalitydata_partner <-dplyr::rename(df_personalitydata_partner, iip_elevation = iip_elevation_partner, pdtot = pdtot_partner, self_coupling = self_coupling_partner, cross_coupling = cross_coupling_partner)
# df_personaitydata_long <- dplyr::bind_rows(df_personalitydata_patient, df_personalitydata_partner)
# 
# 
# #df_personalitydata <- dplyr::select(m13dat4, PTNUM, iip_elevation_patient, iip_elevation_partner, pdcpt, pdcpr, scpr, scpt, ccpt, ccpr)
# df_personalitydata_patient <- dplyr::select(df_personalitydata, iip_elevation_patient, pdtot_patient, self_coupling_patient, cross_coupling_patient, PTNUM)
# df_personalitydata_partner <- dplyr::select(df_personalitydata, iip_elevation_partner, pdtot_partner, self_coupling_partner, cross_coupling_partner, PTNUM)
# df_personalitydata_patient$DyadID <- paste0(df_personalitydata_patient$PTNUM, "_1")
# df_personalitydata_patient$Patient <- 1
# df_personalitydata_partner$DyadID <- paste0(df_personalitydata_partner$PTNUM, "_0")
# df_personalitydata_partner$Patient <- 0
# df_personalitydata_patient <-dplyr::rename(df_personalitydata_patient, iip_elevation = iip_elevation_patient, pdtot = pdtot_patient, self_coupling = self_coupling_patient, cross_coupling = cross_coupling_patient)
# df_personalitydata_partner <-dplyr::rename(df_personalitydata_partner, iip_elevation = iip_elevation_partner, pdtot = pdtot_partner, self_coupling = self_coupling_partner, cross_coupling = cross_coupling_partner)
# df_personaitydata_long <- bind_rows(df_personalitydata_patient, df_personalitydata_partner)
# 
# #merge personality data with SPAFF
# 
# df_personaitydata_long$UsrID <- paste0(df_personaitydata_long$PTNUM, df_personaitydata_long$Patient)
#spaff_simp <- dplyr::select(df, DyadID, DyadID, iip_elevation, pdtot, Patient, UsrID, PTNUM) 
# spaff_patpar_UsrIDs <- dplyr::select(spaff_patpar, UsrID) %>% unique()
# spaff_patpar_UsrIDs <- as.vector(spaff_patpar_UsrIDs[["UsrID"]])
# spaff_simp <- dplyr::filter(spaff_simp, UsrID %in% spaff_patpar_UsrIDs) 
# spaff_simp_split <- split(spaff_simp, spaff_simp$UsrID)



df <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv") %>% dplyr::filter(!is.na(scpt), m1_R2 > .98,PTNUM != 8035, PTNUM != 8040, PTNUM!= 8073)
spaff_simp <- transmute(df, PTNUM = PTNUM, pdtot_patient = pdtot_patient,pdtot_partner= pdtot_partner, el_patient = iip_elevation_patient, el_partner = iip_elevation_partner) %>% gather(key = key, value = value, ends_with("patient"), ends_with("partner")) %>% separate(key, into = c("measure", "DyadID"), sep= "_") %>% mutate(DyadID = if_else(DyadID == "patient", 1, 0)) %>% spread(key = measure, value = value) %>% mutate(UsrID = paste0(PTNUM, DyadID))
spaff_simp_split <- split(spaff_simp, spaff_simp$UsrID)
spaff_patpar <- dplyr::filter(spaff_patpar, PTNUM %in% df$PTNUM) # now only 109
spaff_simp <- dplyr::filter(spaff_simp, UsrID %in% spaff_patpar$UsrID)
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

spaff_summarized <- ldply(spaff_simp_split, data.frame)
spaff_summarized_patient <- dplyr::filter(spaff_summarized, DyadID == 1)
spaff_summarized_partner <- dplyr::filter(spaff_summarized, DyadID == 0)
spaff_summarized_patient <- dplyr::rename(spaff_summarized_patient, pdtot_patient = pdtot, el_patient = el, num_nasty_patient = num_nasty, num_neutral_patient = num_neutral, num_nice_patient = num_nice, nastytime_patient = nastytime, neutraltime_patient = neutraltime, nicetime_patient = nicetime)
spaff_summarized_partner <- dplyr::rename(spaff_summarized_partner, pdtot_partner = pdtot, el_partner = el, num_nasty_partner = num_nasty, num_neutral_partner = num_neutral, num_nice_partner = num_nice, nastytime_partner = nastytime, neutraltime_partner = neutraltime, nicetime_partner= nicetime)
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
                                                          neutralnice_partner= neutraltime_partner/(1+nicetime_partner), 
                                                          niceproportion_patient =nicetime_patient /(nicetime_patient + neutraltime_patient + nastytime_patient),
                                                          niceproportion_partner = nicetime_partner/(nicetime_partner + neutraltime_partner + nastytime_partner))
write.csv(spaff_summarized_wide, "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/spaff_summarized_wide.csv")

