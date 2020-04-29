#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(dplyr)
#change setwd and list.files so that alisonmarie526 is your username on your computer
setwd("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/code/Descriptives")

#setwd("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics")
ibi_edited <- list.files(path = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/negint_edited_ibis", pattern = ".*negint.*\\.txt", full.names = TRUE)
ids_edited <- as.numeric(sub("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/negint_edited_ibis/(8[0-9]{3}).*", "\\1", ibi_edited, perl=TRUE))
position_edited <- sub("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/negint_edited_ibis/8[0-9]{3}_([lr])_.*", "\\1", ibi_edited, perl=TRUE)

#ibis<- list.files(path="/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis", pattern=".*negint.*\\.txt", full.names=TRUE)
#ids <- as.numeric(sub("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis/(8[0-9]{3}).*", "\\1", ibis, perl=TRUE))
#position <- sub("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis/8[0-9]{3}(?:_negative|_part2_oralhx_etc)*_([lr])_.*", "\\1", ibis, perl=TRUE)
tb_edited <- as.data.frame(table(ids_edited))
# missingPartner <- grepl(as.character(paste(tb_old$ids_old[tb_old$Freq < 2], collapse="|")), ids_old, perl=TRUE)
# check_ibifiles_old <- ibis_old[!missingPartner]
# ids_old <- ids_old[!missingPartner]
# position_old <- position_old[!missingPartner]
allibi_edited <- c()

stopifnot(length(unique(sapply(list(ibi_edited, ids_edited, position_edited), length))) == 1)

for (i in 1:length(ibi_edited)) {
  f <- read.table(ibi_edited[i], col.names=c("time", "ibi"))
  f$PTNUM <- factor(ids_edited[i])
  f$position <- factor(position_edited[i])
  allibi_edited<- rbind(allibi_edited, f)
}


# setwd("~/Desktop/vanBse_ibis")
# ibi_unedited <- list.files(path = "/Users/ams939/Desktop/vanBse_ibis", pattern = ".*vanilla.*\\.txt", full.names = TRUE)
# ids_unedited <- as.numeric(sub("/Users/ams939/Desktop/vanBse_ibis/(8[0-9]{3}).*", "\\1", ibi_unedited, perl=TRUE))
# position_unedited <- sub("/Users/ams939/Desktop/vanBse_ibis/8[0-9]{3}(?:_negative|_part2_oralhx_etc)*_([lr])_.*", "\\1", ibis_old, perl=TRUE)
# tb_unedited <- as.data.frame(table(ids_unedited))
# # missingPartner_unedited <- grepl(as.character(paste(tb_unedited$ids_unedited[tb_unedited$Freq < 2], collapse="|")), ids_unedited, perl=TRUE)
# # ibifiles_unedited <- ibi_unedited[!missingPartner_unedited]
# # ids_unedited <- ids_unedited[!missingPartner_unedited]
# # position_unedited <- position_unedited[!missingPartner_unedited]
# allibi_unedited <- c()
# 
# stopifnot(length(unique(sapply(list(ibi_unedited, ids_unedited, position_unedited), length))) == 1)
# 
# 
# for (i in 1:length(ibi_unedited)) {
#   f <- read.table(ibi_unedited[i], col.names=c("time", "ibi"))
#   f$PTNUM <- factor(ids_unedited[i])
#   f$position <- factor(position_unedited[i])
#   allibi_unedited <- rbind(allibi_unedited, f)
# }
# # tb_missingpartner <- as.data.frame(table(ids_unedited))
# # tb_missingpartner <- tb_missingpartner[missingPartner_unedited]
# 
ptnum <- dplyr::distinct(ids_edited, PTNUM)

# allibi_unedited <- allibi_unedited %>% select(-diffibi)
physio_split_edited <- split(allibi_edited, allibi_edited$PTNUM)
#physio_split <- split(allibi_unedited, allibi_unedited$PTNUM)
list <- c()

ibi_firstanalyses <- lapply(align_split_4Hz, function(x) {
  (x$PTNUM[1])
  
  
})



raw_edited <- list.files(path = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/iMac_desktop_data/ecg_raw_local", pattern = "8[0-9]{3}.*negint.*ecgraw\\.txt", full.names = TRUE)
raw_ids <- as.numeric(sub(".*(8[0-9]{3}).*", "\\1", raw_edited, perl=TRUE))
raw_position <- sub(".*8[0-9]{3}_([lr])_.*", "\\1", raw_edited, perl=TRUE)

#ibis<- list.files(path="/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis", pattern=".*negint.*\\.txt", full.names=TRUE)
#ids <- as.numeric(sub("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis/(8[0-9]{3}).*", "\\1", ibis, perl=TRUE))
#position <- sub("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis/8[0-9]{3}(?:_negative|_part2_oralhx_etc)*_([lr])_.*", "\\1", ibis, perl=TRUE)
raw_tb <- as.data.frame(table(raw_edited))
# missingPartner <- grepl(as.character(paste(tb_old$ids_old[tb_old$Freq < 2], collapse="|")), ids_old, perl=TRUE)
# check_ibifiles_old <- ibis_old[!missingPartner]
# ids_old <- ids_old[!missingPartner]s
# position_old <- position_old[!missingPartner]
raw_edited_list <- c()

stopifnot(length(unique(sapply(list(raw_edited, raw_ids, raw_position), length))) == 1)

for (i in 1:length(raw_edited)) {
  f <- read.table(raw_edited[i], col.names=c("signal"))
  f$PTNUM <- factor(raw_ids[i])
  f$position <- factor(raw_position[i])
  f$time <- seq(1, length(f$signal))
  raw_edited_list<- rbind(raw_edited_list, f)
}




raw_split <- split(raw_edited_list, raw_edited_list$PTNUM)
raw_edited_list$UsrID <- paste0(raw_edited_list$PTNUM, raw_edited_list$position)
raw_split <- load(file = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/Raw_List.RData")
raw_split_byUsrID <- split(raw_edited_list, raw_edited_list$UsrID)
toplot_raw_list <- c()
for (i in 1:length(raw_split_torun)) {
  for (j in 1:20) {
    df <- raw_split_torun[[i]]
    dffiltered<- dplyr::filter(df, time > (j-1)*(length(df$signal)/20), time < j*(length(df$signal/20)))
    dffiltered$timepart <- j
    #toplot_raw_list <- rbind(toplot_raw_list)
    
    toplot_raw_list <- rbind(toplot_raw_list, dffiltered)
  }
}


raw_split_byUsrID_timepartioned <- lapply(raw_split_byUsrID, function(x) {
 for (i in 1:20) {
   
   dffiltered <- dplyr::filter(x, time > (j-1)*length(x$signal)/20, time < j*length(x$signal)/20)
   dffiltered$timepart<- j
 }
  
})

rawlist_tobesaved <- c(toplot_raw_list, raw_split_byUsrID)
save(rawlist_tobesaved, "RawNegintFiles_byUsrID")



############################
largelist <- load(file = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/align_split_atdifferentfreqs_patpar_March2017.RData")
data10Hz <- alldata$`10Hz`

data10Hzmelted <- c()
for (i in 1:length(data10Hz)) {
  dflarge <- data10Hz[i]
  df_patient <- dplyr::select(dflarge, ms, ibi_interp_detrend_patient)
  df_patient <- dplyr::rename(df_patient, ibi = ibi_interp_detrend_patient)
  df_patient$position <- "Patient"
  df_partner <-dplyr::select(dflarge, ms, ibi_interp_detrend_partner)
  df_partner <- dplyr::rename(df_partner, ibi = ibi_interp_detrend_partner)
  df_partner$position <- "Partner"
  dftoreturn <- bind_rows(df_patient, df_partner)
  data10Hzmelted <- rbind(data10Hzmelted, dfotreturn)
}


for (i in length(raw))



#don't need to run, already in box
pdf("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/figures/June2017_negint_ibis.pdf", width=10, height=8)
ibi_plots <- lapply(physio_split_edited, function(x) {
  g <- ggplot(x, aes(x=time, y=ibi)) + geom_line() + facet_wrap(~position, ncol=1, scales = "free_y") + ggtitle(x$PTNUM[1])
  g
})
ibi_plots
dev.off()



pdf("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/figures/June2017_negint_ibis.pdf", width=10, height=8)
rawplot_plots <- lapply(raw_split_, function(x) {
  g <- ggplot(x, aes(x=time, y=ibi)) + geom_line() + facet_wrap(~position, ncol=1, scales = "free_y") + ggtitle(x$PTNUM[1])
  g
})
ibi_plots
dev.off()












pdf("testing.pdf", width = 10, height = 8)
g <- ggplot(physio_split$`8001`, aes(x=time, y=ibi)) + geom_line() + facet_wrap(~position, ncol=1, scales = "free_y") + ggtitle(physio_split$`8001`$PTNUM[1])
g
dev.off()
ibicheck <- lapply(physio_split, function(couple) {
  p1 <- filter(couple, position=="l") %>% mutate(diffibi = c(0, diff(ibi))) %>% filter(abs(diffibi) > 300)
  p2 <- filter(couple, position=="r") %>% mutate(diffibi = c(0, diff(ibi))) %>% filter(abs(diffibi) > 300)
  return(list(ldiff=p1, rdiff=p2))
})


lapply(physio_split, function(x) {
  x <-   x  %>% mutate(diffibi = c(0, diff(ibi)))
  
})


pdf("figures/density_diffibis.pdf",
    width = 10,
    height = 8)
lapply(physio_split, function(x){
  p3 <- x %>% mutate(diffibi = c(0, diff(ibi)))
  g <-
    qplot(diffibi, data = p3, geom = "histogram") +  facet_wrap(~ position, ncol = 1, scales  = "free_y") + ggtitle(x$PTNUM[1])
  
  
  
})

dev.off()

#box plots for diffibi


