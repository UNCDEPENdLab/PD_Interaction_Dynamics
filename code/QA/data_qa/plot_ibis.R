#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(dplyr)
#change setwd and list.files so that ams939 is your username on your computer
setwd("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics")

#setwd("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics")
ibis_old<- list.files(path="/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis", pattern=".*negint.*\\.txt", full.names=TRUE)
ids_old <- as.numeric(sub("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis/(8[0-9]{3}).*", "\\1", ibis_old, perl=TRUE))
position_old <- sub("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis/8[0-9]{3}(?:_negative|_part2_oralhx_etc)*_([lr])_.*", "\\1", ibis_old, perl=TRUE)

#ibis<- list.files(path="/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis", pattern=".*negint.*\\.txt", full.names=TRUE)
#ids <- as.numeric(sub("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis/(8[0-9]{3}).*", "\\1", ibis, perl=TRUE))
#position <- sub("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/UPDATED_ibis/ecg_raw_all_ibis/8[0-9]{3}(?:_negative|_part2_oralhx_etc)*_([lr])_.*", "\\1", ibis, perl=TRUE)
tb_old <- as.data.frame(table(ids_old))
missingPartner <- grepl(as.character(paste(tb_old$ids_old[tb_old$Freq < 2], collapse="|")), ids_old, perl=TRUE)
ibifiles_old <- ibis_old[!missingPartner]
ids_old <- ids_old[!missingPartner]
position_old <- position_old[!missingPartner]
allibi_old <- c()

stopifnot(length(unique(sapply(list(ibis_old, ids_old, position_old), length))) == 1)

for (i in 1:length(ibis_old)) {
  f <- read.table(ibis_old[i], col.names=c("time", "ibi"))
  f$PTNUM <- factor(ids_old[i])
  f$position <- factor(position_old[i])
  allibi_old <- rbind(allibi_old, f)
}


setwd("~/Desktop/ecg_raw_local")
ibi_unedited <- list.files(path = "/Users/ams939/Desktop/ecg_raw_local", pattern = ".*negint.*\\.txt", full.names = TRUE)
ids_unedited <- as.numeric(sub("/Users/ams939/Desktop/ecg_raw_local/(8[0-9]{3}).*", "\\1", ibi_unedited, perl=TRUE))
position_unedited <- sub("/Users/ams939/Desktop/ecg_raw_local/8[0-9]{3}([lr])_.*", "\\1", ibi_unedited, perl=TRUE)
tb_unedited <- as.data.frame(table(ids_unedited))
missingPartner_unedited <- grepl(as.character(paste(tb_unedited$ids_unedited[tb_unedited$Freq < 2], collapse="|")), ids_unedited, perl=TRUE)
ibifiles_unedited <- ibi_unedited[!missingPartner_unedited]
ids_unedited <- ids_unedited[!missingPartner_unedited]
position_unedited <- position_unedited[!missingPartner_unedited]

tb_missingpartner <- as.data.frame(table(ids_unedited))
tb_missingpartner <- tb_missingpartner[missingPartner_unedited]

 ptnum <- dplyr::distinct(allibi, PTNUM)

allibi <- allibi %>% select(-diffibi)
physio_split_old <- split(allibi_old, allibi_old$PTNUM)

list <- c()

ibi_firstanalyses <- lapply(align_split_4Hz, function(x) {
  (x$PTNUM[1])

  
})


#don't need to run, already in box
pdf("figures/raw_ibis.pdf", width=10, height=8)
ibi_plots <- lapply(physio_split, function(x) {
  g <- ggplot(x, aes(x=time, y=ibi)) + geom_line() + facet_wrap(~position, ncol=1, scales = "free_y") + ggtitle(x$PTNUM[1])
  plot(g)
})
dev.off()

ibicheck <- lapply(physio_split_old, function(couple) {
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
 

