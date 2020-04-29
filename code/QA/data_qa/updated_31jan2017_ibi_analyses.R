#box plots, histograms, box plots and 300 ms jump script for ALL OF THE DATA WOOOOO!!!

library(ggplot2)
library(dplyr)
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/code/QA")
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

#plots
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/figures")
pdf("updated_31Jan_all_ibis.pdf", width = 10, height = 8)
ibi_plots <- lapply(physio_split, function(x) {
  g <- ggplot(x, aes(x=time, y=ibi)) + geom_line() + facet_wrap(~position, ncol=1, scales = "free_y") + ggtitle(x$PTNUM[1])
  plot(g)
})
dev.off()

#300 ms jumps
#something is wrong with this script --> what printed out as l now prints as r from this script (i.e. same PTNUM and ibis and time but diff position)
ibicheck_updated <- lapply(physio_split, function(couple) {
  p1 <- filter(couple, position=="l") %>% mutate(diffibi = c(0, diff(ibi))) %>% filter(abs(diffibi) > 300)
  p2 <- filter(couple, position=="r") %>% mutate(diffibi = c(0, diff(ibi))) %>% filter(abs(diffibi) > 300)
  return(list(ldiff = p1, rdiff = p2))
})

#hist of diff ibi
pdf("updated_31Jan_density_diffibis.pdf",
    width = 10,
    height = 8)
lapply(physio_split, function(x){
  p3 <- x %>% mutate(diffibi = c(0, diff(ibi)))
  g <-
    qplot(diffibi, data = p3, geom = "histogram") +  facet_wrap(~ position, ncol = 1, scales  = "free_y") + ggtitle(x$PTNUM[1])
  
  
  
})

dev.off()
