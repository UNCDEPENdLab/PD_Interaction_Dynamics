#read in 10Hz data
#take subset of time
#plot 100 seconds
#load("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/align_split_atdifferentfreqs_patpar_Feb2017.RData")
load('/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/align_split_atdifferentfreqs_patpar_Feb2017.RData')
toplot <- alldata$`10Hz`
str(toplot$`8060`)

toplot_subset <- filter(toplot$`8060`)

p1 <- c("#56B4E9", "#009E73")
toplot_df <- as.data.frame(toplot_subset)
toplot_df$ibi_interpdetrend <- c(toplot_subset$ibi_interp_detrend_patient, toplot_subset$ibi_interp_detrend_patient)
g <- ggplot(toplot_subset, aes(x=ms, ibi)) + geom_line(aes(y =ibi_interp_detrend_patient), colour =  "#009E73") + geom_line(aes(y =ibi_interp_detrend_partner), colour = "#56B4E9")+  ggtitle(toplot_subset$PTNUM[1]) 
plot(g)











######

ibifiles <- list.files(path="/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/iMac_desktop_data/ecg_raw_negint", pattern=".*negint.*\\.txt", full.names=TRUE)

ids <- as.numeric(sub("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/iMac_desktop_data/ecg_raw_negint/(8[0-9]{3}).*", "\\1", ibifiles, perl=TRUE))
position <- sub("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/iMac_desktop_data/ecg_raw_negint/8[0-9]{3}_([lr])_.*", "\\1", ibifiles, perl=TRUE)

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

physio_split_8017 <- physio_split$`8017`

physio_split_8017_short <- dplyr::filter(physio_split_8017, time > 200000, time <  400000)

q <- ggplot(physio_split_8017_short, aes(x=time/1000, y=ibi, color = position)) +
  ylab("IBI") + xlab("Time (seconds)") + geom_line(size = 2) + ggtitle("IBI Time Series for PTNUM 8017") + theme_bw(base_size = 20)

png("ibi8017.png", width = 11, height = 3, units = "in", res = 300)
q <- ggplot(physio_split_8017_short, aes(x=time/1000, y=ibi, color = position)) +
  ylab("IBI (ms)") + xlab("Time (seconds)") + geom_line(size = 2) + theme_bw(base_size = 20) +  theme(legend.position = "none")
q
dev.off()


pdf("ibi8017.pdf", width = 20, height = 4)
q <- ggplot(physio_split_8017_short, aes(x=time/1000, y=ibi, color = position)) +
  ylab("IBI (ms)") + xlab("Time (seconds)") + geom_line(size = 3) + theme_bw(base_size = 20) +  theme(legend.position = "none")
q
dev.off()

