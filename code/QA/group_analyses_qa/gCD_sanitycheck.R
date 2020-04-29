
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")
simpledata <- read.csv("tofit_gcd.csv") #univaraite outliers deleted on basis of scpt, scpr, ccpt, ccpr > .3

iipel_self <- "
scpt ~  iip_elcpt
scpr ~ iip_elcpr
ccpt ~ iip_elcpt
ccpr ~ iip_elcpr
"

library(influence.SEM)
library(lavaan)
library(cowplot)
cd<- genCookDist(model = iipel_self, data = simpledata)
listofPTNUM <- c(as.numeric(simpledata$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(simpledata$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = cd

dfnew$PTNUM = simpledata$PTNUM

cthresh <- 4/nsubjs 
cthresh = .9 #reset because cutting too many with 4/nsubjs 
cat("Excluding observations with Cook's D >", cthresh, "\n")
mup <- simpledata #initially just the full model
allbadcases <- c()
while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
  badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(simpledata, !PTNUM %in% allbadcases)
  dftoplot <- simpledata %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))
  g1 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g2 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g3 <- ggplot(dftoplot, aes(x=iip_elevation_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  g4 <- ggplot(dftoplot, aes(x=iip_elevation_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
  obj <- plot_grid(g1, g2, g3, g4, nrow=2)
  plot(obj)
  cd <- genCookDist(iipel_self, mup) 
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
toexclude <- anti_join(simpledata, dfnew, by = "PTNUM") %>% dplyr::select(PTNUM)
