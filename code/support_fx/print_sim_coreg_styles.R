setwd("~/Desktop/")
library(tidyverse)
library(R.matlab)
xsim_dependent <- readMat("feb2019_xsim_dependent.mat")
xsim_dependent_df <- as.data.frame(t(xsim_dependent$xsim))

xsim_dependent_df$xvalue = 0:(length(xsim_dependent_df$V1)-1)
colnames(xsim_dependent_df) <- c("p1", "p2", "xvalue")
redline = data.frame(xvalue = 0:159, key = "p3", yvalue = 0) %>% mutate(key = as.character(key))
xsim_dependent_long <- gather(xsim_dependent_df, key = "key", value = "yvalue", p1, p2)
xsim_dependent_long_wred <- bind_rows(xsim_dependent_long, redline)
fig1b3 <- ggplot(xsim_dependent_long_wred, aes(x = xvalue, y = yvalue, color = key)) + geom_line(size = 3) + ylim(-2, 2) +  scale_color_brewer(palette = "Set1") + theme(legend.position = "none", axis.ticks=element_blank(),
                                                                                                                axis.title.x=element_blank(),
                                                                                                                axis.title.y=element_blank(), 
                                                                                                                axis.text.x=element_blank(),
                                                                                                                axis.text.y=element_blank()) + xlab("") + ylab("")

xsim_independent <- readMat("feb2019_xsim_independent.mat")
xsim_independent_df <- as.data.frame(t(xsim_independent$xsim))

xsim_independent_df$xvalue = 0:(length(xsim_independent_df$V1)-1)
colnames(xsim_independent_df) <- c("p1", "p2", "xvalue")
xsim_independent_long <- gather(xsim_independent_df, key = "key", value = "yvalue", p1, p2)
xsim_independent_long_wred <- bind_rows(xsim_independent_long, redline)

fig1b2 <- ggplot(xsim_independent_long_wred, aes(x = xvalue, y = yvalue, color = key)) + geom_line(size = 3) + ylim(-2, 2) +  scale_color_brewer(palette = "Set1") +theme(legend.position = "none", axis.ticks=element_blank(),
                                                                                                                  axis.title.x=element_blank(),
                                                                                                                  axis.title.y=element_blank(), 
                                                                                                                  axis.text.x=element_blank(),
                                                                                                                  axis.text.y=element_blank())  + xlab("") + ylab("")

xsim_contrarian <- readMat("feb2019_xsim_contrarian.mat")
xsim_contrarian_df <- as.data.frame(t(xsim_contrarian$xsim))
xsim_contrarian_df$xvalue = 0:(length(xsim_contrarian_df$V1)-1)
colnames(xsim_contrarian_df) <- c("p1", "p2", "xvalue")
xsim_contrarian_long <- gather(xsim_contrarian_df, key = "key", value = "yvalue", p1, p2)
xsim_contrarian_long_wred <- bind_rows(xsim_contrarian_long, redline)

fig1b1 <- ggplot(xsim_contrarian_long_wred, aes(x = xvalue, y = yvalue, color = key)) + geom_line(size = 3) + ylim(-2, 2) + scale_color_brewer(palette = "Set1") +theme(legend.position = "none", axis.ticks=element_blank(),
                                                                                                                 axis.title.x=element_blank(),
                                                                                                                 axis.title.y=element_blank(), 
                                                                                                                 axis.text.x=element_blank(),
                                                                                                                 axis.text.y=element_blank())  + xlab("") + ylab("")


library(cowplot)
pdf("feb2019_XSim_ggplot.pdf", width = 30, height = 5)
plot_grid(fig1b1, fig1b2, fig1b3, ncol = 3)
dev.off()
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/") 

# Creating Fig1b in ggplot. Grabbing xsim from VBA

