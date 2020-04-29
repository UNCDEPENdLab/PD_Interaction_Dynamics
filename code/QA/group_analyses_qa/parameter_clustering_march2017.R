#parameter clustering, modelcomparison results, march 2017
pars <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/parameters_model3_modelcomparison_allData.csv")
pars <- dplyr::rename(pars, a1 = self.coupling.patient, a2 = cross.coupling.patient, b1 = self.coupling.partner, b2 = cross.coupling.partner)
pars_outliers <- subset(pars, LL < -70000)
pars <- subset(pars,LL> -70000)
hist(pars$a1)
hist(pars$a2)
hist(pars$b1)
hist(pars$b2)
hist(pars$LL)
pars <- dplyr::filter(pars, PTNUM != 8052, PTNUM != 8063, PTNUM != 8066)
#tocluster <- scale(pars[,c("a1", "a2", "b1")])
tocluster <- pars[,c("a1", "a2", "b1", "b2")]

cor(tocluster)

pr <- prcomp(tocluster)
sum(pr$sdev)
cumvariance <- (cumsum((pr$sdev)^2) / sum(pr$sdev^2))
cumvariance
pr$rotation #varimax or promax rotation if you need to interpret this

library(psych)
psych::principal(tocluster, nfactors=4, rotate="varimax")

fit <- kmeans(tocluster, 4)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(tocluster, fit$cluster, color=TRUE, shade=TRUE, 
         
         labels=2, lines=0)

aggregate(tocluster,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)


#number of clusters
wss <- (nrow(tocluster)-1)*sum(apply(tocluster,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(tocluster, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

library(mclust)
fit <- Mclust(tocluster)
plot(fit) # plot results 
summary(fit) # display the best model

pars$mclust2 <- fit$classification
aggregate(pars,by=list(pars$mclust2),FUN=mean)
subset(pars, mclust2==2)
cor(tocluster)

library(ggplot2)
pdf(file.path(getBoxDir(), "DEPENd/Projects/PD_Interaction_Dynamics/figures", "m5_param_correlations.pdf"), width=10, height=8)
ggplot(tocluster, aes(x=a1, y=b1, color=a2)) + geom_point(size=2) + theme_bw(base_size=15)
dev.off()


require(foreign)
require(MASS)


summary(ols <- lm(iip_elevation_patient ~ self_coupling_patient + cross_coupling_patient, data = df))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
d1 <- cooks.distance(ols)
r <- stdres(ols)
df_shorted <- dplyr::select(df, PTNUM, self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner, iip_elevation_patient, iip_elevation_partner)
a <- cbind(df_shorted, d1, r)
a[d1 > 4/115, ]
###note results before re-running
summary(ols <- lm(iip_elevation_partner ~ self_coupling_partner + cross_coupling_partner, data = df))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
d1 <- cooks.distance(ols)
r <- stdres(ols)
df_shorted <- dplyr::select(df, PTNUM, self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner, iip_elevation_patient, iip_elevation_partner)
a <- cbind(df_shorted, d1, r)
a[d1 > 4/115, ]
















