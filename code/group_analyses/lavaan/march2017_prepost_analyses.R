#prepost scoring
#basedir <- "/Users/agc5141/Tresors/DEPENd/Projects/PersonalityRest"
#basedir <- "/Users/michael/Tresors/DEPENd/Projects/PersonalityRest"
#basedir <- "/Users/michael/Box_Sync/DEPENd/Projects/SPECC/SelfReports"
basedir <- "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/"

setwd(file.path(basedir))

setwd("~/Desktop/Archive_April2017")
vanBse_params <- readMat("vanBse_modelcomparison_logevidence.mat")
vanBse_paramsm1 <- as.data.frame((vanBse_params$rawparameters[,1,]))
vanBse_paramsm2 <- as.data.frame((vanBse_params$rawparameters[,2,]))
vanBse_paramsm3 <- as.data.frame((vanBse_params$rawparameters[,3,]))
vanBse_paramsm4 <- as.data.frame((vanBse_params$rawparameters[,4,]))
vanBse_paramsm4$PTNUM <- as.numeric(vanBse_params$ids)
# ll <- data.frame()
# ll$ll <- as.data.frame((vanBse_params$logEvidence[4,]))
# ll$PTNUM <- as.numeric(vanBse_params$ids)
# vanBse_paramsm4$ll <- as.data.frame((vanBse_params$logEvidence[4,]))
vanBse_paramsm1 <- dplyr::filter(vanBse_paramsm1, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
vanBse_paramsm2 <- dplyr::filter(vanBse_paramsm2, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
vanBse_paramsm3 <- dplyr::filter(vanBse_paramsm3, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)
vanBse_paramsm4 <- dplyr::filter(vanBse_paramsm4, abs(V1) < .3, abs(V2) < .3, abs(V3) < .3, abs(V4) < .3)

vanBse_paramsm4 <- rename(vanBse_paramsm4, self_coupling_patient = V1, cross_coupling_patient = V2, self_coupling_parnter = V3, cross_coupling_partner = V4 )
vanBse_paramsm4 <- rename(vanBse_paramsm4, v_self_coupling_patient = self_coupling_patient, v_cross_coupling_patient = cross_coupling_patient, v_self_coupling_parnter= self_coupling_parnter, v_cross_coupling_partner = cross_coupling_partner  )



#code for SAAM scoring

saampre <- read.csv("saampre.csv", header=TRUE)

str(saampre)

#On BPQ, 1 = True and 2 = False
#Switch to 1 = True and 0 = False
saamitems = paste0("SAAM", 1:21)
# saampre[,saamitems] <- lapply(saampre[,saamitems], function(vec) { 
#   sapply(vec, function(x) {
#     if (is.na(x)) { return (NA) 
#     } else if (x==1) { return(1) #True
#     } else if (x==2) { return(0) #False
#     } else { stop("Cannot match BPQ response: ", x) }
#   })
# })

#handle missingness (mean imputation at the moment)
#check for missing items
for (i in 1:nrow(saampre)) {
  miss <- which(is.na(saampre[i, saamitems]))
  if (length(miss) > 0) {
    cat("For subject id: ", paste(sapply(saampre[i, c("PTNUM")], as.character), collapse="/"), ", these items are missing: ", paste(miss, collapse=", "), "\n", sep="")
  }
}

saampre$ID <- paste0(saampre$PTNUM, saampre$DyadID)


#reverse score items
# reverseItems <- paste("BPQ", c(4, 8, 10, 28, 32, 43, 45, 48, 52, 53, 54, 60, 67), sep=".")
# bpq[,reverseItems] <- lapply(bpq[,reverseItems], function(x) { 1 - x })


#bpq_em <- amelia(bpq[,c("SPECC_ID", bpqitems)], m=1, boot.type="none", cs="SPECC_ID")
#obsmean <- mean(unlist(bpq[which(bpq$LUNA_ID==11178), c("BPQ.4", "BPQ.13", "BPQ.21", "BPQ.29", "BPQ.36", "BPQ.45", "BPQ.51", "BPQ.60")]), na.rm=TRUE)
#bpq[which(bpq$LUNA_ID==11178), "BPQ.13"] <- obsmean


meanreplace <- function(df, ID, varprefix="SAAM", subscaleitems) {
  subjrow <- df$ID == ID
  dfitems <- subset(df, select=paste0(varprefix, subscaleitems))
  dfitems[dfitems == 99] <- NA
  subjitems <- dfitems[subjrow,]
  whichmiss <- names(subjitems)[which(is.na(subjitems))]
  obsmean <- mean(unlist(subjitems), na.rm=TRUE)
  df[subjrow, whichmiss] <- obsmean
  df
}

anybad <- apply(saampre, 1, function(row) {
  any(row == 99)
})



saampre <- meanreplace(saampre, "80720", subscaleitems=c(1, 5, 8, 12, 14, 17, 19)) #miss 14

# bpq <- meanreplace(bpq, "092_RM", subscaleitems=c(1, 10, 26, 34, 42, 57, 64, 68, 71)) #miss 1
# 
# bpq <- meanreplace(bpq, "074_DS", subscaleitems=c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79)) #miss 39
# bpq <- meanreplace(bpq, "074_DS", subscaleitems=c(8, 17, 24, 32, 40, 47, 55, 63, 77, 80)) #miss 40
# bpq <- meanreplace(bpq, "074_DS", subscaleitems=c(9, 18, 25, 33, 41, 48, 56)) #miss 41
# 
# bpq <- meanreplace(bpq, "015_CW", subscaleitems=c(1, 10, 26, 34, 42, 57, 64, 68, 71)) #miss 34
# bpq <- meanreplace(bpq, "015_CW", subscaleitems=c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79)) #miss 39
# bpq <- meanreplace(bpq, "015_CW", subscaleitems=c(4, 13, 21, 29, 36, 45, 51, 60)) #miss 13
# bpq <- meanreplace(bpq, "015_CW", subscaleitems=c(3, 12, 20, 28, 44, 50, 59, 66, 73, 78)) #miss 3
# 

#define variables and score items
saampre$anx <- with(saampre, SAAM1 + SAAM5 + SAAM8 + SAAM12 + SAAM14 + SAAM17 + SAAM19)
saampre$avo <- with(saampre, SAAM2 + SAAM3 + SAAM9 + SAAM10 + SAAM15 + SAAM16 + SAAM21)
saampre$sec <- with(saampre, SAAM4 + SAAM6 + SAAM7 + SAAM11 + SAAM13 + SAAM18 + SAAM20)
saampre$totalscore <- with(saampre, anx + avo + sec)

write.csv(saampre, file="SAAMPREscored.csv", row.names=FALSE)
saampre <- read.csv(file = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/SAAMPREscored.csv")


####SAAMpost


basedir <- "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/"

setwd(file.path(basedir))

#code for SAAM scoring
saampost <- read.csv("SAAMPOST.csv", header=TRUE)

str(saampost)

#On BPQ, 1 = True and 2 = False
#Switch to 1 = True and 0 = False
saamitems = paste0("SAAM", 1:21)
# saampost[,saamitems] <- lapply(saampost[,saamitems], function(vec) { 
#   sapply(vec, function(x) {
#     if (is.na(x)) { return (NA) 
#     } else if (x==1) { return(1) #True
#     } else if (x==2) { return(0) #False
#     } else { stop("Cannot match BPQ response: ", x) }
#   })
# })

#handle missingness (mean imputation at the moment)
#check for missing items
for (i in 1:nrow(saampost)) {
  miss <- which(is.na(saampost[i, saamitems]))
  if (length(miss) > 0) {
    cat("For subject id: ", paste(sapply(saampost[i, c("PTNUM")], as.character), collapse="/"), ", these items are missing: ", paste(miss, collapse=", "), "\n", sep="")
  }
}

saampost$ID <- paste0(saampost$PTNUM, saampost$DyadID)


#reverse score items
# reverseItems <- paste("BPQ", c(4, 8, 10, 28, 32, 43, 45, 48, 52, 53, 54, 60, 67), sep=".")
# bpq[,reverseItems] <- lapply(bpq[,reverseItems], function(x) { 1 - x })


#bpq_em <- amelia(bpq[,c("SPECC_ID", bpqitems)], m=1, boot.type="none", cs="SPECC_ID")
#obsmean <- mean(unlist(bpq[which(bpq$LUNA_ID==11178), c("BPQ.4", "BPQ.13", "BPQ.21", "BPQ.29", "BPQ.36", "BPQ.45", "BPQ.51", "BPQ.60")]), na.rm=TRUE)
#bpq[which(bpq$LUNA_ID==11178), "BPQ.13"] <- obsmean


meanreplace <- function(df, ID, varprefix="SAAM", subscaleitems) {
  subjrow <- df$ID == ID
  dfitems <- subset(df, select=paste0(varprefix, subscaleitems))
  dfitems[dfitems == 99] <- NA
  subjitems <- dfitems[subjrow,]
  whichmiss <- names(subjitems)[which(is.na(subjitems))]
  obsmean <- mean(unlist(subjitems), na.rm=TRUE)
  df[subjrow, whichmiss] <- obsmean
  df
}

anybad <- apply(saampost, 1, function(row) {
  any(row == 99)
})
posted <- saampost
posted$anybad <- anybad


#saampost <- meanreplace(saampost, "80720", subscaleitems=c(1, 5, 8, 12, 14, 17, 19)) #miss 14

# bpq <- meanreplace(bpq, "092_RM", subscaleitems=c(1, 10, 26, 34, 42, 57, 64, 68, 71)) #miss 1
# 
# bpq <- meanreplace(bpq, "074_DS", subscaleitems=c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79)) #miss 39
# bpq <- meanreplace(bpq, "074_DS", subscaleitems=c(8, 17, 24, 32, 40, 47, 55, 63, 77, 80)) #miss 40
# bpq <- meanreplace(bpq, "074_DS", subscaleitems=c(9, 18, 25, 33, 41, 48, 56)) #miss 41
# 
# bpq <- meanreplace(bpq, "015_CW", subscaleitems=c(1, 10, 26, 34, 42, 57, 64, 68, 71)) #miss 34
# bpq <- meanreplace(bpq, "015_CW", subscaleitems=c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79)) #miss 39
# bpq <- meanreplace(bpq, "015_CW", subscaleitems=c(4, 13, 21, 29, 36, 45, 51, 60)) #miss 13
# bpq <- meanreplace(bpq, "015_CW", subscaleitems=c(3, 12, 20, 28, 44, 50, 59, 66, 73, 78)) #miss 3
# 

#define variables and score items
saampost$anx <- with(saampost, SAAM1 + SAAM5 + SAAM8 + SAAM12 + SAAM14 + SAAM17 + SAAM19)
saampost$avo <- with(saampost, SAAM2 + SAAM3 + SAAM9 + SAAM10 + SAAM15 + SAAM16 + SAAM21)
saampost$sec <- with(saampost, SAAM4 + SAAM6 + SAAM7 + SAAM11 + SAAM13 + SAAM18 + SAAM20)
saampost$totalscore <- with(saampost, anx + avo + sec)

write.csv(saampost, file="SAAMPOSTscored.csv", row.names=FALSE)









#################################################### START HERE

saampost <- read.csv(file = "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/SAAMPOSTscored.csv")
saampre <- read.csv(file = "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/SAAMPREscored.csv")
saamprescales <- dplyr::select(saampre, PTNUM, DyadID, ID, anx, avo, sec)
saamprescales <- dplyr::rename(saampre, anx_pre = anx, avo_pre = avo, sec_pre = sec)
saampostscales <- dplyr::select(saampost, PTNUM, DyadID, ID, anx,avo, sec)
saampostscales <- dplyr::rename(saampost, anx_post = anx, avo_post = avo, sec_post = sec)

saamprepostscales <- inner_join(saamprescales, saampostscales, by = "ID")
saamprepostscales_patient <- dplyr::filter(saamprepostscales, DyadID.x == 1)
saamprepostscales_partner <- dplyr::filter(saamprepostscales, DyadID.x == 0)
dt = read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/parameters_model3_modelcomparison_goodData.csv", header = TRUE)
dt_patient <- dplyr::select(dt, self.coupling.patient, cross.coupling.patient, PTNUM)
dt_partner <- dplyr::select(dt, self.coupling.partner, cross.coupling.partner, PTNUM)
saamprepostscales_partner <- dplyr::rename(saamprepostscales_partner, PTNUM = PTNUM.x)
saamprepostscales_patient <- dplyr::rename(saamprepostscales_patient, PTNUM = PTNUM.x)
saamprespostcales_dt_patient <- inner_join(saamprepostscales_patient, dt_patient, by = "PTNUM")
saamprepostscales_dt_partner <- inner_join(saamprepostscales_partner, dt_partner, by = "PTNUM" )
saamprepostscales_dt <- inner_join(saamprespostcales_dt_patient, saamprepostscales_dt_partner, by = "PTNUM")
#panasprepostscales_sfbmc <- inner_join(panasprepostscales_sfbmc_patient, panasprepostscales_partner, by = "PTNUM")
saamprepostscales_dt <- dplyr::rename(saamprepostscales_dt, anx_pre_patient = anx_pre.x, anx_post_patient = anx_post.x, avo_pre_patient = avo_pre.x, avo_post_patient = avo_post.x, sec_pre_patient = sec_pre.x, sec_post_patient = sec_post.x, anx_pre_partner =  anx_pre.y, anx_post_partner = anx_post.y, avo_pre_partner = avo_pre.y, avo_post_partner = avo_post.y, sec_pre_partner = sec_pre.y, sec_post_partner = sec_post.y)
saamprepostscales_dt <- dplyr::rename(saamprepostscales_dt, self_coupling_patient = self.coupling.patient, cross_coupling_patient = cross.coupling.patient, self_coupling_partner = self.coupling.partner, cross_coupling_partner = cross.coupling.partner)
ll <- dplyr::select(dt, LL, PTNUM)
saamprepostscales_dt <- inner_join(saamprepostscales_dt, ll, by = "PTNUM")
saamprepostscales_dt <- dplyr::filter(saamprepostscales_dt, LL > -69999) %>% dplyr::filter(abs(self_coupling_patient) < 1 & abs(cross_coupling_patient)  < 1 & abs(self_coupling_partner) < 1 & abs(cross_coupling_partner) < 1 )


ols <- lm(saamprepostscales_dt$anx_post_partner ~ saamprepostscales_dt$self_coupling_partner, data = saamprepostscales_dt)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
par(opar)
d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(saamprepostscales_dt, d1, r)
#a <- a %>% dplyr::filter(PTNUM == 8007 | PTNUM == 8010 | PTNUM == 8017 | PTNUM == 8018 | PTNUM == 8039 | PTNUM == 8050 | PTNUM == 8057 | PTNUM == 8062 | PTNUM == 8063 | PTNUM == 8069 | PTNUM == 8074 | PTNUM == 8086 | PTNUM == 8091 |PTNUM == 8092 | PTNUM == 8094 |PTNUM == 8095 |PTNUM == 8096| PTNUM == 8105 |PTNUM == 8107 | PTNUM == 8109 |PTNUM == 8110 |PTNUM == 8128)
a[d1 > 4/114, ]
a[d1 > 4/114, ] %>% dplyr::select(self_coupling_partner, self_coupling_patient, cross_coupling_partner, cross_coupling_patient, PTNUM, d1) 
#cut people with undue leverage, 8063, 8066, 8069, 8070 (same people before new param estimates, so perhaps something in their saam)
saamprepostscales_dt <- dplyr::filter(saamprepostscales_dt, PTNUM != 8063, PTNUM != 8066, PTNUM != 8069, PTNUM != 8070)
saamprepostscales_dt <- mutate(saamprepostscales_dt, scpt = 1000*self_coupling_patient, scpr = 1000*self_coupling_partner, ccpt = 1000*cross_coupling_patient, ccpr = 1000*cross_coupling_partner)
##final N: 110
saamprepostscales_dt_vanBse <- merge(saamprepostscales_dt, vanBse_paramsm4, by = c("PTNUM"))
saamprepostscales_dt_vanBse <- mutate(saamprepostscales_dt_vanBse, v_scpt = 1000*v_self_coupling_patient, v_scpr = 1000*v_self_coupling_parnter, v_ccpt = 1000*v_cross_coupling_patient, v_ccpr = 1000*v_cross_coupling_partner)
##final N: 106


#self coupling mediating prepost attachment anxiety
saam_sc_avo <- "
avo_post_partner ~ avo_pre_partner + scpr
scpr ~ avo_pre_partner
avo_post_patient ~ avo_pre_patient + scpt
scpt ~ avo_pre_patient
scpt~~scpr
"
saam_sc_avo_m <- sem(saam_sc_avo, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

#cross coupling mediating prepost attachment avoidance

saam_cc_avo <- "
avo_post_partner ~ avo_pre_partner + ccpr
ccpr ~ avo_pre_partner
avo_post_patient ~ avo_pre_patient + ccpt
ccpt ~ avo_pre_patient
ccpr~~ccpt
"
saam_cc_avo_m <- sem(saam_cc_avo, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)


#cc prpt mediating avoidance
saam_cc_prpt_avo <- "
avo_post_partner ~ avo_pre_partner + ccpr + ccpt
ccpr ~ avo_pre_partner
ccpt ~ avo_pre_partner
avo_post_patient ~ avo_pre_patient + ccpt + ccpr
ccpt ~ avo_pre_patient
ccpr~avo_pre_patient
ccpr~~ccpt
"
saam_cc_prpt_avo_m <- sem(saam_cc_prpt_avo, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE)

#sc prpt mediating avoidance
saam_sc_prpt_avo <- "
avo_post_partner ~ avo_pre_partner + scpr + scpt
scpr ~ avo_pre_partner
scpt ~ avo_pre_partner
avo_post_patient ~ avo_pre_patient + scpt + scpr
scpt ~ avo_pre_patient
scpr~avo_pre_patient
scpr~~scpt
"
saam_sc_prpt_avo_m <- sem(saam_sc_prpt_avo, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE)
saam_sc_prpt_avo_vanBse <- "
avo_post_partner ~ avo_pre_partner + scpr + scpt
scpr ~ avo_pre_partner
scpt ~ avo_pre_partner
scpt ~ v_scpt
scpr ~ v_scpr
avo_post_patient ~ avo_pre_patient + scpt + scpr
scpt ~ avo_pre_patient
scpr~avo_pre_patient
scpr~~scpt
"
saam_sc_prpt_avo_vanBse_m <- sem(saam_sc_prpt_avo_vanBse, saamprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE)



saam_sc_avo <- "
avo_post_partner ~ avo_pre_partner + scpr
scpr ~ avo_pre_partner
avo_post_patient ~ avo_pre_patient + scpt
scpt ~ avo_pre_patient
"
saam_sc_avo_m <- sem(saam_sc_avo, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)


saam_sccc_avo <- "
avo_post_partner ~ avo_pre_partner + scpr + ccpr
scpr ~ avo_pre_partner
ccpr ~ avo_pre_partner
avo_post_patient ~ avo_pre_patient + scpt + ccpt
scpt ~ avo_pre_patient
ccpt ~ avo_pre_patient
scpr~~ccpr
scpt~~ccpt
scpt~~scpr
scpt~~ccpr
ccpt~~ccpr
ccpt~~scpr
"
saam_sccc_avo_m <- sem(saam_sccc_avo, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE)

saam_sccc_prpt_avo <- "
avo_post_partner ~ avo_pre_partner + scpr + ccpr + scpt + ccpt
scpr ~ avo_pre_partner
scpt ~ avo_pre_partner
ccpr ~ avo_pre_partner
ccpt~avo_pre_partner
avo_post_patient ~ avo_pre_patient + scpt + ccpt + scpr + ccpr
scpt ~ avo_pre_patient
ccpt ~ avo_pre_patient
scpr ~ avo_pre_patient
ccpr ~ avo_pre_patient
scpr~~ccpr
scpt~~ccpt
"
saam_sccc_prpt_avo_m <- sem(saam_sccc_prpt_avo, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                            mimic="Mplus", meanstructure = TRUE)



saam_sccc_prpt_avo_vanBse <- "
avo_post_partner ~ avo_pre_partner + scpr + ccpr + scpt + ccpt
scpr ~ avo_pre_partner
scpt ~ avo_pre_partner
ccpr ~ avo_pre_partner
ccpt~avo_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
avo_post_patient ~ avo_pre_patient + scpt + ccpt + scpr + ccpr
scpt ~ avo_pre_patient
ccpt ~ avo_pre_patient
scpr ~ avo_pre_patient
ccpr ~ avo_pre_patient
scpr~~ccpr
scpt~~ccpt
"
saam_sccc_prpt_avo_vanBse_m <- sem(saam_sccc_prpt_avo_vanBse, saamprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                            mimic="Mplus", meanstructure = TRUE)



#sc mediating attachment anxiety
saam_sc_anx <- "
anx_post_partner ~ anx_pre_partner + scpr
scpr ~ anx_pre_partner
anx_post_patient ~ anx_pre_patient + scpt
scpt ~ anx_pre_patient
scpt~~scpr
"
saam_sc_anx_m <- sem(saam_sc_anx, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

#cross coupling mediating prepost attachment anxiety

saam_cc_anx <- "
anx_post_partner ~ anx_pre_partner + ccpr
ccpr ~ anx_pre_partner
anx_post_patient ~ anx_pre_patient + ccpt
ccpt ~ anx_pre_patient
ccpr~~ccpt
"
saam_cc_anx_m <- sem(saam_cc_anx, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)


saam_cc_prpt_anx <- "
anx_post_partner ~ anx_pre_partner + ccpr + ccpt
ccpr ~ anx_pre_partner
ccpt~anx_pre_partner
anx_post_patient ~ anx_pre_patient + ccpt + ccpr
ccpt ~ anx_pre_patient
ccpr~anx_pre_patient
ccpr~~ccpt
"
saam_cc_prpt_anx_m <- sem(saam_cc_prpt_anx, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE)

saam_sc_prpt_anx <- "
anx_post_partner ~ anx_pre_partner + scpr + scpt
scpr ~ anx_pre_partner
scpt  ~anx_pre_partner
anx_post_patient ~ anx_pre_patient + scpt + scpr
scpt ~ anx_pre_patient
scpr~anx_pre_patient
scpr~~scpt
"
saam_sc_prpt_anx_m <- sem(saam_sc_prpt_anx, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE)


saam_sc_prpt_anx_vanBse <- "
anx_post_partner ~ anx_pre_partner + scpr + scpt
scpr ~ anx_pre_partner
scpt  ~anx_pre_partner
scpr ~ v_scpr
scpt ~ v_scpt
anx_post_patient ~ anx_pre_patient + scpt + scpr
scpt ~ anx_pre_patient
scpr~anx_pre_patient
scpr~~scpt
"
saam_sc_prpt_anx_vanBse_m <- sem(saam_sc_prpt_anx_vanBse, saamprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE)





saam_sccc_anx <- "
anx_post_partner ~ anx_pre_partner + scpr + ccpr
scpr ~ anx_pre_partner
ccpr ~ anx_pre_partner
anx_post_patient ~ anx_pre_patient + scpt + ccpt
scpt ~ anx_pre_patient
ccpt ~ anx_pre_patient
scpr~~ccpr
scpt~~ccpt
"
saam_sccc_anx_m <- sem(saam_sccc_anx, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE)

saam_sccc_prpt_anx <- "
anx_post_partner ~ anx_pre_partner + scpr + ccpr + scpt + ccpt
scpr ~ anx_pre_partner
scpt ~ anx_pre_partner
ccpr ~ anx_pre_partner
ccpt~anx_pre_partner
anx_post_patient ~ anx_pre_patient + scpt + ccpt + scpr + ccpr
scpt ~ anx_pre_patient
ccpt ~ anx_pre_patient
scpr ~ anx_pre_patient
ccpr ~ anx_pre_patient
scpr~~ccpr
scpt~~ccpt
"
saam_sccc_prpt_anx_m <- sem(saam_sccc_prpt_anx, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                            mimic="Mplus", meanstructure = TRUE)



#sc mediating security
saam_sc_sec <- "
sec_post_partner ~ sec_pre_partner + scpr
scpr ~ sec_pre_partner
sec_post_patient ~ sec_pre_patient + scpt
scpt ~ sec_pre_patient
"
saam_sc_sec_m <- sem(saam_sc_sec, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

#cross coupling mediating prepost attachment anxiety

saam_cc_sec <- "
sec_post_partner ~ sec_pre_partner + ccpr
ccpr ~ sec_pre_partner
anx_post_patient ~ sec_pre_patient + ccpt
ccpt ~ sec_pre_patient
"
saam_cc_sec_m <- sem(saam_cc_sec, saamprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

anova(saam_cc_sec_m, saam_sc_sec_m, saam_sc_anx_m, saam_cc_anx_m, saam_sccc_anx_m, saam_sc_avo_m, saam_cc_avo_m, saam_sccc_avo_m, saam_sc_prpt_anx_m, saam_cc_prpt_anx_m, saam_sccc_prpt_anx_m, saam_sccc_prpt_avo_m, saam_sc_prpt_avo_m, saam_cc_prpt_avo_m)


saam_sccc_prpt_anx_vanBse <- "
anx_post_partner ~ anx_pre_partner + scpr + ccpr + scpt + ccpt
scpr ~ anx_pre_partner
scpt ~ anx_pre_partner
ccpr ~ anx_pre_partner
ccpt~anx_pre_partner
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr
anx_post_patient ~ anx_pre_patient + scpt + ccpt + scpr + ccpr
scpt ~ anx_pre_patient
ccpt ~ anx_pre_patient
scpr ~ anx_pre_patient
ccpr ~ anx_pre_patient
scpr~~ccpr
scpt~~ccpt
"
saam_sccc_prpt_anx_vanBse_m <- sem(saam_sccc_prpt_anx_vanBse, saamprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                            mimic="Mplus", meanstructure = TRUE)
















#IMICPRE



imicpre <- read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/IMICPRE.csv")
imicpost <- read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/IMICPOST.csv")
imicpre$UsrID <- paste0(imicpre$PTNUM, imicpre$DyadID)
imicpost$UsrID <- paste0(imicpost$PTNUM, imicpost$DyadID)
#now for some imputing

# COMPUTE IMI_D=MEAN.4(IMI1,IMI10,IMI18,IMI23) .
# EXECUTE .
# COMPUTE IMI_HD=MEAN.4(IMI9,IMI13,IMI20,IMI29) .
# EXECUTE .
# COMPUTE IMI_H=MEAN.4(IMI2,IMI12,IMI16,IMI27) .
# EXECUTE .
# COMPUTE IMI_HS=MEAN.4(IMI17,IMI19,IMI24,IMI30) .
# EXECUTE .
# COMPUTE IMI_S=MEAN.4(IMI4,IMI7,IMI25,IMI28) .
# EXECUTE .
# COMPUTE IMI_FS=MEAN.4(IMI3,IMI15,IMI21,IMI31) .
# EXECUTE .
# COMPUTE IMI_F=MEAN.4(IMI5,IMI6,IMI8,IMI11) .
# EXECUTE .
# COMPUTE IMI_FD=MEAN.4(IMI14,IMI22,IMI26,IMI32) 





#look at notebook
#80150 needs to have 24 imputed
imicpre <- meanreplace(imicpre, "80150", varprefix = "IMI", subscaleitems = c(17, 19, 24, 30))
#80291 needs to have 25 imputed
imicpre <- meanreplace(imicpre, "80291", varprefix = "IMI",subscaleitems = c(4, 7, 25, 28))
#80000 needs to have 27 replaced
imicpre <- meanreplace(imicpre, "80000", varprefix = "IMI",subscaleitems = c(2, 12, 16, 27))
#81500 needs to have 28 replaced
imicpre <- meanreplace(imicpre, "81500", varprefix = "IMI",subscaleitems = c(4, 7, 25, 28))
#80300 needs to have 30 and 31 replaced
imicpre <- meanreplace(imicpre, "80300", varprefix = "IMI",subscaleitems = c(17, 19, 24, 30))
imicpre<-meanreplace(imicpre, "80300", varprefix = "IMI",subscaleitems = c(3, 15, 21, 31))
#80440 needs to have 30 replaced
imicpre <- meanreplace(imicpre, "80440", varprefix = "IMI",subscaleitems = c(17, 19, 24, 30))
#8051 needs to have 30 and 31 replaced
imicpre <- meanreplace(imicpre, "80531", varprefix = "IMI",subscaleitems = c(17, 19, 24, 30))
imicpre<-meanreplace(imicpre, "80531", varprefix = "IMI",subscaleitems = c(3, 15, 21, 31))
#80521 imi4 and imi5
imicpre <- meanreplace(imicpre, "80521", varprefix = "IMI",subscaleitems = c(4, 7, 25, 28))
imicpre <- meanreplace(imicpre, "80521", varprefix = "IMI",subscaleitems = c(5, 6, 8, 11))
#81411 6
imicpre <- meanreplace(imicpre, "81411", varprefix = "IMI",subscaleitems = c(5, 6, 8, 11))
#80101 12
imicpre <- meanreplace(imicpre, "80101", varprefix = "IMI",subscaleitems = c(2, 12, 16, 27))
#80921 15
imicpre <- meanreplace(imicpre, "80921", varprefix = "IMI",subscaleitems = c(3, 15, 21, 31))
#80491 18
imicpre <- meanreplace(imicpre, "80491", varprefix = "IMI",subscaleitems = c(1, 10, 18, 23))
#81260 18
imicpre <- meanreplace(imicpre, "81260", varprefix = "IMI",subscaleitems = c(1, 10, 18, 23))
#80161 20
imicpre <- meanreplace(imicpre, "80161", varprefix = "IMI",subscaleitems = c(9, 13, 20, 29))
#81500 21
imicpre<-meanreplace(imicpre, "81500", varprefix = "IMI",subscaleitems = c(3, 15, 21, 31))
#81391 22
imicpre <- meanreplace(imicpre, "81391", varprefix = "IMI",subscaleitems = c(14, 22, 26, 32))

imicpre$D <- with(imicpre, (IMI1 + IMI10 + IMI18 + IMI23)/4)
imicpre$HD <-with(imicpre, (IMI9 + IMI13 + IMI20 + IMI29)/4)
imicpre$H <- with(imicpre, (IMI2 + IMI12 + IMI16 + IMI27)/4)
imicpre$HS <- with(imicpre, (IMI17 + IMI19 + IMI24 + IMI30)/4)
imicpre$S <- with(imicpre, (IMI4 + IMI7 + IMI25 + IMI28)/4)
imicpre$FS <- with(imicpre, (IMI3 + IMI15 + IMI21 + IMI31)/4)
imicpre$octF <- with(imicpre, (IMI5 + IMI6 + IMI8 + IMI11)/4)
imicpre$FD <- with(imicpre, (IMI14 + IMI22 + IMI26 + IMI32)/4)
imicpre <- dplyr::filter(imicpre, PTNUM!=8129)

write.csv(imicpre, "IMICPREscored.csv")
imicpost<- meanreplace(imicpost, "80220", varprefix = "IMI", subscaleitems = c(1, 10, 18, 23))#replace 1
imicpost <-meanreplace(imicpost, "80220", varprefix = "IMI", subscaleitems = c(2, 12, 16, 27)) #replace 2
imicpost <- meanreplace(imicpost, "80220", varprefix = "IMI", subscaleitems = c(3, 15, 21, 31)) #replace 3
imicpost <-meanreplace(imicpost, "81270", varprefix = "IMI", subscaleitems = c(2, 12, 16, 27)) #replace 2
imicpost <- meanreplace(imicpost, "80101", varprefix = "IMI", subscaleitems = c(4, 7, 25, 28)) #replace 4
imicpost <- meanreplace(imicpost, "80841", varprefix = "IMI", subscaleitems = c(4, 7, 25, 28)) #replace 4
imicpost <- meanreplace(imicpost, "81200", varprefix = "IMI", subscaleitems = c(4, 7, 25, 28)) #replace 4
imicpost <- meanreplace(imicpost, "81281", varprefix = "IMI", subscaleitems = c(4, 7, 25, 28)) #replace 7
imicpost <- meanreplace(imicpost, "81281", varprefix ="IMI", subscaleitems = c(5, 6, 8, 11)) #replace 8
imicpost <- meanreplace(imicpost, "80151", varprefix ="IMI", subscaleitems = c(5, 6, 8, 11)) #replace 11
imicpost <- meanreplace(imicpost, "80661", varprefix = "IMI", subscaleitems = c(17, 19, 24, 30)) #replace 19
imicpost <- meanreplace(imicpost, "80921", varprefix = "IMI", subscaleitems = c(9, 13, 20, 29)) #replace 20
imicpost <- meanreplace(imicpost, "81271", varprefix = "IMI", subscaleitems = c(3, 15, 21, 31))#replace 21
imicpost <- meanreplace(imicpost, "80940", varprefix = "IMI", subscaleitems = c(14, 22, 26, 32)) #replace 22
imicpost <- meanreplace(imicpost, "80151", varprefix = "IMI", subscaleitems = c(17, 19, 24, 30)) #replace 24
imicpost <- meanreplace(imicpost, "80211", varprefix = "IMI", subscaleitems = c(17, 19, 24, 30)) #replace 24
imicpost <- meanreplace(imicpost, "80230", varprefix = "IMI", subscaleitems = c(17, 19, 24, 30)) #replace 24
imicpost <- meanreplace(imicpost, "80441", varprefix = "IMI", subscaleitems = c(17, 19, 24, 30)) #replace 24
imicpost <- meanreplace(imicpost, "80490", varprefix = "IMI", subscaleitems = c(17, 19, 24, 30)) #replace 24
imicpost <- meanreplace(imicpost, "81411", varprefix = "IMI", subscaleitems = c(17, 19, 24, 30)) #replace 24
imicpost <- meanreplace(imicpost, "81411", varprefix = "IMI", subscaleitems = c(4, 7, 25, 28)) #replace 25
imicpost <- meanreplace(imicpost, "80230", varprefix = "IMI", subscaleitems = c(14, 22, 26, 32)) #replace 26
imicpost <-meanreplace(imicpost, "81200", varprefix = "IMI", subscaleitems = c(2, 12, 16, 27)) #replace 27
imicpost <- meanreplace(imicpost, "80060", varprefix = "IMI", subscaleitems = c(4, 7, 25, 28)) #replace 28
imicpost <- meanreplace(imicpost, "80131", varprefix = "IMI", subscaleitems = c(17, 19, 24, 30)) #replace 30
imicpost <- meanreplace(imicpost, "81341", varprefix = "IMI", subscaleitems = c(17, 19, 24, 30)) #replace 30
imicpost$D <- with(imicpost, (IMI1 + IMI10 + IMI18 + IMI23)/4)
imicpost$HD <-with(imicpost, (IMI9 + IMI13 + IMI20 + IMI29)/4)
imicpost$H <- with(imicpost, (IMI2 + IMI12 + IMI16 + IMI27)/4)
imicpost$HS <- with(imicpost, (IMI17 + IMI19 + IMI24 + IMI30)/4)
imicpost$S <- with(imicpost, (IMI4 + IMI7 + IMI25 + IMI28)/4)
imicpost$FS <- with(imicpost, (IMI3 + IMI15 + IMI21 + IMI31)/4)
imicpost$octF <- with(imicpost, (IMI5 + IMI6 + IMI8 + IMI11)/4)
imicpost$FD <- with(imicpost, (IMI14 + IMI22 + IMI26 + IMI32)/4)
imicpost <- dplyr::filter(imicpost, PTNUM!=8129) #I cut 8129 because they didn't finish like half the IMI
write.csv(imicpost, "IMICPOSTscored.csv")


##### START HERE FOR ANALYSES

imicpre <- read.csv("IMICPREscored.csv")
imicpost <- read.csv("IMICPOSTscored.csv")
imicprescales <- dplyr::select(imicpre, D, HD, H, HS, S, FS, octF, FD, PTNUM, DyadID, UsrID)
imicprescales <- dplyr::rename(imicprescales, D_pre = D, HD_pre = HD, H_pre = H, HS_pre = HS, S_pre = S, FS_pre = FS, F_pre = octF, FD_pre = FD)
imicpostscales <- dplyr::select(imicpost, D, HD, H, HS, S, FS, octF, FD, PTNUM, DyadID, UsrID)
imicpostscales <- dplyr::rename(imicpostscales, D_post = D, HD_post = HD, H_post = H, HS_post = HS, S_post = S, FS_post = FS, F_post = octF, FD_post = FD)
imicprepostscales <- dplyr::inner_join(imicprescales, imicpostscales, by = "UsrID")
imicprepostscales_patient <- dplyr::filter(imicprepostscales, DyadID.x == 1)
imicprepostscales_partner <- dplyr::filter(imicprepostscales, DyadID.x == 0)
imicprepostscales_partner <- dplyr::rename(imicprepostscales_partner, PTNUM = PTNUM.x)
imicprepostscales_patient <- dplyr::rename(imicprepostscales_patient, PTNUM = PTNUM.x)
imicprepostscales_dt_partner <- inner_join(imicprepostscales_partner, dt_partner, by = "PTNUM")
imicprepostscales_dt_patient <- inner_join(imicprepostscales_patient, dt_patient, by = "PTNUM")
imicprepostscales_dt <- inner_join(imicprepostscales_dt_patient, imicprepostscales_dt_partner, by = "PTNUM")
####now need to rename things
imicprepostscales_dt <- dplyr::rename(imicprepostscales_dt, D_pre_patient =D_pre.x , H_pre_patient = H_pre.x, HD_pre_patient = HD_pre.x, HS_pre_patient= HS_pre.x, S_pre_patient = S_pre.x, FS_pre_patient = FS_pre.x, F_pre_patient = F_pre.x, FD_pre_patient = FD_pre.x,
                                      D_pre_partner = D_pre.y, H_pre_partner = H_pre.y, HD_pre_partner = HD_pre.y, HS_pre_partner= HS_pre.y, S_pre_partner = S_pre.y, FS_pre_partner = FS_pre.y, F_pre_partner = F_pre.y, FD_pre_partner = FD_pre.y,
                                      D_post_patient = D_post.x, H_post_patient = H_post.x, HD_post_patient = HD_post.x, HS_post_patient= HS_post.x, S_post_patient = S_post.x, FS_post_patient = FS_post.x, F_post_patient = F_post.x, FD_post_patient = FD_post.x,
                                      D_post_partner = D_post.y, H_post_partner = H_post.y, HD_post_partner = HD_post.y, HS_post_partner= HS_post.y, S_post_partner = S_post.y, FS_post_partner = FS_post.y, F_post_partner = F_post.y, FD_post_partner = FD_post.y)                                     
imicprepostscales_dt <- dplyr::rename(imicprepostscales_dt, self_coupling_patient = self.coupling.patient, cross_coupling_patient = cross.coupling.patient, self_coupling_partner = self.coupling.partner, cross_coupling_partner = cross.coupling.partner)
#115 couples at this poing
#if ran SAAM prepost then don't need to run this
ll <- select(dt, LL, PTNUM)
imicprepostscales_dt <- inner_join(imicprepostscales_dt, ll, by = "PTNUM")
imicprepostscales_dt <- dplyr::filter(imicprepostscales_dt, LL > -69999) %>% dplyr::filter(abs(self_coupling_patient) < 1 & abs(cross_coupling_patient) < 1 & abs(self_coupling_partner) < 1 & abs(cross_coupling_partner) < 1)
#imicprepostscales_dt <- dplyr::rename(imicprepostscales_dt, self_coupling_patient = self.coupling.patient, cross_coupling_patient = cross.coupling.patient, self_coupling_partner = self.coupling.partner, cross_coupling_partner = cross.coupling.partner)
#cut 2 couples, so now N = 113
#experiencing the other person as dominant
imicprepostscales_dt$dom_pre_patient <- with(imicprepostscales_dt, D_pre_patient + H_pre_patient + HD_pre_patient +FD_pre_patient )
#experiencing the other person as submissive
imicprepostscales_dt$sub_pre_patient <- with(imicprepostscales_dt, F_pre_patient + FS_pre_patient + S_pre_patient +HS_pre_patient )
summary(ols <- lm(dom_pre_patient ~ self_coupling_patient + cross_coupling_patient, data = imicprepostscales_dt))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
d1 <- cooks.distance(ols)
r <- stdres(ols)
dt_shortened <- dplyr::select(imicprepostscales_dt, PTNUM, self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner, D_pre_patient, D_pre_partner)
a <- cbind(dt_shortened, d1, r)
a[d1 > 4/113, ]
#calculated outliers off of dom IMIC score (should be differentially related to cc based on findings from iip) 
#undue leverage form 8013, 8041, 8042, 8074, 8102, 8110, 8137, 8146
imicprepostscales_dt <- dplyr::filter(imicprepostscales_dt, PTNUM != 8013, PTNUM != 8041, PTNUM != 8042, PTNUM != 8074, PTNUM != 8102, PTNUM != 8110, PTNUM != 8137, PTNUM != 8146)
imicprepostscales_dt$dom_pre_partner <- with(imicprepostscales_dt, D_pre_partner + H_pre_partner + HD_pre_partner +FD_pre_partner )
#experiencing the other person as submissive
imicprepostscales_dt$sub_pre_partner <- with(imicprepostscales_dt, F_pre_partner + FS_pre_partner + S_pre_partner +HS_pre_partner )
imicprepostscales_dt$dom_post_patient <- with(imicprepostscales_dt, D_post_patient + H_post_patient + HD_post_patient +FD_post_patient )
#experiencing the other person as submissive
imicprepostscales_dt$sub_post_patient <- with(imicprepostscales_dt, F_post_patient + FS_post_patient + S_post_patient +HS_post_patient )
imicprepostscales_dt$dom_post_partner <- with(imicprepostscales_dt, D_post_partner + H_post_partner + HD_post_partner +FD_post_partner )
#experiencing the other person as submissive
imicprepostscales_dt$sub_post_partner <- with(imicprepostscales_dt, F_post_partner + FS_post_partner + S_post_partner +HS_post_partner)







imicprepostscales_dt <- mutate(imicprepostscales_dt, scpt = 100*self_coupling_patient, scpr = 100*self_coupling_partner, ccpt = 100*cross_coupling_patient, ccpr = 100*cross_coupling_partner)

#do for both sub and dom and each of the octants for (cc, sc, sccc, sc_prpt, cc_prpt, sccc_prpt)
#before cutting outliers N = 105
#outliers leading to some weird stuff with sc
imicprepostscales_dt <- dplyr::filter(imicprepostscales_dt, PTNUM != 8052, PTNUM != 8063, PTNUM != 8075)
#N now is 102
#D, H, HD, FD, F, FS, S, HS
imicprepostscales_dt <- mutate(imicprepostscales_dt, CON_pre_patient = D_pre_patient-S_pre_patient+.707*(HD_pre_patient+FD_pre_patient)-.707*(HS_pre_patient+FS_pre_patient), 
                                                  AFF_pre_patient = F_pre_patient-H_pre_patient + .707*(FD_pre_patient + FS_pre_patient)-.707*(HD_pre_patient+HS_pre_patient),
                                                  CON_pre_partner = D_pre_partner-S_pre_partner+.707*(HD_pre_partner+FD_pre_partner)-.707*(HS_pre_partner+FS_pre_partner), 
                                                  AFF_pre_partner = F_pre_partner-H_pre_partner + .707*(FD_pre_partner + FS_pre_partner)-.707*(HD_pre_partner+HS_pre_partner),
                                                 CON_post_patient = D_post_patient-S_post_patient+.707*(HD_post_patient+FD_post_patient)-.707*(HS_post_patient+FS_post_patient), 
                                                  AFF_post_patient = F_post_patient-H_post_patient + .707*(FD_post_patient + FS_post_patient)-.707*(HD_post_patient+HS_post_patient),
                                                CON_post_partner = D_post_partner-S_post_partner+.707*(HD_post_partner+FD_post_partner)-.707*(HS_post_partner+FS_post_partner), 
                                                 AFF_post_partner = F_post_partner-H_post_partner + .707*(FD_post_partner + FS_post_partner)-.707*(HD_post_partner+HS_post_partner))

imicprepostscales_dt <- mutate(imicprepostscales_dt, EL_pre_patient = (D_pre_patient + HD_pre_patient + H_pre_patient + HS_pre_patient + S_pre_patient + FS_pre_patient + F_pre_patient + FD_pre_patient)/8,
                                                      EL_post_patient = (D_post_patient + HD_post_patient + H_post_patient + HS_post_patient + S_post_patient + FS_post_patient + F_post_patient + FD_post_patient)/8,
                                                   EL_pre_partner = (D_pre_partner + HD_pre_partner + H_pre_partner + HS_pre_partner + S_pre_partner + FS_pre_partner + F_pre_partner + FD_pre_partner)/8,
                                                    EL_post_partner = (D_post_partner + HD_post_partner + H_post_partner + HS_post_partner + S_post_partner + FS_post_partner + F_post_partner + FD_post_partner)/8
)

imicprepostscales_dt_vanBse <- dplyr::inner_join(imicprepostscales_dt, vanBse_paramsm4, by = "PTNUM")
imicprepostscales_dt_vanBse <- mutate(imicprepostscales_dt_vanBse, v_scpt = 100*v_self_coupling_patient, v_scpr = 100*v_self_coupling_parnter, v_ccpt = 100*v_cross_coupling_patient, v_ccpr = 100*v_cross_coupling_partner)



#still need to calculate elevation


D_sc <- "
D_post_patient ~ scpt + D_pre_patient
scpt ~D_pre_patient
D_post_partner ~  scpr + D_pre_partner
scpr~D_pre_partner
scpr~~scpt
"

D_sc_m <- sem(D_sc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                            mimic="Mplus", meanstructure = TRUE)

D_cc <- "
D_post_patient ~ ccpt + D_pre_patient
ccpt ~D_pre_patient
D_post_partner ~  ccpr + D_pre_partner
ccpr~D_pre_partner
ccpr~~ccpt
"

D_cc_m <- sem(D_cc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

D_sccc <- "
D_post_patient ~ scpt + ccpt + D_pre_patient 
ccpt ~D_pre_patient
scpt ~D_pre_patient
D_post_partner ~  scpr + ccpr + D_pre_partner
ccpr~D_pre_partner
scpr ~ D_pre_partner
scpr~~scpt
ccpr~~ccpt
"

D_sccc_m <- sem(D_sccc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

D_sc_prpt <- "
D_post_patient ~ scpt + scpr + D_pre_patient
scpt ~D_pre_patient
scpr ~D_pre_patient
D_post_partner ~  scpr + scpt + D_pre_partner
scpr~D_pre_partner
scpt~D_pre_partner
scpt~~scpr
"

D_sc_prpt_m <- sem(D_sc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

D_cc_prpt <- "
D_post_patient ~ ccpt + ccpr + D_pre_patient
ccpt ~D_pre_patient
ccpr~D_pre_patient
D_post_partner ~  ccpr +ccpt + D_pre_partner
ccpr~D_pre_partner
ccpt ~ D_pre_partner
ccpr~~ccpt
"

D_cc_prpt_m <- sem(D_cc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

D_sccc_prpt <- "
D_post_patient ~ scpt + ccpt + scpr + ccpr + D_pre_patient
ccpt ~D_pre_patient
scpt ~D_pre_patient
ccpr ~D_pre_patient
scpr ~D_pre_patient
D_post_partner ~  scpr + ccpr + scpt + ccpt +D_pre_partner
ccpr~D_pre_partner
scpr ~ D_pre_partner
ccpt ~D_pre_partner
scpt~D_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

D_sccc_prpt_m <- sem(D_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                mimic="Mplus", meanstructure = TRUE)

D_sccc_prpt_vanBse <- "
D_post_patient ~ scpt + ccpt + scpr + ccpr + D_pre_patient 
ccpt ~D_pre_patient
scpt ~D_pre_patient
ccpr ~D_pre_patient
scpr ~D_pre_patient
ccpt ~v_ccpt
ccpr ~ v_ccpr
scpt ~v_scpt
scpr ~ v_scpr
D_post_partner ~  scpr + ccpr + scpt + ccpt +D_pre_partner
ccpr~D_pre_partner
scpr ~ D_pre_partner
ccpt ~D_pre_partner
scpt~D_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

D_sccc_prpt_vanBse_m <- sem(D_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)


H_sc <- "
H_post_patient ~ scpt + H_pre_patient
scpt ~H_pre_patient
H_post_partner ~  scpr + H_pre_partner
scpr~H_pre_partner
scpr~~scpt
"

H_sc_m <- sem(H_sc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

H_cc <- "
H_post_patient ~ ccpt + H_pre_patient
ccpt ~H_pre_patient
H_post_partner ~  ccpr + H_pre_partner
ccpr~H_pre_partner
ccpr~~ccpt
"

H_cc_m <- sem(H_cc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

H_sccc <- "
H_post_patient ~ scpt + ccpt + H_pre_patient
ccpt ~H_pre_patient
scpt ~H_pre_patient
H_post_partner ~  scpr + ccpr + H_pre_partner
ccpr~H_pre_partner
scpr ~ H_pre_partner
scpr~~scpt
ccpr~~ccpt
"

H_sccc_m <- sem(H_sccc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                mimic="Mplus", meanstructure = TRUE)

H_sc_prpt <- "
H_post_patient ~ scpt + scpr + H_pre_patient
scpt ~H_pre_patient
scpr ~H_pre_patient
H_post_partner ~  scpr + scpt + H_pre_partner
scpr~H_pre_partner
scpt~H_pre_partner
scpr~~scpt
"

H_sc_prpt_m <- sem(H_sc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

H_cc_prpt <- "
H_post_patient ~ ccpt + ccpr + H_pre_patient
ccpt ~H_pre_patient
ccpr~H_pre_patient
H_post_partner ~  ccpr +ccpt + H_pre_partner
ccpr~H_pre_partner
ccpt ~ H_pre_partner
ccpr~~ccpt
"

H_cc_prpt_m <- sem(H_cc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

H_sccc_prpt <- "
H_post_patient ~ scpt + ccpt + scpr + ccpr + H_pre_patient
ccpt ~H_pre_patient
scpt ~H_pre_patient
ccpr ~H_pre_patient
scpr ~H_pre_patient
H_post_partner ~  scpr + ccpr + scpt + ccpt +H_pre_partner
ccpr~H_pre_partner
scpr ~ H_pre_partner
ccpt ~H_pre_partner
scpt~H_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

H_sccc_prpt_m <- sem(H_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

H_sccc_prpt_vanBse <- "
H_post_patient ~ scpt + ccpt + scpr + ccpr + H_pre_patient
ccpt ~H_pre_patient
scpt ~H_pre_patient
ccpr ~H_pre_patient
scpr ~H_pre_patient
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
H_post_partner ~  scpr + ccpr + scpt + ccpt +H_pre_partner
ccpr~H_pre_partner
scpr ~ H_pre_partner
ccpt ~H_pre_partner
scpt~H_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

H_sccc_prpt_vanBse_m <- sem(H_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)



HD_sc <- "
HD_post_patient ~ scpt + HD_pre_patient
scpt ~HD_pre_patient
HD_post_partner ~  scpr + HD_pre_partner
scpr~HD_pre_partner
scpr~~scpt
"

HD_sc_m <- sem(HD_sc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

HD_cc <- "
HD_post_patient ~ ccpt + HD_pre_patient
ccpt ~HD_pre_patient
HD_post_partner ~  ccpr + HD_pre_partner
ccpr~HD_pre_partner
ccpr~~ccpt
"

HD_cc_m <- sem(HD_cc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

HD_sccc <- "
HD_post_patient ~ scpt + ccpt + HD_pre_patient
ccpt ~HD_pre_patient
scpt ~HD_pre_patient
HD_post_partner ~  scpr + ccpr + HD_pre_partner
ccpr~HD_pre_partner
scpr ~ HD_pre_partner
scpr~~scpt
ccpr~~ccpt
"

HD_sccc_m <- sem(HD_sccc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                mimic="Mplus", meanstructure = TRUE)

HD_sc_prpt <- "
HD_post_patient ~ scpt + scpr + HD_pre_patient
scpt ~HD_pre_patient
scpr ~HD_pre_patient
HD_post_partner ~  scpr + scpt + HD_pre_partner
scpr~HD_pre_partner
scpt~HD_pre_partner
scpt~~scpr
"

HD_sc_prpt_m <- sem(HD_sc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

HD_cc_prpt <- "
HD_post_patient ~ ccpt + ccpr + HD_pre_patient
ccpt ~HD_pre_patient
ccpr~HD_pre_patient
HD_post_partner ~  ccpr +ccpt + HD_pre_partner
ccpr~HD_pre_partner
ccpt ~ HD_pre_partner
ccpr~~ccpt
"

HD_cc_prpt_m <- sem(HD_cc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

HD_sccc_prpt <- "
HD_post_patient ~ scpt + ccpt + scpr + ccpr + HD_pre_patient
ccpt ~HD_pre_patient
scpt ~HD_pre_patient
ccpr ~HD_pre_patient
scpr ~HD_pre_patient
HD_post_partner ~  scpr + ccpr + scpt + ccpt +HD_pre_partner
ccpr~HD_pre_partner
scpr ~ HD_pre_partner
ccpt ~HD_pre_partner
scpt~HD_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

HD_sccc_prpt_m <- sem(HD_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

HD_sccc_prpt_vanBse <- "
HD_post_patient ~ scpt + ccpt + scpr + ccpr + HD_pre_patient
ccpt ~HD_pre_patient 
scpt ~HD_pre_patient 
scpt ~v_scpt
ccpr ~HD_pre_patient
scpr ~HD_pre_patient 
scpr ~v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr
HD_post_partner ~  scpr + ccpr + scpt + ccpt +HD_pre_partner
ccpr~HD_pre_partner
scpr ~ HD_pre_partner
ccpt ~HD_pre_partner
scpt~HD_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

HD_sccc_prpt_vanBse_m <- sem(HD_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE)



FD_sc <- "
FD_post_patient ~ scpt + FD_pre_patient
scpt ~FD_pre_patient
FD_post_partner ~  scpr + FD_pre_partner
scpr~FD_pre_partner
scpr~~scpt
"

FD_sc_m <- sem(FD_sc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

FD_cc <- "
FD_post_patient ~ ccpt + FD_pre_patient
ccpt ~FD_pre_patient
FD_post_partner ~  ccpr + FD_pre_partner
ccpr~FD_pre_partner
ccpr~~ccpt
"

FD_cc_m <- sem(FD_cc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

FD_sccc <- "
FD_post_patient ~ scpt + ccpt + FD_pre_patient
ccpt ~FD_pre_patient
scpt ~FD_pre_patient
FD_post_partner ~  scpr + ccpr + FD_pre_partner
ccpr~FD_pre_partner
scpr ~ FD_pre_partner
scpr~~scpt
ccpr~~ccpt
"

FD_sccc_m <- sem(FD_sccc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                mimic="Mplus", meanstructure = TRUE)

FD_sc_prpt <- "
FD_post_patient ~ scpt + scpr + FD_pre_patient
scpt ~FD_pre_patient
scpr ~FD_pre_patient
FD_post_partner ~  scpr + scpt + FD_pre_partner
scpr~FD_pre_partner
scpt~FD_pre_partner
scpt~~scpr
"

FD_sc_prpt_m <- sem(FD_sc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

FD_cc_prpt <- "
FD_post_patient ~ ccpt + ccpr + FD_pre_patient
ccpt ~FD_pre_patient
ccpr~FD_pre_patient
FD_post_partner ~  ccpr +ccpt + FD_pre_partner
ccpr~FD_pre_partner
ccpt ~ FD_pre_partner
ccpr~~ccpt
"

FD_cc_prpt_m <- sem(FD_cc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

FD_sccc_prpt <- "
FD_post_patient ~ scpt + ccpt + scpr + ccpr + FD_pre_patient
ccpt ~FD_pre_patient
scpt ~FD_pre_patient
ccpr ~FD_pre_patient
scpr ~FD_pre_patient
FD_post_partner ~  scpr + ccpr + scpt + ccpt +FD_pre_partner
ccpr~FD_pre_partner
scpr ~ FD_pre_partner
ccpt ~FD_pre_partner
scpt~FD_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

FD_sccc_prpt_m <- sem(FD_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)


FD_sccc_prpt_vanBse <- "
FD_post_patient ~ scpt + ccpt + scpr + ccpr + FD_pre_patient
ccpt ~FD_pre_patient
scpt ~FD_pre_patient
ccpr ~FD_pre_patient
scpr ~FD_pre_patient
ccpt ~ v_ccpt
scpt ~ v_scpt
ccpr ~ v_ccpr
scpr ~ v_scpr
FD_post_partner ~  scpr + ccpr + scpt + ccpt +FD_pre_partner
ccpr~FD_pre_partner
scpr ~ FD_pre_partner
ccpt ~FD_pre_partner
scpt~FD_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

FD_sccc_prpt_vanBse_m <- sem(FD_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE)




F_sc <- "
F_post_patient ~ scpt + F_pre_patient
scpt ~F_pre_patient
F_post_partner ~  scpr + F_pre_partner
scpr~F_pre_partner
scpr~~scpt
"

F_sc_m <- sem(F_sc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

F_cc <- "
F_post_patient ~ ccpt + F_pre_patient
ccpt ~F_pre_patient
F_post_partner ~  ccpr + F_pre_partner
ccpr~F_pre_partner
ccpr~~ccpt
"

F_cc_m <- sem(F_cc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

F_sccc <- "
F_post_patient ~ scpt + ccpt + F_pre_patient
ccpt ~F_pre_patient
scpt ~F_pre_patient
F_post_partner ~  scpr + ccpr + F_pre_partner
ccpr~F_pre_partner
scpr ~F_pre_partner
scpr~~scpt
ccpr~~ccpt
"

F_sccc_m <- sem(F_sccc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                mimic="Mplus", meanstructure = TRUE)

F_sc_prpt <- "
F_post_patient ~ scpt + scpr + F_pre_patient
scpt ~F_pre_patient
scpr ~F_pre_patient
F_post_partner ~  scpr + scpt + F_pre_partner
scpr~F_pre_partner
scpt~F_pre_partner
scpt~~scpr
"

F_sc_prpt_m <- sem(F_sc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

F_cc_prpt <- "
F_post_patient ~ ccpt + ccpr + F_pre_patient
ccpt ~F_pre_patient
ccpr~F_pre_patient
F_post_partner ~  ccpr +ccpt + F_pre_partner
ccpr~F_pre_partner
ccpt ~ F_pre_partner
ccpr~~ccpt
"

F_cc_prpt_m <- sem(F_cc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

F_sccc_prpt <- "
F_post_patient ~ scpt + ccpt + scpr + ccpr + F_pre_patient
ccpt ~F_pre_patient
scpt ~F_pre_patient
ccpr ~F_pre_patient
scpr ~F_pre_patient
F_post_partner ~  scpr + ccpr + scpt + ccpt +F_pre_partner
ccpr~F_pre_partner
scpr ~ F_pre_partner
ccpt ~F_pre_partner
scpt~F_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

F_sccc_prpt_m <- sem(F_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)



F_sccc_prpt_vanBse <- "
F_post_patient ~ scpt + ccpt + scpr + ccpr + F_pre_patient
ccpt ~F_pre_patient
scpt ~F_pre_patient
ccpr ~F_pre_patient
scpr ~F_pre_patient
ccpt ~ v_ccpt
ccpr ~ v_ccpr
scpt ~ v_scpt
scpr ~ v_scpr
F_post_partner ~  scpr + ccpr + scpt + ccpt +F_pre_partner
ccpr~F_pre_partner
scpr ~ F_pre_partner
ccpt ~F_pre_partner
scpt~F_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

F_sccc_prpt_vanBse_m <- sem(F_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)








FS_sc <- "
FS_post_patient ~ scpt + FS_pre_patient
scpt ~FS_pre_patient
FS_post_partner ~  scpr + FS_pre_partner
scpr~FS_pre_partner
scpr~~scpt
"

FS_sc_m <- sem(FS_sc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

FS_cc <- "
FS_post_patient ~ ccpt + FS_pre_patient
ccpt ~FS_pre_patient
FS_post_partner ~  ccpr + FS_pre_partner
ccpr~FS_pre_partner
ccpr~~ccpt
"

FS_cc_m <- sem(FS_cc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

FS_sccc <- "
FS_post_patient ~ scpt + ccpt + FS_pre_patient
ccpt ~FS_pre_patient
scpt ~FS_pre_patient
FS_post_partner ~  scpr + ccpr + FS_pre_partner
ccpr~FS_pre_partner
scpr ~ FS_pre_partner
scpr~~scpt
ccpr~~ccpt
"

FS_sccc_m <- sem(FS_sccc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                mimic="Mplus", meanstructure = TRUE)

FS_sc_prpt <- "
FS_post_patient ~ scpt + scpr + FS_pre_patient
scpt ~FS_pre_patient
scpr ~FS_pre_patient
FS_post_partner ~  scpr + scpt + FS_pre_partner
scpr~FS_pre_partner
scpt~FS_pre_partner
scpt~~scpr
"

FS_sc_prpt_m <- sem(FS_sc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

FS_cc_prpt <- "
FS_post_patient ~ ccpt + ccpr + FS_pre_patient
ccpt ~FS_pre_patient
ccpr~FS_pre_patient
FS_post_partner ~  ccpr +ccpt + FS_pre_partner
ccpr~FS_pre_partner
ccpt ~ FS_pre_partner
ccpr~~ccpt
"

FS_cc_prpt_m <- sem(FS_cc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

FS_sccc_prpt <- "
FS_post_patient ~ scpt + ccpt + scpr + ccpr + FS_pre_patient
ccpt ~FS_pre_patient
scpt ~FS_pre_patient
ccpr ~FS_pre_patient
scpr ~FS_pre_patient
FS_post_partner ~  scpr + ccpr + scpt + ccpt +FS_pre_partner
ccpr~FS_pre_partner
scpr ~ FS_pre_partner
ccpt ~FS_pre_partner
scpt~FS_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

FS_sccc_prpt_m <- sem(FS_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)



FS_sccc_prpt_vanBse <- "
FS_post_patient ~ scpt + ccpt + scpr + ccpr + FS_pre_patient
ccpt ~FS_pre_patient
scpt ~FS_pre_patient
ccpr ~FS_pre_patient
scpr ~FS_pre_patient
ccpt ~ v_ccpt
scpt ~ v_scpt
ccpr ~ v_ccpr
scpr ~ v_scpr
FS_post_partner ~  scpr + ccpr + scpt + ccpt +FS_pre_partner
ccpr~FS_pre_partner
scpr ~ FS_pre_partner
ccpt ~FS_pre_partner
scpt~FS_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

FS_sccc_prpt_vanBse_m <- sem(FS_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE)





S_sc <- "
S_post_patient ~ scpt + S_pre_patient
scpt ~S_pre_patient
S_post_partner ~  scpr + S_pre_partner
scpr~S_pre_partner
scpr~~scpt
"

S_sc_m <- sem(S_sc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

S_cc <- "
S_post_patient ~ ccpt + S_pre_patient
ccpt ~S_pre_patient
S_post_partner ~  ccpr + S_pre_partner
ccpr~S_pre_partner
ccpr~~ccpt
"

S_cc_m <- sem(S_cc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

S_sccc <- "
S_post_patient ~ scpt + ccpt + S_pre_patient
ccpt ~S_pre_patient
scpt ~S_pre_patient
S_post_partner ~  scpr + ccpr + S_pre_partner
ccpr~S_pre_partner
scpr ~ S_pre_partner
scpr~~scpt
ccpr~~ccpt
"

S_sccc_m <- sem(S_sccc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                mimic="Mplus", meanstructure = TRUE)

S_sc_prpt <- "
S_post_patient ~ scpt + scpr + S_pre_patient
scpt ~S_pre_patient
scpr ~S_pre_patient
S_post_partner ~  scpr + scpt + S_pre_partner
scpr~S_pre_partner
scpt~S_pre_partner
scpt~~scpr
"

S_sc_prpt_m <- sem(S_sc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

S_cc_prpt <- "
S_post_patient ~ ccpt + ccpr + S_pre_patient
ccpt ~S_pre_patient
ccpr~S_pre_patient
S_post_partner ~  ccpr +ccpt + S_pre_partner
ccpr~S_pre_partner
ccpt ~ S_pre_partner
ccpr~~ccpt
"

S_cc_prpt_m <- sem(S_cc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

S_sccc_prpt <- "
S_post_patient ~ scpt + ccpt + scpr + ccpr + S_pre_patient
ccpt ~S_pre_patient
scpt ~S_pre_patient
ccpr ~S_pre_patient
scpr ~S_pre_patient
S_post_partner ~  scpr + ccpr + scpt + ccpt +S_pre_partner
ccpr~S_pre_partner
scpr ~S_pre_partner
ccpt ~S_pre_partner
scpt~S_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

S_sccc_prpt_m <- sem(S_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)



S_sccc_prpt_vanBse <- "
S_post_patient ~ scpt + ccpt + scpr + ccpr + S_pre_patient
ccpt ~S_pre_patient
scpt ~S_pre_patient
ccpr ~S_pre_patient
scpr ~S_pre_patient
scpt ~ v_scpt
ccpt ~ v_ccpr
scpr ~ v_scpr
ccpr ~ v_ccpr
S_post_partner ~  scpr + ccpr + scpt + ccpt +S_pre_partner
ccpr~S_pre_partner
scpr ~S_pre_partner
ccpt ~S_pre_partner
scpt~S_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

S_sccc_prpt_vanBse_m <- sem(S_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)
imicprepostscales_dt_vanBse_pDat <- merge(imicprepostscales_dt_vanBse, df_nooutliers, by = c(self_coupling_patient, cross_coupling_patient, self_coupling_partner, cross_coupling_partner, PTNUM))





HS_sc <- "
HS_post_patient ~ scpt + HS_pre_patient
scpt ~HS_pre_patient
HS_post_partner ~  scpr + HS_pre_partner
scpr~HS_pre_partner
scpr~~scpt
"

HS_sc_m <- sem(HS_sc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

HS_cc <- "
HS_post_patient ~ ccpt + HS_pre_patient
ccpt ~HS_pre_patient
HS_post_partner ~  ccpr + HS_pre_partner
ccpr~HS_pre_partner
ccpr~~ccpt
"

HS_cc_m <- sem(HS_cc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
              mimic="Mplus", meanstructure = TRUE)

HS_sccc <- "
HS_post_patient ~ scpt + ccpt + HS_pre_patient
ccpt ~HS_pre_patient
scpt ~HS_pre_patient
HS_post_partner ~  scpr + ccpr + HS_pre_partner
ccpr~HS_pre_partner
scpr ~ HS_pre_partner
scpr~~scpt
ccpr~~ccpt
"

HS_sccc_m <- sem(HS_sccc, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                mimic="Mplus", meanstructure = TRUE)

HS_sc_prpt <- "
HS_post_patient ~ scpt + scpr + HS_pre_patient
scpt ~HS_pre_patient
scpr ~HS_pre_patient
HS_post_partner ~  scpr + scpt + HS_pre_partner
scpr~HS_pre_partner
scpt~HS_pre_partner
scpt~~scpr
"

HS_sc_prpt_m <- sem(HS_sc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

HS_cc_prpt <- "
HS_post_patient ~ ccpt + ccpr + HS_pre_patient
ccpt ~HS_pre_patient
ccpr~HS_pre_patient
HS_post_partner ~  ccpr +ccpt + HS_pre_partner
ccpr~HS_pre_partner
ccpt ~ HS_pre_partner
ccpr~~ccpt
"

HS_cc_prpt_m <- sem(HS_cc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                   mimic="Mplus", meanstructure = TRUE)

HS_sccc_prpt <- "
HS_post_patient ~ scpt + ccpt + scpr + ccpr + HS_pre_patient
ccpt ~HS_pre_patient
scpt ~HS_pre_patient
ccpr ~HS_pre_patient
scpr ~HS_pre_patient
HS_post_partner ~  scpr + ccpr + scpt + ccpt +HS_pre_partner
ccpr~HS_pre_partner
scpr ~ HS_pre_partner
ccpt ~HS_pre_partner
scpt~HS_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

HS_sccc_prpt_m <- sem(HS_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

HS_sccc_prpt_vanBse <- "
HS_post_patient ~ scpt + ccpt + scpr + ccpr + HS_pre_patient
ccpt ~HS_pre_patient
scpt ~HS_pre_patient
ccpr ~HS_pre_patient
scpr ~HS_pre_patient
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
HS_post_partner ~  scpr + ccpr + scpt + ccpt +HS_pre_partner
ccpr~HS_pre_partner
scpr ~ HS_pre_partner
ccpt ~HS_pre_partner
scpt~HS_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr
"

HS_sccc_prpt_vanBse_m <- sem(HS_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE)







CON_sccc_prpt <- "
CON_post_patient ~ scpt + ccpt + scpr + ccpr + CON_pre_patient
ccpt ~CON_pre_patient
scpt ~CON_pre_patient
ccpr ~CON_pre_patient
scpr ~CON_pre_patient
CON_post_partner ~  scpr + ccpr + scpt + ccpt +CON_pre_partner
ccpr~CON_pre_partner
scpr ~ CON_pre_partner
ccpt ~CON_pre_partner
scpt~CON_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr

"
CON_sccc_prpt_m <- sem(CON_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE)


CON_sccc_prpt_vanBse <- "
CON_post_patient ~ scpt + ccpt + scpr + ccpr + CON_pre_patient
ccpt ~CON_pre_patient
scpt ~CON_pre_patient
ccpr ~CON_pre_patient
scpr ~CON_pre_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr
CON_post_partner ~  scpr + ccpr + scpt + ccpt +CON_pre_partner
ccpr~CON_pre_partner
scpr ~ CON_pre_partner
ccpt ~CON_pre_partner
scpt~CON_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr

"
CON_sccc_prpt_vanBse_m <- sem(CON_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE)

CON_sccc_prpt_vanBse_pDat <- "
CON_post_patient ~ scpt + ccpt + scpr + ccpr + CON_pre_patient
ccpt ~CON_pre_patient
scpt ~CON_pre_patient
ccpr ~CON_pre_patient
scpr ~CON_pre_patient
scpt ~ v_scpt + iip_elcpt
scpr ~ v_scpr + iip_elcpr 
ccpt ~ v_ccpt + iip_elcpt
ccpr ~ v_ccpr + iip_elcpr
CON_post_partner ~  scpr + ccpr + scpt + ccpt +CON_pre_partner
ccpr~CON_pre_partner
scpr ~ CON_pre_partner
ccpt ~CON_pre_partner
scpt~CON_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr

"
CON_sccc_prpt_vanBse_pDat_m <- sem(CON_sccc_prpt_vanBse_pDat, imicprepostscales_dt_vanBse_pDat, missing = "listwise", estimator = "ML", 
                              mimic="Mplus", meanstructure = TRUE)

CON_sccc_prpt_vanBse_nomissing_m <- sem(CON_sccc_prpt_vanBse, imicprepostscales_dt_vanBse_pDat, missing = "listwise", estimator = "ML", mimic = "Mplus", meanstructure = TRUE)












AFF_sccc_prpt <- "
AFF_post_patient ~ scpt + ccpt + scpr + ccpr + AFF_pre_patient
ccpt ~AFF_pre_patient
scpt ~AFF_pre_patient
ccpr ~AFF_pre_patient
scpr ~AFF_pre_patient
AFF_post_partner ~  scpr + ccpr + scpt + ccpt +AFF_pre_partner
ccpr~AFF_pre_partner
scpr ~ AFF_pre_partner
ccpt ~AFF_pre_partner
scpt~AFF_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr

"
AFF_sccc_prpt_m <- sem(AFF_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE)



AFF_sccc_prpt_vanBse <- "
AFF_post_patient ~ scpt + ccpt + scpr + ccpr + AFF_pre_patient
ccpt ~AFF_pre_patient
scpt ~AFF_pre_patient
ccpr ~AFF_pre_patient
scpr ~AFF_pre_patient
scpt ~ v_scpt
scpr ~ v_scpr
ccpt ~ v_ccpt
ccpr ~ v_ccpr
AFF_post_partner ~  scpr + ccpr + scpt + ccpt +AFF_pre_partner
ccpr~AFF_pre_partner
scpr ~ AFF_pre_partner
ccpt ~AFF_pre_partner
scpt~AFF_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr

"
AFF_sccc_prpt_vanBse_m <- sem(AFF_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE)




AFF_cc_prpt <- "
AFF_post_patient ~ ccpt + ccpr + AFF_pre_patient
ccpt ~AFF_pre_patient
ccpr ~AFF_pre_patient
AFF_post_partner ~ ccpr + ccpt +AFF_pre_partner
ccpr~AFF_pre_partner
ccpt ~AFF_pre_partner
ccpr~~ccpt

"
AFF_cc_prpt_m <- sem(AFF_cc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE)



EL_sccc_prpt <- "
EL_post_patient ~ scpt + ccpt + scpr + ccpr + EL_pre_patient
ccpt ~EL_pre_patient
scpt ~EL_pre_patient
ccpr ~EL_pre_patient
scpr ~EL_pre_patient
EL_post_partner ~  scpr + ccpr + scpt + ccpt +EL_pre_partner
ccpr~EL_pre_partner
scpr ~ EL_pre_partner
ccpt ~EL_pre_partner
scpt~EL_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr

"
EL_sccc_prpt_m <- sem(EL_sccc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE)

EL_sccc_prpt_vanBse <- "
EL_post_patient ~ scpt + ccpt + scpr + ccpr + EL_pre_patient
ccpt ~EL_pre_patient
scpt ~EL_pre_patient
ccpr ~EL_pre_patient
scpr ~EL_pre_patient
ccpt ~ v_ccpt
scpt ~ v_scpt
ccpr ~ v_ccpr
scpr ~ v_scpr
EL_post_partner ~  scpr + ccpr + scpt + ccpt +EL_pre_partner
ccpr~EL_pre_partner
scpr ~ EL_pre_partner
ccpt ~EL_pre_partner
scpt~EL_pre_partner
scpr~~scpt
ccpr~~ccpt
scpr~~ccpt
scpr~~ccpr
scpt~~ccpt
scpt~~ccpr

"
EL_sccc_prpt_vanBse_m <- sem(EL_sccc_prpt_vanBse, imicprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE)


EL_cc_prpt <- "
EL_post_patient ~  ccpt + ccpr + EL_pre_patient
ccpt ~EL_pre_patient
ccpr ~EL_pre_patient
EL_post_partner ~  scpr + ccpr + scpt + ccpt +EL_pre_partner
ccpr~EL_pre_partner
ccpt ~EL_pre_partner
ccpr~~ccpt
"
EL_cc_prpt_m <- sem(EL_cc_prpt, imicprepostscales_dt, missing = "listwise", estimator = "ML", 
                      mimic="Mplus", meanstructure = TRUE)





#D, H, HD, FD, F, FS, S, HS
anova(D_sc_m, D_cc_m, D_sccc_m, D_sc_prpt_m, D_cc_prpt_m, D_sccc_prpt_m, 
      H_sc_m, H_cc_m, H_sccc_m, H_sc_prpt_m, H_cc_prpt_m, H_sccc_prpt_m,
      HD_sc_m, HD_cc_m, HD_sccc_m, HD_sc_prpt_m, HD_cc_prpt_m, HD_sccc_prpt_m,
      FD_sc_m, FD_cc_m, FD_sccc_m, FD_sc_prpt_m, FD_cc_prpt_m, FD_sccc_prpt_m,
      F_sc_m, F_cc_m, F_sccc_m, F_sc_prpt_m, F_cc_prpt_m, F_sccc_prpt_m,
      FS_sc_m, FS_cc_m, FS_sccc_m, FS_sc_prpt_m, FS_cc_prpt_m, FS_sccc_prpt_m,
      S_sc_m, S_cc_m, S_sccc_m, S_sc_prpt_m, S_cc_prpt_m, S_sccc_prpt_m,
      HS_sc_m, HS_cc_m, HS_sccc_m, HS_sc_prpt_m, HS_cc_prpt_m, HS_sccc_prpt_m)


anova(D_sccc_prpt_vanBse_m, H_sccc_prpt_vanBse_m, HD_sccc_prpt_vanBse_m, FD_sccc_prpt_vanBse_m, F_sccc_prpt_vanBse_m, FS_sccc_prpt_vanBse_m, S_sccc_prpt_vanBse_m, HS_sccc_prpt_vanBse_m, CON_sccc_prpt_vanBse_m, AFF_sccc_prpt_vanBse_m, EL_sccc_prpt_vanBse_m)
anova(saam_sccc_prpt_anx_vanBse_m, saam_sccc_prpt_avo_vanBse_m)


#perceived hostility of the patient associated with  greater self coupling in the partner
#nothing for HD or FD
#trending for sub predicting sc in patient
#stronger self coupling during the interaction associated with perceiving the other person as more submissive after the interaction
#perceiving the partner as being hostile and submissive is associated with stronger self coupling in patient during interaction
#D 



















# 
# basedir <- "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/"
# 
# setwd(file.path(basedir))
# 
# #code for SAAM scoring
# imicpre <- read.csv("IMICPRE.csv", header=TRUE)
# 
# str(imicpre)
# 
# #On BPQ, 1 = True and 2 = False
# #Switch to 1 = True and 0 = False
# imicitems = paste0("IMI", 1:32)
# # saampre[,saamitems] <- lapply(saampre[,saamitems], function(vec) { 
# #   sapply(vec, function(x) {
# #     if (is.na(x)) { return (NA) 
# #     } else if (x==1) { return(1) #True
# #     } else if (x==2) { return(0) #False
# #     } else { stop("Cannot match BPQ response: ", x) }
# #   })
# # })
# imicpre$ID <- paste0(imicpre$PTNUM, imicpre$DyadID)
# #handle missingness (mean imputation at the moment)
# #check for missing items
# for (i in 1:nrow(imicpre)) {
#   miss <- which(is.na(imicpre[i, imicitems]))
#   if (length(miss) > 0) {
#     cat("For subject id: ", paste(sapply(saampre[i, c("ID")], as.character), collapse="/"), ", these items are missing: ", paste(miss, collapse=", "), "\n", sep="")
#   }
# }
# 
# imicpre$ID <- paste0(imicpre$PTNUM, imicpre$DyadID)
# 
# imicpre$dominant <- with(saampost, SAAM1 + SAAM5 + SAAM8 + SAAM12 + SAAM14 + SAAM17 + SAAM19)
# imicpre$hostiledominant <- with(saampost, SAAM2 + SAAM3 + SAAM9 + SAAM10 + SAAM15 + SAAM16 + SAAM21)
# imicpre$hostile <- with(saampost, SAAM4 + SAAM6 + SAAM7 + SAAM11 + SAAM13 + SAAM18 + SAAM20)
# imicpre$hostilesubmissive <- with(saampost, anx + avo + sec)
# imicpre$submissive <- with(saampost, anx + avo + sec)
# imicpre$totalscore <- with(saampost, anx + avo + sec)
# imicpre$totalscore <- with(saampost, anx + avo + sec)
# imicpre$totalscore <- with(saampost, anx + avo + sec)
# imicpre$totalscore <- with(saampost, anx + avo + sec)
# imicpre$totalscore <- with(saampost, anx + avo + sec)
# imicpre$totalscore <- with(saampost, anx + avo + sec)
# 

#PANAS PRE POST

panaspre <- read.csv("PANASPRE.csv")
panaspre$ID <- paste0(panaspre$PTNUM, panaspre$DyadID)
panasitems <- paste0("PANAS", 1:20)
for (i in 1:nrow(panaspre)) {
  miss <- which(is.na(panaspre[i, panasitems]))
  if (length(miss) > 0) {
    cat("For subject id: ", paste(sapply(panaspre[i, c("ID")], as.character), collapse="/"), ", these items are missing: ", paste(miss, collapse=", "), "\n", sep="")
  }
}
# subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)
# subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)

meanreplace <- function(df, ID, varprefix="PANAS", subscaleitems) {
  subjrow <- df$UsrID == ID
  dfitems <- subset(df, select=paste0(varprefix, subscaleitems))
  dfitems[dfitems == 99] <- NA
  subjitems <- dfitems[subjrow,]
  whichmiss <- names(subjitems)[which(is.na(subjitems))]
  obsmean <- mean(unlist(subjitems), na.rm=TRUE)
  df[subjrow, whichmiss] <- obsmean
  df
}

panaspre <- meanreplace(panaspre, "80230", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 19
panaspre <- meanreplace(panaspre, "80230", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 2, 18, 19
panaspre <- meanreplace(panaspre, "80290", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 4
panaspre <- meanreplace(panaspre, "80290", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 5, 16, 17
panaspre <- meanreplace(panaspre, "80481", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 12
panaspre <- meanreplace(panaspre, "80481", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 11
panaspre <- meanreplace(panaspre, "80521", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 11
panaspre <- meanreplace(panaspre, "80521", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 12
panaspre <- meanreplace(panaspre, "80611", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 19
panaspre <- meanreplace(panaspre, "80621", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 11
panaspre <- meanreplace(panaspre, "80621", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 12
panaspre <- meanreplace(panaspre, "80671", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 16
panaspre <- meanreplace(panaspre, "80921", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 8
panaspre <- meanreplace(panaspre, "80940", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 17
panaspre <- meanreplace(panaspre, "80940", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 15
panaspre <- meanreplace(panaspre, "80990", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 4
panaspre <- meanreplace(panaspre, "80990", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 5






panaspre$pa <- with(panaspre, PANAS1 + PANAS3 + PANAS5 + PANAS9 + PANAS10 + PANAS12 + PANAS14 + PANAS16 + PANAS17 + PANAS19)
panaspre$na <- with(panaspre, PANAS2 + PANAS4 + PANAS6 + PANAS7 + PANAS8 + PANAS11 + PANAS13 + PANAS15 + PANAS18 + PANAS20)
write.csv(panaspre, file="PANASPREscored.csv", row.names=FALSE)
panaspre <- read.csv("PANASPREscored.csv")

panaspost <- read.csv("PANASPOST.csv")
panaspost$ID <- paste0(panaspost$PTNUM, panaspost$DyadID)
panasitems <- paste0("PANAS", 1:20)
for (i in 1:nrow(panaspost)) {
  miss <- which(is.na(panaspost[i, panasitems]))
  if (length(miss) > 0) {
    cat("For subject id: ", paste(sapply(panaspost[i, c("ID")], as.character), collapse="/"), ", these items are missing: ", paste(miss, collapse=", "), "\n", sep="")
  }
}
# subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)
# subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)

meanreplace <- function(df, ID, varprefix="PANAS", subscaleitems) {
  subjrow <- df$ID == ID
  dfitems <- subset(df, select=paste0(varprefix, subscaleitems))
  dfitems[dfitems == 99] <- NA
  subjitems <- dfitems[subjrow,]
  whichmiss <- names(subjitems)[which(is.na(subjitems))]
  obsmean <- mean(unlist(subjitems), na.rm=TRUE)
  df[subjrow, whichmiss] <- obsmean
  df
}


panaspost <- meanreplace(panaspost, "80070", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 12
panaspost <- meanreplace(panaspost, "80070", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 11
panaspost <- meanreplace(panaspost, "80080", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 7
panaspost <- meanreplace(panaspost, "80341", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 18
panaspost <- meanreplace(panaspost, "80341", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 16
panaspost <- meanreplace(panaspost, "80480", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 15
panaspost <- meanreplace(panaspost, "80611", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 10
panaspost <- meanreplace(panaspost, "80621", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 12
panaspost <- meanreplace(panaspost, "80621", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 11
panaspost <- meanreplace(panaspost, "80921", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 2
panaspost <- meanreplace(panaspost, "80960", subscaleitems = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #miss 15
panaspost <- meanreplace(panaspost, "81411", subscaleitems = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #miss 16










panaspost$pa <- with(panaspost, PANAS1 + PANAS3 + PANAS5 + PANAS9 + PANAS10 + PANAS12 + PANAS14 + PANAS16 + PANAS17 + PANAS19)
panaspost$na <- with(panaspost, PANAS2 + PANAS4 + PANAS6 + PANAS7 + PANAS8 + PANAS11 + PANAS13 + PANAS15 + PANAS18 + PANAS20)
write.csv(panaspost, file="PANASPOSTscored.csv", row.names=FALSE)

panaspre<- read.csv(file="~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/PANASPREscored.csv")
panaspost <- read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/PANASPOSTscored.csv")
panasprescales <- dplyr::select(panaspre, PTNUM, DyadID, ID, pa, na)
panasprescales <- dplyr::rename(panasprescales, pa_pre = pa, na_pre = na)
panaspostscales <- dplyr::select(panaspost, PTNUM, DyadID, ID, pa, na)
panaspostscales <- dplyr::rename(panaspostscales, pa_post = pa, na_post = na)

panasprepostscales <- inner_join(panasprescales, panaspostscales, by = "ID")
panasprepostscales_patient <- dplyr::filter(panasprepostscales, DyadID.x == 1)
panasprepostscales_partner <- dplyr::filter(panasprepostscales, DyadID.x == 0)
panasprepostscales_partner <- dplyr::rename(panasprepostscales_partner, PTNUM = PTNUM.x)
panasprepostscales_patient <- dplyr::rename(panasprepostscales_patient, PTNUM = PTNUM.x)
panasprespostcales_dt_patient <- inner_join(panasprepostscales_patient, dt_patient, by = "PTNUM")

panasprepostscales_dt_partner <- inner_join(panasprepostscales_partner, dt_partner, by = "PTNUM" )
panasprepostscales_dt <- inner_join(panasprespostcales_dt_patient, panasprepostscales_dt_partner, by = "PTNUM")
#panasprepostscales_sfbmc <- inner_join(panasprepostscales_sfbmc_patient, panasprepostscales_partner, by = "PTNUM")
panasprepostscales_dt <- dplyr::rename(panasprepostscales_dt, pa_pre_patient = pa_pre.x, na_pre_patient = na_pre.x, pa_post_patient = pa_post.x, na_post_patient =  na_post.x, pa_pre_partner = pa_pre.y, na_pre_partner = na_pre.y, pa_post_partner = pa_post.y, na_post_partner =  na_post.y)
panasprepostscales_dt <- dplyr::rename(panasprepostscales_dt, self_coupling_patient = self.coupling.patient, cross_coupling_patient = cross.coupling.patient, self_coupling_partner = self.coupling.partner, cross_coupling_partner = cross.coupling.partner)
ll <- as.data.frame(vanBse_params$logEvidence[4,])
ll$ll <- as.vector(vanBse_params$logEvidence[4,])
ll$PTNUM <- as.vector(vanBse_params$ids)
panasprepostscales_dt <- inner_join(panasprepostscales_dt, ll, by = "PTNUM") #at this point, between already cutting out people below -7999 and the panas data tha tis available, only 116
panasprepostscales_dt<- dplyr::filter(panasprepostscales_dt, ll > -69999) %>% dplyr::filter(abs(self_coupling_patient) < 1 & abs(cross_coupling_patient)  < 1 & abs(self_coupling_partner) < 1 & abs(cross_coupling_partner) < 1 ) #2 people cut with this

ols <- lm(panasprepostscales_dt$pa_post_partner ~ panasprepostscales_dt$self_coupling_partner, data = panasprepostscales_dt)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
par(opar)
d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(panasprepostscales_dt, d1, r)
#a <- a %>% dplyr::filter(PTNUM == 8007 | PTNUM == 8010 | PTNUM == 8017 | PTNUM == 8018 | PTNUM == 8039 | PTNUM == 8050 | PTNUM == 8057 | PTNUM == 8062 | PTNUM == 8063 | PTNUM == 8069 | PTNUM == 8074 | PTNUM == 8086 | PTNUM == 8091 |PTNUM == 8092 | PTNUM == 8094 |PTNUM == 8095 |PTNUM == 8096| PTNUM == 8105 |PTNUM == 8107 | PTNUM == 8109 |PTNUM == 8110 |PTNUM == 8128)
a[d1 > 4/114, ]
a[d1 > 4/114, ] %>% dplyr::select(self_coupling_partner, self_coupling_patient, cross_coupling_partner, cross_coupling_patient, PTNUM, d1) 
#cut people with undue leverage, for this data set that is 8023, 8052, 8063, 8066
panasprepostscales_dt <- dplyr::filter(panasprepostscales_dt, PTNUM != 8023, PTNUM != 8052, PTNUM != 8063, PTNUM != 8066)
panasprepostscales_dt <- mutate(panasprepostscales_dt, scpt = 1000*self_coupling_patient, scpr = 1000*self_coupling_partner, ccpt = 1000*cross_coupling_patient, ccpr = 1000*cross_coupling_partner)
panasprepostscales_dt_vanBse <- inner_join(panasprepostscales_dt, vanBse_paramsm4, by = "PTNUM")
panasprepostscales_dt_vanBse <- dplyr::mutate(panasprepostscales_dt_vanBse, v_scpt = 1000*v_self_coupling_patient, v_ccpt = 1000*v_cross_coupling_patient, v_scpr = 1000*v_self_coupling_parnter, v_ccpr = 1000*v_cross_coupling_partner)

#final N is 110 for analyses
#model 1 examines self coupling as a mediator of pre-post na
panas_sc_na <- "
na_post_partner ~ na_pre_partner + scpr
scpr ~ na_pre_partner
na_post_patient ~ na_pre_patient + scpt
scpt ~ na_pre_patient
scpt~~scpr
"
panas_sc_na_m <- sem(panas_sc_na, panasprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

#model 2 examines self coupling as a mediator of pre-post pa

panas_sc_pa <- "
pa_post_partner ~ pa_pre_partner + scpr
scpr ~ pa_pre_partner
pa_post_patient ~ pa_pre_patient + scpt
scpt ~ pa_pre_patient
"
panas_sc_pa_m <- sem(panas_sc_pa, panasprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)
#model 3 examines cross coupling as a mediator of pre-post na

panas_cc_na <- "
na_post_partner ~ na_pre_partner + ccpr 
ccpr ~na_pre_partner
na_post_patient ~ na_pre_patient + ccpt 
ccpt ~ na_pre_patient
ccpt ~~ ccpr
"
panas_cc_na_m <- sem(panas_cc_na, panasprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)
panas_cc_na_mediation <- "
na_post_partner ~ c*na_pre_partner 
ccpr ~ a*na_pre_partner
na_post_partner ~b*ccpr
ab := a*b
total1 := c + (a*b)
na_post_patient ~ f*na_pre_patient 
ccpt ~ d*na_pre_patient
na_post_patient ~ e*ccpt
de := d*e
total2 := f + (d*e)
ccpt ~~ ccpr
"
panas_cc_na_mediation_m <- sem(panas_cc_na_mediation, panasprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

panas_cc_na_moderation <- "
na_post_partner ~ na_pre_partner + ccpr + na_pre_partner*ccpr
na_post_patient ~ na_pre_patient + ccpt + na_pre_patient*ccpt
"
panas_cc_na_moderation_m <- sem(panas_cc_na_moderation, panasprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

#model 4 examines how cc mediates prepost pa
panas_cc_pa <- "
pa_post_partner ~ pa_pre_partner + ccpr
ccpr ~ pa_pre_partner
pa_post_patient ~ pa_pre_patient + ccpt
ccpt ~ pa_pre_patient
"
panas_cc_pa_m <- sem(panas_cc_pa, panasprepostscales_dt, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

#model 6 examines how cc and sc mediate prepost pa

panas_sccc_na <- "
na_post_partner ~ na_pre_partner + scpr + ccpr
scpr ~ na_pre_partner
ccpr ~ na_pre_partner
na_post_patient ~ na_pre_patient + scpt + ccpt
scpt ~ na_pre_patient
ccpt ~ na_pre_patient
scpt ~~ccpt
scpt ~~ ccpt
"
panas_sccc_na_m <- sem(panas_sccc_na, panasprepostscales_dt, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE)
#note pa shows nothing
#note: na cc seems to be most significant and performany muuuuuuch better according to AIC
#still need to test partner effects
#also ask michael if these AIC diffs are way to wacky to take seriously

#model 7 examines patient partner effects for na, cc

panas_cc_prpt_na <- "
na_post_partner ~ na_pre_partner + ccpr + ccpt
ccpr ~ na_pre_partner
ccpt~na_pre_partner
na_post_patient ~ na_pre_patient + ccpt + ccpr
ccpt ~ na_pre_patient
ccpr~na_pre_patient
ccpt~~ccpr
"
panas_cc_prpt_na_m <- sem(panas_cc_prpt_na, panasprepostscales_dt, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE)
#model 7 examines patient partner effects for na, sc

panas_sc_prpt_na <- "
na_post_partner ~ na_pre_partner + scpr + scpt
scpr ~ na_pre_partner
scpt~na_pre_partner
na_post_patient ~ na_pre_patient + scpt + scpr
scpt ~ na_pre_patient
scpr~na_pre_patient
scpt~~scpr
"
panas_sc_prpt_na_m <- sem(panas_sc_prpt_na, panasprepostscales_dt, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE)

#model 8 examines patient partner effects for na, sc and cc

panas_sccc_prpt_na <- "
na_post_partner ~ na_pre_partner + scpr + scpt + ccpr + ccpt
scpr ~ na_pre_partner
scpt~na_pre_partner
ccpt~na_pre_partner
ccpr~na_pre_partner
na_post_patient ~ na_pre_patient + scpt + scpr + ccpr + ccpt
scpt ~ na_pre_patient
scpr~na_pre_patient
ccpr~na_pre_patient
ccpt~na_pre_patient
scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~ccpr
"
panas_sccc_prpt_na_m <- sem(panas_sccc_prpt_na, panasprepostscales_dt, missing = "listwise", estimator = "ML", 
                            mimic="Mplus", meanstructure = TRUE)


panas_sccc_prpt_na_vanBse <- "
#direct effect
na_post_partner ~ c1*na_pre_partner
#mediators
na_post_partner ~ scpr + b2*scpt + ccpr + ccpt
scpr ~ na_pre_partner
scpt~a2*na_pre_partner
ccpt~na_pre_partner
ccpr~na_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt 
scpr ~ v_scpr
ccpr ~ v_ccpr
#indirect effect 1
a2b2 := a2*b2

na_post_patient ~ na_pre_patient 
na_post_patient ~ scpt + scpr + ccpr + ccpt
scpt ~ na_pre_patient
scpr~na_pre_patient
ccpr~na_pre_patient
ccpt~na_pre_patient

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~ccpr
"
panas_sccc_prpt_na_vanBse_m <- sem(panas_sccc_prpt_na_vanBse, panasprepostscales_dt_vanBse, missing = "listwise", estimator = "ML", 
                            mimic="Mplus", meanstructure = TRUE)







anova(panas_sccc_prpt_na_m, panas_sc_prpt_na_m, panas_sccc_na_m, panas_sc_na_m, panas_cc_na_m, panas_cc_prpt_na_m)
anova(panas_cc_na_moderation_m, panas_cc_na_m, panas_cc_na_mediation_m)
#some of the mood stuff goes away....
#Ask Michael about what could be driving the huge differences in AIC... Is something going unmodeled? (no differences in missiningess)
missingness <- lapply(c(panasprepostscales_dt$na_pre_patient, panasprepostscales_dt$na_pre_partner, panasprepostscales_dt$na_post_patient, panasprepostscales_dt$na_post_partner, panasprepostscales_dt$scpt, panasprepostscales_dt$scpr, panasprepostscales_dt$ccpt, panasprepostscales_dt$ccpr), length)









