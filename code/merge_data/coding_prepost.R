#prepost scoring
#basedir <- "/Users/agc5141/Tresors/DEPENd/Projects/PersonalityRest"
#basedir <- "/Users/michael/Tresors/DEPENd/Projects/PersonalityRest"
#basedir <- "/Users/michael/Box_Sync/DEPENd/Projects/SPECC/SelfReports"
basedir <- "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/"

setwd(file.path(basedir))

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
saampre <- read.csv("SAAMPREscored.csv")


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
saampost <- read.csv("SAAMPOSTscored.csv")

saamprescales <- dplyr::select(saampre, PTNUM, DyadID, ID, anx, avo, sec)
saamprescales <- dplyr::rename(saampre, anx_pre = anx, avo_pre = avo, sec_pre = sec)
saampostscales <- dplyr::select(saampost, PTNUM, DyadID, ID, anx,avo, sec)
saampostscales <- dplyr::rename(saampost, anx_post = anx, avo_post = avo, sec_post = sec)

saamprepostscales <- inner_join(saamprescales, saampostscales, by = "ID")
saamprepostscales_patient <- dplyr::filter(saamprepostscales, DyadID.x == 1)
saamprepostscales_partner <- dplyr::filter(saamprepostscales, DyadID.x == 0)
dtm4_patient <- dplyr::select(dtm4, self_coupling_patient, cross_coupling_patient, PTNUM)
dtm4_partner <- dplyr::select(dtm4, self_coupling_partner, cross_coupling_partner, PTNUM)
dtm4_partner$PTNUM <- as.vector(dtm4_partner$PTNUM)
dtm4_patient$PTNUM <- as.vector(dtm4_patient$PTNUM)
saamprepostscales_partner <- dplyr::rename(saamprepostscales_partner, PTNUM = PTNUM.x)
saamprepostscales_patient <- dplyr::rename(saamprepostscales_patient, PTNUM = PTNUM.x)

saamprespostcales_sfbmc_patient <- inner_join(saamprepostscales_patient, dtm4_patient, by = "PTNUM")

saamprepostscales_sfbmc_partner <- inner_join(saamprepostscales_partner, dtm4_partner, by = "PTNUM" )
saamprepostscales_sfbmc <- inner_join(saamprespostcales_sfbmc_patient, saamprepostscales_sfbmc_partner, by = "PTNUM")
#panasprepostscales_sfbmc <- inner_join(panasprepostscales_sfbmc_patient, panasprepostscales_partner, by = "PTNUM")
saamprepostscales_sfbmc <- dplyr::rename(saamprepostscales_sfbmc, anx_pre_patient = anx_pre.x, anx_post_patient = anx_post.x, avo_pre_patient = avo_pre.x, avo_post_patient = avo_post.x, sec_pre_patient = sec_pre.x, sec_post_patient = sec_post.x, anx_pre_partner =  anx_pre.y, anx_post_partner = anx_post.y, avo_pre_partner = avo_pre.y, avo_post_partner = avo_post.y, sec_pre_partner = sec_pre.y, sec_post_partner = sec_post.y)
saamprepostscales_sfbmc <- inner_join(saamprepostscales_sfbmc, logEvidenceModel4, by = "PTNUM")
saamprepostscales_sfbmc <- dplyr::rename(saamprepostscales_sfbmc, ll = V4)
saamprepostscales_sfbmc <- dplyr::filter(saamprepostscales_sfbmc, ll > -69999) %>% dplyr::filter(abs(self_coupling_patient) < 1 & abs(cross_coupling_patient)  < 1 & abs(self_coupling_partner) < 1 & abs(cross_coupling_partner) < 1 )


ols <- lm(saamprepostscales_sfbmc$anx_post_partner ~ saamprepostscales_sfbmc$self_coupling_partner, data = saamprepostscales_sfbmc)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
par(opar)
d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(saamprepostscales_sfbmc, d1, r)
#a <- a %>% dplyr::filter(PTNUM == 8007 | PTNUM == 8010 | PTNUM == 8017 | PTNUM == 8018 | PTNUM == 8039 | PTNUM == 8050 | PTNUM == 8057 | PTNUM == 8062 | PTNUM == 8063 | PTNUM == 8069 | PTNUM == 8074 | PTNUM == 8086 | PTNUM == 8091 |PTNUM == 8092 | PTNUM == 8094 |PTNUM == 8095 |PTNUM == 8096| PTNUM == 8105 |PTNUM == 8107 | PTNUM == 8109 |PTNUM == 8110 |PTNUM == 8128)
a[d1 > 4/112, ]
a[d1 > 4/112, ] %>% dplyr::select(self_coupling_partner, self_coupling_patient, cross_coupling_partner, cross_coupling_patient, PTNUM, d1) 
#cut people with undue leverage 
saamprepostscales_sfbmc <- dplyr::filter(saamprepostscales_sfbmc, PTNUM != 8063, PTNUM != 8066, PTNUM != 8069, PTNUM != 8070)
saamprepostscales_sfbmc <- mutate(saamprepostscales_sfbmc, scpt = 1000*self_coupling_patient, scpr = 1000*self_coupling_partner, ccpt = 1000*cross_coupling_patient, ccpr = 1000*cross_coupling_partner)
##final N: 108

#self coupling mediating prepost attachment anxiety
saam_sc_avo <- "
avo_post_partner ~ avo_pre_partner + scpr
scpr ~ avo_pre_partner
avo_post_patient ~ avo_pre_patient + scpt
scpt ~ avo_pre_patient
scpt~~scpr
"
saam_sc_avo_m <- sem(saam_sc_avo, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

#cross coupling mediating prepost attachment avoidance

saam_cc_avo <- "
avo_post_partner ~ avo_pre_partner + ccpr
ccpr ~ avo_pre_partner
avo_post_patient ~ avo_pre_patient + ccpt
ccpt ~ avo_pre_patient
ccpr~~ccpt
"
saam_cc_avo_m <- sem(saam_cc_avo, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
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
saam_cc_prpt_avo_m <- sem(saam_cc_prpt_avo, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
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
saam_sc_prpt_avo_m <- sem(saam_sc_prpt_avo, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE)



saam_sc_avo <- "
avo_post_partner ~ avo_pre_partner + scpr
scpr ~ avo_pre_partner
avo_post_patient ~ avo_pre_patient + scpt
scpt ~ avo_pre_patient
"
saam_sc_avo_m <- sem(saam_sc_avo, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
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
saam_sccc_avo_m <- sem(saam_sccc_avo, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
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
saam_sccc_prpt_avo_m <- sem(saam_sccc_prpt_avo, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                            mimic="Mplus", meanstructure = TRUE)




#sc mediating attachment anxiety
saam_sc_anx <- "
anx_post_partner ~ anx_pre_partner + scpr
scpr ~ anx_pre_partner
anx_post_patient ~ anx_pre_patient + scpt
scpt ~ anx_pre_patient
scpt~~scpr
"
saam_sc_anx_m <- sem(saam_sc_anx, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

#cross coupling mediating prepost attachment anxiety

saam_cc_anx <- "
anx_post_partner ~ anx_pre_partner + ccpr
ccpr ~ anx_pre_partner
anx_post_patient ~ anx_pre_patient + ccpt
ccpt ~ anx_pre_patient
ccpr~~ccpt
"
saam_cc_anx_m <- sem(saam_cc_anx, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
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
saam_cc_prpt_anx_m <- sem(saam_cc_prpt_anx, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
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
saam_sc_prpt_anx_m <- sem(saam_sc_prpt_anx, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
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
saam_sccc_anx_m <- sem(saam_sccc_anx, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
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
saam_sccc_prpt_anx_m <- sem(saam_sccc_prpt_anx, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE)



#sc mediating security
saam_sc_sec <- "
sec_post_partner ~ sec_pre_partner + scpr
scpr ~ sec_pre_partner
sec_post_patient ~ sec_pre_patient + scpt
scpt ~ sec_pre_patient
"
saam_sc_sec_m <- sem(saam_sc_sec, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

#cross coupling mediating prepost attachment anxiety

saam_cc_sec <- "
sec_post_partner ~ sec_pre_partner + ccpr
ccpr ~ sec_pre_partner
anx_post_patient ~ sec_pre_patient + ccpt
ccpt ~ sec_pre_patient
"
saam_cc_sec_m <- sem(saam_cc_sec, saamprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE, conditional)



#IMICPRE

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
  subjrow <- df$ID == ID
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
panaspost <- read.csv("PANASPOSTscored.csv")
panasprescales <- dplyr::select(panaspre, PTNUM, DyadID, ID, pa, na)
panasprescales <- dplyr::rename(panasprescales, pa_pre = pa, na_pre = na)
panaspostscales <- dplyr::select(panaspost, PTNUM, DyadID, ID, pa, na)
panaspostscales <- dplyr::rename(panaspostscales, pa_post = pa, na_post = na)

panasprepostscales <- inner_join(panasprescales, panaspostscales, by = "ID")
panasprepostscales_patient <- dplyr::filter(panasprepostscales, DyadID.x == 1)
panasprepostscales_partner <- dplyr::filter(panasprepostscales, DyadID.x == 0)
dtm4_patient <- dplyr::select(dtm4, self_coupling_patient, cross_coupling_patient, PTNUM)
dtm4_partner <- dplyr::select(dtm4, self_coupling_partner, cross_coupling_partner, PTNUM)
dtm4_partner$PTNUM <- as.vector(dtm4_partner$PTNUM)
dtm4_patient$PTNUM <- as.vector(dtm4_patient$PTNUM)
panasprepostscales_partner <- dplyr::rename(panasprepostscales_partner, PTNUM = PTNUM.x)
panasprepostscales_patient <- dplyr::rename(panasprepostscales_patient, PTNUM = PTNUM.x)

panasprespostcales_sfbmc_patient <- inner_join(panasprepostscales_patient, dtm4_patient, by = "PTNUM")

panasprepostscales_sfbmc_partner <- inner_join(panasprepostscales_partner, dtm4_partner, by = "PTNUM" )
panasprepostscales_sfbmc <- inner_join(panasprespostcales_sfbmc_patient, panasprepostscales_sfbmc_partner, by = "PTNUM")
#panasprepostscales_sfbmc <- inner_join(panasprepostscales_sfbmc_patient, panasprepostscales_partner, by = "PTNUM")
panasprepostscales_sfbmc <- dplyr::rename(panasprepostscales_sfbmc, pa_pre_patient = pa_pre.x, na_pre_patient = na_pre.x, pa_post_patient = pa_post.x, na_post_patient =  na_post.x, pa_pre_partner = pa_pre.y, na_pre_partner = na_pre.y, pa_post_partner = pa_post.y, na_post_partner =  na_post.y)
panasprepostscales_sfbmc <- inner_join(panasprepostscales_sfbmc, logEvidenceModel4, by = "PTNUM")
panasprepostscales_sfbmc <- dplyr::rename(panasprepostscales_sfbmc, ll = V4)
panasprepostscales_sfbmc <- dplyr::filter(panasprepostscales_sfbmc, ll > -69999) %>% dplyr::filter(abs(self_coupling_patient) < 1 & abs(cross_coupling_patient)  < 1 & abs(self_coupling_partner) < 1 & abs(cross_coupling_partner) < 1 )


ols <- lm(panasprepostscales_sfbmc$pa_post_partner ~ panasprepostscales_sfbmc$self_coupling_partner, data = panasprepostscales_sfbmc)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
par(opar)
d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(panasprepostscales_sfbmc, d1, r)
#a <- a %>% dplyr::filter(PTNUM == 8007 | PTNUM == 8010 | PTNUM == 8017 | PTNUM == 8018 | PTNUM == 8039 | PTNUM == 8050 | PTNUM == 8057 | PTNUM == 8062 | PTNUM == 8063 | PTNUM == 8069 | PTNUM == 8074 | PTNUM == 8086 | PTNUM == 8091 |PTNUM == 8092 | PTNUM == 8094 |PTNUM == 8095 |PTNUM == 8096| PTNUM == 8105 |PTNUM == 8107 | PTNUM == 8109 |PTNUM == 8110 |PTNUM == 8128)
a[d1 > 4/112, ]
a[d1 > 4/112, ] %>% dplyr::select(self_coupling_partner, self_coupling_patient, cross_coupling_partner, cross_coupling_patient, PTNUM, d1) 
#cut people with undue leverage 
panasprepostscales_sfbmc <- dplyr::filter(panasprepostscales_sfbmc, PTNUM != 8023, PTNUM != 8052, PTNUM != 8063, PTNUM != 8066)
panasprepostscales_sfbmc <- mutate(panasprepostscales_sfbmc, scpt = 1000*self_coupling_patient, scpr = 1000*self_coupling_partner, ccpt = 1000*cross_coupling_patient, ccpr = 1000*cross_coupling_partner)

#model 1 examines self coupling as a mediator of pre-post na
panas_sc_na <- "
na_post_partner ~ na_pre_partner + scpr
scpr ~ na_pre_partner
na_post_patient ~ na_pre_patient + scpt
scpt ~ na_pre_patient
scpt~~scpr
"
panas_sc_na_m <- sem(panas_sc_na, panasprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                                     mimic="Mplus", meanstructure = TRUE)

#model 2 examines self coupling as a mediator of pre-post pa

panas_sc_pa <- "
pa_post_partner ~ pa_pre_partner + scpr
scpr ~ pa_pre_partner
pa_post_patient ~ pa_pre_patient + scpt
scpt ~ pa_pre_patient
"
panas_sc_pa_m <- sem(panas_sc_pa, panasprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)
#model 3 examines cross coupling as a mediator of pre-post na

panas_cc_na <- "
na_post_partner ~ na_pre_partner + ccpr
ccpr ~ na_pre_partner
na_post_patient ~ na_pre_patient + ccpt
ccpt ~ na_pre_patient
ccpt ~~ ccpr
"
panas_cc_na_m <- sem(panas_cc_na, panasprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

#model 4 examines how cc mediates prepost pa
panas_cc_pa <- "
pa_post_partner ~ pa_pre_partner + ccpr
ccpr ~ pa_pre_partner
pa_post_patient ~ pa_pre_patient + ccpt
ccpt ~ pa_pre_patient
"
panas_cc_pa_m <- sem(panas_cc_pa, panasprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                     mimic="Mplus", meanstructure = TRUE)

 #model 6 examines how cc and sc mediate prepost pa

panas_sccc_na <- "
pa_post_partner ~ na_pre_partner + scpr + ccpr
scpr ~ na_pre_partner
ccpr ~ na_pre_partner
na_post_patient ~ na_pre_patient + scpt + ccpt
scpt ~ na_pre_patient
ccpt ~ na_pre_patient
scpt ~~ccpt
scpt ~~ ccpt
"
panas_sccc_na_m <- sem(panas_sccc_na, panasprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                       mimic="Mplus", meanstructure = TRUE)
#note pa shows nothing
#note: na cc seems to be most significant and performany muuuuuuch better according to AIC
#still need to test partner effects
#also ask michael if these AIC diffs are way to wacky to take seriously

#model 7 examines patient partner effects for na, cc

panas_cc_prpt_na <- "
na_post_partner ~ c*na_pre_partner + ccpr + ccpt
ccpr ~ a1*na_pre_partner
ccpt~a2*na_pre_partner
ab
ab := a*b
na_post_patient ~ na_pre_patient + c*ccpt + d*ccpr
ccpt ~ na_pre_patient
ccpr~na_pre_patient
ccpt~~ccpr
cd := c*d
"
panas_cc_prpt_na_m <- sem(panas_cc_prpt_na, panasprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
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
panas_sc_prpt_na_m <- sem(panas_sc_prpt_na, panasprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
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
panas_sccc_prpt_na_m <- sem(panas_sccc_prpt_na, panasprepostscales_sfbmc, missing = "listwise", estimator = "ML", 
                          mimic="Mplus", meanstructure = TRUE)


#model 9 examines patient partner effects for na, sc and cc