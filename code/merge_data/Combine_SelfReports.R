#at the moment, self-reports and clinical data are spread across different datasets that are incremental, not complete.
library(foreign)
library(plyr)
library(reshape2)
#setwd("/Users/michael/Tresors/PD_SNA/Couples_Baseline_SNA")
source(file.path(getMainDir(), "Miscellaneous", "Global_Functions.R"))

#merge together self-reports etc.
biq <- read.spss("BIQ.sav", to.data.frame=TRUE)
biq <- subset(biq, select=c(UsrID, PTNUM, DyadID, age, sex, race_n))
# biq_append <- read.spss("data/selfreports/BIQ_24Sep2015.sav", to.data.frame=TRUE) #NB: this is just an updated copy, not a full file
# biq_append <- plyr::rename(biq_append, c(gender="sex"))
# biq_append <- subset(biq_append, select=c(UsrID, PTNUM, DyadID, age, sex, race_n))
# biq_all <- rbind(biq, biq_append)
# biq_all <- biq_all[!duplicated(biq_all$UsrID, fromLast = FALSE), ] #drop duplicated UsrIDs, preferring earlier row (i.e., older data)

cts <- read.spss("~/Desktop/INTAKE_20170223/CTS.sav", to.data.frame=TRUE)
# cts_append <- read.spss("data/selfreports/CTS_24Sep2015.sav", to.data.frame=TRUE) #NB: this is also just updates to prior file
# cts_append <- plyr::rename(cts_append, c(dyad="DyadID"))
# cts_append2 <- read.spss("data/selfreports/CTS_incremental_8Oct2015.sav", to.data.frame=TRUE)
# cts_append2 <- plyr::rename(cts_append2, c(dyadID="DyadID"))
# cts_append2 <- subset(cts_append2, mth==0) #only baseline
# cts_append2 <- cts_append2[,names(cts_append)] #only keep scores
# cts_append3 <- read.spss("data/selfreports/CTS_incremental_9Oct2015.sav", to.data.frame=TRUE)
# cts_append3 <- plyr::rename(cts_append3, c(dyadID="DyadID"))
# cts_append3 <- subset(cts_append3, mth==0) #only baseline
# cts_append3 <- cts_append3[,names(cts_append)] #only keep scores
# cts_all <- rbind(cts, cts_append, cts_append2, cts_append3) 
# cts_all <- cts_all[!duplicated(cts_all$UsrID, fromLast = FALSE), ] #drop duplicated UsrIDs, preferring earlier row (i.e., older data)

das <- read.spss("~/Desktop/INTAKE_20170223/DAS.sav", to.data.frame=TRUE)
# das_append <- read.spss("data/selfreports/DAS_24Sep2015.sav", to.data.frame=TRUE)
# das_append <- plyr::rename(das_append, c(Dyad="DyadID"))
# das_append$DASrelationship <- factor(das_append$DASrelationship, levels=c(0,1), labels=c("no", "yes"))
# das_append2 <- read.spss("data/selfreports/DAS_incremental_8Oct2015.sav", to.data.frame=TRUE) #LOOKS LIKE THERE ARE TWO ENTRIES FOR ONE PERSON. MTH ISSUE
# das_append2 <- subset(das_append2, mth==0)
# das_append2$DASrelationship <- factor(das_append2$DASrelationship, levels=c(0,1), labels=c("no", "yes"))
# das_append2 <- das_append2[,names(das_append)] #only keep scores
# das_append3 <- read.spss("data/selfreports/DAS_incremental_9Oct2015.sav", to.data.frame=TRUE) #LOOKS LIKE THERE ARE TWO ENTRIES FOR ONE PERSON. MTH ISSUE
# das_append3 <- subset(das_append3, mth==0)
# das_append3$DASrelationship <- factor(das_append3$DASrelationship, levels=c(0,1), labels=c("no", "yes"))
# das_append3 <- das_append3[,names(das_append)] #only keep scores
# das_all <- rbind(das, das_append, das_append2, das_append3)
# das_all <- das_all[!duplicated(das_all$UsrID, fromLast = FALSE), ] #drop duplicated UsrIDs, preferring earlier row (i.e., older data)

ecr <- read.spss("~/Desktop/INTAKE_20170223/ECR.sav", to.data.frame=TRUE)
# ecr_append <- read.spss("data/selfreports/ECR_24Sep2015.sav", to.data.frame=TRUE)
# ecr_append <- plyr::rename(ecr_append, c(Dyad="DyadID"))
# ecr_append2 <- read.spss("data/selfreports/ECR_incremental_8Oct2015.sav", to.data.frame=TRUE)
# ecr_append2 <- subset(ecr_append2, mth==0)
# ecr_append2 <- ecr_append2[,names(ecr_append)] #only keep scores
# ecr_append3 <- read.spss("data/selfreports/ECR_incremental_9Oct2015.sav", to.data.frame=TRUE)
# ecr_append3 <- subset(ecr_append3, mth==0)
# ecr_append3 <- ecr_append3[,names(ecr_append)] #only keep scores
# ecr_all <- rbind(ecr, ecr_append, ecr_append2, ecr_append3)
# ecr_all <- ecr_all[!duplicated(ecr_all$UsrID, fromLast = FALSE), ] #drop duplicated UsrIDs, preferring earlier row (i.e., older data)

pai <- read.spss("~/Desktop/INTAKE_20170223/PAI.sav", to.data.frame=TRUE)
# pai_append <- read.spss("data/selfreports/PAIBOR_24Sep2015.sav", to.data.frame=TRUE)
# pai_append <- plyr::rename(pai_append, c(Dyad="DyadID"))
# pai_append2 <- read.spss("data/selfreports/PAIBOR_incremental_8Oct2015.sav", to.data.frame=TRUE)
# pai_append2 <- subset(pai_append2, mth==0)
# pai_append2 <- pai_append2[,names(pai_append)]
# pai_append3 <- read.spss("data/selfreports/PAIBOR_incremental_9Oct2015.sav", to.data.frame=TRUE)
# pai_append3 <- subset(pai_append3, mth==0)
# pai_append3 <- pai_append3[,names(pai_append)]
# pai_all <- rbind(pai, pai_append, pai_append2, pai_append3)
# pai_all <- pai_all[!duplicated(pai_all$UsrID, fromLast = FALSE), ] #drop duplicated UsrIDs, preferring earlier row (i.e., older data)

##iip <- read.spss("data/IIP intake scores.sav", to.data.frame=TRUE)
iip <- read.spss("~/Desktop/INTAKE_20170223/IIP90 intake.sav", to.data.frame=TRUE)
iip <- subset(iip, select=c(UsrID, PTNUM, DyadID, IIP_PD1, IIP_PD2, IIP_PD3, IIP_C1, IIP_C2, iip_pa,
        iip_bc, iip_de, iip_fg, iip_hi, iip_jk, iip_lm, iip_no, iip_bpd, PD_on_IIP))
# iip_append <- read.spss("data/selfreports/IIP_24Sep2015.sav", to.data.frame=TRUE)
# iip_append <- plyr::rename(iip_append, c(Dyad="DyadID"))
# iip_append$IIPSumValue <- NULL
# iip_append2 <- read.spss("data/selfreports/IIP90_incremental_8Oct2015.sav", to.data.frame=TRUE)
# iip_append2 <- subset(iip_append2, mth==0)
# iip_append2 <- iip_append2[,names(iip_append)]
# iip_all <- rbind(iip, iip_append, iip_append2)
# iip_all <- iip_all[!duplicated(iip_all$UsrID, fromLast = FALSE), ] #drop duplicated UsrIDs, preferring earlier row (i.e., older data)
iip$iip_agency <- with(iip_all, .25*(iip_pa - iip_hi + .707*(iip_bc + iip_no - iip_fg - iip_jk)))
iip$iip_communion <- with(iip_all, .25*(iip_lm - iip_de + .707*(iip_no + iip_jk - iip_bc - iip_fg)))
iip$iip_elevation <- with(iip_all, (iip_pa + iip_bc + iip_de + iip_fg + iip_hi + iip_jk + iip_lm + iip_no)/8)

#sidp <- read.spss("data/selfreports/SIDP4 intake consensus scored 4.2.15.sav", to.data.frame=TRUE)
#sidp_append <- read.spss("data/selfreports/SIDP_24Sep2015.sav", to.data.frame=TRUE)
#sidp_append$NumMiss <- NULL
#sidp_all <- rbind(sidp, sidp_append)
#sidp_all <- sidp_all[!duplicated(sidp_all$UsrID, fromLast = FALSE), ] #drop duplicated UsrIDs, preferring earlier row (i.e., older data)
#new dataset has all individuals at baseline
sidp <- read.spss("~/Desktop/INTAKE_20170223/SIDP.sav", to.data.frame=TRUE)
sidp <- subset(sidp, mth==0 & raterID==0 & ratercode==6) #raterID 0 is case conference; ratercode 6 is case conference; mth 0 is intake
sidp_all <- sidp[, c("UsrID", "PTNUM", "DyadID", grep(".*_sidp$", names(sidp), value=TRUE), grep(".*Count$", names(sidp), value=TRUE))]

#counts are present/absent ratings, whereas dimensional scores account for 0, 1, 2 coding
#use dimensional (_sidp) for now.

#has 8888 as missing code
lapply(das_all, function(x) { if (is.numeric(x)) { range(x, na.rm=TRUE) } } )

recodeMissing <- function(vec, targetval=99) {
  vec[vec==targetval] <- NA
  return(vec)
}

das_all <- colwise(recodeMissing, targetval=8888)(das_all)

couples_baseline_clin = Reduce(function(...) { merge(..., by=c("UsrID", "PTNUM", "DyadID"), all=TRUE) },
    list(biq_all, cts_all, das_all, ecr_all, pai_all, iip_all, sidp_all))

#rename age and sex variables to be clear that these pertain to participant, not alter
couples_baseline_clin <- plyr::rename(couples_baseline_clin, c(age="p_age", sex="p_sex"))

#recode 9999 as missing

#outlier check
lapply(couples_baseline_clin, function(x) { if (is.numeric(x)) { range(x, na.rm=TRUE) } } )

couples_baseline_clin <- subset(couples_baseline_clin, PTNUM < 9000)

#list of rule-out and break-up IDs from Emily Landis
ro_ids <- c(8002, 8012, 8021, 8025, 8037, 8054, 8068, 8076, 8077, 8079, 8089, 8090, 8040, 8047, 8048, 8097, 8119, 8125)
message("Removing rule-out/incomplete IDs: ", paste(ro_ids, collapse=", "))
couples_baseline_clin <- subset(couples_baseline_clin, !PTNUM %in% ro_ids)

#PD_on_IIP is coded 0/1 for some data and no/yes for others
couples_baseline_clin$PD_on_IIP <- sapply(couples_baseline_clin$PD_on_IIP, function(x) { 
      if (x %in% c("1", "yes")) { 1 } else { 0 }
    })

couples_baseline_clin$p_female <- as.numeric(couples_baseline_clin$p_sex == "female") #define as 0/1
couples_baseline_clin$DASrelationship <- as.numeric(couples_baseline_clin$DASrelationship == "yes") #recode as 0/1

#total of the DSM-IV PDs (excluding depressive and negativistic)
couples_baseline_clin$pdtot <- with(couples_baseline_clin, szoid_sidp + stypl_sidp + parnd_sidp + histr_sidp + bordl_sidp +
        narci_sidp + antso_sidp + obcmp_sidp + depen_sidp + avoid_sidp)

#total PD features excluding BPD (note that this differs from OPD because OPD includes DSM-III PDs)
couples_baseline_clin$nobpd <- with(couples_baseline_clin, szoid_sidp + stypl_sidp + parnd_sidp + histr_sidp +
        narci_sidp + antso_sidp + obcmp_sidp + depen_sidp + avoid_sidp)

couples_baseline_clin$nobpdCount <- with(couples_baseline_clin, szoidCount + styplCount + parndCount + histrCount +
        narciCount + antsoCount + obcmpCount + depenCount + avoidCount)

couples_baseline_clin$nonarc <- with(couples_baseline_clin, szoid_sidp + stypl_sidp + parnd_sidp + histr_sidp + bordl_sidp +
        antso_sidp + obcmp_sidp + depen_sidp + avoid_sidp) #non-narc Sx

couples_baseline_clin$nonarcCount <- with(couples_baseline_clin, szoidCount + styplCount + parndCount + histrCount + bordlCount +
        antsoCount + obcmpCount + depenCount + avoidCount) #non-narc Sx

couples_baseline_clin$allpdCount <- with(couples_baseline_clin, szoidCount + styplCount + parndCount + 
        bordlCount + narciCount + antsoCount + histrCount + avoidCount + depenCount + obcmpCount)

#save a wide version of the clinical data with _0 and _1 variable name suffix denoting the partner and patient, respectively
couples_melt <- melt(couples_baseline_clin[,sapply(couples_baseline_clin, is.numeric)], id.vars=c("PTNUM", "DyadID"))
couples_clin_wide <- dcast(couples_melt, PTNUM ~ variable + DyadID)

save(couples_baseline_clin, couples_clin_wide, file="data/selfreports/couples_baseline_clinical_9Oct2015.RData")
write.csv(x=couples_baseline_clin, file="data/selfreports/couples_baseline_clinical_9Oct2015.csv", row.names=FALSE)

#sink("private/clinical_missing_report_7Oct2015.txt")
missingDataReport(couples_baseline_clin, idVars="UsrID")
#sink()
