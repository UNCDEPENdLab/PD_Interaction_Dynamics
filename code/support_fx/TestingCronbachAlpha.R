#at the moment, self-reports and clinical data are spread across different datasets that are incremental, not complete.
library(dependlab)
library(tidyverse)
library(haven)
setwd("~/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/CombineSelfReports/INTAKE_20170223/")
sumsheet <- haven::read_spss("~/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/CombineSelfReports/COUPLES_SCHREIBER_REQUEST_20180326.sav")
das <- read_spss('DAS.sav')%>% as.data.frame()
ecr <- read_spss('ECR.sav')%>% as.data.frame()
pai <- read_spss('PAIBOR.sav')%>% as.data.frame()
iip <- read_spss('IIP90.sav') %>% as.data.frame() %>% dplyr::filter(mth == 0)
sidp <- read_spss("SIDP.sav") %>% as.data.frame() 
meanreplace <- function(df, ID, varprefix="IIP", subscaleitems) {
  subjrow <- df$usrid == ID
  dfitems <- subset(df, select=paste0(varprefix, subscaleitems))
  #dfitems[dfitems == 99] <- NA
  subjitems <- dfitems[subjrow,]
  whichmiss <- names(subjitems)[which(is.na(subjitems))]
  obsmean <- mean(unlist(subjitems), na.rm=TRUE)
  df[subjrow, whichmiss] <- obsmean
  df
}


#### IIP

iip <- meanreplace(iip, "80900", subscaleitems=c(21, 40, 57, 65, 68, 76, 80)) 
iip <- meanreplace(iip, "81341", subscaleitems=c(11, 18, 20, 24, 27, 31, 46, 82))
iip <- meanreplace(iip, "81341", subscaleitems = c(5, 6, 8, 9, 12, 15, 23, 49))
iip <- meanreplace(iip, "80430", subscaleitems = c(25, 37, 47, 59, 64, 67, 70, 87))
iip <- meanreplace(iip, "81011", subscaleitems = c(1, 35, 36, 42, 51, 55, 60, 78, 79, 81, 86))
iip$PA <- with(iip, (IIP21 + IIP40 + IIP57 + IIP58 + IIP65 + IIP68 + IIP76 + IIP80))
iip$BC=with(iip, (IIP1 + IIP26 + IIP28 + IIP38 + IIP41 + IIP50 + IIP73 + IIP88))
#81341 has missing value at 11 (DE) and 74 which does not need to be imputed (not needed for octant scales)
iip$DE=with(iip, (IIP11 + IIP18 +IIP20 + IIP24 + IIP27 + IIP31 + IIP46 + IIP82))
iip$FG=with(iip, (IIP3 + IIP7 + IIP17 + IIP22 + IIP43 + IIP45 + IIP71 + IIP85))
#81341 has missing value at 5 which needs to be imputed
iip$HI=with(iip, (IIP5 + IIP6 + IIP8 + IIP9 + IIP12 + IIP15 + IIP23 + IIP49))
iip$JK=with(iip, (IIP2 +IIP10 + IIP29 + IIP44 + IIP48 + IIP54 +IIP69 + IIP83)) 
#80430, impute q 72, q64
iip$LM=with(iip, (IIP25 +  IIP37+ IIP47 +  IIP59 + IIP64 +  IIP67 + IIP70 + IIP87))
iip$NO=with(iip, (IIP4 + IIP30 + IIP39 + IIP52 + IIP56 + IIP61 + IIP62 + IIP78))
#81011, impute 42 
iip$IIP_PD1=with(iip, (IIP1 +  IIP35 + IIP36 + IIP42 + IIP51 + IIP55 +IIP60 +IIP78 +IIP79 +IIP81 + IIP86)/11)
iip$IIP_PD2=with(iip, (IIP13 + IIP14 +IIP26 + IIP28 + IIP32 +IIP34 +IIP38 +IIP40 +IIP41+IIP84)/10)
iip$IIP_PD3=with(iip, (IIP50 + IIP53 + IIP58 +IIP63 +IIP77 +IIP80 +IIP88)/7)
iip$iip_bpd=with(iip, (IIP51 +IIP53 +IIP55 +IIP66 +IIP77 +IIP80+IIP89+IIP90)/8)
#8090 and 8134 need to impute
iip$iip_agency <- with(iip, .25*(PA - HI + .707*(BC + NO - FG - JK)))
#8043 and 8143 need to impute
iip$iip_communion <- with(iip, .25*(LM - DE + .707*(NO + JK - BC - FG)))
#8043, 8090, and 8143 need to impute
iip$iip_elevation <- with(iip, (PA + BC + DE + FG + HI + JK + LM + NO)/8)


# Now test cronbach alpha for each octant
library(psych)
df <- read.csv("/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv") %>% dplyr::filter(m1_R2 > .98, PTNUM != 8040, PTNUM != 8048, PTNUM != 8073)
iip_111 <- dplyr::filter(iip, PTNUM %in% df$PTNUM)
alpha_pa <- psych::alpha(dplyr::select(iip_111, IIP21, IIP40, IIP57, IIP58, IIP65, IIP68, IIP76, IIP80))
alpha_bc <-psych::alpha(dplyr::select(iip_111, IIP1,IIP26,IIP28,IIP38,IIP41,IIP50,IIP73,IIP88))
alpha_de <- psych::alpha(dplyr::select(iip_111,IIP11,IIP18,IIP20,IIP24,IIP27,IIP31,IIP46,IIP82))
alpha_fg <- psych::alpha(dplyr::select(iip_111, IIP3,IIP7,IIP17,IIP22,IIP43,IIP45,IIP71,IIP85))
alpha_hi <- psych::alpha(dplyr::select(iip_111, IIP5,IIP6,IIP8,IIP9,IIP12,IIP15,IIP23,IIP49))
alpha_jk <- psych::alpha(dplyr::select(iip_111, IIP2,IIP10,IIP29,IIP44,IIP48,IIP54,IIP69,IIP83))
alpha_lm <- psych::alpha(dplyr::select(iip_111, IIP25,IIP37,IIP47,IIP59,IIP64,IIP67,IIP70,IIP87))
alpha_no=psych::alpha(dplyr::select(iip_111, IIP4,IIP30,IIP39,IIP52,IIP56,IIP61,IIP62,IIP78))
alpha_elevation <- psych::alpha(dplyr::select(iip_111, starts_with("IIP"), -starts_with("IIP_")))



iip <- subset(iip, select=c(usrid, ptnum, dyadid, IIP_PD1, IIP_PD2, IIP_PD3, PA,
                            BC, DE, FG, HI, JK, LM, NO, iip_bpd,iip_agency, iip_communion, iip_elevation ))
### SIDP


sidp <- subset(sidp, mth==0 & raterID==0 & ratercode==6) #raterID 0 is case conference; ratercode 6 is case conference; mth 0 is intake
sidp$usrid <- as.numeric(paste0(sidp$ptnum, sidp$dyadid))
sidp$szoid_sidp <- with(sidp, szoid1 + szoid2 + szoid3+ szoid4 + szoid5stypl8 + szoid6+ szoid7)
sidp$stypl_sidp <- with(sidp, stypl1+ stypl2 + stypl3 + stypl4 + stypl5 + stypl6 + stypl7 + szoid5stypl8 + stypl9)
sidp$obcmp_sidp <- with(sidp, obcmp1 + obcmp2 + obcmp3 + obcmp4 + obcmp5 + obcmp6 + obcmp7 + obcmp8)
sidp$bordl_sidp <- with(sidp, bordl1 + bordl2 + bordl3 + bordl4 + bordl5 + bordl6 + bordl7 + bordl8 + bordl9)
sidp$narci_sidp <- with(sidp, narci1 +narci2 + narci3 + narci4 + narci5 + narci6 + narci7 + narci8 + narci9)
sidp$antso_sidp <- with(sidp, antso1 + antso2 + antso3 + antso4 + antso5 + antso6 + antso7) #what to do about cantso?
sidp$histr_sidp <- with(sidp, histr1 + histr2 + histr3 + histr4 + histr5 + histr6 + histr7 + histr8)
sidp$parnd_sidp <- with(sidp, parnd1+ parnd2 + parnd3 + parnd4 + parnd5 + parnd6 + parnd7)
sidp$depen_sidp <- with(sidp, depen1 + depen2 + depen3 + depen4 + depen5 + depen6 + depen7 + depen8)
sidp$deprs_sidp <- with(sidp, deprs1 + deprs2 + deprs3 + deprs4 + deprs5 + deprs6 + deprs7)
sidp$negtv_sidp <- with(sidp, negtv1 + negtv2 + negtv3 + negtv4 + negtv5 + negtv6 + negtv7)
sidp$avoid_sidp <- with(sidp, avoid1 + avoid2 + avoid3 + avoid4 + avoid5 + avoid6 + avoid7)
sidp$szoidCount <- with(sidp, (szoid1==2) + (szoid2==2) + (szoid3==2)+ (szoid4==2) + (szoid5stypl8==2) + (szoid6==2)+ (szoid7==2))
sidp$styplCount <- with(sidp, (stypl1==2)+ (stypl2==2) + (stypl3==2) + (stypl4==2) + (stypl5==2) + (stypl6==2) + (stypl7==2) + (szoid5stypl8==2) + (stypl9==2))
sidp$obcmpCount <- with(sidp, (obcmp1==2) + (obcmp2==2) + (obcmp3==2) + (obcmp4==2) + (obcmp5==2) + (obcmp6==2) + (obcmp7==2) + (obcmp8==2))
sidp$bordlCount <- with(sidp, (bordl1==2) + (bordl2==2) + (bordl3==2) + (bordl4==2) + (bordl5==2) + (bordl6==2) + (bordl7==2) + (bordl8==2) + (bordl9==2))
sidp$narciCount <- with(sidp, (narci1==2) +(narci2==2) + (narci3==2) + (narci4==2) + (narci5==2) + (narci6==2) + (narci7==2) + (narci8==2) + (narci9==2))
sidp$antsoCount <- with(sidp, (antso1==2) + (antso2==2) + (antso3==2) + (antso4==2) + (antso5==2) + (antso6==2) + (antso7==2)) #what to do about cantso?
sidp$histrCount <- with(sidp, (histr1==2) + (histr2==2) + (histr3==2) + (histr4==2) + (histr5==2) + (histr6==2) + (histr7==2) + (histr8==2))
sidp$parndCount <- with(sidp, (parnd1==2)+ (parnd2==2) + (parnd3==2) + (parnd4==2) + (parnd5==2) + (parnd6==2)+ (parnd7==2))
sidp$depenCount <- with(sidp, (depen1==2) + (depen2==2) + (depen3==2) + (depen4==2) + (depen5==2) + (depen6==2) + (depen7==2) + (depen8==2))
sidp$deprsCount <- with(sidp, (deprs1==2) + (deprs2==2) + (deprs3==2) + (deprs4==2) + (deprs5==2) + (deprs6==2) + (deprs7==2))
sidp$negtvCount <- with(sidp, (negtv1==2) + (negtv2==2) + (negtv3==2) + (negtv4==2) + (negtv5==2) + (negtv6==2) + (negtv7==2))
sidp$avoidCount <- with(sidp, (avoid1==2) + (avoid2==2) + (avoid3==2) + (avoid4==2) + (avoid5==2) + (avoid6==2) + (avoid7==2))
sidp_111 <- dplyr::filter(sidp, PTNUM %in% df$PTNUM)
alpha_bordl <- psych::alpha(dplyr::select(sidp_111, starts_with("bordl"), -ends_with("_sidp"), -ends_with("Count")))
alpha_pdtot <- psych::alpha(dplyr::select(sidp_111, -ends_with("_sidp"), -ends_with("Count"),-PTNUM, -DyadID, -ScrnID, -mth, -ratercode, -raterID, -starts_with("section"), -initials, -cantso, -starts_with("slfinj")))


### DAS

das_111 <- dplyr::filter(das, PTNUM %in% df$PTNUM, mth == 0)
das_111_scored <- score_das(das_111)
alpha_dastotal <- psych::alpha(dplyr::select(das_111, starts_with("DAS"), -DASrelationship), keys = c("DAS16", "DAS17", "DAS20", "DAS21", "DAS22", "DAS25", "DAS26", "DAS27", "DAS28", "DAS31"))

alpha_dassat_bl <- psych::alpha(dplyr::select(das_111,DAS16, DAS17, DAS18, DAS19, DAS20, DAS21, DAS22, DAS23, DAS31, DAS32), keys = c("DAS16", "DAS17", "DAS20", "DAS21", "DAS22", "DAS31"))
das_111_fu <- dplyr::filter(das, PTNUM %in% df$PTNUM, mth == 12)

alpha_dassat_fu <- psych::alpha(dplyr::select(das_111_fu,DAS16, DAS17, DAS18, DAS19, DAS20, DAS21, DAS22, DAS23, DAS31, DAS32), keys = c("DAS16", "DAS17", "DAS20", "DAS21", "DAS22", "DAS31"))


                             

### PANAS PRE
panaspre <- read.csv("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/PANASPRE.csv")
meanreplace <- function(df, ID, varprefix="PANAS", subscaleitems) {
  subjrow <- df$usrid == ID
  dfitems <- subset(df, select=paste0(varprefix, subscaleitems))
  #dfitems[dfitems == 99] <- NA
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

panaspre_111 <- dplyr::filter(panaspre, PTNUM %in% df$PTNUM)
cronbach_prepa <- psych::alpha(dplyr::select(panaspre_111, PANAS1,PANAS3,PANAS5,PANAS9,PANAS10,PANAS12,PANAS14,PANAS16,PANAS17,PANAS19))
cronbach_prena <- psych::alpha(dplyr::select(panaspre_111, PANAS2,PANAS4,PANAS6,PANAS7,PANAS8,PANAS11,PANAS13,PANAS15,PANAS18,PANAS20))





#### PANAS POST
panaspost <- read.csv("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/PANASPOST.csv")

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

panaspost_111 <- dplyr::filter(panaspost, PTNUM %in% df$PTNUM)
cronbach_postpa <- psych::alpha(dplyr::select(panaspost_111, PANAS1,PANAS3,PANAS5,PANAS9,PANAS10,PANAS12,PANAS14,PANAS16,PANAS17,PANAS19))
cronbach_postna <- psych::alpha(dplyr::select(panaspost_111, PANAS2,PANAS4,PANAS6,PANAS7,PANAS8,PANAS11,PANAS13,PANAS15,PANAS18,PANAS20))


### IMIC Affiliatoin

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
imicpost<- meanreplace(imicpost, "80220", varprefix = "IMI", subscaleitems = c(1, 10, 18, 23))#replace 1
imicpost <-meanreplace(imicpost, "80220", varprefix = "IMI", subscaleitems = c(2, 12, 16, 27)) #replace 2
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



imicpre_111 <- dplyr::filter(imicpre, PTNUM %in% df$PTNUM) # only 110 because 8129 doesn't have data 
alpha_imicpre <- psych::alpha(dplyr::select(imicpre_111, starts_with("IMI")), check.keys =TRUE)

imicpost_111 <- dplyr::filter(imicpost, PTNUM %in% df$PTNUM) # only 110 because 8129 doesn't have data 
alpha_imicpost <- psych::alpha(dplyr::select(imicpost_111, starts_with("IMI")), check.keys =TRUE)

### ECR Anxiety
#reverse= c(2, 22, 3, 5, 11,15, 17, 19, 25, 27, 29, 31, 33, 35)
#anx ECR2, ECR4, ECR6, ECR8, ECR10, ECR12, ECR14,  ECR16, ECR18, ECR20, ECR22, ECR24, ECR26, ECR28, ECR30, ECR32, ECR34, ECR36 #2 22
#avo ECR1, ECR3, ECR5, ECR7, ECR9, ECR11, ECR13, ECR15, ECR17, ECR19, ECR21, ECR23, ECR25, ECR27, ECR29, ECR31, ECR33, ECR35 #3 5 11 15 17 19 25 27 29 31 33 35
ecr_111 <- dplyr::filter(ecr, PTNUM %in% df$PTNUM, mth == 0)
alpha_anx <- psych::alpha(dplyr::select(ecr_111, ECR2, ECR4, ECR6, ECR8, ECR10, ECR12, ECR14,  ECR16, ECR18, ECR20, ECR22, ECR24, ECR26, ECR28, ECR30, ECR32, ECR34, ECR36), keys = c("ECR2", "ECR22"))
### ECR avoidance
alpha_avo <- psych::alpha(dplyr::select(ecr_111, ECR1, ECR3, ECR5, ECR7, ECR9, ECR11, ECR13, ECR15, ECR17, ECR19, ECR21, ECR23, ECR25, ECR27, ECR29, ECR31, ECR33, ECR35), keys = c("ECR3", "ECR5", "ECR11", "ECR15", "ECR17", "ECR19", "ECR25","ECR27" ,"ECR29","ECR31","ECR33" ,"ECR35")) #, keys = c("ECR20", "ECR22", "ECR26", "ECR27", "ECR28", "ECR29", "ECR30", "ECR31", "ECR33", "ECR34", "ECR35", "ECR36"))









                               
                               
                               
                               
                               