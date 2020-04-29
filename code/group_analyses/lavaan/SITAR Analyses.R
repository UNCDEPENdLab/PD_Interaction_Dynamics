#analyses for SITAR
#initially exclude based on PTNUM inclusion spreadsheet
#vanBse based purely on outliers
#then exclude based off of multivariate outliers using generalized Cook's Distance

#STRUCTURE OF THIS SCRIPT
#LOAD IN DATA SETS
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/SITAR_Analyses")
write.csv(dat1, "dat1.csv")
write.csv(dat2, "dat2.csv")
write.csv(dat3, "dat3.csv")
write.csv(dat4, "dat4.csv")
write.csv(dat5, "dat5.csv")
write.csv(dat6, "dat6.csv")
write.csv(dat7, "dat7.csv")
write.csv(dat8, "dat8.csv")
write.csv(dat9, "dat9.csv")
write.csv(dat10, "dat10.csv")
write.csv(dat11, "dat11.csv")
write.csv(dat12, "dat12.csv")
write.csv(dat13, "dat13.csv")
write.csv(dat14, "dat14.csv")
write.csv(dat15, "dat15.csv")
write.csv(m2dat1, "m2dat1.csv")
write.csv(m4dat1, "m4dat1.csv")
write.csv(m5dat1, "m5dat1.csv")
write.csv(m8dat1, "m8dat1.csv")
write.csv(m10dat1, "m10dat1.csv")
write.csv(m11dat1, "m11dat1.csv")

write.csv(m14dat1, "m14dat1.csv")
write.csv(m1dat4, "m1dat4.csv")
write.csv(m2dat4, "m2dat4.csv")
write.csv(m4dat4, "m4dat4.csv")
write.csv(m5dat4, "m5dat4.csv")
write.csv(m7dat4, "m7dat4.csv")
write.csv(m8dat4, "m8dat4.csv")
write.csv(m10dat4, "m10dat4.csv")
write.csv(m11dat4, "m11dat4.csv")
write.csv(m13dat4, "m13dat4.csv")
write.csv(m14dat4, "m14dat4.csv")
write.csv(m1dat5, "m1dat5.csv")
write.csv(m2dat5, "m2dat5.csv")
write.csv(m4dat5, "m4dat5.csv")
write.csv(m5dat5, "m5dat5.csv")
write.csv(m7dat5, "m7dat5.csv")
write.csv(m8dat5, "m8dat5.csv")
write.csv(m10dat5, "m10dat5.csv")
write.csv(m11dat5, "m11dat5.csv")
write.csv(m13dat5, "m13dat5.csv")
write.csv(m14dat5, "m14dat5.csv")
write.csv(m1dat6, "m1dat6.csv")
write.csv(m2dat6, "m2dat6.csv")
write.csv(m4dat6, "m4dat6.csv")
write.csv(m5dat6, "m5dat6.csv")
write.csv(m7dat6, "m7dat6.csv")
write.csv(m8dat6, "m8dat6.csv")
write.csv(m10dat6, "m10dat6.csv")
write.csv(m11dat6, "m11dat6.csv")
write.csv(m13dat6, "m13dat6.csv")
write.csv(m14dat6, "m14dat6.csv")
write.csv(m1dat4thr2, "m1dat4thr2.csv")
write.csv(m2dat4thr2, "m2dat4thr2.csv")
write.csv(m4dat4thr2, "m4dat4thr2.csv")
write.csv(m5dat4thr2, "m5dat4thr2.csv")
write.csv(m7dat4thr2, "m7dat4thr2.csv")
write.csv(m8dat4thr2, "m8dat4thr2.csv")
write.csv(m10dat4thr2, "m10dat4thr2.csv")
write.csv(m11dat4thr2, "m11dat4thr2.csv")
write.csv(m13dat4thr2, "m13dat4thr2.csv")
write.csv(m14dat4thr2, "m14dat4thr2.csv")
write.csv(m1dat5thr2, "m1dat5thr2.csv")
write.csv(m2dat5thr2, "m2dat5thr2.csv")
write.csv(m4dat5thr2, "m4dat5thr2.csv")
write.csv(m5dat5thr2, "m5dat5thr2.csv")
write.csv(m7dat5thr2, "m7dat5thr2.csv")
write.csv(m8dat5thr2, "m8dat5thr2.csv")
write.csv(m10dat5thr2, "m10dat5thr2.csv")
write.csv(m11dat5thr2, "m11dat5thr2.csv")
write.csv(m13dat5thr2, "m13dat5thr2.csv")
write.csv(m14dat5thr2, "m14dat5thr2.csv")
write.csv(m1dat6thr2, "m1dat6thr2.csv")
write.csv(m2dat6thr2, "m2dat6thr2.csv")
write.csv(m4dat6thr2, "m4dat6thr2.csv")
write.csv(m5dat6thr2, "m5dat6thr2.csv")
write.csv(m7dat6thr2, "m7dat6thr2.csv")
write.csv(m8dat6thr2, "m8dat6thr2.csv")
write.csv(m10dat6thr2, "m10dat6thr2.csv")
write.csv(m11dat6thr2, "m11dat6thr2.csv")
write.csv(m13dat6thr2, "m13dat6thr2.csv")
write.csv(m14dat6thr2, "m14dat6thr2.csv")
write.csv(m1dat1thr2, "m1dat1thr2.csv")
write.csv(m2dat1thr2, "m2dat1thr2.csv")
write.csv(m4dat1thr2, "m4dat1thr2.csv")
write.csv(m5dat1thr2, "m5dat1thr2.csv")
write.csv(m7dat1thr2, "m7dat1thr2.csv")
write.csv(m8dat1thr2, "m8dat1thr2.csv")
write.csv(m10dat1thr2, "m10dat1thr2.csv")
write.csv(m11dat1thr2, "m11dat1thr2.csv")
write.csv(m13dat1thr2, "m13dat1thr2.csv")
write.csv(m14dat1thr2, "m14dat1thr2.csv")
write.csv(m1dat2thr2, "m1dat1thr2.csv")
write.csv(m4dat2thr2, "m4dat1thr2.csv")
write.csv(m7dat2thr2, "m7dat1thr2.csv")
write.csv(m10dat2thr2, "m10dat1thr2.csv")
write.csv(m13dat2thr2, "m13dat1thr2.csv")
#next set that worked
write.csv(m16dat7, "m16dat7.csv")
write.csv(m17dat7, "m17dat7.csv")
write.csv(m19dat7, "m19dat7.csv")
write.csv(m20dat7, "m20dat7.csv")
write.csv(m16dat8, "m16dat8.csv")
write.csv(m17dat8, "m17dat8.csv")
write.csv(m19dat8, "m19dat8.csv")
write.csv(m20dat8, "m20dat8.csv")
write.csv(m16dat9, "m16dat9.csv")
write.csv(m17dat9, "m17dat9.csv")
write.csv(m19dat9, "m19dat9.csv")
write.csv(m20dat9, "m20dat9.csv")
write.csv(m22dat10, "m22dat10.csv")
write.csv(m25dat10, "m25dat10.csv")
write.csv(m22dat11, "m22dat11.csv")
write.csv(m25dat11, "m25dat11.csv")
write.csv(m22dat12, "m22dat12.csv")
write.csv(m25dat12, "m25dat12.csv")
write.csv(m28dat13, "m28dat13.csv")
write.csv(m28dat14, "m28dat14.csv")
write.csv(m28dat15, "m28dat15.csv")
write.csv(m29dat15, "m29dat15.csv")
write.csv(m16dat7thr2, "m16dat7thr2.csv")
write.csv(m17dat7thr2, "m17dat7thr2.csv")
write.csv(m19dat7thr2, "m19dat7thr2.csv")
write.csv(m20dat7thr2, "m20dat7thr2.csv")
write.csv(m16dat8thr2, "m16dat8thr2.csv")
write.csv(m17dat8thr2, "m17dat8thr2.csv")
write.csv(m19dat8thr2, "m19dat8thr2.csv")
write.csv(m20dat8thr2, "m20dat8thr2.csv")
write.csv(m16dat9thr2, "m16dat9thr2.csv")
write.csv(m17dat9thr2, "m17dat9thr2.csv")
write.csv(m19dat9thr2, "m19dat9thr2.csv")
write.csv(m20dat9thr2, "m20dat9thr2.csv")
write.csv(m22dat10thr2, "m22dat10thr2.csv")
write.csv(m25dat10thr2, "m25dat10thr2.csv")
write.csv(m22dat11thr2, "m22dat11thr2.csv")
write.csv(m25dat11thr2, "m25dat11thr2.csv")
write.csv(m22dat12thr2, "m22dat12thr2.csv")
write.csv(m25dat12thr2, "m25dat12thr2.csv")

write.csv(m19dat7, "m19dat7.csv")
write.csv(m20dat7, "m20dat7.csv")


write.csv(m7dat1, "m7dat1.csv")
write.csv(m13dat1, "m13dat1.csv")
write.csv(m1dat2, "m1dat2.csv")
write.csv(m4dat2, "m4dat2.csv")
write.csv(m23dat10, "m23dat10.csv")
write.csv(m26dat10, "m26dat10.csv")
write.csv(m23dat11, "m23dat11.csv")
write.csv(m26dat11, "m26dat11.csv")
write.csv(m23dat12, "m23dat12.csv")
write.csv(m26dat12, "m26dat12.csv")
write.csv(m29dat14, "m29dat14.csv")
write.csv(m23dat10thr2, "m23dat10thr2.csv")
write.csv(m26dat10thr2, "m26dat10thr2.csv")
write.csv(m23dat11thr2, "m23dat11thr2.csv")
write.csv(m26dat11thr2, "m26dat11thr2.csv")
write.csv(m23dat12thr2, "m23dat12thr2.csv")
write.csv(m26dat12thr2, "m26dat12thr2.csv")
write.csv(m28dat13thr2, "m28dat13thr2.csv")
write.csv(m29dat13thr2, "m29dat13thr2.csv")
write.csv(m28dat14thr2, "m28dat14thr2.csv")
write.csv(m29dat14thr2, "m29dat14thr2.csv")
write.csv(m28dat15thr2, "m28dat15thr2.csv")
write.csv(m29dat15thr2, "m29dat15thr2.csv")

#now load in models
write.csv(dat1, "dat1.csv")
write.csv(dat2, "dat2.csv")
write.csv(dat3, "dat3.csv")
write.csv(dat4, "dat4.csv")
write.csv(dat5, "dat5.csv")
write.csv(dat6, "dat6.csv")
write.csv(dat7, "dat7.csv")
write.csv(dat8, "dat8.csv")
write.csv(dat9, "dat9.csv")
write.csv(dat10, "dat10.csv")
write.csv(dat11, "dat11.csv")
write.csv(dat12, "dat12.csv")
write.csv(dat13, "dat13.csv")
write.csv(dat14, "dat14.csv")
write.csv(dat15, "dat15.csv")
write.csv(m2dat1, "m2dat1.csv")
write.csv(m4dat1, "m4dat1.csv")
write.csv(m5dat1, "m5dat1.csv")
write.csv(m8dat1, "m8dat1.csv")
write.csv(m10dat1, "m10dat1.csv")
write.csv(m11dat1, "m11dat1.csv")
write.csv(m14dat1, "m14dat1.csv")
write.csv(m1dat4, "m1dat4.csv")
write.csv(m2dat4, "m2dat4.csv")
write.csv(m4dat4, "m4dat4.csv")
write.csv(m5dat4, "m5dat4.csv")
write.csv(m7dat4, "m7dat4.csv")
write.csv(m8dat4, "m8dat4.csv")
write.csv(m10dat4, "m10dat4.csv")
write.csv(m11dat4, "m11dat4.csv")
write.csv(m13dat4, "m13dat4.csv")
write.csv(m14dat4, "m14dat4.csv")
write.csv(m1dat5, "m1dat5.csv")
write.csv(m2dat5, "m2dat5.csv")
write.csv(m4dat5, "m4dat5.csv")
write.csv(m5dat5, "m5dat5.csv")
write.csv(m7dat5, "m7dat5.csv")
write.csv(m8dat5, "m8dat5.csv")
write.csv(m10dat5, "m10dat5.csv")
write.csv(m11dat5, "m11dat5.csv")
write.csv(m13dat5, "m13dat5.csv")
write.csv(m14dat5, "m14dat5.csv")
write.csv(m1dat6, "m1dat6.csv")
write.csv(m2dat6, "m2dat6.csv")
write.csv(m4dat6, "m4dat6.csv")
write.csv(m5dat6, "m5dat6.csv")
write.csv(m7dat6, "m7dat6.csv")
write.csv(m8dat6, "m8dat6.csv")
write.csv(m10dat6, "m10dat6.csv")
write.csv(m11dat6, "m11dat6.csv")
write.csv(m13dat6, "m13dat6.csv")
write.csv(m14dat6, "m14dat6.csv")
write.csv(m1dat4thr2, "m1dat4thr2.csv")
write.csv(m2dat4thr2, "m2dat4thr2.csv")
write.csv(m4dat4thr2, "m4dat4thr2.csv")
write.csv(m5dat4thr2, "m5dat4thr2.csv")
write.csv(m7dat4thr2, "m7dat4thr2.csv")
write.csv(m8dat4thr2, "m8dat4thr2.csv")
write.csv(m10dat4thr2, "m10dat4thr2.csv")
write.csv(m11dat4thr2, "m11dat4thr2.csv")
write.csv(m13dat4thr2, "m13dat4thr2.csv")
write.csv(m14dat4thr2, "m14dat4thr2.csv")
write.csv(m1dat5thr2, "m1dat5thr2.csv")
write.csv(m2dat5thr2, "m2dat5thr2.csv")
write.csv(m4dat5thr2, "m4dat5thr2.csv")
write.csv(m5dat5thr2, "m5dat5thr2.csv")
write.csv(m7dat5thr2, "m7dat5thr2.csv")
write.csv(m8dat5thr2, "m8dat5thr2.csv")
write.csv(m10dat5thr2, "m10dat5thr2.csv")
write.csv(m11dat5thr2, "m11dat5thr2.csv")
write.csv(m13dat5thr2, "m13dat5thr2.csv")
write.csv(m14dat5thr2, "m14dat5thr2.csv")
write.csv(m1dat6thr2, "m1dat6thr2.csv")
write.csv(m2dat6thr2, "m2dat6thr2.csv")
write.csv(m4dat6thr2, "m4dat6thr2.csv")
write.csv(m5dat6thr2, "m5dat6thr2.csv")
write.csv(m7dat6thr2, "m7dat6thr2.csv")
write.csv(m8dat6thr2, "m8dat6thr2.csv")
write.csv(m10dat6thr2, "m10dat6thr2.csv")
write.csv(m11dat6thr2, "m11dat6thr2.csv")
write.csv(m13dat6thr2, "m13dat6thr2.csv")
write.csv(m14dat6thr2, "m14dat6thr2.csv")
write.csv(m1dat1thr2, "m1dat1thr2.csv")
write.csv(m2dat1thr2, "m2dat1thr2.csv")
write.csv(m4dat1thr2, "m4dat1thr2.csv")
write.csv(m5dat1thr2, "m5dat1thr2.csv")
write.csv(m7dat1thr2, "m7dat1thr2.csv")
write.csv(m8dat1thr2, "m8dat1thr2.csv")
write.csv(m10dat1thr2, "m10dat1thr2.csv")
write.csv(m11dat1thr2, "m11dat1thr2.csv")
write.csv(m13dat1thr2, "m13dat1thr2.csv")
write.csv(m14dat1thr2, "m14dat1thr2.csv")
write.csv(m1dat2thr2, "m1dat1thr2.csv")
write.csv(m4dat2thr2, "m4dat1thr2.csv")
write.csv(m7dat2thr2, "m7dat1thr2.csv")
write.csv(m10dat2thr2, "m10dat1thr2.csv")
write.csv(m13dat2thr2, "m13dat1thr2.csv")
#next set that worked
###########################################################
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/SITAR_Analyses")
dat4 <- read.csv("dat4.csv")
m13dat4 <- read.csv("m13dat4.csv")
m16dat7 <- read.csv("m16dat7.csv")
m17dat7 <- read.csv("m17dat7.csv")
m19dat7 <- read.csv("m19dat7.csv")
m20dat7 <- read.csv("m20dat7.csv")
m16dat8 <- read.csv("m16dat8.csv")
m17dat8 <- read.csv("m17dat8.csv")
m19dat8 <- read.csv("m19dat8.csv")
m20dat8 <- read.csv("m20dat8.csv")
m16dat9 <- read.csv("m16dat9.csv")
m17dat9 <- read.csv("m17dat9.csv")
m19dat9 <- read.csv("m19dat9.csv")
m20dat9 <- read.csv("m20dat9.csv")
m22dat10 <- read.csv("m22dat10.csv")
m25dat10 <- read.csv("m25dat10.csv")
m22dat11 <- read.csv("m22dat11.csv")
m25dat11 <- read.csv("m25dat11.csv")
m22dat12 <- read.csv("m22dat12.csv")
m25dat12 <- read.csv("m25dat12.csv")
m28dat13 <- read.csv("m28dat13.csv")
m28dat14 <- read.csv("m28dat14.csv")
m28dat15 <- read.csv("m28dat15.csv")
m29dat15 <- read.csv("m29dat15.csv")
m16dat7thr2 <- read.csv("m16dat7thr2.csv")
m17dat7thr2 <- read.csv("m17dat7thr2.csv")
m19dat7thr2 <- read.csv("m19dat7thr2.csv")
m20dat7thr2 <- read.csv("m20dat7thr2.csv")
m16dat8thr2 <- read.csv("m16dat8thr2.csv")
m17dat8thr2 <- read.csv("m17dat8thr2.csv")
m19dat8thr2 <- read.csv("m19dat8thr2.csv")
m20dat8thr2 <- read.csv("m20dat8thr2.csv")
m16dat9thr2 <- read.csv("m16dat9thr2.csv")
m17dat9thr2 <- read.csv("m17dat9thr2.csv")
m19dat9thr2 <- read.csv("m19dat9thr2.csv")
m20dat9thr2 <- read.csv("m20dat9thr2.csv")
m22dat10thr2 <- read.csv("m22dat10thr2.csv")
m25dat10thr2 <- read.csv("m25dat10thr2.csv")
m22dat11thr2 <- read.csv("m22dat11thr2.csv")
m25dat11thr2 <- read.csv("m25dat11thr2.csv")
m22dat12thr2 <- read.csv("m22dat12thr2.csv")
m25dat12thr2 <- read.csv("m25dat12thr2.csv")

m19dat7 <- read.csv("m19dat7.csv")
m20dat7 <- read.csv("m20dat7.csv")


m7dat1 <- read.csv("m7dat1.csv")
m13dat1 <- read.csv("m13dat1.csv")
m1dat2 <- read.csv("m1dat2.csv")
m4dat2 <- read.csv("m4dat2.csv")
m23dat10 <- read.csv("m23dat10.csv")
m26dat10 <- read.csv("m26dat10.csv")
m23dat11 <- read.csv("m23dat11.csv")
m26dat11 <- read.csv("m26dat11.csv")
m23dat12 <- read.csv("m23dat12.csv")
m26dat12 <- read.csv("m26dat12.csv")
m29dat14 <- read.csv("m29dat14.csv")
m23dat10thr2 <- read.csv("m23dat10thr2.csv")
m26dat10thr2 <- read.csv("m26dat10thr2.csv")
m23dat11thr2 <- read.csv("m23dat11thr2.csv")
m26dat11thr2 <- read.csv("m26dat11thr2.csv")
m23dat12thr2 <- read.csv("m23dat12thr2.csv")
m26dat12thr2 <- read.csv("m26dat12thr2.csv")
m28dat13thr2 <- read.csv("m28dat13thr2.csv")
m29dat13thr2 <- read.csv("m29dat13thr2.csv")
m28dat14thr2 <- read.csv("m28dat14thr2.csv")
m29dat14thr2 <- read.csv("m29dat14thr2.csv")
m28dat15thr2 <- read.csv("m28dat15thr2.csv")
m29dat15thr2 <- read.csv("m29dat15thr2.csv")






#A--Dynamical Model of Coregulation during negative nteraction
#A1 load in the data
#A2 make all the models to test --> BOTH CLASSIC APIM and MEDIATION ANALYSES
#IIP-Elevation (w/ and w/o vanBse), Agency (w/ and w/o vanBse), Communion (w/ and w/o vanBse)
#PD Sx (w/ and w/o vanBse)
#PD Sx + Elevation (w/ and w/o vanBse)
#Pre-Post SAAM anxious (w/ and w/o vanBse)
#Pre-post SAAM anxidant (w/ and w/o vanBse)
#Pre-Post PANAS NA (w/ and w/o vanBse)
#Pre-post IMI Agency (w/ and w/o vanBse)
#Pre-post IMI Comm (w/ and w/o vanBse)
#A3 outlier detection using generalized Cook's Distance (thresh = 3*mean(cook'sDistance))
#break into different cxlasses of outlier detection so that can customize the while loop appropriately
#
#B--RSA
#B1 load in ibi time series for each couple
#B2 make all the models to test--> only classical apim (mediation doesn't make sense in this context)
#IIP-Elevation, Agency, Communion
#PD Sx
#PD Sx + Elevation
#B3 outlier detection using generalized Cook's Distance (thresh = 3*mean(cook'sDistance))
#1. Load in the data and get overall to be tested data sets
library(dplyr)
library(tidyr)
library(R.matlab)
library(ggplot2)
library(cowplot)
library(influence.SEM)
df <-
  read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/params_personalitydata_all.csv")

df_personalitydata <-
  dplyr::select(
    df,
    PTNUM,
    iip_elevation_patient,
    iip_elevation_partner,
    pdtot_patient,
    pdtot_partner,
    self_coupling_patient,
    self_coupling_partner,
    cross_coupling_patient,
    cross_coupling_partner
  )
df_personalitydata_patient <-
  dplyr::select(
    df_personalitydata,
    iip_elevation_patient,
    pdtot_patient,
    self_coupling_patient,
    cross_coupling_patient,
    PTNUM
  )
df_personalitydata_partner <-
  dplyr::select(
    df_personalitydata,
    iip_elevation_partner,
    pdtot_partner,
    self_coupling_partner,
    cross_coupling_partner,
    PTNUM
  )
df_personalitydata_patient$DyadID <-
  paste0(df_personalitydata_patient$PTNUM, "_1")
df_personalitydata_patient$Patient <- 1
df_personalitydata_partner$DyadID <-
  paste0(df_personalitydata_partner$PTNUM, "_0")
df_personalitydata_partner$Patient <- 0
df_personalitydata_patient <-
  dplyr::rename(
    df_personalitydata_patient,
    iip_elevation = iip_elevation_patient,
    pdtot = pdtot_patient,
    self_coupling = self_coupling_patient,
    cross_coupling = cross_coupling_patient
  )
df_personalitydata_partner <-
  dplyr::rename(
    df_personalitydata_partner,
    iip_elevation = iip_elevation_partner,
    pdtot = pdtot_partner,
    self_coupling = self_coupling_partner,
    cross_coupling = cross_coupling_partner
  )
df_personaitydata_long <-
  dplyr::bind_rows(df_personalitydata_patient, df_personalitydata_partner)
g <-
  ggplot(df_personaitydata_long,
         aes(
           x = pdtot,
           y = self_coupling,
           fill = as.factor(Patient)
         ))


df_personalitydata <-
  dplyr::select(
    m13dat4,
    PTNUM,
    iip_elevation_patient,
    iip_elevation_partner,
    pdcpt,
    pdcpr,
    scpr,
    scpt,
    ccpt,
    ccpr
  )
df_personalitydata_patient <-
  dplyr::select(df_personalitydata,
                iip_elevation_patient,
                pdcpt,
                scpt,
                ccpt,
                PTNUM)
df_personalitydata_partner <-
  dplyr::select(df_personalitydata,
                iip_elevation_partner,
                pdcpr,
                scpr,
                ccpr,
                PTNUM)
df_personalitydata_patient$DyadID <-
  paste0(df_personalitydata_patient$PTNUM, "_1")
df_personalitydata_patient$Patient <- 1
df_personalitydata_partner$DyadID <-
  paste0(df_personalitydata_partner$PTNUM, "_0")
df_personalitydata_partner$Patient <- 0
df_personalitydata_patient <-
  dplyr::rename(
    df_personalitydata_patient,
    iip_elevation = iip_elevation_patient,
    pdtot = pdcpt,
    self_coupling = scpt,
    cross_coupling = ccpt
  )
df_personalitydata_partner <-
  dplyr::rename(
    df_personalitydata_partner,
    iip_elevation = iip_elevation_partner,
    pdtot = pdcpr,
    self_coupling = scpr,
    cross_coupling = ccpr
  )
df_personaitydata_long <-
  bind_rows(df_personalitydata_patient, df_personalitydata_partner)
toplotdf = df_personaitydata_long
g <-
  ggplot(toplotdf, aes(
    x = pdtot,
    y = self_coupling,
    colour = as.factor(Patient)
  )) + geom_point() + geom_smooth(method = "lm")


df <-
  df %>% mutate(
    iip_elcpr = iip_elevation_partner - mean(iip_elevation_partner),
    pdcountcpr = allpdCount_partner - mean(na.omit(allpdCount_partner)),
    iip_x_pdcount_partner = iip_elcpr * pdcountcpr,
    iip_elcpt = iip_elevation_patient - mean(na.omit(iip_elevation_patient)),
    pdcountcpt = allpdCount_patient - mean(na.omit(allpdCount_patient)),
    iip_x_pdcount_patient = iip_elcpt * pdcountcpt
  )

df_params_patient <-
  dplyr::select(df, self_coupling_patient, cross_coupling_patient)
df_params_patient$summed_cross_coupling_patient <-
  with(df_params_patient,
       (-1) * self_coupling_patient + cross_coupling_patient)
df_params_partner <-
  dplyr::select(df, self_coupling_partner, cross_coupling_partner)
df_params_partner$summed_cross_coupling_partner <-
  with(df_params_partner,
       (-1) * self_coupling_partner + cross_coupling_partner)
df$summed_cross_coupling_partner <-
  df_params_partner$summed_cross_coupling_partner
df$summed_cross_coupling_patient <-
  df_params_patient$summed_cross_coupling_patient



setwd("/Users/alisonmarie526/Desktop/Archive_April2017/")
setwd(
  "/Users/ams939/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Archive_April2017/"
)
vanBse_params <- readMat("vanBse_modelcomparison_logevidence.mat")
vanBse_paramsm1 <- as.data.frame((vanBse_params$rawparameters[, 1, ]))
vanBse_paramsm2 <- as.data.frame((vanBse_params$rawparameters[, 2, ]))
vanBse_paramsm3 <- as.data.frame((vanBse_params$rawparameters[, 3, ]))
vanBse_paramsm4 <- as.data.frame((vanBse_params$rawparameters[, 4, ]))
vanBse_paramsm4$PTNUM <- as.numeric(vanBse_params$ids)
vanBse_paramsm1 <-
  dplyr::filter(vanBse_paramsm1,
                abs(V1) < .3,
                abs(V2) < .3,
                abs(V3) < .3,
                abs(V4) < .3)
vanBse_paramsm2 <-
  dplyr::filter(vanBse_paramsm2,
                abs(V1) < .3,
                abs(V2) < .3,
                abs(V3) < .3,
                abs(V4) < .3)
vanBse_paramsm3 <-
  dplyr::filter(vanBse_paramsm3,
                abs(V1) < .3,
                abs(V2) < .3,
                abs(V3) < .3,
                abs(V4) < .3)
vanBse_paramsm4 <-
  dplyr::filter(vanBse_paramsm4,
                abs(V1) < .3,
                abs(V2) < .3,
                abs(V3) < .3,
                abs(V4) < .3)

vanBse_paramsm4 <-
  rename(
    vanBse_paramsm4,
    self_coupling_patient = V1,
    cross_coupling_patient = V2,
    self_coupling_parnter = V3,
    cross_coupling_partner = V4
  )
vanBse_paramsm4 <-
  rename(
    vanBse_paramsm4,
    v_self_coupling_patient = self_coupling_patient,
    v_cross_coupling_patient = cross_coupling_patient,
    v_self_coupling_parnter = self_coupling_parnter,
    v_cross_coupling_partner = cross_coupling_partner
  )

dt_vanBse <- merge(df, vanBse_paramsm4, by = c("PTNUM"))
dt_vanBse <-
  dplyr::mutate(
    dt_vanBse,
    scpt = 1000 * self_coupling_patient,
    scpr = 1000 * self_coupling_partner,
    ccpt = 1000 * cross_coupling_patient,
    ccpr = 1000 * cross_coupling_partner,
    v_scpt = 1000 * v_self_coupling_patient,
    v_scpr = 1000 * v_self_coupling_parnter,
    v_ccpt = 1000 * v_cross_coupling_patient,
    v_ccpr = 1000 * v_cross_coupling_partner,
    copt = 1000 * summed_cross_coupling_patient,
    copr = 1000 * summed_cross_coupling_partner
  )



dt_vanBse <-
  dt_vanBse %>% mutate(
    pdcpr = pdtot_partner - mean(na.omit(pdtot_partner)),
    iip_x_pdtot_partner = iip_elcpr * pdcpr,
    pdcpt = pdtot_patient - mean(na.omit(pdtot_patient)),
    iip_x_pdtot_patient = iip_elcpt * pdcpt
  )
dt_vanBse <-
  dplyr::mutate(
    dt_vanBse,
    d_self_coupling_partner = self_coupling_partner - v_self_coupling_parnter,
    d_self_coupling_patient = self_coupling_patient - v_self_coupling_patient,
    d_cross_coupling_partner = cross_coupling_partner - v_cross_coupling_partner,
    d_cross_coupling_patient = cross_coupling_patient - v_cross_coupling_patient
  )



dt_vanBse  <-
  dplyr::mutate(
    dt_vanBse,
    delta_scpt = d_self_coupling_patient - mean(na.omit(d_self_coupling_patient)),
    delta_scpr = d_self_coupling_partner - mean(na.omit(d_self_coupling_partner)),
    delta_ccpt = d_cross_coupling_patient - mean(na.omit(d_cross_coupling_patient)),
    delta_ccpr = d_cross_coupling_partner - mean(na.omit(d_cross_coupling_partner))
  )

dt_vanBse_simp <-
  dplyr::select(
    dt_vanBse,
    scpt,
    ccpt,
    scpr,
    ccpr,
    iip_elcpt,
    iip_elcpr,
    pdtot_patient,
    pdtot_partner,
    iip_elevation_patient,
    iip_elevation_partner,
    v_scpt,
    v_scpr,
    v_ccpt,
    v_ccpr,
    delta_scpt,
    delta_ccpt,
    delta_scpr,
    delta_ccpr,
    pdcpt,
    pdcpr,
    copt,
    copr,
    iip_agency_patient,
    iip_agency_partner,
    iip_communion_patient,
    iip_communion_partner,
    PTNUM
  )

df_noOutliers <-
  dplyr::filter(
    df,
    self_coupling_patient > -.1,
    self_coupling_patient < .15,
    self_coupling_partner < .15,
    cross_coupling_patient < .06,
    cross_coupling_partner < .06
  ) #after cuts: n = 106
df_noOutliers_vanBse <-
  dplyr::inner_join(df_noOutliers, vanBse_paramsm4, by = "PTNUM") #n = 102; exclude 8115, 8093. 8078, 8065
df_noOutliers_vanBse <-
  dplyr::mutate(
    df_noOutliers_vanBse,
    scpt = 1000 * self_coupling_patient,
    scpr = 1000 * self_coupling_partner,
    ccpt = 1000 * cross_coupling_patient,
    ccpr = 1000 * cross_coupling_partner,
    v_scpt = 1000 * v_self_coupling_patient,
    v_scpr = 1000 * v_self_coupling_parnter,
    v_ccpt = 1000 * v_cross_coupling_patient,
    v_ccpr = 1000 * v_cross_coupling_partner,
    copt = 1000 * summed_cross_coupling_patient,
    copr = 1000 * summed_cross_coupling_partner
  )



df_noOutliers_vanBse <-
  df_noOutliers_vanBse %>% mutate(
    pdcpr = pdtot_partner - mean(na.omit(pdtot_partner)),
    iip_x_pdtot_partner = iip_elcpr * pdcpr,
    pdcpt = pdtot_patient - mean(na.omit(pdtot_patient)),
    iip_x_pdtot_patient = iip_elcpt * pdcpt
  )
df_noOutliers_vanBse <-
  dplyr::mutate(
    df_noOutliers_vanBse,
    d_self_coupling_partner = self_coupling_partner - v_self_coupling_parnter,
    d_self_coupling_patient = self_coupling_patient - v_self_coupling_patient,
    d_cross_coupling_partner = cross_coupling_partner - v_cross_coupling_partner,
    d_cross_coupling_patient = cross_coupling_patient - v_cross_coupling_patient
  )



df_noOutliers_vanBse  <-
  dplyr::mutate(
    df_noOutliers_vanBse,
    delta_scpt = d_self_coupling_patient - mean(na.omit(d_self_coupling_patient)),
    delta_scpr = d_self_coupling_partner - mean(na.omit(d_self_coupling_partner)),
    delta_ccpt = d_cross_coupling_patient - mean(na.omit(d_cross_coupling_patient)),
    delta_ccpr = d_cross_coupling_partner - mean(na.omit(d_cross_coupling_partner))
  )

df_noOutliers_vanBse_simp <-
  dplyr::select(
    df_noOutliers_vanBse,
    scpt,
    ccpt,
    scpr,
    ccpr,
    iip_elcpt,
    iip_elcpr,
    pdtot_patient,
    pdtot_partner,
    iip_elevation_patient,
    iip_elevation_partner,
    v_scpt,
    v_scpr,
    v_ccpt,
    v_ccpr,
    delta_scpt,
    delta_ccpt,
    delta_scpr,
    delta_ccpr,
    pdtot_patient,
    pdtot_partner,
    copt,
    copr,
    iip_communion_patient,
    iip_communion_partner,
    iip_agency_patient,
    iip_agency_partner,
    PTNUM
  )


dt_vanBse_simp_fewOutliers <-
  dplyr::filter(dt_vanBse_simp,
                abs(scpt) < 300,
                abs(scpr) < 300,
                abs(ccpt) < 300,
                abs(ccpr) < 300)

dt_vanBse_simp_noOutliers_ptnum_inclusion_spreadsheet <-
  dplyr::filter(
    dt_vanBse_simp,
    PTNUM != 8133,
    PTNUM != 8127,
    PTNUM != 8126,
    PTNUM != 8112,
    PTNUM != 8106,
    PTNUM != 8100,
    PTNUM != 8073,
    PTNUM != 8060,
    PTNUM != 8052,
    PTNUM != 8035,
    PTNUM != 8020,
    PTNUM != 8016
  )
#n = 104
dt_vanBse_simp_noWackies_ptnum_inclusion_spreadsheet <-
  dplyr::filter(
    dt_vanBse_simp,
    PTNUM != 8133,
    PTNUM != 8112,
    PTNUM != 8106,
    PTNUM != 8073,
    PTNUM != 8063,
    PTNUM != 8060,
    PTNUM != 8035,
    PTNUM != 8016
  )

dat1 = df_noOutliers_vanBse
dat2 = df_noOutliers
dat3 = dt_vanBse_simp
dat4 = dt_vanBse_simp_noOutliers_ptnum_inclusion_spreadsheet
dat5 = dt_vanBse_simp_noWackies_ptnum_inclusion_spreadsheet
dat6 = dt_vanBse_simp_fewOutliers

#now need to load prepost for SAAM, IMI and PANAS

#SAAM DATA
#read in param data
dt = read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/parameters_model3_modelcomparison_allData.csv", header = TRUE)
dt_patient <-
  dplyr::select(dt, self.coupling.patient, cross.coupling.patient, PTNUM)
dt_partner <-
  dplyr::select(dt, self.coupling.partner, cross.coupling.partner, PTNUM)

saampost <-
  read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/SAAMPOSTscored.csv")
saampre <-
  read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/SAAMPREscored.csv")
saamprescales <-
  dplyr::select(saampre, PTNUM, DyadID, ID, anx, avo, sec)
saamprescales <-
  dplyr::rename(saampre,
                anx_pre = anx,
                avo_pre = avo,
                sec_pre = sec)
saampostscales <-
  dplyr::select(saampost, PTNUM, DyadID, ID, anx, avo, sec)
saampostscales <-
  dplyr::rename(saampost,
                anx_post = anx,
                avo_post = avo,
                sec_post = sec)

saamprepostscales <-
  inner_join(saamprescales, saampostscales, by = "ID")
saamprepostscales_patient <-
  dplyr::filter(saamprepostscales, DyadID.x == 1)
saamprepostscales_partner <-
  dplyr::filter(saamprepostscales, DyadID.x == 0)
#dt = read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/parameters_model3_modelcomparison_allData.csv", header = TRUE)
saamprepostscales_partner <-
  dplyr::rename(saamprepostscales_partner, PTNUM = PTNUM.x)
saamprepostscales_patient <-
  dplyr::rename(saamprepostscales_patient, PTNUM = PTNUM.x)
saamprespostcales_dt_patient <-
  inner_join(saamprepostscales_patient, dt_patient, by = "PTNUM")
saamprepostscales_dt_partner <-
  inner_join(saamprepostscales_partner, dt_partner, by = "PTNUM")
saamprepostscales_dt <-
  inner_join(saamprespostcales_dt_patient,
             saamprepostscales_dt_partner,
             by = "PTNUM")
#panasprepostscales_sfbmc <- inner_join(panasprepostscales_sfbmc_patient, panasprepostscales_partner, by = "PTNUM")
saamprepostscales_dt <-
  dplyr::rename(
    saamprepostscales_dt,
    anx_pre_patient = anx_pre.x,
    anx_post_patient = anx_post.x,
    avo_pre_patient = avo_pre.x,
    avo_post_patient = avo_post.x,
    sec_pre_patient = sec_pre.x,
    sec_post_patient = sec_post.x,
    anx_pre_partner =  anx_pre.y,
    anx_post_partner = anx_post.y,
    avo_pre_partner = avo_pre.y,
    avo_post_partner = avo_post.y,
    sec_pre_partner = sec_pre.y,
    sec_post_partner = sec_post.y
  )
saamprepostscales_dt <-
  dplyr::rename(
    saamprepostscales_dt,
    self_coupling_patient = self.coupling.patient,
    cross_coupling_patient = cross.coupling.patient,
    self_coupling_partner = self.coupling.partner,
    cross_coupling_partner = cross.coupling.partner
  )
ll <- dplyr::select(dt, LL, PTNUM)
saamprepostscales_dt <-
  inner_join(saamprepostscales_dt, ll, by = "PTNUM")
saamprepostscales_dt <-
  dplyr::filter(saamprepostscales_dt, LL > -69999) %>% dplyr::filter(
    abs(self_coupling_patient) < 1 &
      abs(cross_coupling_patient)  < 1 &
      abs(self_coupling_partner) < 1 & abs(cross_coupling_partner) < 1
  )
saamprepostscales_dt_vanBse <-
  dplyr::inner_join(saamprepostscales_dt, vanBse_paramsm4, by = "PTNUM")
saamprepostscales_dt_vanBse <-
  dplyr::mutate(
    saamprepostscales_dt_vanBse,
    scpt = 1000 * self_coupling_patient,
    scpr = 1000 * self_coupling_partner,
    ccpt = 1000 * cross_coupling_patient,
    ccpr = 1000 * cross_coupling_partner,
    v_scpt = 1000 * v_self_coupling_patient,
    v_scpr = 1000 * v_self_coupling_parnter,
    v_ccpt = 1000 * v_cross_coupling_patient,
    v_ccpr = 1000 * v_cross_coupling_partner
  )
#8144, 8133, 8112, 8106, 8104, 8073, 8063, 8060, 8040, 8035, 8016
#8126, 8127, 8100, 8052, 8020

saamprepostscales_dt_vanBse_noOutliers_ptnum_inclusion_spreadsheet <-
  dplyr::filter(
    saamprepostscales_dt_vanBse,
    PTNUM != 8144,
    PTNUM != 8133,
    PTNUM != 8112,
    PTNUM != 8106,
    PTNUM != 8104,
    PTNUM != 8073,
    PTNUM != 8063,
    PTNUM != 8060,
    PTNUM != 8040,
    PTNUM != 8035,
    PTNUM != 8016,
    PTNUM != 8126,
    PTNUM != 8127,
    PTNUM != 8100,
    PTNUM != 8052,
    PTNUM != 8020
  )

saamprepostscales_dt_vanBse_noWackies_ptnum_inclusion_spreadsheet <-
  dplyr::filter(
    saamprepostscales_dt_vanBse,
    PTNUM != 8144,
    PTNUM != 8133,
    PTNUM != 8112,
    PTNUM != 8106,
    PTNUM != 8104,
    PTNUM != 8073,
    PTNUM != 8063,
    PTNUM != 8060,
    PTNUM != 8040,
    PTNUM != 8035,
    PTNUM != 8016
  )





#IMI
imicpre <-
  read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/IMICPREscored.csv")
imicpost <-
  read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/IMICPOSTscored.csv")
imicprescales <-
  dplyr::select(imicpre, D, HD, H, HS, S, FS, octF, FD, PTNUM, DyadID, UsrID)
imicprescales <-
  dplyr::rename(
    imicprescales,
    D_pre = D,
    HD_pre = HD,
    H_pre = H,
    HS_pre = HS,
    S_pre = S,
    FS_pre = FS,
    F_pre = octF,
    FD_pre = FD
  )
imicpostscales <-
  dplyr::select(imicpost, D, HD, H, HS, S, FS, octF, FD, PTNUM, DyadID, UsrID)
imicpostscales <-
  dplyr::rename(
    imicpostscales,
    D_post = D,
    HD_post = HD,
    H_post = H,
    HS_post = HS,
    S_post = S,
    FS_post = FS,
    F_post = octF,
    FD_post = FD
  )
imicprepostscales <-
  dplyr::inner_join(imicprescales, imicpostscales, by = "UsrID")
imicprepostscales_patient <-
  dplyr::filter(imicprepostscales, DyadID.x == 1)
imicprepostscales_partner <-
  dplyr::filter(imicprepostscales, DyadID.x == 0)
imicprepostscales_partner <-
  dplyr::rename(imicprepostscales_partner, PTNUM = PTNUM.x)
imicprepostscales_patient <-
  dplyr::rename(imicprepostscales_patient, PTNUM = PTNUM.x)
imicprepostscales_dt_partner <-
  inner_join(imicprepostscales_partner, dt_partner, by = "PTNUM")
imicprepostscales_dt_patient <-
  inner_join(imicprepostscales_patient, dt_patient, by = "PTNUM")
imicprepostscales_dt <-
  inner_join(imicprepostscales_dt_patient,
             imicprepostscales_dt_partner,
             by = "PTNUM")
####now need to rename things
imicprepostscales_dt <-
  dplyr::rename(
    imicprepostscales_dt,
    D_pre_patient = D_pre.x ,
    H_pre_patient = H_pre.x,
    HD_pre_patient = HD_pre.x,
    HS_pre_patient = HS_pre.x,
    S_pre_patient = S_pre.x,
    FS_pre_patient = FS_pre.x,
    F_pre_patient = F_pre.x,
    FD_pre_patient = FD_pre.x,
    D_pre_partner = D_pre.y,
    H_pre_partner = H_pre.y,
    HD_pre_partner = HD_pre.y,
    HS_pre_partner = HS_pre.y,
    S_pre_partner = S_pre.y,
    FS_pre_partner = FS_pre.y,
    F_pre_partner = F_pre.y,
    FD_pre_partner = FD_pre.y,
    D_post_patient = D_post.x,
    H_post_patient = H_post.x,
    HD_post_patient = HD_post.x,
    HS_post_patient = HS_post.x,
    S_post_patient = S_post.x,
    FS_post_patient = FS_post.x,
    F_post_patient = F_post.x,
    FD_post_patient = FD_post.x,
    D_post_partner = D_post.y,
    H_post_partner = H_post.y,
    HD_post_partner = HD_post.y,
    HS_post_partner = HS_post.y,
    S_post_partner = S_post.y,
    FS_post_partner = FS_post.y,
    F_post_partner = F_post.y,
    FD_post_partner = FD_post.y
  )
imicprepostscales_dt <-
  dplyr::rename(
    imicprepostscales_dt,
    self_coupling_patient = self.coupling.patient,
    cross_coupling_patient = cross.coupling.patient,
    self_coupling_partner = self.coupling.partner,
    cross_coupling_partner = cross.coupling.partner
  )
#115 couples at this poing
#if ran SAAM prepost then don't need to run this
ll <- select(dt, LL, PTNUM)
imicprepostscales_dt <-
  inner_join(imicprepostscales_dt, ll, by = "PTNUM")
imicprepostscales_dt <-
  dplyr::filter(imicprepostscales_dt, LL > -69999) %>% dplyr::filter(
    abs(self_coupling_patient) < 1 &
      abs(cross_coupling_patient) < 1 &
      abs(self_coupling_partner) < 1 & abs(cross_coupling_partner) < 1
  )
imicprepostscales_dt <-
  mutate(
    imicprepostscales_dt,
    CON_pre_patient = D_pre_patient - S_pre_patient + .707 * (HD_pre_patient +
                                                                FD_pre_patient) - .707 * (HS_pre_patient + FS_pre_patient),
    AFF_pre_patient = F_pre_patient - H_pre_patient + .707 *
      (FD_pre_patient + FS_pre_patient) - .707 * (HD_pre_patient + HS_pre_patient),
    CON_pre_partner = D_pre_partner - S_pre_partner +
      .707 * (HD_pre_partner + FD_pre_partner) - .707 * (HS_pre_partner + FS_pre_partner),
    AFF_pre_partner = F_pre_partner - H_pre_partner + .707 *
      (FD_pre_partner + FS_pre_partner) - .707 * (HD_pre_partner + HS_pre_partner),
    CON_post_patient = D_post_patient - S_post_patient +
      .707 * (HD_post_patient + FD_post_patient) - .707 * (HS_post_patient + FS_post_patient),
    AFF_post_patient = F_post_patient - H_post_patient + .707 *
      (FD_post_patient + FS_post_patient) - .707 * (HD_post_patient + HS_post_patient),
    CON_post_partner = D_post_partner - S_post_partner +
      .707 * (HD_post_partner + FD_post_partner) - .707 * (HS_post_partner + FS_post_partner),
    AFF_post_partner = F_post_partner - H_post_partner + .707 *
      (FD_post_partner + FS_post_partner) - .707 * (HD_post_partner + HS_post_partner)
  )

imicprepostscales_dt_vanBse <-
  dplyr::inner_join(imicprepostscales_dt, vanBse_paramsm4, by = "PTNUM")
imicprepostscales_dt_vanBse <-
  dplyr::mutate(
    imicprepostscales_dt_vanBse,
    scpt = 100 * self_coupling_patient,
    scpr = 100 * self_coupling_partner,
    ccpt = 100 * cross_coupling_patient,
    ccpr = 100 * cross_coupling_partner,
    v_scpt = 100 * v_self_coupling_patient,
    v_scpr = 100 * v_self_coupling_parnter,
    v_ccpt = 100 * v_cross_coupling_patient,
    v_ccpr = 100 * v_cross_coupling_partner
  )



imicprepostscales_dt_vanBse_noOutliers_ptnum_inclusion_spreadsheet <-
  dplyr::filter(
    imicprepostscales_dt_vanBse,
    PTNUM != 8144,
    PTNUM != 8133,
    PTNUM != 8112,
    PTNUM != 8106,
    PTNUM != 8104,
    PTNUM != 8073,
    PTNUM != 8063,
    PTNUM != 8060,
    PTNUM != 8040,
    PTNUM != 8035,
    PTNUM != 8016,
    PTNUM != 8126,
    PTNUM != 8127,
    PTNUM != 8100,
    PTNUM != 8052,
    PTNUM != 8020
  )

imicprepostscales_dt_vanBse_noWackies_ptnum_inclusion_spreadsheet <-
  dplyr::filter(
    imicprepostscales_dt_vanBse,
    PTNUM != 8144,
    PTNUM != 8133,
    PTNUM != 8112,
    PTNUM != 8106,
    PTNUM != 8104,
    PTNUM != 8073,
    PTNUM != 8063,
    PTNUM != 8060,
    PTNUM != 8040,
    PTNUM != 8035,
    PTNUM != 8016
  )




#PANAS

panaspre <-
  read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/PANASPREscored.csv")
panaspost <-
  read.csv(file = "~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/interaction_selfreports/PANASPOSTscored.csv")
panasprescales <- dplyr::select(panaspre, PTNUM, DyadID, ID, pa, na)
panasprescales <-
  dplyr::rename(panasprescales, pa_pre = pa, na_pre = na)
panaspostscales <-
  dplyr::select(panaspost, PTNUM, DyadID, ID, pa, na)
panaspostscales <-
  dplyr::rename(panaspostscales, pa_post = pa, na_post = na)

panasprepostscales <-
  inner_join(panasprescales, panaspostscales, by = "ID")
panasprepostscales_patient <-
  dplyr::filter(panasprepostscales, DyadID.x == 1)
panasprepostscales_partner <-
  dplyr::filter(panasprepostscales, DyadID.x == 0)
panasprepostscales_partner <-
  dplyr::rename(panasprepostscales_partner, PTNUM = PTNUM.x)
panasprepostscales_patient <-
  dplyr::rename(panasprepostscales_patient, PTNUM = PTNUM.x)
panasprespostcales_dt_patient <-
  inner_join(panasprepostscales_patient, dt_patient, by = "PTNUM")

panasprepostscales_dt_partner <-
  inner_join(panasprepostscales_partner, dt_partner, by = "PTNUM")
panasprepostscales_dt <-
  inner_join(panasprespostcales_dt_patient,
             panasprepostscales_dt_partner,
             by = "PTNUM")
#panasprepostscales_sfbmc <- inner_join(panasprepostscales_sfbmc_patient, panasprepostscales_partner, by = "PTNUM")
panasprepostscales_dt <-
  dplyr::rename(
    panasprepostscales_dt,
    pa_pre_patient = pa_pre.x,
    na_pre_patient = na_pre.x,
    pa_post_patient = pa_post.x,
    na_post_patient =  na_post.x,
    pa_pre_partner = pa_pre.y,
    na_pre_partner = na_pre.y,
    pa_post_partner = pa_post.y,
    na_post_partner =  na_post.y
  )
panasprepostscales_dt <-
  dplyr::rename(
    panasprepostscales_dt,
    self_coupling_patient = self.coupling.patient,
    cross_coupling_patient = cross.coupling.patient,
    self_coupling_partner = self.coupling.partner,
    cross_coupling_partner = cross.coupling.partner
  )
ll <- as.data.frame(vanBse_params$logEvidence[4, ])
ll$ll <- as.vector(vanBse_params$logEvidence[4, ])
ll$PTNUM <- as.vector(vanBse_params$ids)
panasprepostscales_dt <-
  inner_join(panasprepostscales_dt, ll, by = "PTNUM") #at this point, between already cutting out people below -7999 and the panas data tha tis available, only 116
panasprepostscales_dt <-
  dplyr::filter(panasprepostscales_dt, ll > -69999) %>% dplyr::filter(
    abs(self_coupling_patient) < 1 &
      abs(cross_coupling_patient)  < 1 &
      abs(self_coupling_partner) < 1 &
      abs(cross_coupling_partner) < 1
  ) #2 people cut with this
panasprepostscales_dt_vanBse <-
  dplyr::inner_join(panasprepostscales_dt, vanBse_paramsm4, by = "PTNUM")
panasprepostscales_dt_vanBse <-
  dplyr::mutate(
    panasprepostscales_dt_vanBse,
    scpt = 1000 * self_coupling_patient,
    scpr = 1000 * self_coupling_partner,
    ccpt = 1000 * cross_coupling_patient,
    ccpr = 1000 * cross_coupling_partner,
    v_scpt = 1000 * v_self_coupling_patient,
    v_scpr = 1000 * v_self_coupling_parnter,
    v_ccpt = 1000 * v_cross_coupling_patient,
    v_ccpr = 1000 * v_cross_coupling_partner
  )


panasprepostscales_dt_vanBse_noOutliers_ptnum_inclusion_spreadsheet <-
  dplyr::filter(
    panasprepostscales_dt_vanBse,
    PTNUM != 8144,
    PTNUM != 8133,
    PTNUM != 8112,
    PTNUM != 8106,
    PTNUM != 8104,
    PTNUM != 8073,
    PTNUM != 8063,
    PTNUM != 8060,
    PTNUM != 8040,
    PTNUM != 8035,
    PTNUM != 8016,
    PTNUM != 8126,
    PTNUM != 8127,
    PTNUM != 8100,
    PTNUM != 8052,
    PTNUM != 8020
  )

panasprepostscales_dt_vanBse_noWackies_ptnum_inclusion_spreadsheet <-
  dplyr::filter(
    panasprepostscales_dt_vanBse,
    PTNUM != 8144,
    PTNUM != 8133,
    PTNUM != 8112,
    PTNUM != 8106,
    PTNUM != 8104,
    PTNUM != 8073,
    PTNUM != 8063,
    PTNUM != 8060,
    PTNUM != 8040,
    PTNUM != 8035,
    PTNUM != 8016
  )
dat7 = saamprepostscales_dt_vanBse
dat8 = saamprepostscales_dt_vanBse_noOutliers_ptnum_inclusion_spreadsheet
dat9 = saamprepostscales_dt_vanBse_noWackies_ptnum_inclusion_spreadsheet
dat10 = imicprepostscales_dt_vanBse
dat11 = imicprepostscales_dt_vanBse_noOutliers_ptnum_inclusion_spreadsheet
dat12 = imicprepostscales_dt_vanBse_noWackies_ptnum_inclusion_spreadsheet
dat13 = panasprepostscales_dt_vanBse
dat14 = panasprepostscales_dt_vanBse_noOutliers_ptnum_inclusion_spreadsheet
dat15 = panasprepostscales_dt_vanBse_noWackies_ptnum_inclusion_spreadsheet


#2. Load in the models
#for dat1-dat6

m1 <- "
scpt ~ iip_elevation_patient
ccpt ~ iip_elevation_patient
scpr ~ iip_elevation_partner
ccpr ~ iip_elevation_partner

"

m2 <- "
scpt ~ iip_elevation_patient + v_scpt
scpr ~ iip_elevation_partner + v_scpr
ccpt ~ iip_elevation_patient + v_ccpt
ccpr ~ iip_elevation_partner+ v_ccpr




"

#m2dat4, m2dat5, m2dat4thr2, m2dat5thr2 are all the same data set and seem to be pretty consistent and good
m3 <- "
scpt ~ iip_elevation_patient + v_scpt
scpr ~ iip_elevation_partner + v_scpr
ccpt ~ iip_elevation_patient + v_ccpt
ccpr ~ iip_elevation_partner+ v_ccpr
scpr ~ v_ccpr
ccpr ~ v_scpr
ccpr ~ v_scpt
ccpr ~ v_ccpt
"
#this model works yay!! -- some of the effects went away...

m4 <- "
scpt ~ iip_agency_patient
ccpt ~ iip_agency_patient
scpr ~ iip_agency_partner
ccpr ~ iip_agency_partner

"

m5 <- "
scpt ~ iip_agency_patient + v_scpt
scpr ~ iip_agency_partner + v_scpr
ccpt ~ iip_agency_patient + v_ccpt
ccpr ~ iip_agency_partner+ v_ccpr


"



m7 <- "
scpt ~ iip_communion_patient
ccpt ~ iip_communion_patient
scpr ~ iip_communion_partner
ccpr ~ iip_communion_partner

"

m8 <- "
scpt ~ iip_communion_patient + v_scpt
scpr ~ iip_communion_partner + v_scpr
ccpt ~ iip_communion_patient + v_ccpt
ccpr ~ iip_communion_partner+ v_ccpr

"

m10 <- "
scpt ~ pdtot_patient
scpr ~ pdtot_partner
ccpt ~ pdtot_patient
ccpr ~ pdtot_partner
"

m11 <- "
scpt ~ pdtot_patient + v_scpt
scpr ~ pdtot_partner + v_scpr
ccpt ~ pdtot_patient + v_ccpt
ccpr ~ pdtot_partner + v_ccpr
"

m13 <- "
scpt ~ pdtot_patient + iip_elevation_patient
scpr ~ pdtot_partner + iip_elevation_partner
ccpt ~ pdtot_patient + iip_elevation_patient
ccpr ~ pdtot_partner + iip_elevation_partner
"
m14 <- "
scpt ~ pdtot_patient + iip_elevation_patient + v_scpt
scpr ~ pdtot_partner + iip_elevation_partner + v_scpr
ccpt ~ pdtot_patient + iip_elevation_patient + v_ccpt
ccpr ~ pdtot_partner + iip_elevation_partner + v_ccpr

"
m15 <- "
scpt ~ pdtot_patient + iip_elevation_patient + v_scpt
scpr ~ pdtot_partner + iip_elevation_partner + v_scpr
ccpt ~ pdtot_patient + iip_elevation_patient + v_ccpt
ccpr ~ pdtot_partner + iip_elevation_partner + v_ccpr
scpr ~ v_ccpr
ccpt ~ pdtot_partner
ccpr ~ v_scpr
ccpr ~ iip_elevation_patient


"
m2dat5_forplotting <-
  dplyr::select(
    m2dat5,
    scpt,
    scpr,
    ccpt,
    ccpr,
    pdtot_patient,
    pdtot_partner,
    iip_elevation_patient,
    iip_elevation_partner,
    v_scpt,
    v_scpr,
    v_ccpt,
    v_ccpr
  )
m2dat5_forplotting$elptresid <-
  residuals(lm(iip_elevation_patient ~ v_scpt + v_ccpt, data = m2dat5_forplotting))
m2dat5_forplotting$elprresid <-
  residuals(lm(iip_elevation_partner ~ v_scpr + v_ccpr, data = m2dat5_forplotting))
m2dat5_forplotting$pdcptresid <-
  residuals(lm(pdtot_patient ~ v_scpt + v_ccpt, data = m2dat5_forplotting))
m2dat5_forplotting$pdcprresid <-
  residuals(lm(pdtot_partner ~ v_scpr + v_ccpr, data = m2dat5_forplotting))
m2dat5_forplotting$scptresid <-
  residuals(lm(scpt ~ v_scpt, data = m2dat5_forplotting))
m2dat5_forplotting$scprresid <-
  residuals(lm(scpr ~ v_scpr, data = m2dat5_forplotting))
m2dat5_forplotting$ccptresid <-
  residuals(lm(ccpt ~ v_ccpt, data = m2dat5_forplotting))
m2dat5_forplotting$ccprresid <-
  residuals(lm(ccpr ~ v_ccpr, data = m2dat5_forplotting))
m2dat5_forplotting$coptresid <-
  with(m2dat5_forplotting,-scptresid + ccptresid)
m2dat5_forplotting$coprresid <-
  with(m2dat5_forplotting,-scprresid + ccprresid)
g <- ggplot(m2dat5_forplotting, aes(x = elptresid, y = coptresid))
g + geom_point() + geom_smooth(method = "lm") + ggtitle("The Relationship between Interpersonal Problems and Coregulation Style in Patients") + xlab("Interpersonal Problems") + ylab("Coregulation Style (from Contrarian to Dependent") +
  
  
  m16 <- saam_sccc_prpt_avo <- "
#direct effect
avo_post_partner ~ c1*avo_pre_partner
#mediators
avo_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*avo_pre_partner
scpt~a2*avo_pre_partner
ccpt~a3*avo_pre_partner
ccpr~a4*avo_pre_partner

#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4

avo_post_patient ~ c2*avo_pre_patient
avo_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*avo_pre_patient
scpr~a6*avo_pre_patient
ccpr~a7*avo_pre_patient
ccpt~a8*avo_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"
m17 <- saam_sccc_prpt_avo_vanBse <- "
#direct effect
avo_post_partner ~ c1*avo_pre_partner
#mediators
avo_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*avo_pre_partner
scpt~a2*avo_pre_partner
ccpt~a3*avo_pre_partner
ccpr~a4*avo_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4

avo_post_patient ~ c2*avo_pre_patient
avo_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*avo_pre_patient
scpr~a6*avo_pre_patient
ccpr~a7*avo_pre_patient
ccpt~a8*avo_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"



m19 <- saam_sccc_prpt_anx_vanBse <- "
#direct effect
anx_post_partner ~ c1*anx_pre_partner
#mediators
anx_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*anx_pre_partner
scpt~a2*anx_pre_partner
ccpt~a3*anx_pre_partner
ccpr~a4*anx_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4

anx_post_patient ~ c2*anx_pre_patient
anx_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*anx_pre_patient
scpr~a6*anx_pre_patient
ccpr~a7*anx_pre_patient
ccpt~a8*anx_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"
m20 <- saam_sccc_prpt_anx <- "
#direct effect
anx_post_partner ~ c1*anx_pre_partner
#mediators
anx_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*anx_pre_partner
scpt~a2*anx_pre_partner
ccpt~a3*anx_pre_partner
ccpr~a4*anx_pre_partner

#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4

anx_post_patient ~ c2*anx_pre_patient
anx_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*anx_pre_patient
scpr~a6*anx_pre_patient
ccpr~a7*anx_pre_patient
ccpt~a8*anx_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"



# m19 <- saam_sccc_prpt_anx_vanBse <- "
# #direct effect
# anx_post_partner ~ c1*anx_pre_partner
# #mediators
# anx_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
# scpr ~ a1*anx_pre_partner
# scpt~a2*anx_pre_partner
# ccpt~a3*anx_pre_partner
# ccpr~a4*anx_pre_partner
# scpt ~ v_scpt
# ccpt ~ v_ccpt
# scpr ~ v_scpr
# ccpr ~ v_ccpr
# #indirect effect
# a1b1 := a1*b1
# a2b2 := a2*b2
# a3b3:=a3*b3
# a4b4:=a4*b4
#
# anx_post_patient ~ c2*anx_pre_patient
# anx_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
# scpt ~ a5*anx_pre_patient
# scpr~a6*anx_pre_patient
# ccpr~a7*anx_pre_patient
# ccpt~a8*anx_pre_patient
# a5b5 := a5*b5
# a6b6 := a6*b6
# a7b7 := a7*b7
# a8b8 := a8*b8
#
# scpt~~ccpt
# scpr~~ccpr
# scpt~~ccpr
# scpr~~ccpt
# scpt~~scpr
# ccpt~~ccpr
# "
# m22 <- imic_sccc_prpt_CON <- "
# #direct effect
# CON_post_partner ~ c1*CON_pre_partner
# #mediators
# CON_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
# scpr ~ a1*CON_pre_partner
# scpt~a2*CON_pre_partner
# ccpt~a3*CON_pre_partner
# ccpr~a4*CON_pre_partner
# #indirect effect
# a1b1 := a1*b1
# a2b2 := a2*b2
# a3b3:=a3*b3
# a4b4:=a4*b4
#
# CON_post_patient ~ c2*CON_pre_patient
# CON_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
# scpt ~ a5*CON_pre_patient
# scpr~a6*CON_pre_patient
# ccpr~a7*CON_pre_patient
# ccpt~a8*anx_pre_patient
# a5b5 := a5*b5
# a6b6 := a6*b6
# a7b7 := a7*b7
# a8b8 := a8*b8
#
# scpt~~ccpt
# scpr~~ccpr
# scpt~~ccpr
# scpr~~ccpt
# scpt~~scpr
# ccpt~~ccpr
# "

m22 <- imic_sccc_prpt_CON_vanBse <- "
#direct effect
CON_post_partner ~ c1*CON_pre_partner
#mediators
CON_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*CON_pre_partner
scpt~a2*CON_pre_partner
ccpt~a3*CON_pre_partner
ccpr~a4*CON_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4

CON_post_patient ~ c2*CON_pre_patient
CON_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*CON_pre_patient
scpr~a6*CON_pre_patient
ccpr~a7*CON_pre_patient
ccpt~a8*CON_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"

m23 <- imic_sccc_prpt_CON <- "
#direct effect
CON_post_partner ~ c1*CON_pre_partner
#mediators
CON_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*CON_pre_partner
scpt~a2*CON_pre_partner
ccpt~a3*CON_pre_partner
ccpr~a4*CON_pre_partner

#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4

CON_post_patient ~ c2*CON_pre_patient
CON_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*CON_pre_patient
scpr~a6*CON_pre_patient
ccpr~a7*CON_pre_patient
ccpt~a8*CON_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"
m24 <- "

#direct effect
CON_post_partner ~ c1*CON_pre_partner
#mediators
CON_post_partner ~  b1*scpt +  b2*ccpt
scpt~a1*CON_pre_partner
ccpt~a2*CON_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
#indirect effect
a1b1 := a1*b1
a2b2:=a2*b2
a1b2 := a1*b2
a2b1 := a2*b1
dw1b1 := dw1*b1

CON_post_patient ~ c2*CON_pre_patient
CON_post_partner ~ CON_pre_patient
#CON_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ dw1*CON_pre_patient
scpr~CON_pre_patient
ccpr~CON_pre_patient

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr


"
m25 <- imic_sccc_prpt_AFF_vanBse <- "
#direct effect
AFF_post_partner ~ c1*AFF_pre_partner
#mediators
AFF_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*AFF_pre_partner
scpt~a2*AFF_pre_partner
ccpt~a3*AFF_pre_partner
ccpr~a4*AFF_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4

AFF_post_patient ~ c2*AFF_pre_patient
AFF_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*AFF_pre_patient
scpr~a6*AFF_pre_patient
ccpr~a7*AFF_pre_patient
ccpt~a8*AFF_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"

m26 <- imic_sccc_prpt_AFF <- "
#direct effect
AFF_post_partner ~ c1*AFF_pre_partner
#mediators
AFF_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*AFF_pre_partner
scpt~a2*AFF_pre_partner
ccpt~a3*AFF_pre_partner
ccpr~a4*AFF_pre_partner

#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4

AFF_post_patient ~ c2*AFF_pre_patient
AFF_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*AFF_pre_patient
scpr~a6*AFF_pre_patient
ccpr~a7*AFF_pre_patient
ccpt~a8*AFF_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"

m27 <- "#direct effect
AFF_post_partner ~ c1*AFF_pre_partner
#mediators
AFF_post_partner ~ b1*scpt + b2*ccpt + scpr
scpt~a1*AFF_pre_partner
ccpt~a2*AFF_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
ccpt ~ v_ccpr
ccpr ~ v_scpr
#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2

AFF_post_patient ~ c2*AFF_pre_patient
AFF_post_patient ~  b7*ccpr + b8*ccpt
scpt ~ AFF_pre_patient
ccpr~a7*AFF_pre_patient
ccpt~a8*AFF_pre_patient
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"
m28 <- panas_sccc_prpt_na_vanBse <- "
#direct effect
na_post_partner ~ c1*na_pre_partner
#mediators
na_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*na_pre_partner
scpt~a2*na_pre_partner
ccpt~a3*na_pre_partner
ccpr~a4*na_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4

na_post_patient ~ c2*na_pre_patient
na_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*na_pre_patient
scpr~a6*na_pre_patient
ccpr~a7*na_pre_patient
ccpt~a8*na_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"
m29 <- panas_sccc_prpt_na <- "
#direct effect
na_post_partner ~ c1*na_pre_partner
#mediators
na_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*na_pre_partner
scpt~a2*na_pre_partner
ccpt~a3*na_pre_partner
ccpr~a4*na_pre_partner

#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4

na_post_patient ~ c2*na_pre_patient
na_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*na_pre_patient
scpr~a6*na_pre_patient
ccpr~a7*na_pre_patient
ccpt~a8*na_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"

m30 <- panas_sccc_prpt_na_vanBse <- "
#direct effect
na_post_partner ~ c1*na_pre_partner
#mediators
na_post_partner ~ b1*scpr + b2*scpt + b3*ccpr + b4*ccpt
scpr ~ a1*na_pre_partner
scpt~a2*na_pre_partner
ccpt~a3*na_pre_partner
ccpr~a4*na_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
ccpr ~ v_scpr
#indirect effect
a1b1 := a1*b1
a2b2 := a2*b2
a3b3:=a3*b3
a4b4:=a4*b4
a5b2 := a5*b2
na_post_partner ~ na_pre_patient
na_post_patient ~ c2*na_pre_patient
na_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ a5*na_pre_patient
scpr~a6*na_pre_patient
ccpr~a7*na_pre_patient
ccpt~a8*na_pre_patient
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8b8 := a8*b8

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
"


#3. Multivariate Outlier Detection. Vary the model, data set, and threshhold for gCD (4 times the average gCD is a good measure apparently)



#sub out modeltofit, datatofit, xpt, xpr
#modeltofit: m1, 2, 4, 5, 7, 8, 10, 11, 13, 14 for dat1-6

modeltofit = m1
datatofit = dat1
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat1toexclude = allbadcases
m1dat1 = dplyr::filter(dat1,!(PTNUM %in% m1dat1toexclude))
#error
######################################################RUN
modeltofit = m2
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)

sem(
  modeltofit,
  datatofit,
  missing = "listwise",
  estimator = "ML",
  mimic = "Mplus",
  meanstructure = TRUE,
  conditional.x = TRUE
)

ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat1toexclude = allbadcases
m2dat1 = dplyr::filter(dat1,!(PTNUM %in% m2dat1toexclude))

########################################################Run -- worked
modeltofit = m4
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat1toexclude = allbadcases
m4dat1 = dplyr::filter(dat1,!(PTNUM %in% m4dat1toexclude))
##########################################run-worked
modeltofit = m5
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat1toexclude = allbadcases
m5dat1 = dplyr::filter(dat1,!(PTNUM %in% m5dat1toexclude))
#run!!

modeltofit = m7
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
#run -- did not work
m7dat1toexclude = allbadcases
m7dat1 = dplyr::filter(dat1,!(PTNUM %in% m7dat1toexclude))

modeltofit = m8
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}


#run -- worked
m8dat1toexclude = allbadcases
m8dat1 = dplyr::filter(dat1,!(PTNUM %in% m8dat1toexclude))


modeltofit = m10
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat1toexclude = allbadcases
m10dat1 = dplyr::filter(dat1,!(PTNUM %in% m10dat1toexclude))
#run -- worked

modeltofit = m11
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
#run -worked
m11dat1toexclude = allbadcases
m11dat1 = dplyr::filter(dat1,!(PTNUM %in% m11dat1toexclude))



modeltofit = m13
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat1toexclude = allbadcases
m13dat1 = dplyr::filter(dat1,!(PTNUM %in% m13dat1toexclude))
#run -- did not work
modeltofit = m14
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat1toexclude = allbadcases
m14dat1 = dplyr::filter(dat1,!(PTNUM %in% m14dat1toexclude))

#run worked

#DAT 2
m1dat2_m <- "
self_coupling_patient ~ iip_elevation_patient
cross_coupling_patient ~ iip_elevation_patient
self_coupling_partner ~ iip_elevation_partner
cross_coupling_partner ~ iip_elevation_partner

"


modeltofit = m1dat2_m
datatofit = dat2
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                  filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                  filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat2toexclude = allbadcases
m1dat2 = dplyr::filter(dat2,!(PTNUM %in% m1dat2toexclude))
#run did not work
modeltofit = m2
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                  filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                  filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat2toexclude = allbadcases
m2dat2 = dplyr::filter(dat2,!(PTNUM %in% m2dat2toexclude))
#model doesn't work
m4dat2_m <- "
self_coupling_patient ~ iip_agency_patient
cross_coupling_patient ~ iip_agency_patient
self_coupling_partner ~ iip_agency_partner
cross_coupling_partner ~ iip_agency_partner

"

modeltofit = m4dat2_m
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = iip_agency_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                               filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = iip_agency_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                               filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = iip_agency_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = iip_agency_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat2toexclude = allbadcases
m4dat2 = dplyr::filter(dat2,!(PTNUM %in% m4dat2toexclude))
#run -- didn't work
modeltofit = m5
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat2toexclude = allbadcases
m5dat2 = dplyr::filter(dat2,!(PTNUM %in% m5dat2toexclude))
#model won't work
m7dat2_m <- "
self_coupling_patient ~ pdtot_patient
cross_coupling_patient ~ pdtot_patient
self_coupling_partner ~ pdtot_partner
cross_coupling_partner ~ pdtot_partner
"

modeltofit = m7dat2_m
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = iip_communion_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                  filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = iip_communion_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                  filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = iip_communion_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = iip_communion_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat2toexclude = allbadcases
m7dat2 = dplyr::filter(dat2,!(PTNUM %in% m7dat2toexclude))
#run- did not work
modeltofit = m8
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat2toexclude = allbadcases
m8dat2 = dplyr::filter(dat2,!(PTNUM %in% m8dat2toexclude))
#did not work
m10dat2_m <- "

self_coupling_patient ~ pdtot_patient
cross_coupling_patient ~ pdtot_patient
self_coupling_partner ~ pdtot_partner
cross_coupling_partner ~ pdtot_partner


"
modeltofit = m10dat2_m
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = pdtot_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                          filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = pdtot_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                          filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = pdtot_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = pdtot_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat2toexclude = allbadcases
m10dat2 = dplyr::filter(dat2,!(PTNUM %in% m10dat2toexclude))

modeltofit = m11
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat2toexclude = allbadcases
m11dat2 = dplyr::filter(dat2,!(PTNUM %in% m11dat2toexclude))


m13dat2_m <- "
self_coupling_patient ~ pdtot_patient + iip_elevation_patient
self_coupling_partner ~ pdtot_partner + iip_elevation_partner
cross_coupling_patient ~ pdtot_patient + iip_elevation_patient
cross_coupling_partner ~ pdtot_partner + iip_elevation_partner

"
modeltofit = m13dat2_m
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = pdtot_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                          filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = pdtot_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                          filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = pdtot_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = pdtot_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat2toexclude = allbadcases
m13dat2 = dplyr::filter(dat2,!(PTNUM %in% m13dat2toexclude))

modeltofit = m14
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat2toexclude = allbadcases
m14dat2 = dplyr::filter(dat2,!(PTNUM %in% m14dat2toexclude))



modeltofit = m1
datatofit = dat3
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat3toexclude = allbadcases
m1dat3 = dplyr::filter(dat3,!(PTNUM %in% m1dat3toexclude))

modeltofit = m2
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat3toexclude = allbadcases
m2dat3 = dplyr::filter(dat3,!(PTNUM %in% m2dat3toexclude))


modeltofit = m4
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat3toexclude = allbadcases
m4dat3 = dplyr::filter(dat3,!(PTNUM %in% m4dat3toexclude))

modeltofit = m5
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat3toexclude = allbadcases
m5dat3 = dplyr::filter(dat3,!(PTNUM %in% m5dat3toexclude))


modeltofit = m7
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat3toexclude = allbadcases
m7dat3 = dplyr::filter(dat3,!(PTNUM %in% m7dat3toexclude))

modeltofit = m8
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat3toexclude = allbadcases
m8dat3 = dplyr::filter(dat3,!(PTNUM %in% m8dat3toexclude))


modeltofit = m10
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat3toexclude = allbadcases
m10dat3 = dplyr::filter(dat3,!(PTNUM %in% m10dat3toexclude))

modeltofit = m11
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat3toexclude = allbadcases
m11dat3 = dplyr::filter(dat3,!(PTNUM %in% m11dat3toexclude))



modeltofit = m13
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat3toexclude = allbadcases
m13dat3 = dplyr::filter(dat3,!(PTNUM %in% m13dat3toexclude))

modeltofit = m14
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat3toexclude = allbadcases
m14dat3 = dplyr::filter(dat3,!(PTNUM %in% m14dat3toexclude))


modeltofit = m1
datatofit = dat4
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat4toexclude = allbadcases
m1dat4 = dplyr::filter(dat4,!(PTNUM %in% m1dat4toexclude))

modeltofit = m2
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat4toexclude = allbadcases
m2dat4 = dplyr::filter(dat4,!(PTNUM %in% m2dat4toexclude))


modeltofit = m4
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat4toexclude = allbadcases
m4dat4 = dplyr::filter(dat4,!(PTNUM %in% m4dat4toexclude))

modeltofit = m5
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat4toexclude = allbadcases
m5dat4 = dplyr::filter(dat4,!(PTNUM %in% m5dat4toexclude))


modeltofit = m7
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat4toexclude = allbadcases
m7dat4 = dplyr::filter(dat4,!(PTNUM %in% m7dat4toexclude))

modeltofit = m8
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat4toexclude = allbadcases
m8dat4 = dplyr::filter(dat4,!(PTNUM %in% m8dat4toexclude))


modeltofit = m10
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat4toexclude = allbadcases
m10dat4 = dplyr::filter(dat4,!(PTNUM %in% m10dat4toexclude))

modeltofit = m11
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat4toexclude = allbadcases
m11dat4 = dplyr::filter(dat4,!(PTNUM %in% m11dat4toexclude))



modeltofit = m13
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat4toexclude = allbadcases
m13dat4 = dplyr::filter(dat4,!(PTNUM %in% m13dat4toexclude))

modeltofit = m14
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat4toexclude = allbadcases
m14dat4 = dplyr::filter(dat4,!(PTNUM %in% m14dat4toexclude))



modeltofit = m1
datatofit = dat5
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat5toexclude = allbadcases
m1dat5 = dplyr::filter(dat5,!(PTNUM %in% m1dat5toexclude))

modeltofit = m2
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat5toexclude = allbadcases
m2dat5 = dplyr::filter(dat5,!(PTNUM %in% m2dat5toexclude))


modeltofit = m4
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat5toexclude = allbadcases
m4dat5 = dplyr::filter(dat5,!(PTNUM %in% m4dat5toexclude))

modeltofit = m5
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat5toexclude = allbadcases
m5dat5 = dplyr::filter(dat5,!(PTNUM %in% m5dat5toexclude))


modeltofit = m7
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat5toexclude = allbadcases
m7dat5 = dplyr::filter(dat5,!(PTNUM %in% m7dat5toexclude))

modeltofit = m8
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat5toexclude = allbadcases
m8dat5 = dplyr::filter(dat5,!(PTNUM %in% m8dat5toexclude))


modeltofit = m10
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat5toexclude = allbadcases
m10dat5 = dplyr::filter(dat5,!(PTNUM %in% m10dat5toexclude))

modeltofit = m11
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat5toexclude = allbadcases
m11dat5 = dplyr::filter(dat5,!(PTNUM %in% m11dat5toexclude))



modeltofit = m13
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat5toexclude = allbadcases
m13dat5 = dplyr::filter(dat5,!(PTNUM %in% m13dat5toexclude))

modeltofit = m14
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat5toexclude = allbadcases
m14dat5 = dplyr::filter(dat5,!(PTNUM %in% m14dat5toexclude))


modeltofit = m1
datatofit = dat6
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat6toexclude = allbadcases
m1dat6 = dplyr::filter(dat6,!(PTNUM %in% m1dat6toexclude))

modeltofit = m2
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat6toexclude = allbadcases
m2dat6 = dplyr::filter(dat6,!(PTNUM %in% m2dat6toexclude))


modeltofit = m4
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat6toexclude = allbadcases
m4dat6 = dplyr::filter(dat6,!(PTNUM %in% m4dat6toexclude))

modeltofit = m5
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat6toexclude = allbadcases
m5dat6 = dplyr::filter(dat6,!(PTNUM %in% m5dat6toexclude))


modeltofit = m7
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat6toexclude = allbadcases
m7dat6 = dplyr::filter(dat6,!(PTNUM %in% m7dat6toexclude))

modeltofit = m8
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat6toexclude = allbadcases
m8dat6 = dplyr::filter(dat6,!(PTNUM %in% m8dat6toexclude))


modeltofit = m10
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat6toexclude = allbadcases
m10dat6 = dplyr::filter(dat6,!(PTNUM %in% m10dat6toexclude))

modeltofit = m11
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat6toexclude = allbadcases
m11dat6 = dplyr::filter(dat6,!(PTNUM %in% m11dat6toexclude))



modeltofit = m13
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat6toexclude = allbadcases
m13dat6 = dplyr::filter(dat6,!(PTNUM %in% m13dat6toexclude))

modeltofit = m14
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat6toexclude = allbadcases
m14dat6 = dplyr::filter(dat6,!(PTNUM %in% m14dat6toexclude))


#NOW TESTING DIFFERENT THRESHHOLDS--cthresh of 2 for all models with all data sets

modeltofit = m1
datatofit = dat1
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat1toexclude = allbadcases
m1dat1thr2 = dplyr::filter(dat1,!(PTNUM %in% m1dat1toexclude))

modeltofit = m2
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat1toexclude = allbadcases
m2dat1thr2 = dplyr::filter(dat1,!(PTNUM %in% m2dat1toexclude))


modeltofit = m4
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat1toexclude = allbadcases
m4dat1thr2 = dplyr::filter(dat1,!(PTNUM %in% m4dat1toexclude))

modeltofit = m5
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat1toexclude = allbadcases
m5dat1thr2 = dplyr::filter(dat1,!(PTNUM %in% m5dat1toexclude))


modeltofit = m7
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat1toexclude = allbadcases
m7dat1thr2 = dplyr::filter(dat1,!(PTNUM %in% m7dat1toexclude))

modeltofit = m8
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat1toexclude = allbadcases
m8dat1thr2 = dplyr::filter(dat1,!(PTNUM %in% m8dat1toexclude))


modeltofit = m10
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat1toexclude = allbadcases
m10dat1thr2 = dplyr::filter(dat1,!(PTNUM %in% m10dat1toexclude))

modeltofit = m11
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat1toexclude = allbadcases
m11dat1thr2 = dplyr::filter(dat1,!(PTNUM %in% m11dat1toexclude))



modeltofit = m13
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat1toexclude = allbadcases
m13dat1thr2 = dplyr::filter(dat1,!(PTNUM %in% m13dat1toexclude))

modeltofit = m14
datatofit = dat1
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat1toexclude = allbadcases
m14dat1thr2 = dplyr::filter(dat1,!(PTNUM %in% m14dat1toexclude))

#now Dat2 with threshold of 2
m1dat2_m <- "
self_coupling_patient ~ iip_elevation_patient
cross_coupling_patient ~ iip_elevation_patient
self_coupling_partner ~ iip_elevation_partner
cross_coupling_partner ~ iip_elevation_partner

"

modeltofit = m1dat2_m
datatofit = dat2
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                  filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                  filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat2toexclude = allbadcases
m1dat2thr2 = dplyr::filter(dat2,!(PTNUM %in% m1dat2toexclude))

modeltofit = m2
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat2toexclude = allbadcases
m2dat2thr2 = dplyr::filter(dat2,!(PTNUM %in% m2dat2toexclude))

m4dat2_m <- "
self_coupling_patient ~ iip_agency_patient
cross_coupling_patient ~ iip_agency_patient
self_coupling_partner ~ iip_agency_partner
cross_coupling_partner ~ iip_agency_partner

"

modeltofit = m4dat2_m
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = iip_agency_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                               filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = iip_agency_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                               filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = iip_agency_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = iip_agency_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat2toexclude = allbadcases
m4dat2thr2 = dplyr::filter(dat2,!(PTNUM %in% m4dat2toexclude))

modeltofit = m5
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat2toexclude = allbadcases
m5dat2thr2 = dplyr::filter(dat2,!(PTNUM %in% m5dat2toexclude))


modeltofit = m7dat2_m
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = iip_communion_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                  filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = iip_communion_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                  filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = iip_communion_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = iip_communion_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat2toexclude = allbadcases
m7dat2thr2 = dplyr::filter(dat2,!(PTNUM %in% m7dat2toexclude))

modeltofit = m8
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat2toexclude = allbadcases
m8dat1thr2 = dplyr::filter(dat2,!(PTNUM %in% m8dat2toexclude))


modeltofit = m10dat2_m
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = pdtot_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                          filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = pdtot_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                          filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = pdtot_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = pdtot_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat2toexclude = allbadcases
m10dat2thr2 = dplyr::filter(dat2,!(PTNUM %in% m10dat2toexclude))

modeltofit = m11
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat2toexclude = allbadcases
m11dat2thr2 = dplyr::filter(dat2,!(PTNUM %in% m11dat2toexclude))



modeltofit = m13dat2_m
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = pdtot_patient, y = self_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                          filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = pdtot_partner, y = self_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                          filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot,
           aes(x = pdtot_patient, y = cross_coupling_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot,
           aes(x = pdtot_partner, y = cross_coupling_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat2toexclude = allbadcases
m13dat2thr2 = dplyr::filter(dat2,!(PTNUM %in% m13dat2toexclude))

modeltofit = m14
datatofit = dat2
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat2toexclude = allbadcases
m14dat2thr2 = dplyr::filter(dat2,!(PTNUM %in% m14dat2toexclude))

#dat3
modeltofit = m1
datatofit = dat3
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat3toexclude = allbadcases
m1dat3thr2 = dplyr::filter(dat3,!(PTNUM %in% m1dat3toexclude))

modeltofit = m2
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat3toexclude = allbadcases
m2dat3thr2 = dplyr::filter(dat3,!(PTNUM %in% m2dat3toexclude))


modeltofit = m4
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat3toexclude = allbadcases
m4dat3thr2 = dplyr::filter(dat3,!(PTNUM %in% m4dat3toexclude))

modeltofit = m5
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat3toexclude = allbadcases
m5dat3thr2 = dplyr::filter(dat3,!(PTNUM %in% m5dat3toexclude))


modeltofit = m7
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat3toexclude = allbadcases
m7dat3thr2 = dplyr::filter(dat3,!(PTNUM %in% m7dat3toexclude))

modeltofit = m8
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat3toexclude = allbadcases
m8dat3thr2 = dplyr::filter(dat3,!(PTNUM %in% m8dat3toexclude))


modeltofit = m10
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat3toexclude = allbadcases
m10dat3thr2 = dplyr::filter(dat3,!(PTNUM %in% m10dat3toexclude))

modeltofit = m11
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat3toexclude = allbadcases
m11dat3thr2 = dplyr::filter(dat3,!(PTNUM %in% m11dat3toexclude))



modeltofit = m13
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat3toexclude = allbadcases
m13dat3thr2 = dplyr::filter(dat3,!(PTNUM %in% m13dat3toexclude))

modeltofit = m14
datatofit = dat3
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat3toexclude = allbadcases
m14dat3thr2 = dplyr::filter(dat3,!(PTNUM %in% m14dat3toexclude))


#dat4
modeltofit = m1
datatofit = dat4
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat4toexclude = allbadcases
m1dat4thr2 = dplyr::filter(dat4,!(PTNUM %in% m1dat4toexclude))

modeltofit = m2
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat4toexclude = allbadcases
m2dat4thr2 = dplyr::filter(dat4,!(PTNUM %in% m2dat4toexclude))


modeltofit = m4
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat4toexclude = allbadcases
m4dat4thr2 = dplyr::filter(dat4,!(PTNUM %in% m4dat4toexclude))

modeltofit = m5
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat4toexclude = allbadcases
m5dat4thr2 = dplyr::filter(dat4,!(PTNUM %in% m5dat4toexclude))


modeltofit = m7
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat4toexclude = allbadcases
m7dat4thr2 = dplyr::filter(dat4,!(PTNUM %in% m7dat4toexclude))

modeltofit = m8
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat4toexclude = allbadcases
m8dat4thr2 = dplyr::filter(dat4,!(PTNUM %in% m8dat4toexclude))


modeltofit = m10
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat4toexclude = allbadcases
m10dat4thr2 = dplyr::filter(dat4,!(PTNUM %in% m10dat4toexclude))

modeltofit = m11
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat4toexclude = allbadcases
m11dat4thr2 = dplyr::filter(dat4,!(PTNUM %in% m11dat4toexclude))



modeltofit = m13
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat4toexclude = allbadcases
m13dat4thr2 = dplyr::filter(dat4,!(PTNUM %in% m13dat4toexclude))

modeltofit = m14
datatofit = dat4
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat4toexclude = allbadcases
m14dat4thr2 = dplyr::filter(dat4,!(PTNUM %in% m14dat4toexclude))

#dat5
modeltofit = m1
datatofit = dat5
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat5toexclude = allbadcases
m1dat5thr2 = dplyr::filter(dat5,!(PTNUM %in% m1dat5toexclude))

modeltofit = m2
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat5toexclude = allbadcases
m2dat5thr2 = dplyr::filter(dat5,!(PTNUM %in% m2dat5toexclude))


modeltofit = m4
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat5toexclude = allbadcases
m4dat5thr2 = dplyr::filter(dat5,!(PTNUM %in% m4dat5toexclude))

modeltofit = m5
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat5toexclude = allbadcases
m5dat5thr2 = dplyr::filter(dat5,!(PTNUM %in% m5dat5toexclude))


modeltofit = m7
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat5toexclude = allbadcases
m7dat5thr2 = dplyr::filter(dat5,!(PTNUM %in% m7dat5toexclude))

modeltofit = m8
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat5toexclude = allbadcases
m8dat5thr2 = dplyr::filter(dat5,!(PTNUM %in% m8dat5toexclude))


modeltofit = m10
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat5toexclude = allbadcases
m10dat5thr2 = dplyr::filter(dat5,!(PTNUM %in% m10dat5toexclude))

modeltofit = m11
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat5toexclude = allbadcases
m11dat5thr2 = dplyr::filter(dat5,!(PTNUM %in% m11dat5toexclude))



modeltofit = m13
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat5toexclude = allbadcases
m13dat5thr2 = dplyr::filter(dat5,!(PTNUM %in% m13dat5toexclude))

modeltofit = m14
datatofit = dat5
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat5toexclude = allbadcases
m14dat5thr2 = dplyr::filter(dat5,!(PTNUM %in% m14dat5toexclude))


#dat6

modeltofit = m1
datatofit = dat6
#xpt = "p_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m1dat6toexclude = allbadcases
m1dat6thr2 = dplyr::filter(dat6,!(PTNUM %in% m1dat6toexclude))

modeltofit = m2
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_elevation_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_elevation_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m2dat6toexclude = allbadcases
m2dat6thr2 = dplyr::filter(dat6,!(PTNUM %in% m2dat6toexclude))


modeltofit = m4
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m4dat6toexclude = allbadcases
m4dat6thr2 = dplyr::filter(dat6,!(PTNUM %in% m4dat6toexclude))

modeltofit = m5
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_agency_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_agency_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                        filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m5dat6toexclude = allbadcases
m5dat6thr2 = dplyr::filter(dat6,!(PTNUM %in% m5dat6toexclude))


modeltofit = m7
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m7dat6toexclude = allbadcases
m7dat6thr2 = dplyr::filter(dat6,!(PTNUM %in% m7dat6toexclude))

modeltofit = m8
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = iip_communion_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = iip_communion_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                           filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m8dat6toexclude = allbadcases
m8dat6thr2 = dplyr::filter(dat6,!(PTNUM %in% m8dat6toexclude))


modeltofit = m10
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m10dat6toexclude = allbadcases
m10dat6thr2 = dplyr::filter(dat6,!(PTNUM %in% m10dat6toexclude))

modeltofit = m11
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m11dat6toexclude = allbadcases
m11dat6thr2 = dplyr::filter(dat6,!(PTNUM %in% m11dat6toexclude))



modeltofit = m13
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m13dat6toexclude = allbadcases
m13dat6thr2 = dplyr::filter(dat6,!(PTNUM %in% m13dat6toexclude))

modeltofit = m14
datatofit = dat6
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = pdtot_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = pdtot_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m14dat6toexclude = allbadcases
m14dat6thr2 = dplyr::filter(dat6,!(PTNUM %in% m14dat6toexclude))



#################################################

modeltofit = m16
datatofit = dat7
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m16dat7toexclude = allbadcases
m16dat7 = dplyr::filter(dat7,!(PTNUM %in% m16dat7toexclude))

modeltofit = m17
datatofit = dat7
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m17dat7toexclude = allbadcases
m17dat7 = dplyr::filter(dat7,!(PTNUM %in% m17dat7toexclude))

#dat8
modeltofit = m16
datatofit = dat8
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m16dat8toexclude = allbadcases
m16dat8 = dplyr::filter(dat8,!(PTNUM %in% m16dat8toexclude))

modeltofit = m17
datatofit = dat8
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m17dat8toexclude = allbadcases
m17dat8 = dplyr::filter(dat8,!(PTNUM %in% m17dat8toexclude))
#dat9

modeltofit = m16
datatofit = dat9
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m16dat9toexclude = allbadcases
m16dat9 = dplyr::filter(dat9,!(PTNUM %in% m16dat9toexclude))

modeltofit = m17
datatofit = dat9
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m17dat9toexclude = allbadcases
m17dat9 = dplyr::filter(dat9,!(PTNUM %in% m17dat9toexclude))

#now changing threshold to 2

modeltofit = m16
datatofit = dat7
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m16dat7toexclude = allbadcases
m16dat7thr2 = dplyr::filter(dat7,!(PTNUM %in% m16dat7toexclude))

modeltofit = m17
datatofit = dat7
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m17dat7toexclude = allbadcases
m17dat7thr2 = dplyr::filter(dat7,!(PTNUM %in% m17dat7toexclude))

#dat8
modeltofit = m16
datatofit = dat8
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m16dat8toexclude = allbadcases
m16dat8thr2 = dplyr::filter(dat8,!(PTNUM %in% m16dat8toexclude))

modeltofit = m17
datatofit = dat8
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m17dat8toexclude = allbadcases
m17dat8thr2 = dplyr::filter(dat8,!(PTNUM %in% m17dat8toexclude))
#dat9

modeltofit = m16
datatofit = dat9
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m16dat9toexclude = allbadcases
m16dat9thr2 = dplyr::filter(dat9,!(PTNUM %in% m16dat9toexclude))

modeltofit = m17
datatofit = dat9
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = avo_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = avo_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m17dat9toexclude = allbadcases
m17dat9thr2 = dplyr::filter(dat9,!(PTNUM %in% m17dat9toexclude))


########################################################### now testing models 19 and 20 with the same data block above
modeltofit = m19
datatofit = dat7
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m19dat7toexclude = allbadcases
m19dat7 = dplyr::filter(dat7,!(PTNUM %in% m19dat7toexclude))

modeltofit = m19
datatofit = dat7
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m19dat7toexclude = allbadcases
m19dat7 = dplyr::filter(dat10,!(PTNUM %in% m19dat7toexclude))

#dat8
modeltofit = m19
datatofit = dat8
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m19dat8toexclude = allbadcases
m19dat8 = dplyr::filter(dat8,!(PTNUM %in% m19dat8toexclude))

modeltofit = m19
datatofit = dat8
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m19dat8toexclude = allbadcases
m19dat8 = dplyr::filter(dat8,!(PTNUM %in% m19dat8toexclude))
#dat9

modeltofit = m19
datatofit = dat9
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m19dat9toexclude = allbadcases
m19dat9 = dplyr::filter(dat9,!(PTNUM %in% m19dat9toexclude))

modeltofit = m20
datatofit = dat9
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m20dat9toexclude = allbadcases
m20dat9 = dplyr::filter(dat9,!(PTNUM %in% m20dat9toexclude))

#now changing threshold to 2

modeltofit = m19
datatofit = dat7
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m19dat7toexclude = allbadcases
m19dat7thr2 = dplyr::filter(dat7,!(PTNUM %in% m19dat7toexclude))

modeltofit = m20
datatofit = dat7
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m20dat7toexclude = allbadcases
m20dat7thr2 = dplyr::filter(dat7,!(PTNUM %in% m20dat7toexclude))

modeltofit = m20
datatofit = dat7
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m20dat7toexclude = allbadcases
m20dat7 = dplyr::filter(dat7,!(PTNUM %in% m20dat7toexclude))




#dat8
modeltofit = m19
datatofit = dat8
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m19dat8toexclude = allbadcases
m19dat8thr2 = dplyr::filter(dat8,!(PTNUM %in% m19dat8toexclude))

modeltofit = m20
datatofit = dat8
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m20dat8toexclude = allbadcases
m20dat8thr2 = dplyr::filter(dat8,!(PTNUM %in% m20dat8toexclude))



modeltofit = m20
datatofit = dat8
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * mean(ef)
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m20dat8toexclude = allbadcases
m20dat8 = dplyr::filter(dat8,!(PTNUM %in% m20dat8toexclude))



#dat9

modeltofit = m19
datatofit = dat9
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m19dat9toexclude = allbadcases
m19dat9thr2 = dplyr::filter(dat9,!(PTNUM %in% m19dat9toexclude))

modeltofit = m20
datatofit = dat9
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = anx_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = anx_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m20dat9toexclude = allbadcases
m20dat9thr2 = dplyr::filter(dat9,!(PTNUM %in% m20dat9toexclude))

################################################ now do the same with imi con (22 and 23) and aff (25 and 26)



#######first 22 and 23
###########################################################
modeltofit = m22
datatofit = dat10
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m22dat10toexclude = allbadcases
m22dat10 = dplyr::filter(dat10,!(PTNUM %in% m22dat10toexclude))

modeltofit = m22
datatofit = dat10
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m22dat10toexclude = allbadcases
m22dat10 = dplyr::filter(dat10,!(PTNUM %in% m22dat10toexclude))

#dat11
modeltofit = m22
datatofit = dat11
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m22dat11toexclude = allbadcases
m22dat11 = dplyr::filter(dat11,!(PTNUM %in% m22dat11toexclude))

modeltofit = m22
datatofit = dat11
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m22dat11toexclude = allbadcases
m22dat11 = dplyr::filter(dat11,!(PTNUM %in% m22dat11toexclude))
#dat12

modeltofit = m22
datatofit = dat12
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m22dat12toexclude = allbadcases
m22dat12 = dplyr::filter(dat12,!(PTNUM %in% m22dat12toexclude))

modeltofit = m23
datatofit = dat12
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m23dat12toexclude = allbadcases
m23dat12 = dplyr::filter(dat12,!(PTNUM %in% m23dat12toexclude))

#now changing threshold to 2

modeltofit = m22
datatofit = dat10
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m22dat10toexclude = allbadcases
m22dat10thr2 = dplyr::filter(dat10,!(PTNUM %in% m22dat10toexclude))

modeltofit = m23
datatofit = dat10
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m23dat10toexclude = allbadcases
m23dat10thr2 = dplyr::filter(dat10,!(PTNUM %in% m23dat10toexclude))



modeltofit = m23
datatofit = dat10
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * mean(ef)
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m23dat10toexclude = allbadcases
m23dat10 = dplyr::filter(dat10,!(PTNUM %in% m23dat10toexclude))

#dat11
modeltofit = m22
datatofit = dat11
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m22dat11toexclude = allbadcases
m22dat11thr2 = dplyr::filter(dat11,!(PTNUM %in% m22dat11toexclude))

modeltofit = m23
datatofit = dat11
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m23dat11toexclude = allbadcases
m23dat11thr2 = dplyr::filter(dat11,!(PTNUM %in% m23dat11toexclude))

modeltofit = m23
datatofit = dat11
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * mean(ef)
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m23dat11toexclude = allbadcases
m23dat11 = dplyr::filter(dat11,!(PTNUM %in% m23dat11toexclude))

#dat12

modeltofit = m22
datatofit = dat12
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m22dat12toexclude = allbadcases
m22dat12thr2 = dplyr::filter(dat12,!(PTNUM %in% m22dat12toexclude))

modeltofit = m23
datatofit = dat12
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m23dat12toexclude = allbadcases
m23dat12thr2 = dplyr::filter(dat12,!(PTNUM %in% m23dat12toexclude))




#######now with and 25 and 26
###########################################################
modeltofit = m25
datatofit = dat10
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m25dat10toexclude = allbadcases
m25dat10 = dplyr::filter(dat10,!(PTNUM %in% m25dat10toexclude))

modeltofit = m26
datatofit = dat10
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m26dat10toexclude = allbadcases
m26dat10thr2 = dplyr::filter(dat10,!(PTNUM %in% m26dat10toexclude))

#dat11
modeltofit = m25
datatofit = dat11
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m25dat11toexclude = allbadcases
m25dat11 = dplyr::filter(dat11,!(PTNUM %in% m25dat11toexclude))

modeltofit = m25
datatofit = dat11
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m25dat11toexclude = allbadcases
m25dat11 = dplyr::filter(dat11,!(PTNUM %in% m25dat11toexclude))
#dat12

modeltofit = m25
datatofit = dat12
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m25dat12toexclude = allbadcases
m25dat12 = dplyr::filter(dat12,!(PTNUM %in% m25dat12toexclude))

modeltofit = m26
datatofit = dat12
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m26dat12toexclude = allbadcases
m26dat12 = dplyr::filter(dat12,!(PTNUM %in% m26dat12toexclude))

#now changing threshold to 2

modeltofit = m25
datatofit = dat10
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m25dat10toexclude = allbadcases
m25dat10thr2 = dplyr::filter(dat10,!(PTNUM %in% m25dat10toexclude))

modeltofit = m26
datatofit = dat10
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * mean(ef)
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m26dat10toexclude = allbadcases
m26dat10 = dplyr::filter(dat10,!(PTNUM %in% m26dat10toexclude))

#dat11
modeltofit = m25
datatofit = dat11
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m25dat11toexclude = allbadcases
m25dat11thr2 = dplyr::filter(dat11,!(PTNUM %in% m25dat11toexclude))

modeltofit = m26
datatofit = dat11
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * mean(ef)
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m26dat11toexclude = allbadcases
m26dat11 = dplyr::filter(dat11,!(PTNUM %in% m26dat11toexclude))


modeltofit = m26
datatofit = dat11
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m26dat11toexclude = allbadcases
m26dat11thr2 = dplyr::filter(dat11,!(PTNUM %in% m26dat11toexclude))


#dat12

modeltofit = m25
datatofit = dat12
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m25dat12toexclude = allbadcases
m25dat12thr2 = dplyr::filter(dat12,!(PTNUM %in% m25dat12toexclude))

modeltofit = m26
datatofit = dat12
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = AFF_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = AFF_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m26dat12toexclude = allbadcases
m26dat12thr2 = dplyr::filter(dat12,!(PTNUM %in% m26dat12toexclude))

##########################finally m28 and m29

###########################################################
modeltofit = m28
datatofit = dat13
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m28dat13toexclude = allbadcases
m28dat13 = dplyr::filter(dat13,!(PTNUM %in% m28dat13toexclude))

modeltofit = m28
datatofit = dat13
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



# ef<- genCookDist(model = modeltofit, data = datatofit)
# cthresh = 3*(mean(ef))
# listofPTNUM <- c(as.numeric(datatofit$PTNUM))
# #row.names(as.data.frame(cd)) <- listofPTNUM
# nsubjs = length(datatofit$PTNUM)
# #which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
# #another rule of thumb is 4/(n - k - 1)
# #which(cd > .04) #a more sane threshold looking at some of the plots
# dfnew = data.frame(1:nsubjs)
# dfnew$leverage = ef
#
# dfnew$PTNUM = datatofit$PTNUM
#
# cat("Excluding observations with Cook's D >", cthresh, "\n")
#
# mup <- datatofit #initially just the full model
# allbadcases <- c()
#
# library(ggplot2); library(cowplot)
# while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
#   badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
#   allbadcases <- c(allbadcases, badcase)
#   cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
#   #cat("  Excluding ", badcase, "\n")
#   #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
#   mup <- dplyr::filter(datatofit, !PTNUM %in% allbadcases)
#   #print(summary(mup), correlation = FALSE)
#
#   dftoplot <- datatofit %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))
#   #if 1a
#   g1 <- ggplot(dftoplot, aes(x=na_post_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
#   g2 <- ggplot(dftoplot, aes(x=na_post_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
#   g3 <- ggplot(dftoplot, aes(x=na_post_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
#   g4 <- ggplot(dftoplot, aes(x=na_post_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
#
#
#
#
#   obj <- plot_grid(g1, g2, g3, g4, nrow=2)
#   plot(obj)
#
#   cd <- genCookDist(modeltofit, mup)
#   plot(cd)
#   dfnew <- data.frame(1:length(mup$PTNUM))
#   dfnew$PTNUM <- mup$PTNUM
#   dfnew$leverage <- cd
# }
# m28dat13toexclude = allbadcases
# m28dat13 = dplyr::filter(dat13, !(PTNUM %in% m28dat13toexclude))

#dat14
# modeltofit = m28
# datatofit = dat14
# #xpt = "pdtot_patient"
# #xpr = "pdtot_partner"
# fittedmodel <- sem(modeltofit, datatofit, missing = "listwise", estimator = "ML",
#                    mimic="Mplus", meanstructure = TRUE, conditional.x=TRUE)
# summary(fittedmodel, fit.measures = TRUE)
#
#
#
# ef<- genCookDist(model = modeltofit, data = datatofit)
# cthresh = 3*(mean(ef))
# listofPTNUM <- c(as.numeric(datatofit$PTNUM))
# #row.names(as.data.frame(cd)) <- listofPTNUM
# nsubjs = length(datatofit$PTNUM)
# #which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
# #another rule of thumb is 4/(n - k - 1)
# #which(cd > .04) #a more sane threshold looking at some of the plots
# dfnew = data.frame(1:nsubjs)
# dfnew$leverage = ef
#
# dfnew$PTNUM = datatofit$PTNUM
#
# cat("Excluding observations with Cook's D >", cthresh, "\n")
#
# mup <- datatofit #initially just the full model
# allbadcases <- c()
#
# library(ggplot2); library(cowplot)
# while (max(dfnew$leverage) > cthresh) { #a more sane threshold looking at some of the plots
#   badcase<- dfnew[which.max(dfnew$leverage),] %>% dplyr::select(PTNUM)
#   allbadcases <- c(allbadcases, badcase)
#   cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
#   #cat("  Excluding ", badcase, "\n")
#   #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
#   mup <- dplyr::filter(datatofit, !PTNUM %in% allbadcases)
#   #print(summary(mup), correlation = FALSE)
#
#   dftoplot <- datatofit %>% mutate(bad=factor(PTNUM %in% unlist(allbadcases), levels=c(TRUE, FALSE), labels=c("Bad", "Good")))
#   #if 1a
#   g1 <- ggplot(dftoplot, aes(x=na_post_patient, y=scpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
#   g2 <- ggplot(dftoplot, aes(x=na_post_partner, y=scpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
#   g3 <- ggplot(dftoplot, aes(x=na_post_patient, y=ccpt, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
#   g4 <- ggplot(dftoplot, aes(x=na_post_partner, y=ccpr, color=bad)) + geom_point() + stat_smooth(data=filter(dftoplot, bad=="Good"), method="lm")
#
#
#
#
#   obj <- plot_grid(g1, g2, g3, g4, nrow=2)
#   plot(obj)
#
#   cd <- genCookDist(modeltofit, mup)
#   plot(cd)
#   dfnew <- data.frame(1:length(mup$PTNUM))
#   dfnew$PTNUM <- mup$PTNUM
#   dfnew$leverage <- cd
# }
# m28dat14toexclude = allbadcases
# m28dat14 = dplyr::filter(dat14, !(PTNUM %in% m28dat14toexclude))

modeltofit = m28
datatofit = dat14
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m28dat14toexclude = allbadcases
m28dat14 = dplyr::filter(dat14,!(PTNUM %in% m28dat14toexclude))
#dat15

modeltofit = m28
datatofit = dat15
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m28dat15toexclude = allbadcases
m28dat15 = dplyr::filter(dat15,!(PTNUM %in% m28dat15toexclude))

modeltofit = m29
datatofit = dat15
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m29dat15toexclude = allbadcases
m29dat15 = dplyr::filter(dat15,!(PTNUM %in% m29dat15toexclude))

#now changing threshold to 2

modeltofit = m28
datatofit = dat13
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m28dat13toexclude = allbadcases
m28dat13thr2 = dplyr::filter(dat13,!(PTNUM %in% m28dat13toexclude))

modeltofit = m29
datatofit = dat13
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * mean(ef)
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m29dat13toexclude = allbadcases
m29dat13 = dplyr::filter(dat13,!(PTNUM %in% m29dat13toexclude))

modeltofit = m29
datatofit = dat13
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m29dat13toexclude = allbadcases
m29dat13thr2 = dplyr::filter(dat13,!(PTNUM %in% m29dat13toexclude))


#dat14
modeltofit = m28
datatofit = dat14
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m28dat14toexclude = allbadcases
m28dat14thr2 = dplyr::filter(dat14,!(PTNUM %in% m28dat14toexclude))

modeltofit = m29
datatofit = dat14
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * mean(ef)
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m29dat14toexclude = allbadcases
m29dat14 = dplyr::filter(dat14,!(PTNUM %in% m29dat14toexclude))

modeltofit = m29
datatofit = dat14
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m29dat14toexclude = allbadcases
m29dat14thr2 = dplyr::filter(dat14,!(PTNUM %in% m29dat14toexclude))

#dat15

modeltofit = m28
datatofit = dat15
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m28dat15toexclude = allbadcases
m28dat15thr2 = dplyr::filter(dat15,!(PTNUM %in% m28dat15toexclude))

modeltofit = m29
datatofit = dat15
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "listwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 2
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = na_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = na_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                     filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
m29dat15toexclude = allbadcases
m29dat15thr2 = dplyr::filter(dat15,!(PTNUM %in% m29dat15toexclude))

#############################################################################
#B: load in ibi time series
library(RHRV)
setwd(
  "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/iMac_desktop_data/excluded_vanBse/vanBse_ibis"
)
vanillaibifiles <-
  list.files(path = "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/iMac_desktop_data/excluded_vanBse/vanBse_ibis",
             pattern = "*vanilla*",
             full.names = TRUE)
beatdir <-
  "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/rsa_vanBse_beatemp"
dir.create(beatdir, showWarnings = FALSE)
restingrsa <- c()
#setwd(beatdir) #for CMETX to work
for (f in vanillaibifiles) {
  id <-
    sub(
      "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/iMac_desktop_data/excluded_vanBse/vanBse_ibis/(8[0-9]{3}).*",
      "\\1",
      f,
      perl = TRUE
    )
  position <-
    tolower(
      sub(
        "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/iMac_desktop_data/excluded_vanBse/vanBse_ibis/8[0-9]{3}_([lr])_.*",
        "\\1",
        f,
        perl = TRUE
      )
    )
  x <- read.table(f)
  beatfile <- paste0(id, position, "_beattimes.txt")
  ibidosfile <- paste0(id, position, "_ibidos.txt")
  write.table(
    x[, 1],
    file = file.path(beatdir, beatfile),
    row.names = FALSE,
    col.names = FALSE
  )
  write.table(
    x[, 2],
    file = file.path(beatdir, ibidosfile),
    row.names = FALSE,
    col.names = FALSE,
    eol = "\r\n"
  )
  hrv.data  = CreateHRVData(Verbose = TRUE)
  hrv.data = LoadBeatAscii(hrv.data, beatfile, RecordPath = beatdir, scale =
                             .001) #in ms
  hrv.data = BuildNIHR(hrv.data)
  hrv.data = FilterNIHR(hrv.data, minbpm = 35, maxbpm = 180)
  hrv.data = InterpolateNIHR (hrv.data, freqhr = 10)
  #PlotNIHR(hrv.data)
  #PlotHR(hrv.data)
  hrv.data = CreateFreqAnalysis(hrv.data)
  #get overall RSA during vanilla baseline by creating a single window of the length of the time series
  
  #new tack: use wavelet analysis to get overall power (this matches above very closely, but avoids the window size problem)
  hrv.data = CalculatePowerBand(
    hrv.data,
    indexFreqAnalysis = 1,
    type = "wavelet",
    wavelet = "la8",
    bandtolerance = 0.01,
    relative = FALSE,
    ULFmin = 0,
    ULFmax = 0.03,
    VLFmin = 0.03,
    VLFmax = 0.05,
    LFmin = 0.05,
    LFmax = 0.12,
    HFmin = 0.12,
    HFmax = 0.4
  )
  
  #hrv.data = CalculatePowerBand(hrv.data , indexFreqAnalysis=1,
  #    size = floor(max(hrv.data$Beat$Time)) -3, shift = floor(max(hrv.data$Beat$Time)) -3, type = "fourier",
  #    ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
  #    LFmin = 0.05, LFmax = 0.12, HFmin = 0.12,   HFmax = 0.4)
  
  #~60-second bins with ~30-second windows
  hrv.data = CreateFreqAnalysis(hrv.data)
  #get overall RSA during vanilla baseline by creating a single window of the length of the time series
  hrv.data = CalculatePowerBand(
    hrv.data ,
    indexFreqAnalysis = 2,
    size = 54,
    shift = 24,
    type = "fourier",
    ULFmin = 0,
    ULFmax = 0.03,
    VLFmin = 0.03,
    VLFmax = 0.05,
    LFmin = 0.05,
    LFmax = 0.12,
    HFmin = 0.12,
    HFmax = 0.4
  )
  
  moving_rsa <- log(2 * hrv.data$FreqAnalysis[[2]]$HF)
  
  restingrsa <-
    rbind(
      restingrsa,
      data.frame(
        PTNUM = id,
        position = position,
        restingrsa_all = mean(log(2 * hrv.data$FreqAnalysis[[1]]$HF)),
        restingrsa_tmean = mean(moving_rsa),
        restingrsa_tsd = sd(moving_rsa)
      )
    )
  #system(paste("/usr/local/bin/wine ~/CMETX.EXE ", tools::file_path_sans_ext(ibidosfile), " -o CMHRV", sep="\t"))
}

#reform dataframe into the l_ and r_ convention (rather than long format)
#remove 8104 and 8120
restingrsa <-
  subset(restingrsa,!PTNUM %in% c("8104", "8120")) #people that were cut throughout the study


m <- melt(restingrsa, id.variable = c("PTNUM", "position"))
restingrsa_wide <-
  dcast(m, PTNUM ~ position + variable, value.var = "value")

#rough comparison to cmetx
#cmetx <- read.csv("/Users/michael/ics/couples_stage/ecg_raw/vanillabeat_temp/CMHRV.CSV", header=TRUE)
#cmetx$id <- sub("(\\d+)[LR]_ibidos.*", "\\1", cmetx$ID, perl=TRUE)
#cmetx$position <- sub("\\d+([LR])_ibidos.*", "\\1", cmetx$ID, perl=TRUE)
#both <- merge(restingrsa, cmetx, by=c("id", "position"))
#levels(cmetx$Arts.Found) <- c("TRUE", "FALSE")
#cmetx[cmetx$Arts.Found=="TRUE",]

#8001 R is quite suspicious -- I believe this was after the electrodes were yanked by 8000.
#this couple was already excluded from the interaction data anyhow
#cor(subset(both, !id=="8001", select=c(rsa, LogRSA)))
#plot(subset(both, !id=="8001", select=c(rsa, LogRSA)))
#[1] 0.9874755
df = restingrsa
load(file = "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/PSYCHOPHYS_LOG_20170118.RData")
df_split <- split(df, df$PTNUM)
logfile = dplyr::filter(logfile, PTNUM != 8000, PTNUM != 8104, PTNUM != 8120, PTNUM != 8078)
#need to cut
df_split$`8120` <- NULL
alignLR_PatientPartner <- function(df) {
  #	require(xlsx)
  #	logfile <- read.xlsx("~/Tresors/Couples_Interaction/data/psychophys_log_7May2015.xlsx", sheetIndex=1, as.data.frame=TRUE)[,c("PTNum", "DyadID", "DyadText", "L.R", "Kaeleen.P1.P2", "PInitials")]
  #	logfile <- plyr::rename(logfile, c(PTNum="PTNUM"))
  #save(logfile, file="~/Tresors/Couples_Interaction/data/psychophys_log_7May2015.RData")
  #load(file="~/Desktop/ams939/Couples Project/R/Data/psychophys_log_7May2015.RData")
  #load(file="/Users/michael/Box_Sync/DEPENd/Couples Data/data/psychophys_log_7May2015.RData")
  #logfile <- read.csv("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/PSYCHOPHYS_LOG_20170118.csv")
  #logfile <- select(logfile, one_of(c("PTNUM", "Dyad.ID", "Dyad.Text", "Left.or.Right.Seat", "Kaeleen.P1.P2", "Particpant.s.Initials")))
  #save(logfile, file="~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/PSYCHOPHYS_LOG_20170118.RData")
  load(file = "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/PSYCHOPHYS_LOG_20170118.RData")
  df_split <- split(df, df$PTNUM)
  df_split$`8120` <- NULL
  recode <- do.call(rbind, lapply(df_split, function(coupledf) {
    lookup <-
      subset(logfile, PTNUM == coupledf$PTNUM[1] &
               Left.or.Right.Seat == "L")
    if (nrow(lookup) != 1L) {
      stop("Unable to find lookup for ID: ", coupledf$PTNUM[1])
    }
    if (lookup$Dyad.Text == "PARTNER") {
      #partner is on left, patient on right
      names(coupledf) <-
        sub("^l_(.*)", "\\1_partner", names(coupledf), perl = TRUE)
      names(coupledf) <-
        sub("^r_(.*)", "\\1_patient", names(coupledf), perl = TRUE)
    } else if (lookup$Dyad.Text == "PATIENT") {
      #patient on left, partner on right
      names(coupledf) <-
        sub("^l_(.*)", "\\1_patient", names(coupledf), perl = TRUE)
      names(coupledf) <-
        sub("^r_(.*)", "\\1_partner", names(coupledf), perl = TRUE)
    } else {
      stop("Cannot match dyad text: ", lookup$Dyad.Text)
    }
    
    coupledf
  }))
  return(recode)
}

restingrsa_patpar <- alignLR_PatientPartner(restingrsa_wide)
restingrsa_patient = dplyr::select(restingrsa_patpar, PTNUM, restingrsa_all_patient)
restingrsa_partner = dplyr::select(restingrsa_patpar, PTNUM, restingrsa_all_partner)

load(
  "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/couples_baseline_clinical_27Jul2017.RData"
)
personalitymeasures <- couples_baseline_clin
personalitymeasures <-
  rename(
    personalitymeasures,
    PTNUM = ptnum,
    DyadID = dyadid,
    UsrID = usrid
  )
pDatPatient <- dplyr::filter(personalitymeasures, DyadID == 1)
pDatPatient <- dplyr::filter(pDatPatient, PTNUM != 8123)
pDatPatient$PTNUM <- as.factor(pDatPatient$PTNUM)
excludePatient <-
  anti_join(pDatPatient, restingrsa_patient, by = "PTNUM") #excludes 81441, 81331, 81271
pDatPartner <- dplyr::filter(personalitymeasures, DyadID == 0)
pDatPartner <- dplyr::filter(pDatPartner, PTNUM != 8123)
pDatPartner$PTNUM <- as.factor(pDatPartner$PTNUM)
names(pDatPartner) <-
  paste(names(pDatPartner), "partner", sep = "_")
names(pDatPatient) <-
  paste(names(pDatPatient), "patient", sep = "_")
pDatPatient$PTNUM <- pDatPatient$PTNUM_patient
pDatPartner$PTNUM <- pDatPartner$PTNUM_partner
pDatPatient$PTNUM <- as.character(pDatPatient$PTNUM)
pDatPartner$PTNUM <- as.character(pDatPartner$PTNUM)
restingrsa_patient$PTNUM <- as.character(restingrsa_patient$PTNUM)
restingrsa_partner$PTNUM <- as.character(restingrsa_partner$PTNUM)
pDataPatient <-
  inner_join(pDatPatient, restingrsa_patient, by = "PTNUM")

pDataPartner <-
  inner_join(pDatPartner, restingrsa_partner, by = "PTNUM")

#not must cut from patient 8123, 8139
# dtm4_partner <- dplyr::select(dtm4, self_coupling_partner, cross_coupling_partner, PTNUM)
# dtm4_partner$self_coupling <- dtm4_partner$self_coupling_partner
# dtm4_partner$cross_coupling <- dtm4_partner$cross_coupling_partner
#dtm4_partner <- dplyr::select(dtm4_partner, -cross_coupling_partner, -self_coupling_partner)
pDataPatient <- dplyr::filter(pDataPatient, PTNUM != "8139")
pDataPartner <- dplyr::filter(pDataPartner, PTNUM != "8139")
personalitydata <- data.frame(pDataPatient, pDataPartner)
personalitydata$PTNUM <- personalitydata$PTNUM_patient
datatofit <- personalitydata
rsa_m1 <- "
restingrsa_all_patient ~iip_elevation_patient
restingrsa_all_partner ~ iip_elevation_partner


"
rsa_m2 <- "
restingrsa_all_patient ~iip_agency_patient
restingrsa_all_partner ~ iip_agency_partner


"

rsa_m3 <- "
restingrsa_all_patient ~iip_communion_patient
restingrsa_all_partner ~ iip_communion_partner


"


rsa_m4 <- "
restingrsa_all_patient ~pdtot_patient + pdtot_partner
restingrsa_all_partner ~ pdtot_partner + pdtot_patient


"
rsa_m5 <- "
restingrsa_all_patient ~pdtot_patient + iip_elevation_patient
restingrsa_all_partner ~ pdtot_partner + iip_elevation_partner


"

rsa_m6 <- "
exprestingrsa_all_patient ~pdtot_patient + iip_elevation_patient
exprestingrsa_all_partner ~ pdtot_partner + iip_elevation_partner


"


rsa_m4_m <-
  sem(
    rsa_m4,
    datatofit,
    missing = "pairwise",
    estimator = "ML",
    mimic = "Mplus",
    meanstructure = TRUE,
    conditional.x = TRUE
  )

rsa_cd <- genCookDist(rsa_m1, datatofit)



modeltofit = rsa_m5
datatofit = rsadata_nopvcs
rsa_cd <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(rsa_cd))
listofPTNUM <- c(as.numeric(as.character(datatofit$PTNUM)))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = rsa_cd

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  datatofit$PTNUM <- as.numeric(as.character(datatofit$PTNUM))
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_patient, y = restingrsa_all_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_partner, y = restingrsa_all_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
rsam1gcdtoexclude = allbadcases
rsam1gcd = dplyr::filter(datatofit,!(PTNUM %in% rsam1gcdtoexclude))

modeltofit = rsa_m1
rsa_cd <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(rsa_cd))
listofPTNUM <- c(as.numeric(as.character(datatofit$PTNUM)))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = rsa_cd

dfnew$PTNUM = datatofit$PTNUM
cthresh = .4
cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()

library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  datatofit$PTNUM <- as.numeric(as.character(datatofit$PTNUM))
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_patient, y = restingrsa_all_patient, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot,
           aes(x = iip_elevation_partner, y = restingrsa_all_partner, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                                   filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
rsam1gcdtoexclude = allbadcases
rsam1gcd = dplyr::filter(datatofit,!(PTNUM %in% rsam1gcdtoexclude))

rsadata_nopvcs <-
  dplyr::filter(personalitydata,
                PTNUM != 8016,
                PTNUM != 8032,
                PTNUM != 8034,
                PTNUM != 8060,
                PTNUM != 8100)

sem(
  rsa_m5,
  rsadata_nopvcs,
  missing = "listwise",
  estimator = "ML",
  mimic = "Mplus",
  meanstructure = TRUE
)

#df_split$`8120` <- NULL

dat4_rsa <- merge(dat4, datatofit, by = "PTNUM")

#is there a relationship between vanBse and restingrsa? Feels like there should be (overall influence of outside world on physiology)
vanBse_m <- "
v_scpt ~ restingrsa_all_patient
v_ccpt ~ restingrsa_all_patient
v_scpr ~ restingrsa_all_partner
v_ccpr ~ restingrsa_all_partner

"

negint_m <- "
scpt ~ restingrsa_all_patient + restingrsa_all_partner
ccpt ~ restingrsa_all_patient  + restingrsa_all_partner
scpr ~ restingrsa_all_partner + restingrsa_all_patient
ccpr ~ restingrsa_all_partner + restingrsa_all_patient

"


antso_m <- "
antso_sidp_patient ~ restingrsa_all_patient
antso_sidp_partner ~ restingrsa_all_partner


"




###############################FINAL SET OF ANALYSES
#moderated mediation with SPAFF Events and Personality Data
setwd("~/Box\ Sync/DEPENd/Projects/GSR/data/")
load(
  "/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/SPAFFlists.RData"
)
#first get all the spaff events. Use align patient partner to get
#all times in these data frames are with respect to light on = 0.0s
physio_split <- split(allibi, allibi$PTNUM)
idfolders <-
  list.files(path = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/GSR/data/mckayla_SPAFF",
             full.names = TRUE,
             recursive = FALSE)
idlist <-
  as.numeric(
    sub(
      "/Users/alisonmarie526/Box Sync/DEPENd/Projects/GSR/data/mckayla_SPAFF/(8[0-9]{3}).*",
      "\\1",
      idfolders,
      perl = TRUE
    )
  )
# tb <- as.data.frame(table(idlist))
# allSPAFFfilesP1 <- c()
# for (i in 1:length(idfolders)) {
#   fname <- paste0(idlist[i],"_P1.txt")
#   filename <- paste0(idfolders[i])
#   if (!file.exists(filename)) {
#     fname <- paste0(idlist[i], "_P1_ASCII.txt")
#     filename <- paste0(idfolders[i])
#     }
#     if (!file.exists(filename)) {
#       fname <- paste0(idlist[i], "_P1_ascii.txt")
#       filename <- paste0(idfolders[i])
#
#     }
#   if (!file.exists(filename)) {
#     fname <- paste0(idlist[i], "_P1ASCII.txt")
#     filename <- paste0(idfolders[i])
#
#   }
#   if (!file.exists(filename)) {
#     fname <- paste0(idlist[i], "_P1_C1_R.txt")
#     filename <- paste0(idfolders[i])
#
#   }
#     if (!file.exists(filename)) {
#       fname <- paste0(idlist[i], "_P1_UPDATE.txt")
#       filename <- paste0(idfolders[i])
#     }
#   if (!file.exists(filename)) {
#     fname <- paste0(idlist[i], "_p1_R.txt")
#     filename <- paste0(idfolders[i])
#
#   }
#   if (!file.exists(filename)) {
#     fname <- paste0(idlist[i], "_P1_C1ASCII.txt")
#     filename <- paste0(idfolders[i])
#
#   }
#   if (!file.exists(filename)) {
#     fname <- paste0(idlist[i], "_F_M_R.txt")
#     filename <- paste0(idfolders[i])
#
#   }
#     if (!file.exists(filename)) {
#        filename <- paste0(idfolders[i])
#     }
#     if (!file.exists(filename)) {
#       fname <- paste0(idlist[i], "_P1_updated_V2.txt")
#       filename <- paste0(idfolders[i])
#
#     }
#     if (!file.exists(filename)) {
#       fname <- paste0(idlist[i], "_P1 (1).txt")
#       filename <- paste0(idfolders[i])
#     }
#     if (!file.exists(filename)) {
#       fname <- paste0(idlist[i], "_P1_V3.txt")
#       filename <- paste0(idfolders[i])
#     }
#     if (!file.exists(filename)) {
#       fname <- paste0(idlist[i], "_P1_R.txt")
#       filename <- paste0(idfolders[i])
#     }
#   if (!file.exists(filename)) {
#     fname <- paste0(idlist[i], "_P1 .txt")
#     filename <- paste0(idfolders[i])
#   }
#   f <- readSPAFFFile(filename)
#   f <- dplyr::rename(f, time = Relative.Time..seconds.)
#   f <- dplyr::filter(f, Event.Type == "State start")
#   f <- dplyr::mutate(f, diff_event = c(0, diff(time)))
#   f <- dplyr::filter(f, Event.Type == "State start")
#   f$PTNUM <- factor(idlist[i])
#   allSPAFFfilesP1 <-rbind(allSPAFFfilesP1, f)
# }
# allSPAFFfilesP1SPLIT <- split(allSPAFFfilesP1, allSPAFFfilesP1$PTNUM)

tb <- as.data.frame(table(idlist))
allSPAFFfiles <- c()
for (i in 1:length(idfolders)) {
  filename = idfolders[i]
  f <- readSPAFFFile(filename)
  f <- dplyr::rename(f, time = Relative.Time..seconds.)
  f <- dplyr::filter(f, Event.Type == "State stop")
  f <- dplyr::mutate(f, diff_event = c(f$time[[1]], diff(time)))
  f$PTNUM <- factor(idlist[i])
  f$K <-
    sub(
      "/Users/alisonmarie526/Box Sync/DEPENd/Projects/GSR/data/mckayla_SPAFF/8[0-9]{3}.*(P[1-2]{1}).*",
      "\\1",
      idfolders[i],
      perl = TRUE
    )
  allSPAFFfiles <- rbind(allSPAFFfiles, f)
}
physLog <-
  read.csv(file = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/GSR/data/physLog.csv")
physLog <- dplyr::select(physLog, PTNUM, DyadID, UsrID, K)
physLog$PTNUM <- as.character(physLog$PTNUM)
physLog$K <- as.character(physLog$K)
allSPAFFfiles$K <- as.character(allSPAFFfiles$K)
allSPAFFfiles$PTNUM <- as.character(allSPAFFfiles$PTNUM)
spaff_patpar <-
  dplyr::inner_join(allSPAFFfiles, physLog, by = c("PTNUM", "K"))

#spaff_patpar_split <-
spaff_patpar$valence <- sapply(spaff_patpar$Behavior, function(x) {
  x <- tolower(as.character(x))
  
  if (x == "disgust") {
    -3
  } else if (x == "contempt") {
    -4
  } else if (x == "belligerence") {
    -2
  } else if (x == "low domineering") {
    -1
  } else if (x == "high domineering") {
    -1
  } else if (x == "criticism") {
    -2
  } else if (x == "anger") {
    -1
  } else if (x == "tension") {
    0
  } else if (x == "tense humor" ||
             x == "tension/humor") {
    2
  } else if (x == "defensive") {
    -2
  } else if (x == "whining") {
    -1
  } else if (x == "sadness") {
    -1
  } else if (x == "stonewalling" || x == "stonewall") {
    -2
  } else if (x == "neutral") {
    0.1
  } else if (x == "interest") {
    2
  } else if (x == "low validation") {
    4
  } else if (x == "high validation") {
    4
  } else if (x == "affection") {
    4
  } else if (x == "humor") {
    4
  } else if (x == "surprise/joy") {
    4
  } else if (x == "physical affection") {
    4
  } else {
    stop("cannot match code: ", x)
  }
})

spaff_patpar$n3 <-
  factor(sapply(spaff_patpar$Behavior, function(x) {
    x <- tolower(as.character(x))
    
    if (x == "disgust") {
      "nasty"
    } else if (x == "contempt") {
      "nasty"
    } else if (x == "belligerence") {
      "nasty"
    } else if (x == "low domineering") {
      "nasty"
    } else if (x == "high domineering") {
      "nasty"
    } else if (x == "criticism") {
      "nasty"
    } else if (x == "anger") {
      "nasty"
    } else if (x == "tension") {
      "nasty"
    } else if (x == "tense humor" ||
               x == "tension/humor") {
      "nice"
    } else if (x == "defensive") {
      "nasty"
    } else if (x == "whining") {
      "nasty"
    } else if (x == "sadness") {
      "nasty"
    } else if (x == "stonewalling" || x == "stonewall") {
      "nasty"
    } else if (x == "neutral") {
      "neutral"
    } else if (x == "interest") {
      "nice"
    } else if (x == "low validation") {
      "nice"
    } else if (x == "high validation") {
      "nice"
    } else if (x == "affection") {
      "nice"
    } else if (x == "humor") {
      "nice"
    } else if (x == "surprise/joy") {
      "nice"
    } else if (x == "physical affection") {
      "nice"
    } else {
      stop("cannot match code: ", x)
    }
  }))
df_personaitydata_long$UsrID <-
  paste0(df_personaitydata_long$PTNUM,
         df_personaitydata_long$Patient)
spaff_simp <-
  dplyr::select(df_personaitydata_long,
                DyadID,
                DyadID,
                iip_elevation,
                pdtot,
                Patient,
                UsrID,
                PTNUM)
spaff_patpar_UsrIDs <-
  dplyr::select(spaff_patpar, UsrID) %>% unique()
spaff_patpar_UsrIDs <- as.vector(spaff_patpar_UsrIDs[["UsrID"]])
spaff_simp <-
  dplyr::filter(spaff_simp, UsrID %in% spaff_patpar_UsrIDs)
spaff_simp_split <- split(spaff_simp, spaff_simp$UsrID)
spaff_patpar <-
  dplyr::filter(spaff_patpar, UsrID %in% as.vector(spaff_simp$UsrID))
spaff_patpar$n3 <- as.character(spaff_patpar$n3)
spaff_split <- split(spaff_patpar, spaff_patpar$UsrID)
# spaff_split <- lapply(spaff_split, function(x) {
#   x$n3 <- as.character(x$n3)
# })
for (i in 1:length(unique(spaff_patpar$UsrID))) {
  #num_nasty <- sum(as.character(x$n3) =="nasty")
  # num_nice <- sum(as.character(x$n3) == "nice")
  # num_neutral <- sum(as.character(x$n3) == "neutral")
  # time_nasty <- dplyr::filter(x, n3 == "nasty") %>% summarise(total_time= sum(diff_event)
  # time_neutral <- dplyr::filter(x,n3 == "neutral") %>% summarise(total_time = sum(diff_event)
  # time_nice <- dplyr::filter(x, n3 == "nice") %>% summarise(total_time = sum(diff_event)
  time_nasty = dplyr::filter(spaff_split[[i]], n3 == "nasty") %>% summarise(sum(diff_event))
  time_nice = dplyr::filter(spaff_split[[i]], n3 == "nice") %>% summarise(sum(diff_event))
  time_neutral = dplyr::filter(spaff_split[[i]], n3 == "neutral") %>% summarise(sum(diff_event))
  spaff_simp_split[[i]] <- dplyr::mutate(
    spaff_simp_split[[i]],
    num_nasty = sum(as.character(spaff_split[[i]]$n3) == "nasty"),
    num_nice = sum(as.character(spaff_split[[i]]$n3) == "nice"),
    num_neutral = sum(as.character(spaff_split[[i]]$n3) == "neutral"),
    nastytime = time_nasty[[1]],
    neutraltime = time_neutral[[1]],
    nicetime = time_nice[[1]]
  )
}
#cuts out 8093, because got cut from personality data

spaff_summarized <- ldply(spaff_simp_split, data.frame)
spaff_summarized_patient <-
  dplyr::filter(spaff_summarized, Patient == 1)
spaff_summarized_partner <-
  dplyr::filter(spaff_summarized, Patient == 0)
spaff_summarized_patient <-
  dplyr::rename(
    spaff_summarized_patient,
    pdtot_patient = pdtot,
    iip_elevation_patient = iip_elevation,
    num_nasty_patient = num_nasty,
    num_neutral_patient = num_neutral,
    num_nice_patient = num_nice,
    nastytime_patient = nastytime,
    neutraltime_patient = neutraltime,
    nicetime_patient = nicetime
  )
spaff_summarized_partner <-
  dplyr::rename(
    spaff_summarized_partner,
    pdtot_partner = pdtot,
    iip_elevation_partner = iip_elevation,
    num_nasty_partner = num_nasty,
    num_neutral_partner = num_neutral,
    num_nice_partner = num_nice,
    nastytime_partner = nastytime,
    neutraltime_partner = neutraltime,
    nicetime_partner = nicetime
  )
spaff_summarized_wide <-
  merge(spaff_summarized_patient,
        spaff_summarized_partner,
        by = c("PTNUM")) #goes from 116 to 115 because missing data for 8082 partner
m22dat11_spaff_personalitydata <-
  inner_join(m22dat11, spaff_summarized_wide, by = "PTNUM") #goes from 90 to 84 because no personality data for 8139, 8082 was cut because partner had no data (on ICS too), 8049 had no spaff data (confimed on psychophys log), 8048's personality data didn't make the cut, 8043 had no SPAFF data (on ICS too; confirmed on psychophys log), 8036 had bad spaff data
#
m22dat11_spaff_personalitydata <-
  inner_join(m22dat11, spaff_summarized_wide, by = "PTNUM")
#m22dat11_spaff_personalitydata_all <- bind_rows(m22dat11_spaff_personalitydata, m22dat11_spaff_personalitydataz_archives)
m22dat11_spaff_personalitydata_all_rescaled <-
  mutate(
    m22dat11_spaff_personalitydata,
    nastytime_patient = nastytime_patient /
      20,
    nastytime_partner = nastytime_partner /
      20,
    nicetime_patient = nicetime_patient /
      20,
    nicetime_partner = nicetime_partner /
      20,
    neutraltime_patient = neutraltime_patient /
      20,
    neutraltime_partner = neutraltime_partner /
      20
  )
m22dat11_spaff_personalitydata_all_rescaled_noOutliers <-
  dplyr::filter(
    m22dat11_spaff_personalitydata_all_rescaled,
    neutraltime_patient > 0,
    nastytime_patient > 0,
    nicetime_patient > 0,
    nastytime_partner > 0,
    nicetime_partner > 0,
    neutraltime_partner > 0
  )


t3 <- m24withSPAFF <- "

#direct effect
CON_post_partner ~ c1*CON_pre_partner
#mediators
CON_post_partner ~  b1*scpt +  b2*ccpt + b3*nastytime_patient + b4*nicetime_patient + b5*neutraltime_patient + b6*nastytime_partner + b7*nicetime_partner + b8*neutraltime_partner
scpt~a1*CON_pre_partner
ccpt~a2*CON_pre_partner
nastytime_patient ~ a3*CON_pre_partner + e3*CON_pre_patient
nicetime_patient ~ a4*CON_pre_partner + e4*CON_pre_patient
neutraltime_patient ~ a5*CON_pre_partner + e5*CON_pre_patient
nastytime_partner ~ a6*CON_pre_partner + e6*CON_pre_patient
nicetime_partner ~ a7*CON_pre_partner + e7*CON_pre_patient
neutraltime_partner ~ a8*CON_pre_partner + e8*CON_pre_patient
scpt ~ v_scpt + iip_elevation_patient + pdtot_patient
ccpt ~ v_ccpt + iip_elevation_patient + pdtot_patient + pdtot_partner
scpr ~ v_scpr + iip_elevation_partner + pdtot_partner + v_ccpr
ccpr ~ v_ccpr + iip_elevation_partner + pdtot_partner + v_scpr + iip_elevation_patient
#indirect effect
a1b1 := a1*b1
a2b2:=a2*b2
a1b2 := a1*b2
a2b1 := a2*b1
dw1b1 := dw1*b1
a3e3 := a3*b3
a4b4 := a4*b4
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8e8 := a8*b8
# #a3e3:=a3*e3
# a4e4 := a4*e4
# #a5e5 := a5*e5
# a6e6 := a6*e6
# a7e7 := a7*e7
# #a8e8 := a8*e8
a3d3 :=a3*d3
a4d4 := a4*d4
a5d5 := a5*d5
a6d6 := a6*d6
a7d7 := a7*d7
a8d8 := a8*d8
e3d3 :=a3*d3
e4d4 := e4*d4
e5d5 := e5*d5
e6d6 := e6*d6
e7d7 := e7*d7
e8d8 := e8*d8
e4b4 := e4*b4
e6b6 := e6*b6
e7b7 := e7*b7

CON_post_patient ~ c2*CON_pre_patient + d3*nastytime_patient + d4*nicetime_patient + d5*neutraltime_patient + d6*nastytime_partner + d7*nicetime_partner + d8*neutraltime_partner
CON_post_partner ~ CON_pre_patient
#CON_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ dw1*CON_pre_patient
scpr~CON_pre_patient
ccpr~CON_pre_patient

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
nastytime_patient ~~ nastytime_partner
nastytime_patient ~~ nicetime_partner
nastytime_patient ~~ neutraltime_partner
nastytime_patient ~~ nicetime_patient
nastytime_patient ~~ neutraltime_patient
nicetime_patient ~~nastytime_partner
nicetime_patient ~~ nicetime_partner
nicetime_patient ~~ neutraltime_partner
nicetime_patient ~~ neutraltime_patient
neutraltime_patient ~~ nastytime_partner
neutraltime_patient ~~ nicetime_partner
neutraltime_patient ~~ neutraltime_partner
nastytime_partner ~~ neutraltime_partner
nastytime_partner ~~ nicetime_partner
neutraltime_partner ~~ nicetime_partner

"
max(m22dat11_spaff_personalitydata$iip_elevation_patient)
m22dat11_spaff_personalitydata_rescaled <-
  mutate(
    m22dat11_spaff_personalitydata,
    nastytime_patient = nastytime_patient /
      20,
    nastytime_partner = nastytime_partner /
      20,
    nicetime_patient = nicetime_patient /
      20,
    nicetime_partner = nicetime_partner /
      20,
    neutraltime_patient = neutraltime_patient /
      20,
    neutraltime_partner = neutraltime_partner /
      20
  )
m22dat11_spaff_personalitydata_rescaled_noOutliers <-
  dplyr::filter(
    m22dat11_spaff_personalitydata_rescaled,
    neutraltime_patient > 0,
    nastytime_patient > 0,
    nicetime_patient > 0,
    nastytime_partner > 0,
    nicetime_partner > 0,
    neutraltime_partner > 0
  )
summary(
  sem(
    m24withSPAFF,
    m22dat11_spaff_personalitydata_rescaled_noOutliers,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    bootstrap = 5000,
    boot.ci.type = "bca.simple"
  ),
  fit.measures = TRUE
)
t1 <- m24 <- "

#direct effect
CON_post_partner ~ c1*CON_pre_partner
#mediators
CON_post_partner ~  b1*scpt +  b2*ccpt
scpt~a1*CON_pre_partner
ccpt~a2*CON_pre_partner
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
#indirect effect
a1b1 := a1*b1
a2b2:=a2*b2
a1b2 := a1*b2
a2b1 := a2*b1
dw1b1 := dw1*b1

CON_post_patient ~ c2*CON_pre_patient
CON_post_partner ~ CON_pre_patient
#CON_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ dw1*CON_pre_patient
scpr~CON_pre_patient
ccpr~CON_pre_patient

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr


"
t2 <- m24withSPAFFonly <- "


#direct effect
CON_post_partner ~ c1*CON_pre_partner
#mediators
CON_post_partner ~  b1*scpt +  b2*ccpt + b3*nastytime_patient + b4*nicetime_patient + b5*neutraltime_patient + b6*nastytime_partner + b7*nicetime_partner + b8*neutraltime_partner
scpt~a1*CON_pre_partner
ccpt~a2*CON_pre_partner
nastytime_patient ~ a3*CON_pre_partner + e3*CON_pre_patient
nicetime_patient ~ a4*CON_pre_partner + e4*CON_pre_patient
neutraltime_patient ~ a5*CON_pre_partner + e5*CON_pre_patient
nastytime_partner ~ a6*CON_pre_partner + e6*CON_pre_patient
nicetime_partner ~ a7*CON_pre_partner + e7*CON_pre_patient
neutraltime_partner ~ a8*CON_pre_partner + e8*CON_pre_patient
scpt ~ v_scpt
ccpt ~ v_ccpt
scpr ~ v_scpr
ccpr ~ v_ccpr
#indirect effect
a1b1 := a1*b1
a2b2:=a2*b2
a1b2 := a1*b2
a2b1 := a2*b1
dw1b1 := dw1*b1
a3e3 := a3*b3
a4b4 := a4*b4
a5b5 := a5*b5
a6b6 := a6*b6
a7b7 := a7*b7
a8e8 := a8*b8
# #a3e3:=a3*e3
# a4e4 := a4*e4
# #a5e5 := a5*e5
# a6e6 := a6*e6
# a7e7 := a7*e7
# #a8e8 := a8*e8
a3d3 :=a3*d3
a4d4 := a4*d4
a5d5 := a5*d5
a6d6 := a6*d6
a7d7 := a7*d7
a8d8 := a8*d8
e3d3 :=a3*d3
e4d4 := e4*d4
e5d5 := e5*d5
e6d6 := e6*d6
e7d7 := e7*d7
e8d8 := e8*d8
e4b4 := e4*b4
e6b6 := e6*b6
e7b7 := e7*b7

CON_post_patient ~ c2*CON_pre_patient + d3*nastytime_patient + d4*nicetime_patient + d5*neutraltime_patient + d6*nastytime_partner + d7*nicetime_partner + d8*neutraltime_partner
CON_post_partner ~ CON_pre_patient
#CON_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ dw1*CON_pre_patient
scpr~CON_pre_patient
ccpr~CON_pre_patient

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr
nastytime_patient ~~ nastytime_partner
nastytime_patient ~~ nicetime_partner
nastytime_patient ~~ neutraltime_partner
nastytime_patient ~~ nicetime_patient
nastytime_patient ~~ neutraltime_patient
nicetime_patient ~~nastytime_partner
nicetime_patient ~~ nicetime_partner
nicetime_patient ~~ neutraltime_partner
nicetime_patient ~~ neutraltime_patient
neutraltime_patient ~~ nastytime_partner
neutraltime_patient ~~ nicetime_partner
neutraltime_patient ~~ neutraltime_partner
nastytime_partner ~~ neutraltime_partner
nastytime_partner ~~ nicetime_partner
neutraltime_partner ~~ nicetime_partner

"
t4 <- "


#direct effect
CON_post_partner ~ c1*CON_pre_partner
#mediators
CON_post_partner ~  b1*scpt +  b2*ccpt
scpt~a1*CON_pre_partner
ccpt~a2*CON_pre_partner
scpt ~ v_scpt + iip_elevation_patient + pdtot_patient
ccpt ~ v_ccpt + iip_elevation_patient + pdtot_patient + pdtot_partner
scpr ~ v_scpr + iip_elevation_partner + pdtot_partner + v_ccpr
ccpr ~ v_ccpr + iip_elevation_partner + pdtot_partner + v_scpr + iip_elevation_patient
#indirect effect
a1b1 := a1*b1
a2b2:=a2*b2
a1b2 := a1*b2
a2b1 := a2*b1
dw1b1 := dw1*b1

CON_post_patient ~ c2*CON_pre_patient
CON_post_partner ~ CON_pre_patient
CON_post_patient ~ b5*scpt + b6*scpr + b7*ccpr + b8*ccpt
scpt ~ dw1*CON_pre_patient
scpr~CON_pre_patient
ccpr~CON_pre_patient

scpt~~ccpt
scpr~~ccpr
scpt~~ccpr
scpr~~ccpt
scpt~~scpr
ccpt~~ccpr

"
df_personalitydata <- dplyr::mutate(
  df_personalitydata,
  scpt = 100 * self_coupling_patient,
  scpr = 100 * self_coupling_partner,
  ccpt = 100 * cross_coupling_patient,
  ccpr = 100 * cross_coupling_partner
)
m22dat11_personalitydata <-
  inner_join(m22dat11, df_personalitydata, by = "PTNUM") %>% dplyr::rename(
    scpt = scpt.x,
    ccpt = ccpt.x,
    scpr = scpr.x,
    ccpr = ccpr.x
  )
t1_m <-
  sem(
    t1,
    m22dat11_spaff_personalitydata_all_rescaled_noOutliers,
    missing = "ML",
    estimator = "MLR",
    mimic = "Mplus",
    meanstructure = TRUE
  ) #accpetable fit 3488.604; with conditional.x = TRUE 1518
t2_m <-
  sem(
    t2,
    m22dat11_spaff_personalitydata_all_rescaled_noOutliers,
    missing = "ML",
    estimator = "MLR",
    mimic = "Mplus",
    meanstructure = TRUE
  ) #good fit 5475.771; with condiotnal.x = TRUE 3483.326
t3_m <-
  sem(
    t3,
    m22dat11_spaff_personalitydata_all_rescaled_noOutliers,
    missing = "ML",
    estimator = "MLR",
    mimic = "Mplus",
    meanstructure = TRUE
  ) #acceptable fit AIC 7288.017;with conditional.x = TRUE 3468
t4_m <-
  sem(
    t4,
    m22dat11_personalitydata,
    missing = "ML",
    estimator = "MLR",
    mimic = "Mplus",
    meanstructure = TRUE
  )
datatofit = m22dat11_spaff_personalitydata_rescaled_noOutliers
modeltofit = m24withSPAFF
#xpt = "pdtot_patient"
#xpr = "pdtot_partner"
fittedmodel <-
  sem(
    modeltofit,
    datatofit,
    missing = "ML",
    estimator = "MLR",
    mimic = "Mplus",
    meanstructure = TRUE
  )
summary(fittedmodel, fit.measures = TRUE)



ef <- genCookDist(model = modeltofit, data = datatofit)
cthresh = 3 * (mean(ef))
listofPTNUM <- c(as.numeric(datatofit$PTNUM))
#row.names(as.data.frame(cd)) <- listofPTNUM
nsubjs = length(datatofit$PTNUM)
#which(cd > 4/length(unique(icStats$LunaID))) #leads to a lot of exclusions for some ICs... n=18
#another rule of thumb is 4/(n - k - 1)
#which(cd > .04) #a more sane threshold looking at some of the plots
dfnew = data.frame(1:nsubjs)
dfnew$leverage = ef

dfnew$PTNUM = datatofit$PTNUM

cat("Excluding observations with Cook's D >", cthresh, "\n")

mup <- datatofit #initially just the full model
allbadcases <- c()
cthresh = 10
library(ggplot2)
library(cowplot)
while (max(dfnew$leverage) > cthresh) {
  #a more sane threshold looking at some of the plots
  badcase <-
    dfnew[which.max(dfnew$leverage), ] %>% dplyr::select(PTNUM)
  allbadcases <- c(allbadcases, badcase)
  cat("\nMax Cook's Distance is: ", max(dfnew$leverage), "\n")
  #cat("  Excluding ", badcase, "\n")
  #mup <- exclude.influence(mup, grouping="LunaID", level=badcase)
  mup <- dplyr::filter(datatofit,!PTNUM %in% allbadcases)
  #print(summary(mup), correlation = FALSE)
  
  dftoplot <-
    datatofit %>% mutate(bad = factor(
      PTNUM %in% unlist(allbadcases),
      levels = c(TRUE, FALSE),
      labels = c("Bad", "Good")
    ))
  #if 1a
  g1 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = scpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g2 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = scpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g3 <-
    ggplot(dftoplot, aes(x = CON_post_patient, y = ccpt, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  g4 <-
    ggplot(dftoplot, aes(x = CON_post_partner, y = ccpr, color = bad)) + geom_point() + stat_smooth(data =
                                                                                                      filter(dftoplot, bad == "Good"), method = "lm")
  
  
  
  
  obj <- plot_grid(g1, g2, g3, g4, nrow = 2)
  plot(obj)
  
  cd <- genCookDist(modeltofit, mup)
  plot(cd)
  dfnew <- data.frame(1:length(mup$PTNUM))
  dfnew$PTNUM <- mup$PTNUM
  dfnew$leverage <- cd
}
