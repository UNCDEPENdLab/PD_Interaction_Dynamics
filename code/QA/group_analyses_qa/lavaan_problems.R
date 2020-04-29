library(lavaan)
#load data
simpleData <- read.csv("simpleData.csv")

#model that works
cxsetting = TRUE
#cxsetting = "default" #this will unexpectedly lead to a change in LL scaling for models 3 and 4

simple1 <- "
large_self_coupling_patient ~  iip_elcpt + iip_elcpr
large_self_coupling_partner ~ iip_elcpr + iip_elcpt
large_cross_coupling_patient ~ iip_elcpt + iip_elcpr
large_cross_coupling_partner ~ iip_elcpt + iip_elcpr
"

simple1_m <- sem(simple1, simpleData, missing = "listwise", estimator = "ML", 
                 mimic="Mplus", meanstructure = TRUE, conditional.x=cxsetting)

#nested model where only self-effects retained, also works
#this model is not saturated (e.g., no path from iip_elcpt to large_self_coupling_partner)
simple2 <- "
large_self_coupling_patient ~  iip_elcpt
large_self_coupling_partner ~ iip_elcpr
large_cross_coupling_patient ~ iip_elcpt
large_cross_coupling_partner ~ iip_elcpr
"

simple2_m <- sem(simple2, simpleData, missing = "listwise", estimator = "ML", 
                 mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)
summary(simple2_m)
anova(simple1_m, simple2_m) #simple 2 is nested in simple 1

#### from here on, if we do not specify conditional.x = TRUE, the LL is much worse than models above
#this model is saturated
simple3 <- "
large_self_coupling_patient ~  iip_elcpr + iip_elcpt + pdcountcpt + pdcountcpr
large_self_coupling_partner ~ iip_elcpr + iip_elcpt + pdcountcpt + pdcountcpr
large_cross_coupling_patient ~   iip_elcpr + iip_elcpt + pdcountcpt + pdcountcpr
large_cross_coupling_partner ~   iip_elcpr + iip_elcpt + pdcountcpt + pdcountcpr
"

simple3_m <- sem(simple3, simpleData, missing = "listwise", estimator = "ML", 
                 mimic="Mplus", meanstructure = TRUE, conditional.x = cxsetting)

#this model is saturated
simple4 <- "
large_self_coupling_patient ~   pdcountcpt + pdcountcpr
large_self_coupling_partner ~   pdcountcpt + pdcountcpr
large_cross_coupling_patient ~  pdcountcpt + pdcountcpr
large_cross_coupling_partner ~  pdcountcpt + pdcountcpr
"

simple4_m <- sem(simple4, simpleData, missing = "listwise", estimator = "ML", 
                 mimic = "Mplus", meanstructure = TRUE, conditional.x = cxsetting)

anova(simple3_m, simple4_m) #simple 3 is nested in simple 4

sapply(list(simple1_m, simple2_m, simple3_m, simple4_m, simple5_m), logLik)
sapply(list(simple1_m, simple2_m, simple3_m, simple4_m, simple5_m), AIC)

#differences in missingness (does not seem to be the case)
#sapply(simpleData, function(x) { sum(!is.na(x)) })

#can probably trim from here down



#no a model with good fit and has similarly low AIC, but still running into issues
simple5 <- "
large_self_coupling_patient ~   p_sex_patient + p_sex_partner
large_self_coupling_partner ~ p_sex_patient + p_sex_partner
large_cross_coupling_patient ~   p_sex_patient + p_sex_partner
large_cross_coupling_partner ~   p_sex_patient + p_sex_partner
"

simple5_m <- sem(simple5, simpleData, missing = "listwise", estimator = "ML", 
                 mimic = "Mplus", meanstructure = TRUE, conditional.x = TRUE)

anova(simple1_m, simple5_m)

summary(simple5_m, fit.measures = TRUE)

anova(simple1_m,simple5_m) #grumpy about different set of variables even though fit statistics are much more similar than in other anovas



simple6 <- "
large_self_coupling_patient ~   p_sex_patient + p_sex_partner + iip_elcpt + iip_elcpr
large_self_coupling_partner ~ p_sex_patient + p_sex_partner + iip_elcpt + iip_elcpr
large_cross_coupling_patient ~   p_sex_patient + p_sex_partner + iip_elcpt + iip_elcpr
large_cross_coupling_partner ~   p_sex_patient + p_sex_partner + iip_elcpt + iip_elcpr
"

simple6_m <- sem(simple6, simpleData, missing = "listwise", estimator = "ML", 
                 mimic = "Mplus", meanstructure = TRUE, conditional.x = TRUE)

sapply(list(simple1_m, simple2_m, simple3_m, simple4_m, simple5_m, simple6_m), logLik)
sapply(list(simple1_m, simple2_m, simple3_m, simple4_m, simple5_m, simple6_m), AIC)