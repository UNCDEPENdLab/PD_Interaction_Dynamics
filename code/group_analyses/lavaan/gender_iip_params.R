library(lavaan)
#load data
simpleData <- read.csv("/data/simpleData.csv")

#model that works

simple1 <- "
large_self_coupling_patient ~  iip_elcpt + iip_elcpr
large_self_coupling_partner ~ iip_elcpr + iip_elcpt
large_cross_coupling_patient ~ iip_elcpt + iip_elcpr
large_cross_coupling_partner ~ iip_elcpt + iip_elcpr
"

simple1_m <- sem(simple1, simpleData, missing = "listwise", estimator = "ML", 
                 mimic="Mplus", meanstructure = TRUE, conditional.x=TRUE)

#nested model where only self-effects retained, also works
simple2 <- "
large_self_coupling_patient ~  iip_elcpt
large_self_coupling_partner ~ iip_elcpr
large_cross_coupling_patient ~ iip_elcpt
large_cross_coupling_partner ~ iip_elcpr
"

simple2_m <- sem(simple2, simpleData, missing = "listwise", estimator = "ML", 
                 mimic="Mplus", meanstructure = TRUE, conditional.x = TRUE)

anova(simple1_m, simple2_m) #simple 2 is nested in simple 1

#### from here on, if we do not specify conditional.x = TRUE, the LL is much worse than models above
simple3 <- "
large_self_coupling_patient ~  p_sex_partner + p_sex_patient
large_self_coupling_partner ~ p_sex_partner + p_sex_patient
large_cross_coupling_patient ~   p_sex_partner + p_sex_patient
large_cross_coupling_partner ~   p_sex_partner + p_sex_patient
"

simple3_m <- sem(simple3, simpleData, missing = "listwise", estimator = "ML", 
                 mimic="Mplus", meanstructure = TRUE, conditional.x = TRUE)

simple4 <- "
large_self_coupling_patient ~   p_sex_partner + p_sex_patient + iip_elcpt + iip_elcpr
large_self_coupling_partner ~  p_sex_partner + p_sex_patient + iip_elcpt + iip_elcpr
large_cross_coupling_patient ~  p_sex_partner + p_sex_patient + iip_elcpt + iip_elcpr
large_cross_coupling_partner ~  p_sex_partner + p_sex_patient + iip_elcpt + iip_elcpr
"

simple4_m <- sem(simple4, simpleData, missing = "listwise", estimator = "ML", 
                 mimic = "Mplus", meanstructure = TRUE, conditional.x = TRUE)

anova(simple3_m, simple4_m) #simple 3 is nested in simple 4
anova(simple1_m, simple3_m)
anova(simple1_m, simple4_m)
sapply(list(simple1_m, simple2_m, simple3_m, simple4_m), logLik)


#differences in missingness (does not seem to be the case)
sapply(simpleData, function(x) { sum(!is.na(x)) })

anova(simple1_m,simple4_m) #grumpy about different set of variables even though fit statistics are much more similar than in other anovas

