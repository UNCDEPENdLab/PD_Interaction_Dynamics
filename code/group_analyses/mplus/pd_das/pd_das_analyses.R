# Set up paths ------------------------------------------------------------
getusr <- Sys.getenv()
usr <- getusr[[which(names(getusr) == "LOGNAME")]] 
if(usr == "alisonmarie526") { 
  box_dir <- "Box" 
} else if (usr == "ams939") {
  box_dir <- "Box Sync"
} else {
  print("need to manually specify parent directory")
}
project_dir <- paste0("/Users/", usr, "/", box_dir, "/DEPENd/Projects/PD_Interaction_Dynamics")
bsem_dir <- bsem_personalitymod_dir <- paste0(project_dir, "/code/pd_das")
setwd(paste0(project_dir, "/code/pd_das"))
# Source info and functions --------------------------------------------------------

source(paste0(project_dir, "/code/pd_coreg/support_fx.R"))


# Load Packages -----------------------------------------------------------

if (!require(pacman)) { install.packages("pacman"); library(pacman) }
p_load(tidyverse, R.matlab,lavaan,lattice, MplusAutomation)


# Load Data ---------------------------------------------------------------

load(paste0(project_dir, "/data/couples_baseline_clinical_27Jul2017.RData"))
df_99 <-  read.table(file = paste0("/Users/", usr, "/", box_dir, "/DEPENd/Projects/PD_Interaction_Dynamics/code/mplus_modelcomparison_dec2018/df67_das.dat"), sep = "\t")
df_99 <- df_99%>% dplyr::mutate_all(list(~if_else(.==".", as.integer(NA_real_), as.integer(.))))
names(df_99) <- c("PTNUM", "scpt", "ccpt", "scpr", "ccpr", "ECRanx_1", "ECRanx_0", "prnapt", "prnapr", "pnapt","pnapr",
                "elpt", "elpr", "cmpt", "cmpr", "prafpt", "prafpr", "pafpt", "pafpr", "ECRavo_0", "ECRavo_1", "whichdf_num", "id",
                "rel", "dasfu1", "dasfu0", "dasbl1", "dasbl0")
df <- left_join(dplyr::select(df_99,PTNUM, dasfu1, dasfu0, dasbl1, dasbl0),  dplyr::select(couples_clin_wide, contains("sidp"), PTNUM))

df <- df %>% rename_all(.funs = funs(sub("sidp_", "",.)))


# aspd --------------------------------------------------------------------
antso_allfree <- "
dasbl1 ~ antso_1
dasbl1 ~ antso_0
dasbl0 ~ antso_1
dasbl0 ~ antso_0
dasbl1 ~~ dasbl0
antso_1 ~~ antso_0
"
antso_allfree_m <- sem(model = antso_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

antso_afixed <- "
dasbl1 ~ a*antso_1
dasbl1 ~ antso_0
dasbl0 ~ antso_1
dasbl0 ~ a*antso_0
dasbl1 ~~ dasbl0
antso_1 ~~ antso_0
"
antso_afixed_m <- sem(model = antso_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

antso_pfixed <- "
dasbl1 ~ antso_1
dasbl1 ~ b*antso_0
dasbl0 ~ b*antso_1
dasbl0 ~ antso_0
dasbl1 ~~ dasbl0
antso_1 ~~ antso_0
"
antso_pfixed_m <- sem(model = antso_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

antso_apfixed <- "
dasbl1 ~ a*antso_1
dasbl1 ~ b*antso_0
dasbl0 ~ b*antso_1
dasbl0 ~ a*antso_0
dasbl1 ~~ dasbl0
antso_1 ~~ antso_0
"
antso_apfixed_m <- sem(model = antso_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

antso_aonly <- "
dasbl1 ~ antso_1
dasbl1 ~ 0*antso_0
dasbl0 ~ 0*antso_1
dasbl0 ~ antso_0
dasbl1 ~~ dasbl0
antso_1 ~~ antso_0
"
antso_aonly_m <- sem(model = antso_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

antso_ponly <- "
dasbl1 ~ 0*antso_1
dasbl1 ~ antso_0
dasbl0 ~ antso_1
dasbl0 ~ 0*antso_0
dasbl1 ~~ dasbl0
antso_1 ~~ antso_0
"
antso_ponly_m <- sem(model = antso_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(antso_afixed_m, antso_pfixed_m, antso_apfixed_m, antso_aonly_m, antso_ponly_m)
bsem_out <- runBSEM(modsyntax(dat = df, antso_aonly_m))

# avpd --------------------------------------------------------------------
avoid_allfree <- "
dasbl1 ~ avoid_1
dasbl1 ~ avoid_0
dasbl0 ~ avoid_1
dasbl0 ~ avoid_0
dasbl1 ~~ dasbl0
avoid_1 ~~ avoid_0
"
avoid_allfree_m <- sem(model = avoid_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

avoid_afixed <- "
dasbl1 ~ a*avoid_1
dasbl1 ~ avoid_0
dasbl0 ~ avoid_1
dasbl0 ~ a*avoid_0
dasbl1 ~~ dasbl0
avoid_1 ~~ avoid_0
"
avoid_afixed_m <- sem(model = avoid_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

avoid_pfixed <- "
dasbl1 ~ avoid_1
dasbl1 ~ b*avoid_0
dasbl0 ~ b*avoid_1
dasbl0 ~ avoid_0
dasbl1 ~~ dasbl0
avoid_1 ~~ avoid_0
"
avoid_pfixed_m <- sem(model = avoid_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

avoid_apfixed <- "
dasbl1 ~ a*avoid_1
dasbl1 ~ b*avoid_0
dasbl0 ~ b*avoid_1
dasbl0 ~ a*avoid_0
dasbl1 ~~ dasbl0
avoid_1 ~~ avoid_0
"
avoid_apfixed_m <- sem(model = avoid_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

avoid_aonly <- "
dasbl1 ~ avoid_1
dasbl1 ~ 0*avoid_0
dasbl0 ~ 0*avoid_1
dasbl0 ~ avoid_0
dasbl1 ~~ dasbl0
avoid_1 ~~ avoid_0
"
avoid_aonly_m <- sem(model = avoid_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

avoid_ponly <- "
dasbl1 ~ 0*avoid_1
dasbl1 ~ avoid_0
dasbl0 ~ avoid_1
dasbl0 ~ 0*avoid_0
dasbl1 ~~ dasbl0
avoid_1 ~~ avoid_0
"
avoid_ponly_m <- sem(model = avoid_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(avoid_afixed_m, avoid_pfixed_m, avoid_apfixed_m, avoid_aonly_m, avoid_ponly_m)
bsem_out <- runBSEM(modsyntax(dat = df, avoid_ponly_m))

# dpd ---------------------------------------------------------------------

depen_allfree <- "
dasbl1 ~ depen_1
dasbl1 ~ depen_0
dasbl0 ~ depen_1
dasbl0 ~ depen_0
dasbl1 ~~ dasbl0
depen_1 ~~ depen_0
"
depen_allfree_m <- sem(model = depen_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

depen_afixed <- "
dasbl1 ~ a*depen_1
dasbl1 ~ depen_0
dasbl0 ~ depen_1
dasbl0 ~ a*depen_0
dasbl1 ~~ dasbl0
depen_1 ~~ depen_0
"
depen_afixed_m <- sem(model = depen_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

depen_pfixed <- "
dasbl1 ~ depen_1
dasbl1 ~ b*depen_0
dasbl0 ~ b*depen_1
dasbl0 ~ depen_0
dasbl1 ~~ dasbl0
depen_1 ~~ depen_0
"
depen_pfixed_m <- sem(model = depen_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

depen_apfixed <- "
dasbl1 ~ a*depen_1
dasbl1 ~ b*depen_0
dasbl0 ~ b*depen_1
dasbl0 ~ a*depen_0
dasbl1 ~~ dasbl0
depen_1 ~~ depen_0
"
depen_apfixed_m <- sem(model = depen_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

depen_aonly <- "
dasbl1 ~ depen_1
dasbl1 ~ 0*depen_0
dasbl0 ~ 0*depen_1
dasbl0 ~ depen_0
dasbl1 ~~ dasbl0
depen_1 ~~ depen_0
"
depen_aonly_m <- sem(model = depen_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

depen_ponly <- "
dasbl1 ~ 0*depen_1
dasbl1 ~ depen_0
dasbl0 ~ depen_1
dasbl0 ~ 0*depen_0
dasbl1 ~~ dasbl0
depen_1 ~~ depen_0
"
depen_ponly_m <- sem(model = depen_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(depen_afixed_m, depen_pfixed_m, depen_apfixed_m, depen_aonly_m, depen_ponly_m)
bsem_out <- runBSEM(modsyntax(dat = df, depen_ponly_m))
# hpd ---------------------------------------------------------------------

histr_allfree <- "
dasbl1 ~ histr_1
dasbl1 ~ histr_0
dasbl0 ~ histr_1
dasbl0 ~ histr_0
dasbl1 ~~ dasbl0
histr_1 ~~ histr_0
"
histr_allfree_m <- sem(model = histr_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

histr_afixed <- "
dasbl1 ~ a*histr_1
dasbl1 ~ histr_0
dasbl0 ~ histr_1
dasbl0 ~ a*histr_0
dasbl1 ~~ dasbl0
histr_1 ~~ histr_0
"
histr_afixed_m <- sem(model = histr_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

histr_pfixed <- "
dasbl1 ~ histr_1
dasbl1 ~ b*histr_0
dasbl0 ~ b*histr_1
dasbl0 ~ histr_0
dasbl1 ~~ dasbl0
histr_1 ~~ histr_0
"
histr_pfixed_m <- sem(model = histr_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

histr_apfixed <- "
dasbl1 ~ a*histr_1
dasbl1 ~ b*histr_0
dasbl0 ~ b*histr_1
dasbl0 ~ a*histr_0
dasbl1 ~~ dasbl0
histr_1 ~~ histr_0
"
histr_apfixed_m <- sem(model = histr_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

histr_aonly <- "
dasbl1 ~ histr_1
dasbl1 ~ 0*histr_0
dasbl0 ~ 0*histr_1
dasbl0 ~ histr_0
dasbl1 ~~ dasbl0
histr_1 ~~ histr_0
"
histr_aonly_m <- sem(model = histr_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

histr_ponly <- "
dasbl1 ~ 0*histr_1
dasbl1 ~ histr_0
dasbl0 ~ histr_1
dasbl0 ~ 0*histr_0
dasbl1 ~~ dasbl0
histr_1 ~~ histr_0
"
histr_ponly_m <- sem(model = histr_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(histr_afixed_m, histr_pfixed_m, histr_apfixed_m, histr_aonly_m, histr_ponly_m)
bsem_out <- runBSEM(modsyntax(dat = df, histr_aonly_m))
# npd ---------------------------------------------------------------------

narci_allfree <- "
dasbl1 ~ narci_1
dasbl1 ~ narci_0
dasbl0 ~ narci_1
dasbl0 ~ narci_0
dasbl1 ~~ dasbl0
narci_1 ~~ narci_0
"
narci_allfree_m <- sem(model = narci_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

narci_afixed <- "
dasbl1 ~ a*narci_1
dasbl1 ~ narci_0
dasbl0 ~ narci_1
dasbl0 ~ a*narci_0
dasbl1 ~~ dasbl0
narci_1 ~~ narci_0
"
narci_afixed_m <- sem(model = narci_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

narci_pfixed <- "
dasbl1 ~ narci_1
dasbl1 ~ b*narci_0
dasbl0 ~ b*narci_1
dasbl0 ~ narci_0
dasbl1 ~~ dasbl0
narci_1 ~~ narci_0
"
narci_pfixed_m <- sem(model = narci_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

narci_apfixed <- "
dasbl1 ~ a*narci_1
dasbl1 ~ b*narci_0
dasbl0 ~ b*narci_1
dasbl0 ~ a*narci_0
dasbl1 ~~ dasbl0
narci_1 ~~ narci_0
"
narci_apfixed_m <- sem(model = narci_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

narci_aonly <- "
dasbl1 ~ narci_1
dasbl1 ~ 0*narci_0
dasbl0 ~ 0*narci_1
dasbl0 ~ narci_0
dasbl1 ~~ dasbl0
narci_1 ~~ narci_0
"
narci_aonly_m <- sem(model = narci_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

narci_ponly <- "
dasbl1 ~ 0*narci_1
dasbl1 ~ narci_0
dasbl0 ~ narci_1
dasbl0 ~ 0*narci_0
dasbl1 ~~ dasbl0
narci_1 ~~ narci_0
"
narci_ponly_m <- sem(model = narci_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(narci_afixed_m, narci_pfixed_m, narci_apfixed_m, narci_aonly_m, narci_ponly_m)
bsem_out <- runBSEM(modsyntax(dat = df, narci_pfixed_m))

# ocpd --------------------------------------------------------------------
obcmp_allfree <- "
dasbl1 ~ obcmp_1
dasbl1 ~ obcmp_0
dasbl0 ~ obcmp_1
dasbl0 ~ obcmp_0
dasbl1 ~~ dasbl0
obcmp_1 ~~ obcmp_0
"
obcmp_allfree_m <- sem(model = obcmp_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

obcmp_afixed <- "
dasbl1 ~ a*obcmp_1
dasbl1 ~ obcmp_0
dasbl0 ~ obcmp_1
dasbl0 ~ a*obcmp_0
dasbl1 ~~ dasbl0
obcmp_1 ~~ obcmp_0
"
obcmp_afixed_m <- sem(model = obcmp_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

obcmp_pfixed <- "
dasbl1 ~ obcmp_1
dasbl1 ~ b*obcmp_0
dasbl0 ~ b*obcmp_1
dasbl0 ~ obcmp_0
dasbl1 ~~ dasbl0
obcmp_1 ~~ obcmp_0
"
obcmp_pfixed_m <- sem(model = obcmp_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

obcmp_apfixed <- "
dasbl1 ~ a*obcmp_1
dasbl1 ~ b*obcmp_0
dasbl0 ~ b*obcmp_1
dasbl0 ~ a*obcmp_0
dasbl1 ~~ dasbl0
obcmp_1 ~~ obcmp_0
"
obcmp_apfixed_m <- sem(model = obcmp_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

obcmp_aonly <- "
dasbl1 ~ obcmp_1
dasbl1 ~ 0*obcmp_0
dasbl0 ~ 0*obcmp_1
dasbl0 ~ obcmp_0
dasbl1 ~~ dasbl0
obcmp_1 ~~ obcmp_0
"
obcmp_aonly_m <- sem(model = obcmp_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

obcmp_ponly <- "
dasbl1 ~ 0*obcmp_1
dasbl1 ~ obcmp_0
dasbl0 ~ obcmp_1
dasbl0 ~ 0*obcmp_0
dasbl1 ~~ dasbl0
obcmp_1 ~~ obcmp_0
"
obcmp_ponly_m <- sem(model = obcmp_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(obcmp_afixed_m, obcmp_pfixed_m, obcmp_apfixed_m, obcmp_aonly_m, obcmp_ponly_m)
bsem_out <- runBSEM(modsyntax(dat = df, obcmp_aonly_m))

# ppd ---------------------------------------------------------------------
parnd_allfree <- "
dasbl1 ~ parnd_1
dasbl1 ~ parnd_0
dasbl0 ~ parnd_1
dasbl0 ~ parnd_0
dasbl1 ~~ dasbl0
parnd_1 ~~ parnd_0
"
parnd_allfree_m <- sem(model = parnd_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

parnd_afixed <- "
dasbl1 ~ a*parnd_1
dasbl1 ~ parnd_0
dasbl0 ~ parnd_1
dasbl0 ~ a*parnd_0
dasbl1 ~~ dasbl0
parnd_1 ~~ parnd_0
"
parnd_afixed_m <- sem(model = parnd_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

parnd_pfixed <- "
dasbl1 ~ parnd_1
dasbl1 ~ b*parnd_0
dasbl0 ~ b*parnd_1
dasbl0 ~ parnd_0
dasbl1 ~~ dasbl0
parnd_1 ~~ parnd_0
"
parnd_pfixed_m <- sem(model = parnd_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

parnd_apfixed <- "
dasbl1 ~ a*parnd_1
dasbl1 ~ b*parnd_0
dasbl0 ~ b*parnd_1
dasbl0 ~ a*parnd_0
dasbl1 ~~ dasbl0
parnd_1 ~~ parnd_0
"
parnd_apfixed_m <- sem(model = parnd_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

parnd_aonly <- "
dasbl1 ~ parnd_1
dasbl1 ~ 0*parnd_0
dasbl0 ~ 0*parnd_1
dasbl0 ~ parnd_0
dasbl1 ~~ dasbl0
parnd_1 ~~ parnd_0
"
parnd_aonly_m <- sem(model = parnd_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

parnd_ponly <- "
dasbl1 ~ 0*parnd_1
dasbl1 ~ parnd_0
dasbl0 ~ parnd_1
dasbl0 ~ 0*parnd_0
dasbl1 ~~ dasbl0
parnd_1 ~~ parnd_0
"
parnd_ponly_m <- sem(model = parnd_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(parnd_afixed_m, parnd_pfixed_m, parnd_apfixed_m, parnd_aonly_m, parnd_ponly_m)
bsem_out <- runBSEM(modsyntax(dat = df, parnd_pfixed_m))

# stypl -------------------------------------------------------------------

stypl_allfree <- "
dasbl1 ~ stypl_1
dasbl1 ~ stypl_0
dasbl0 ~ stypl_1
dasbl0 ~ stypl_0
dasbl1 ~~ dasbl0
stypl_1 ~~ stypl_0
"
stypl_allfree_m <- sem(model = stypl_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

stypl_afixed <- "
dasbl1 ~ a*stypl_1
dasbl1 ~ stypl_0
dasbl0 ~ stypl_1
dasbl0 ~ a*stypl_0
dasbl1 ~~ dasbl0
stypl_1 ~~ stypl_0
"
stypl_afixed_m <- sem(model = stypl_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

stypl_pfixed <- "
dasbl1 ~ stypl_1
dasbl1 ~ b*stypl_0
dasbl0 ~ b*stypl_1
dasbl0 ~ stypl_0
dasbl1 ~~ dasbl0
stypl_1 ~~ stypl_0
"
stypl_pfixed_m <- sem(model = stypl_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

stypl_apfixed <- "
dasbl1 ~ a*stypl_1
dasbl1 ~ b*stypl_0
dasbl0 ~ b*stypl_1
dasbl0 ~ a*stypl_0
dasbl1 ~~ dasbl0
stypl_1 ~~ stypl_0
"
stypl_apfixed_m <- sem(model = stypl_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

stypl_aonly <- "
dasbl1 ~ stypl_1
dasbl1 ~ 0*stypl_0
dasbl0 ~ 0*stypl_1
dasbl0 ~ stypl_0
dasbl1 ~~ dasbl0
stypl_1 ~~ stypl_0
"
stypl_aonly_m <- sem(model = stypl_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

stypl_ponly <- "
dasbl1 ~ 0*stypl_1
dasbl1 ~ stypl_0
dasbl0 ~ stypl_1
dasbl0 ~ 0*stypl_0
dasbl1 ~~ dasbl0
stypl_1 ~~ stypl_0
"
stypl_ponly_m <- sem(model = stypl_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(stypl_afixed_m, stypl_pfixed_m, stypl_apfixed_m, stypl_aonly_m, stypl_ponly_m)
bsem_out <- runBSEM(modsyntax(dat = df, stypl_afixed_m))
# szoid --------------------------------------------------------------------

szoid_allfree <- "
dasbl1 ~ szoid_1
dasbl1 ~ szoid_0
dasbl0 ~ szoid_1
dasbl0 ~ szoid_0
dasbl1 ~~ dasbl0
szoid_1 ~~ szoid_0
"
szoid_allfree_m <- sem(model = szoid_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

szoid_afixed <- "
dasbl1 ~ a*szoid_1
dasbl1 ~ szoid_0
dasbl0 ~ szoid_1
dasbl0 ~ a*szoid_0
dasbl1 ~~ dasbl0
szoid_1 ~~ szoid_0
"
szoid_afixed_m <- sem(model = szoid_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

szoid_pfixed <- "
dasbl1 ~ szoid_1
dasbl1 ~ b*szoid_0
dasbl0 ~ b*szoid_1
dasbl0 ~ szoid_0
dasbl1 ~~ dasbl0
szoid_1 ~~ szoid_0
"
szoid_pfixed_m <- sem(model = szoid_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

szoid_apfixed <- "
dasbl1 ~ a*szoid_1
dasbl1 ~ b*szoid_0
dasbl0 ~ b*szoid_1
dasbl0 ~ a*szoid_0
dasbl1 ~~ dasbl0
szoid_1 ~~ szoid_0
"
szoid_apfixed_m <- sem(model = szoid_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

szoid_aonly <- "
dasbl1 ~ szoid_1
dasbl1 ~ 0*szoid_0
dasbl0 ~ 0*szoid_1
dasbl0 ~ szoid_0
dasbl1 ~~ dasbl0
szoid_1 ~~ szoid_0
"
szoid_aonly_m <- sem(model = szoid_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

szoid_ponly <- "
dasbl1 ~ 0*szoid_1
dasbl1 ~ szoid_0
dasbl0 ~ szoid_1
dasbl0 ~ 0*szoid_0
dasbl1 ~~ dasbl0
szoid_1 ~~ szoid_0
"
szoid_ponly_m <- sem(model = szoid_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(szoid_afixed_m, szoid_pfixed_m, szoid_apfixed_m, szoid_aonly_m, szoid_ponly_m)
bsem_out <- runBSEM(modsyntax(dat = df, szoid_ponly_m))

