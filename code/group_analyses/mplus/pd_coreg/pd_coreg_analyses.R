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
bsem_dir <- bsem_personalitymod_dir <- paste0(project_dir, "/code/pd_coreg")
setwd(paste0(project_dir, "/code/pd_coreg"))
# Source info and functions --------------------------------------------------------

source(paste0(project_dir, "/code/pd_coreg/support_fx.R"))


# Load Packages -----------------------------------------------------------

if (!require(pacman)) { install.packages("pacman"); library(pacman) }
p_load(tidyverse, R.matlab,lavaan,lattice, MplusAutomation)


# Load Data ---------------------------------------------------------------

load(paste0(project_dir, "/data/couples_baseline_clinical_27Jul2017.RData"))
df_99 <-  read.table(file = paste0("/Users/", usr, "/", box_dir, "/DEPENd/Projects/PD_Interaction_Dynamics/code/mplus_modelcomparison_dec2018/df67.dat"), sep = "\t")
df_99 <- df_99%>% dplyr::mutate_all(list(~if_else(.==".", as.integer(NA_real_), as.integer(.))))
names(df_99) <- c("PTNUM", "scpt", "ccpt", "scpr", "ccpr", "ECRanx_1", "ECRanx_0", "prnapt", "prnapr", "pnapt", "pnapr",
                  "elpt", "elpr", "cmpt", "cmpr", "prafpt", "prafpr","pafpt", "pafpr", "ECRavo_0", "ECRavo_1", "whichdf_num", "id")

df <- left_join(dplyr::select(df_99,PTNUM, scpt, ccpt, scpr, ccpr),  dplyr::select(couples_clin_wide, contains("sidp"), PTNUM))

df <- df %>% rename_all(.funs = funs(sub("sidp_", "",.)))


# el ----------------------------------------------------------------------

el_aonly <- "
ccpt ~ elpt
scpr ~ elpr
ccpr ~ elpr
scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr
elpt ~~ elpr

"
el_aonly_m <- sem(model = el_aonly, data = df_99, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
bsem_out <- runBSEM(modsyntax(dat = df_99, el_aonly))

# aspd --------------------------------------------------------------------

aspd_allfree <- "
scpt ~ antso_1
scpt ~ antso_0
ccpt ~ antso_1
ccpt ~ antso_0

scpr ~ antso_1
scpr ~ antso_0
ccpr ~ antso_1
ccpr ~ antso_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

antso_1 ~~ antso_0

"
aspd_allfree_m <- sem(model = aspd_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

aspd_afixed <- "
scpt ~ a*antso_1
scpt ~ antso_0
ccpt ~ b*antso_1
ccpt ~ antso_0

scpr ~ antso_1
scpr ~ a*antso_0
ccpr ~ antso_1
ccpr ~ b*antso_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

antso_1 ~~ antso_0

"
aspd_afixed_m <- sem(model = aspd_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

aspd_pfixed <- "
scpt ~ antso_1
scpt ~ c*antso_0
ccpt ~ antso_1
ccpt ~ d*antso_0

scpr ~ c*antso_1
scpr ~ antso_0
ccpr ~ d*antso_1
ccpr ~ antso_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

antso_1 ~~ antso_0

"
aspd_pfixed_m <- sem(model = aspd_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


aspd_apfixed <- "
scpt ~ a*antso_1
scpt ~ c*antso_0
ccpt ~ b*antso_1
ccpt ~ d*antso_0

scpr ~ c*antso_1
scpr ~ a*antso_0
ccpr ~ d*antso_1
ccpr ~ b*antso_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

antso_1 ~~ antso_0

"
aspd_apfixed_m <- sem(model = aspd_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
aspd_aonly <- "
scpt ~ antso_1
scpt ~ 0*antso_0
ccpt ~ antso_1
ccpt ~ 0*antso_0

scpr ~ 0*antso_1
scpr ~ antso_0
ccpr ~ 0*antso_1
ccpr ~ antso_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

antso_1 ~~ antso_0

"
aspd_aonly_m <- sem(model = aspd_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


aspd_ponly <- "
scpt ~ 0*antso_1
scpt ~ antso_0
ccpt ~ 0*antso_1
ccpt ~ antso_0

scpr ~ antso_1
scpr ~ 0*antso_0
ccpr ~ antso_1
ccpr ~ 0*antso_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

antso_1 ~~ antso_0

"
aspd_ponly_m <- sem(model = aspd_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(aspd_afixed_m, aspd_pfixed_m, aspd_apfixed_m, aspd_aonly_m, aspd_ponly_m)
# one effect for partners
bsem_out <- runBSEM(modsyntax(dat = df, aspd_aonly_m))
# avpd --------------------------------------------------------------------

avoid_allfree <- "
scpt ~ avoid_1
scpt ~ avoid_0
ccpt ~ avoid_1
ccpt ~ avoid_0

scpr ~ avoid_1
scpr ~ avoid_0
ccpr ~ avoid_1
ccpr ~ avoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

avoid_1 ~~ avoid_0

"
avoid_allfree_m <- sem(model = avoid_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

avoid_afixed <- "
scpt ~ a*avoid_1
scpt ~ avoid_0
ccpt ~ b*avoid_1
ccpt ~ avoid_0

scpr ~ avoid_1
scpr ~ a*avoid_0
ccpr ~ avoid_1
ccpr ~ b*avoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

avoid_1 ~~ avoid_0

"
avoid_afixed_m <- sem(model = avoid_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

avoid_pfixed <- "
scpt ~ avoid_1
scpt ~ c*avoid_0
ccpt ~ avoid_1
ccpt ~ d*avoid_0

scpr ~ c*avoid_1
scpr ~ avoid_0
ccpr ~ d*avoid_1
ccpr ~ avoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

avoid_1 ~~ avoid_0

"
avoid_pfixed_m <- sem(model = avoid_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


avoid_apfixed <- "
scpt ~ a*avoid_1
scpt ~ c*avoid_0
ccpt ~ b*avoid_1
ccpt ~ d*avoid_0

scpr ~ c*avoid_1
scpr ~ a*avoid_0
ccpr ~ d*avoid_1
ccpr ~ b*avoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

avoid_1 ~~ avoid_0

"
avoid_apfixed_m <- sem(model = avoid_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
avoid_aonly <- "
scpt ~ avoid_1
scpt ~ 0*avoid_0
ccpt ~ avoid_1
ccpt ~ 0*avoid_0

scpr ~ 0*avoid_1
scpr ~ avoid_0
ccpr ~ 0*avoid_1
ccpr ~ avoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

avoid_1 ~~ avoid_0

"
avoid_aonly_m <- sem(model = avoid_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


avoid_ponly <- "
scpt ~ 0*avoid_1
scpt ~ avoid_0
ccpt ~ 0*avoid_1
ccpt ~ avoid_0

scpr ~ avoid_1
scpr ~ 0*avoid_0
ccpr ~ avoid_1
ccpr ~ 0*avoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

avoid_1 ~~ avoid_0

"
avoid_ponly_m <- sem(model = avoid_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(avoid_afixed_m, avoid_pfixed_m, avoid_apfixed_m, avoid_aonly_m, avoid_ponly_m)
#one path for self-reg
bsem_out <- runBSEM(modsyntax(dat = df, avoid_ponly_m))

# bpd ---------------------------------------------------------------------

bordl_allfree <- "
scpt ~ bordl_1
scpt ~ bordl_0
ccpt ~ bordl_1
ccpt ~ bordl_0

scpr ~ bordl_1
scpr ~ bordl_0
ccpr ~ bordl_1
ccpr ~ bordl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

bordl_1 ~~ bordl_0

"
bordl_allfree_m <- sem(model = bordl_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

bordl_afixed <- "
scpt ~ a*bordl_1
scpt ~ bordl_0
ccpt ~ b*bordl_1
ccpt ~ bordl_0

scpr ~ bordl_1
scpr ~ a*bordl_0
ccpr ~ bordl_1
ccpr ~ b*bordl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

bordl_1 ~~ bordl_0

"
bordl_afixed_m <- sem(model = bordl_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

bordl_pfixed <- "
scpt ~ bordl_1
scpt ~ c*bordl_0
ccpt ~ bordl_1
ccpt ~ d*bordl_0

scpr ~ c*bordl_1
scpr ~ bordl_0
ccpr ~ d*bordl_1
ccpr ~ bordl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

bordl_1 ~~ bordl_0

"
bordl_pfixed_m <- sem(model = bordl_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


bordl_apfixed <- "
scpt ~ a*bordl_1
scpt ~ c*bordl_0
ccpt ~ b*bordl_1
ccpt ~ d*bordl_0

scpr ~ c*bordl_1
scpr ~ a*bordl_0
ccpr ~ d*bordl_1
ccpr ~ b*bordl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

bordl_1 ~~ bordl_0

"
bordl_apfixed_m <- sem(model = bordl_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
bordl_aonly <- "
scpt ~ bordl_1
scpt ~ 0*bordl_0
ccpt ~ bordl_1
ccpt ~ 0*bordl_0

scpr ~ 0*bordl_1
scpr ~ bordl_0
ccpr ~ 0*bordl_1
ccpr ~ bordl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

bordl_1 ~~ bordl_0

"
bordl_aonly_m <- sem(model = bordl_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


bordl_ponly <- "
scpt ~ 0*bordl_1
scpt ~ bordl_0
ccpt ~ 0*bordl_1
ccpt ~ bordl_0

scpr ~ bordl_1
scpr ~ 0*bordl_0
ccpr ~ bordl_1
ccpr ~ 0*bordl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

bordl_1 ~~ bordl_0

"
bordl_ponly_m <- sem(model = bordl_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(bordl_afixed_m, bordl_pfixed_m, bordl_apfixed_m, bordl_aonly_m, bordl_ponly_m)
# whichever is preferred goes here
bsem_out <- runBSEM(modsyntax(dat = df, bordl_pfixed_m))

# dpd ---------------------------------------------------------------------


depen_allfree <- "
scpt ~ depen_1
scpt ~ depen_0
ccpt ~ depen_1
ccpt ~ depen_0

scpr ~ depen_1
scpr ~ depen_0
ccpr ~ depen_1
ccpr ~ depen_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

depen_1 ~~ depen_0

"
depen_allfree_m <- sem(model = depen_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

depen_afixed <- "
scpt ~ a*depen_1
scpt ~ depen_0
ccpt ~ b*depen_1
ccpt ~ depen_0

scpr ~ depen_1
scpr ~ a*depen_0
ccpr ~ depen_1
ccpr ~ b*depen_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

depen_1 ~~ depen_0

"
depen_afixed_m <- sem(model = depen_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

depen_pfixed <- "
scpt ~ depen_1
scpt ~ c*depen_0
ccpt ~ depen_1
ccpt ~ d*depen_0

scpr ~ c*depen_1
scpr ~ depen_0
ccpr ~ d*depen_1
ccpr ~ depen_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

depen_1 ~~ depen_0

"
depen_pfixed_m <- sem(model = depen_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


depen_apfixed <- "
scpt ~ a*depen_1
scpt ~ c*depen_0
ccpt ~ b*depen_1
ccpt ~ d*depen_0

scpr ~ c*depen_1
scpr ~ a*depen_0
ccpr ~ d*depen_1
ccpr ~ b*depen_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

depen_1 ~~ depen_0

"
depen_apfixed_m <- sem(model = depen_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
depen_aonly <- "
scpt ~ depen_1
scpt ~ 0*depen_0
ccpt ~ depen_1
ccpt ~ 0*depen_0

scpr ~ 0*depen_1
scpr ~ depen_0
ccpr ~ 0*depen_1
ccpr ~ depen_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

depen_1 ~~ depen_0

"
depen_aonly_m <- sem(model = depen_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


depen_ponly <- "
scpt ~ 0*depen_1
scpt ~ depen_0
ccpt ~ 0*depen_1
ccpt ~ depen_0

scpr ~ depen_1
scpr ~ 0*depen_0
ccpr ~ depen_1
ccpr ~ 0*depen_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

depen_1 ~~ depen_0

"
depen_ponly_m <- sem(model = depen_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(depen_afixed_m, depen_pfixed_m, depen_apfixed_m, depen_aonly_m, depen_ponly_m)
#nothing going on here
bsem_out <- runBSEM(modsyntax(dat = df, depen_apfixed_m))
# histr -------------------------------------------------------------------

histr_allfree <- "
scpt ~ histr_1
scpt ~ histr_0
ccpt ~ histr_1
ccpt ~ histr_0

scpr ~ histr_1
scpr ~ histr_0
ccpr ~ histr_1
ccpr ~ histr_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

histr_1 ~~ histr_0

"
histr_allfree_m <- sem(model = histr_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

histr_afixed <- "
scpt ~ a*histr_1
scpt ~ histr_0
ccpt ~ b*histr_1
ccpt ~ histr_0

scpr ~ histr_1
scpr ~ a*histr_0
ccpr ~ histr_1
ccpr ~ b*histr_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

histr_1 ~~ histr_0

"
histr_afixed_m <- sem(model = histr_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

histr_pfixed <- "
scpt ~ histr_1
scpt ~ c*histr_0
ccpt ~ histr_1
ccpt ~ d*histr_0

scpr ~ c*histr_1
scpr ~ histr_0
ccpr ~ d*histr_1
ccpr ~ histr_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

histr_1 ~~ histr_0

"
histr_pfixed_m <- sem(model = histr_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


histr_apfixed <- "
scpt ~ a*histr_1
scpt ~ c*histr_0
ccpt ~ b*histr_1
ccpt ~ d*histr_0

scpr ~ c*histr_1
scpr ~ a*histr_0
ccpr ~ d*histr_1
ccpr ~ b*histr_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

histr_1 ~~ histr_0

"
histr_apfixed_m <- sem(model = histr_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
histr_aonly <- "
scpt ~ histr_1
scpt ~ 0*histr_0
ccpt ~ histr_1
ccpt ~ 0*histr_0

scpr ~ 0*histr_1
scpr ~ histr_0
ccpr ~ 0*histr_1
ccpr ~ histr_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

histr_1 ~~ histr_0

"
histr_aonly_m <- sem(model = histr_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


histr_ponly <- "
scpt ~ 0*histr_1
scpt ~ histr_0
ccpt ~ 0*histr_1
ccpt ~ histr_0

scpr ~ histr_1
scpr ~ 0*histr_0
ccpr ~ histr_1
ccpr ~ 0*histr_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

histr_1 ~~ histr_0

"
histr_ponly_m <- sem(model = histr_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(histr_afixed_m, histr_pfixed_m, histr_apfixed_m, histr_aonly_m, histr_ponly_m)
# whichever is preferred goes here
bsem_out <- runBSEM(modsyntax(dat = df, histr_allfree_m))

# narci -------------------------------------------------------------------

narci_allfree <- "
scpt ~ narci_1
scpt ~ narci_0
ccpt ~ narci_1
ccpt ~ narci_0

scpr ~ narci_1
scpr ~ narci_0
ccpr ~ narci_1
ccpr ~ narci_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

narci_1 ~~ narci_0

"
narci_allfree_m <- sem(model = narci_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

narci_afixed <- "
scpt ~ a*narci_1
scpt ~ narci_0
ccpt ~ b*narci_1
ccpt ~ narci_0

scpr ~ narci_1
scpr ~ a*narci_0
ccpr ~ narci_1
ccpr ~ b*narci_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

narci_1 ~~ narci_0

"
narci_afixed_m <- sem(model = narci_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

narci_pfixed <- "
scpt ~ narci_1
scpt ~ c*narci_0
ccpt ~ narci_1
ccpt ~ d*narci_0

scpr ~ c*narci_1
scpr ~ narci_0
ccpr ~ d*narci_1
ccpr ~ narci_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

narci_1 ~~ narci_0

"
narci_pfixed_m <- sem(model = narci_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


narci_apfixed <- "
scpt ~ a*narci_1
scpt ~ c*narci_0
ccpt ~ b*narci_1
ccpt ~ d*narci_0

scpr ~ c*narci_1
scpr ~ a*narci_0
ccpr ~ d*narci_1
ccpr ~ b*narci_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

narci_1 ~~ narci_0

"
narci_apfixed_m <- sem(model = narci_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
narci_aonly <- "
scpt ~ narci_1
scpt ~ 0*narci_0
ccpt ~ narci_1
ccpt ~ 0*narci_0

scpr ~ 0*narci_1
scpr ~ narci_0
ccpr ~ 0*narci_1
ccpr ~ narci_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

narci_1 ~~ narci_0

"
narci_aonly_m <- sem(model = narci_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


narci_ponly <- "
scpt ~ 0*narci_1
scpt ~ narci_0
ccpt ~ 0*narci_1
ccpt ~ narci_0

scpr ~ narci_1
scpr ~ 0*narci_0
ccpr ~ narci_1
ccpr ~ 0*narci_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

narci_1 ~~ narci_0

"
narci_ponly_m <- sem(model = narci_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(narci_afixed_m, narci_pfixed_m, narci_apfixed_m, narci_aonly_m, narci_ponly_m)
# one effect
bsem_out <- runBSEM(modsyntax(dat = df, narci_aonly_m))

# ocpd --------------------------------------------------------------------

obcmp_allfree <- "
scpt ~ obcmp_1
scpt ~ obcmp_0
ccpt ~ obcmp_1
ccpt ~ obcmp_0

scpr ~ obcmp_1
scpr ~ obcmp_0
ccpr ~ obcmp_1
ccpr ~ obcmp_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

obcmp_1 ~~ obcmp_0

"
obcmp_allfree_m <- sem(model = obcmp_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

obcmp_afixed <- "
scpt ~ a*obcmp_1
scpt ~ obcmp_0
ccpt ~ b*obcmp_1
ccpt ~ obcmp_0

scpr ~ obcmp_1
scpr ~ a*obcmp_0
ccpr ~ obcmp_1
ccpr ~ b*obcmp_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

obcmp_1 ~~ obcmp_0

"
obcmp_afixed_m <- sem(model = obcmp_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

obcmp_pfixed <- "
scpt ~ obcmp_1
scpt ~ c*obcmp_0
ccpt ~ obcmp_1
ccpt ~ d*obcmp_0

scpr ~ c*obcmp_1
scpr ~ obcmp_0
ccpr ~ d*obcmp_1
ccpr ~ obcmp_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

obcmp_1 ~~ obcmp_0

"
obcmp_pfixed_m <- sem(model = obcmp_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


obcmp_apfixed <- "
scpt ~ a*obcmp_1
scpt ~ c*obcmp_0
ccpt ~ b*obcmp_1
ccpt ~ d*obcmp_0

scpr ~ c*obcmp_1
scpr ~ a*obcmp_0
ccpr ~ d*obcmp_1
ccpr ~ b*obcmp_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

obcmp_1 ~~ obcmp_0

"
obcmp_apfixed_m <- sem(model = obcmp_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
obcmp_aonly <- "
scpt ~ obcmp_1
scpt ~ 0*obcmp_0
ccpt ~ obcmp_1
ccpt ~ 0*obcmp_0

scpr ~ 0*obcmp_1
scpr ~ obcmp_0
ccpr ~ 0*obcmp_1
ccpr ~ obcmp_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

obcmp_1 ~~ obcmp_0

"
obcmp_aonly_m <- sem(model = obcmp_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


obcmp_ponly <- "
scpt ~ 0*obcmp_1
scpt ~ obcmp_0
ccpt ~ 0*obcmp_1
ccpt ~ obcmp_0

scpr ~ obcmp_1
scpr ~ 0*obcmp_0
ccpr ~ obcmp_1
ccpr ~ 0*obcmp_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

obcmp_1 ~~ obcmp_0

"
obcmp_ponly_m <- sem(model = obcmp_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(obcmp_afixed_m, obcmp_pfixed_m, obcmp_apfixed_m, obcmp_aonly_m, obcmp_ponly_m)
# whichever is preferred goes here
bsem_out <- runBSEM(modsyntax(dat = df, obcmp_ponly_m))


# ppd ---------------------------------------------------------------------

parnd_allfree <- "
scpt ~ parnd_1
scpt ~ parnd_0
ccpt ~ parnd_1
ccpt ~ parnd_0

scpr ~ parnd_1
scpr ~ parnd_0
ccpr ~ parnd_1
ccpr ~ parnd_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

parnd_1 ~~ parnd_0

"
parnd_allfree_m <- sem(model = parnd_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

parnd_afixed <- "
scpt ~ a*parnd_1
scpt ~ parnd_0
ccpt ~ b*parnd_1
ccpt ~ parnd_0

scpr ~ parnd_1
scpr ~ a*parnd_0
ccpr ~ parnd_1
ccpr ~ b*parnd_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

parnd_1 ~~ parnd_0

"
parnd_afixed_m <- sem(model = parnd_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

parnd_pfixed <- "
scpt ~ parnd_1
scpt ~ c*parnd_0
ccpt ~ parnd_1
ccpt ~ d*parnd_0

scpr ~ c*parnd_1
scpr ~ parnd_0
ccpr ~ d*parnd_1
ccpr ~ parnd_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

parnd_1 ~~ parnd_0

"
parnd_pfixed_m <- sem(model = parnd_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


parnd_apfixed <- "
scpt ~ a*parnd_1
scpt ~ c*parnd_0
ccpt ~ b*parnd_1
ccpt ~ d*parnd_0

scpr ~ c*parnd_1
scpr ~ a*parnd_0
ccpr ~ d*parnd_1
ccpr ~ b*parnd_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

parnd_1 ~~ parnd_0

"
parnd_apfixed_m <- sem(model = parnd_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
parnd_aonly <- "
scpt ~ parnd_1
scpt ~ 0*parnd_0
ccpt ~ parnd_1
ccpt ~ 0*parnd_0

scpr ~ 0*parnd_1
scpr ~ parnd_0
ccpr ~ 0*parnd_1
ccpr ~ parnd_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

parnd_1 ~~ parnd_0

"
parnd_aonly_m <- sem(model = parnd_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


parnd_ponly <- "
scpt ~ 0*parnd_1
scpt ~ parnd_0
ccpt ~ 0*parnd_1
ccpt ~ parnd_0

scpr ~ parnd_1
scpr ~ 0*parnd_0
ccpr ~ parnd_1
ccpr ~ 0*parnd_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

parnd_1 ~~ parnd_0

"
parnd_ponly_m <- sem(model = parnd_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(parnd_afixed_m, parnd_pfixed_m, parnd_apfixed_m, parnd_aonly_m, parnd_ponly_m)
# a couple of self reg effects 
bsem_out <- runBSEM(modsyntax(dat = df, parnd_aonly_m))

# stypl -------------------------------------------------------------------

stypl_allfree <- "
scpt ~ stypl_1
scpt ~ stypl_0
ccpt ~ stypl_1
ccpt ~ stypl_0

scpr ~ stypl_1
scpr ~ stypl_0
ccpr ~ stypl_1
ccpr ~ stypl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

stypl_1 ~~ stypl_0

"
stypl_allfree_m <- sem(model = stypl_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

stypl_afixed <- "
scpt ~ a*stypl_1
scpt ~ stypl_0
ccpt ~ b*stypl_1
ccpt ~ stypl_0

scpr ~ stypl_1
scpr ~ a*stypl_0
ccpr ~ stypl_1
ccpr ~ b*stypl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

stypl_1 ~~ stypl_0

"
stypl_afixed_m <- sem(model = stypl_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

stypl_pfixed <- "
scpt ~ stypl_1
scpt ~ c*stypl_0
ccpt ~ stypl_1
ccpt ~ d*stypl_0

scpr ~ c*stypl_1
scpr ~ stypl_0
ccpr ~ d*stypl_1
ccpr ~ stypl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

stypl_1 ~~ stypl_0

"
stypl_pfixed_m <- sem(model = stypl_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


stypl_apfixed <- "
scpt ~ a*stypl_1
scpt ~ c*stypl_0
ccpt ~ b*stypl_1
ccpt ~ d*stypl_0

scpr ~ c*stypl_1
scpr ~ a*stypl_0
ccpr ~ d*stypl_1
ccpr ~ b*stypl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

stypl_1 ~~ stypl_0

"
stypl_apfixed_m <- sem(model = stypl_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
stypl_aonly <- "
scpt ~ stypl_1
scpt ~ 0*stypl_0
ccpt ~ stypl_1
ccpt ~ 0*stypl_0

scpr ~ 0*stypl_1
scpr ~ stypl_0
ccpr ~ 0*stypl_1
ccpr ~ stypl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

stypl_1 ~~ stypl_0

"
stypl_aonly_m <- sem(model = stypl_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


stypl_ponly <- "
scpt ~ 0*stypl_1
scpt ~ stypl_0
ccpt ~ 0*stypl_1
ccpt ~ stypl_0

scpr ~ stypl_1
scpr ~ 0*stypl_0
ccpr ~ stypl_1
ccpr ~ 0*stypl_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

stypl_1 ~~ stypl_0

"
stypl_ponly_m <- sem(model = stypl_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(stypl_afixed_m, stypl_pfixed_m, stypl_apfixed_m, stypl_aonly_m, stypl_ponly_m)
# self-reg effects
bsem_out <- runBSEM(modsyntax(dat = df, stypl_apfixed_m))

# szoid -------------------------------------------------------------------

                                                                                                                                                                                                                                                                                                                                                                                  

szoid_allfree <- "
scpt ~ szoid_1
scpt ~ szoid_0
ccpt ~ szoid_1
ccpt ~ szoid_0

scpr ~ szoid_1
scpr ~ szoid_0
ccpr ~ szoid_1
ccpr ~ szoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

szoid_1 ~~ szoid_0

"
szoid_allfree_m <- sem(model = szoid_allfree, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

szoid_afixed <- "
scpt ~ a*szoid_1
scpt ~ szoid_0
ccpt ~ b*szoid_1
ccpt ~ szoid_0

scpr ~ szoid_1
scpr ~ a*szoid_0
ccpr ~ szoid_1
ccpr ~ b*szoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

szoid_1 ~~ szoid_0

"
szoid_afixed_m <- sem(model = szoid_afixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

szoid_pfixed <- "
scpt ~ szoid_1
scpt ~ c*szoid_0
ccpt ~ szoid_1
ccpt ~ d*szoid_0

scpr ~ c*szoid_1
scpr ~ szoid_0
ccpr ~ d*szoid_1
ccpr ~ szoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

szoid_1 ~~ szoid_0

"
szoid_pfixed_m <- sem(model = szoid_pfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


szoid_apfixed <- "
scpt ~ a*szoid_1
scpt ~ c*szoid_0
ccpt ~ b*szoid_1
ccpt ~ d*szoid_0

scpr ~ c*szoid_1
scpr ~ a*szoid_0
ccpr ~ d*szoid_1
ccpr ~ b*szoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

szoid_1 ~~ szoid_0

"
szoid_apfixed_m <- sem(model = szoid_apfixed, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
szoid_aonly <- "
scpt ~ szoid_1
scpt ~ 0*szoid_0
ccpt ~ szoid_1
ccpt ~ 0*szoid_0

scpr ~ 0*szoid_1
scpr ~ szoid_0
ccpr ~ 0*szoid_1
ccpr ~ szoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

szoid_1 ~~ szoid_0

"
szoid_aonly_m <- sem(model = szoid_aonly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


szoid_ponly <- "
scpt ~ 0*szoid_1
scpt ~ szoid_0
ccpt ~ 0*szoid_1
ccpt ~ szoid_0

scpr ~ szoid_1
scpr ~ 0*szoid_0
ccpr ~ szoid_1
ccpr ~ 0*szoid_0

scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr

szoid_1 ~~ szoid_0

"
szoid_ponly_m <- sem(model = szoid_ponly, data = df, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(szoid_afixed_m, szoid_pfixed_m, szoid_apfixed_m, szoid_aonly_m, szoid_ponly_m)
# no effects
bsem_out <- runBSEM(modsyntax(dat = df, szoid_apfixed_m))



