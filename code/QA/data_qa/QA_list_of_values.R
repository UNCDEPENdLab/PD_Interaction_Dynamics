setwd("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")
cleaning <- read.csv("6feb_simple_QA.csv", header = TRUE)


need_to_be_pulled <- dplyr::filter(cleaning, Pull > 0)



write.csv(need_to_be_pulled, "Please_pull_for_QA.csv")
setwd("/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/")

cleaningvanbse <- read.csv("vanbse_19mar_simple_QA.csv", header = TRUE)
cleaningvanbse$ID <- paste0(cleaningvanbse$PTNUM, cleaningvanbse$L.or.R)
vanbse_need_to_be_pulled <- dplyr::filter(cleaningvanbse, Pull > 0)
vanbse_need_to_be_pulled_nums <- c()
vanbse_need_to_be_pulled_nums <- need_to_be_pulled$ID
write.csv(vanbse_need_to_be_pulled, "VanBse_QA.csv")

