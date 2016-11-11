library(R.matlab)
library(dplyr)
load("/Users/ams939/Box Sync/DEPENd/Couples Data/data/align_split_atdifferentfreqs.RData")

basedir <- "/Users/ams939/Box Sync/DEPENd/Couples Data/data/dyad_txt"

align_split_10Hz_PatientPartner <- lapply(align_split_10Hz, alignLR_PatientPartner)

##4Hz version
lapply(align_split_10Hz_PatientPartner, function(couple) {
    couple$time <- couple$ms / 1000
    write.table(file=file.path(basedir, paste0(couple$PTNUM[1], "_ibis.txt")), couple[,c("PTNUM", "ms", "ibi_interp_detrend_patient", "ibi_interp_detrend_partner",
                                                                                         "dom_interp_detrend_patient", "dom_interp_detrend_partner", "aff_interp_detrend_patient", "aff_interp_detrend_partner")],
                row.names=FALSE, col.names=FALSE)
})
