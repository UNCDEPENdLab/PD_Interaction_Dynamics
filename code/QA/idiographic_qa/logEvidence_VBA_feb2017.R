library(R.matlab)
setwd("~/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/icsVBA/")
data <- readMat("IBImodels_logEvidence_feb2017s.mat")
logEvidence <- t(data$logEvidence)
logEvidence <- as.data.frame(logEvidence)
#data <- dplyr::filter(logEvidence, logEvidence < -79999)
anybad <- apply(logEvidence, 1, function(row) {
  any(row < -79999)
})
idlistgood <- as.data.frame(as.vector(data$ids))
idlistgood$anybad <- as.vector(anybad)
rawParamsModel1 <- as.data.frame(data$rawparameters[,1,])
rawParamsModel1 <- dplyr::select(rawParamsModel1, V1)
rawParamsModel1$PTNUM <- as.vector(data$ids)
rawParamsModel1$anybad <- as.vector(idlistgood$anybad)
logEvidence$PTNUM <- as.vector(data$ids)
logEvidenceModel1 <- select(logEvidence, V1, as.vector(PTNUM))
logEvidenceModel5 <- select(logEvidence, V5, as.vector(PTNUM))
logEvidenceModel4 <- select(logEvidence, V4, as.vector(PTNUM))
rawParamsModel1 <- inner_join(rawParamsModel1, logEvidenceModel1, by = "PTNUM")
#rawParamsModel1goodData <- dplyr::filter(rawParamsModel1, anybad == FALSE)
#rawParamsModel1goodData <- inner_join(rawParamsModel1goodData, model1fits, by = "PTNUM", copy = TRUE)
rawParamsModel1goodData <- select(rawParamsModel1goodData, -anybad)
write.csv(rawParamsModel1, file = "parameters_model1_feb2017.csv", row.names = TRUE)
rawParamsModel5 <- as.data.frame(data$rawparameters[,5,])
rawParamsModel5 <- dplyr::select(rawParamsModel5, -V4)
rawParamsModel5$PTNUM <- as.vector(data$ids)
rawParamsModel5 <- inner_join(rawParamsModel5, logEvidenceModel5, by = "PTNUM")
write.csv(rawParamsModel5, file = "parameters_model5_feb2017.csv", row.names = TRUE)
logEvidence$PTNUM <- as.vector(data$ids)
logEvidenceModel1 <- select(logEvidence, V1, PTNUM)
logEvidenceModel5 <- select(logEvidence, V5, PTNUM)
#logEvidence$PTNUM <- logEvidence$id
baddata <- logEvidence[anybad,]
gooddata <- logEvidence[!anybad,]

str(data)
rawParamsModel4 <-  as.data.frame(data$rawparameters[,4,])
rawParamsModel4$PTNUM <- as.vector(data$ids)
rawParamsModel4$anybad <- as.vector(idlistgood$anybad)
rawParamsModel4 <- inner_join(rawParamsModel4, logEvidenceModel4, by = "PTNUM")
rawParamsModel4 <- rename(rawParamsModel4, V4 = V4.x)
write.csv(rawParamsModel4, file = "parameters_model4_feb2017.csv", row.names = TRUE)


#filter(logEvidence, logEvidence < -79999)
# vec <- c()
# for (i in 1:125) {
#   for (j in 1:10) 
#     if (data$logEvidence[[j,i]] < -79999) {
#       id <- data$ids[[i]]
#       logev <- data$logEvidence[[j, i]]
#       a <- c(id, logev, i)
#     }
#   else a <- c(data$ids[[i]], NA, i)
#   
#   vec <- c(a)
#   print(vec)
#   
# }

##look at list elements 13, 28, 35, 39, 47, 84 for poor fits (i.e. less than 80000)