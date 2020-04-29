library(R.matlab)
setwd("/Users/ams939/ics/pd_interaction_dynamics_analyses_feb2017")
data <- readMat("modelcomparison_logevidence.mat")
setwd("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/")
dt <- readMat("modelcomparison_rawparams.mat")
rawparamsModel3 <- as.data.frame(dt$sfrawparamsm3[, 1, ])
rawparamsModel3$PTNUM <- data$ids[1, ]
rawparamsModel4 <- as.data.frame(dt$sfrawparamsm4[, 1, ])
rawparamsModel4$PTNUM <- data$ids[1, ]
write.csv(rawparamsModel4, "rawparamsModel4.csv")
rawparamsModel1 <- as.data.frame(dt$sfrawparamsm1[, 1, ])
rawparamsModel1$PTNUM <- data$ids[1, ]
write.csv(rawparamsModel1, "rawparamsModel1.csv")
rawparamsModel2 <- as.data.frame(dt$sfrawparamsm2[, 1, ])
rawparamsModel2$PTNUM <- data$ids[1, ]
write.csv(rawparamsModel2, "rawparamsModel2.csv")

mean(rawparamsModel3)

nowackies_rawparamsM1 <-
  dplyr::filter(
    rawparamsModel1,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144
  )
nowackies_rawparamsM2 <-
  dplyr::filter(
    rawparamsModel2,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144
  )
nowackies_rawparamsM3 <-
  dplyr::filter(
    rawparamsModel3,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144
  )
nowackies_rawparamsM4 <-
  dplyr::filter(
    rawparamsModel4,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144
  )
nooutliers_rawparamsM1 <-
  dplyr::filter(
    rawparamsModel1,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8060,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144,
    PTNUM != 8020,
    PTNUM != 8052,
    PTNUM != 8100,
    PTNUM != 8126,
    PTNUM != 8127
  )
nooutliers_rawparamsM2 <-
  dplyr::filter(
    rawparamsModel2,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8060,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144,
    PTNUM != 8020,
    PTNUM != 8052,
    PTNUM != 8100,
    PTNUM != 8126,
    PTNUM != 8127
  )
nooutliers_rawparamsM3 <-
  dplyr::filter(
    rawparamsModel3,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8060,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144,
    PTNUM != 8020,
    PTNUM != 8052,
    PTNUM != 8100,
    PTNUM != 8126,
    PTNUM != 8127
  )
nooutliers_rawparamsM4 <-
  dplyr::filter(
    rawparamsModel4,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8060,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144,
    PTNUM != 8020,
    PTNUM != 8052,
    PTNUM != 8100,
    PTNUM != 8126,
    PTNUM != 8127
  )

readparams <-
  read.csv(file = "/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/PTNUM_Inclusion Spreadheet.csv")


setwd(
  "/Users/ams939/ics/pd_interaction_dynamics_analyses_feb2017/modelcomparison_matfiles"
)
SigmaThetas <- readMat("SigmaTheta_byModel.mat")
covM1 <- as.data.frame(t(as.data.frame(SigmaThetas$SigmaThetasM1)))
covM1$id <- rep(1:125, each = 6)
ids <- as.data.frame(seq(1, 125))
ids$PTNUM <- as.vector(SigmaThetas$ids)
ids$id <- ids$`seq(1, 125)`
covM1 <- merge(covM1, ids, by = "id")
covM1_nowackies <-
  dplyr::filter(
    covM1,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144
  )
covM1_nowackies <- split(covM1_nowackies, covM1_nowackies$id)
covM1df_nowackies <- lapply(covM1_nowackies, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})

covM1_nooutliers <- dplyr::filter(
  covM1,
  PTNUM != 8016,
  PTNUM != 8035,
  PTNUM != 8040,
  PTNUM != 8060,
  PTNUM != 8063,
  PTNUM != 8073,
  PTNUM != 8104,
  PTNUM != 8106,
  PTNUM != 8112,
  PTNUM != 8133,
  PTNUM != 8144,
  PTNUM != 8020,
  PTNUM != 8052,
  PTNUM != 8100,
  PTNUM != 8126,
  PTNUM != 8127
)
covM1_nooutliers <- split(covM1_nooutliers, covM1_nooutliers$id)
covM1df_nooutliers <- lapply(covM1_nooutliers, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})
covM1 <- split(covM1, covM1$id)

covM1df <- lapply(covM1, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})
covM1array <- array(dim = c(125, 6, 6))
for (i in 1:125) {
  for (j in 1:6) {
    for (k in 1:6) {
      covM1array[[i, j, k]] = covM1df[[i]][j, k]
      
      
      
    }
    
    
  }
}

covM1array_nowackies <- array(dim = c(116, 6, 6))
for (i in 1:116) {
  for (j in 1:6) {
    for (k in 1:6) {
      covM1array_nowackies[[i, j, k]] = covM1df_nowackies[[i]][j, k]
      
      
      
    }
    
    
  }
  
  
}



covM1array_nooutliers <- array(dim = c(110, 6, 6))
for (i in 1:110) {
  for (j in 1:6) {
    for (k in 1:6) {
      covM1array_nooutliers[[i, j, k]] = covM1df_nooutliers[[i]][j, k]
      
      
      
    }
    
    
  }
  
  
}
cov_m1 <- apply(covM1array, c(2, 3), function(x) {
  quantile(x, .75)
})



cov_m1_nowackies <- apply(covM1array_nowackies, c(2, 3), function(x) {
  quantile(x, .75)
})

cov_m1_nooutliers<- apply(covM1array_nooutliers, c(2, 3), function(x) {
  quantile(x, .75)
})

covM2 <- as.data.frame(t(as.data.frame(SigmaThetas$SigmaThetasM2)))
covM2$id <- rep(1:125, each = 4)
ids <- as.data.frame(seq(1, 125))
ids$PTNUM <- as.vector(SigmaThetas$ids)
ids$id <- ids$`seq(1, 125)`
covM2 <- merge(covM2, ids, by = "id")
covM2_nowackies <-
  dplyr::filter(
    covM2,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144
  )
covM2_nowackies <- split(covM2_nowackies, covM2_nowackies$id)
covM2df_nowackies <- lapply(covM2_nowackies, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})

covM2_nooutliers <- dplyr::filter(
  covM2,
  PTNUM != 8016,
  PTNUM != 8035,
  PTNUM != 8040,
  PTNUM != 8060,
  PTNUM != 8063,
  PTNUM != 8073,
  PTNUM != 8104,
  PTNUM != 8106,
  PTNUM != 8112,
  PTNUM != 8133,
  PTNUM != 8144,
  PTNUM != 8020,
  PTNUM != 8052,
  PTNUM != 8100,
  PTNUM != 8126,
  PTNUM != 8127
)
covM2_nooutliers <- split(covM2_nooutliers, covM2_nooutliers$id)
covM2df_nooutliers <- lapply(covM2_nooutliers, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})
covM2 <- split(covM2, covM2$id)

covM2df <- lapply(covM2, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})
covM2array <- array(dim = c(125, 4, 4))
for (i in 1:125) {
  for (j in 1:4) {
    for (k in 1:4) {
      covM2array[[i, j, k]] = covM2df[[i]][j, k]
      
      
      
    }
    
    
  }
}

covM2array_nowackies <- array(dim = c(116, 4, 4))
for (i in 1:116) {
  for (j in 1:4) {
    for (k in 1:4) {
      covM2array_nowackies[[i, j, k]] = covM2df_nowackies[[i]][j, k]
      
      
      
    }
    
    
  }
  
  
}



covM2array_nooutliers <- array(dim = c(110, 4, 4))
for (i in 1:110) {
  for (j in 1:4) {
    for (k in 1:4) {
      covM2array_nooutliers[[i, j, k]] = covM2df_nooutliers[[i]][j, k]
      
      
      
    }
    
    
  }
  
  
}
m2_cov <- apply(covM2array, c(2, 3), function(x) {
  quantile(x, .75)
})



m2_cov_nowackies <- apply(covM2array_nowackies, c(2, 3), function(x) {
  quantile(x, .75)
})

m2_cov_nooutliers <- apply(covM2array_nooutliers, c(2, 3), function(x) {
  quantile(x, .75)
})



covM3 <- as.data.frame(t(as.data.frame(SigmaThetas$SigmaThetasM3)))
covM3$id <- rep(1:125, each = 6)
ids <- as.data.frame(seq(1, 125))
ids$PTNUM <- as.vector(SigmaThetas$ids)
ids$id <- ids$`seq(1, 125)`
covM3 <- merge(covM3, ids, by = "id")
covM3_nowackies <-
  dplyr::filter(
    covM3,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144
  )
covM3_nowackies <- split(covM3_nowackies, covM3_nowackies$id)
covM3df_nowackies <- lapply(covM3_nowackies, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})

covM3_nooutliers <- dplyr::filter(
  covM3,
  PTNUM != 8016,
  PTNUM != 8035,
  PTNUM != 8040,
  PTNUM != 8060,
  PTNUM != 8063,
  PTNUM != 8073,
  PTNUM != 8104,
  PTNUM != 8106,
  PTNUM != 8112,
  PTNUM != 8133,
  PTNUM != 8144,
  PTNUM != 8020,
  PTNUM != 8052,
  PTNUM != 8100,
  PTNUM != 8126,
  PTNUM != 8127
)
covM3_nooutliers <- split(covM3_nooutliers, covM3_nooutliers$id)
covM3df_nooutliers <- lapply(covM3_nooutliers, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})
covM3 <- split(covM3, covM3$id)

covM3df <- lapply(covM3, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})
covM3array <- array(dim = c(125, 6, 6))
for (i in 1:125) {
  for (j in 1:6) {
    for (k in 1:6) {
      covM3array[[i, j, k]] = covM3df[[i]][j, k]
      
      
      
    }
    
    
  }
}

covM3array_nowackies <- array(dim = c(116, 6, 6))
for (i in 1:116) {
  for (j in 1:6) {
    for (k in 1:6) {
      covM3array_nowackies[[i, j, k]] = covM3df_nowackies[[i]][j, k]
      
      
      
    }
    
    
  }
  
  
}



covM3array_nooutliers <- array(dim = c(110, 6, 6))
for (i in 1:110) {
  for (j in 1:6) {
    for (k in 1:6) {
      covM3array_nooutliers[[i, j, k]] = covM3df_nooutliers[[i]][j, k]
      
      
      
    }
    
    
  }
  
  
}
cov_M3 <- apply(covM3array, c(2, 3), function(x) {
  quantile(x, .75)
})



cov_M3_nowackies <- apply(covM3array_nowackies, c(2, 3), function(x) {
  quantile(x, .75)
})

cov_M3_nooutliers<- apply(covM3array_nooutliers, c(2, 3), function(x) {
  quantile(x, .75)
})

covM4 <- as.data.frame(t(as.data.frame(SigmaThetas$SigmaThetasM4)))
covM4$id <- rep(1:125, each = 4)
ids <- as.data.frame(seq(1, 125))
ids$PTNUM <- as.vector(SigmaThetas$ids)
ids$id <- ids$`seq(1, 125)`
covM4 <- merge(covM4, ids, by = "id")
covM4_nowackies <-
  dplyr::filter(
    covM4,
    PTNUM != 8016,
    PTNUM != 8035,
    PTNUM != 8040,
    PTNUM != 8063,
    PTNUM != 8073,
    PTNUM != 8104,
    PTNUM != 8106,
    PTNUM != 8112,
    PTNUM != 8133,
    PTNUM != 8144
  )
covM4_nowackies <- split(covM4_nowackies, covM4_nowackies$id)
covM4df_nowackies <- lapply(covM4_nowackies, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})

covM4_nooutliers <- dplyr::filter(
  covM4,
  PTNUM != 8016,
  PTNUM != 8035,
  PTNUM != 8040,
  PTNUM != 8060,
  PTNUM != 8063,
  PTNUM != 8073,
  PTNUM != 8104,
  PTNUM != 8106,
  PTNUM != 8112,
  PTNUM != 8133,
  PTNUM != 8144,
  PTNUM != 8020,
  PTNUM != 8052,
  PTNUM != 8100,
  PTNUM != 8126,
  PTNUM != 8127
)
covM4_nooutliers <- split(covM4_nooutliers, covM4_nooutliers$id)
covM4df_nooutliers <- lapply(covM4_nooutliers, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})
covM4 <- split(covM4, covM4$id)

covM4df <- lapply(covM4, function(x) {
  x <- dplyr::select(x,-PTNUM,-id,-`seq(1, 125)`)
})
covM4array <- array(dim = c(125, 4, 4))
for (i in 1:125) {
  for (j in 1:4) {
    for (k in 1:4) {
      covM4array[[i, j, k]] = covM4df[[i]][j, k]
      
      
      
    }
    
    
  }
}

covM4array_nowackies <- array(dim = c(116, 4, 4))
for (i in 1:116) {
  for (j in 1:4) {
    for (k in 1:4) {
      covM4array_nowackies[[i, j, k]] = covM4df_nowackies[[i]][j, k]
      
      
      
    }
    
    
  }
  
  
}



covM4array_nooutliers <- array(dim = c(110, 4, 4))
for (i in 1:110) {
  for (j in 1:4) {
    for (k in 1:4) {
      covM4array_nooutliers[[i, j, k]] = covM4df_nooutliers[[i]][j, k]
      
      
      
    }
    
    
  }
  
  
}
M4_cov <- apply(covM4array, c(2, 3), function(x) {
  quantile(x, .75)
})



M4_cov_nowackies <- apply(covM4array_nowackies, c(2, 3), function(x) {
  quantile(x, .75)
})

M4_cov_nooutliers <- apply(covM4array_nooutliers, c(2, 3), function(x) {
  quantile(x, .75)
})







