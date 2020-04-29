#Lavaan and Mplus APIM functions
#Lavaan is setup for conventional APIMs (4 variables)
#Mplus is multilevel APIM where alters are clustered within individuals (e.g., degree centrality or anger ratings)

runAPIM <- function(df, DV, predictors, partners=c("_0", "_1"), additional="", printall=FALSE) {
  require(lavaan)
  #by default, only print indistinguishable dyads model (printall=FALSE)
  
  indistinguishable_syn <- ""
  for (p in 1:length(predictors)) {
    indistinguishable_syn <- paste0(indistinguishable_syn, DV, partners[1], additional, " ~ a", p, "*", predictors[p], partners[1], additional,
        " + p", p, "*", predictors[p], partners[2], additional, "\n ",
        DV, partners[2], additional, " ~ a", p, "*", predictors[p], partners[2], additional,
        " + p", p, "*", predictors[p], partners[1], additional, "\n")
  }
  
  afree_syn <- pfree_syn <- allfree_syn <- indistinguishable_syn
  for (p in 1:length(predictors)) {
    afree_syn <- sub(paste0("a", p, "*"), paste0("a", p, partners[1], "*"), afree_syn, fixed=TRUE) #replace first instance of actor with _0
    afree_syn <- sub(paste0("a", p, "*"), paste0("a", p, partners[2], "*"), afree_syn, fixed=TRUE) #replace second instance of actor with _1
    pfree_syn <- sub(paste0("p", p, "*"), paste0("p", p, partners[1], "*"), pfree_syn, fixed=TRUE) #replace first instance of partner with _0
    pfree_syn <- sub(paste0("p", p, "*"), paste0("p", p, partners[2], "*"), pfree_syn, fixed=TRUE) #replace second instance of partner with _1
    
    allfree_syn <- sub(paste0("a", p, "*"), paste0("a", p, partners[1], "*"), allfree_syn, fixed=TRUE) #replace first instance of actor with _0
    allfree_syn <- sub(paste0("a", p, "*"), paste0("a", p, partners[2], "*"), allfree_syn, fixed=TRUE) #replace second instance of actor with _1
    allfree_syn <- sub(paste0("p", p, "*"), paste0("p", p, partners[1], "*"), allfree_syn, fixed=TRUE) #replace first instance of partner with _0
    allfree_syn <- sub(paste0("p", p, "*"), paste0("p", p, partners[2], "*"), allfree_syn, fixed=TRUE) #replace second instance of partner with _1
    
  }
  #  syntax <- paste0(DV, partners[1], additional, " ~ asame*", predictor, partners[1], additional, " + psame*", predictor, partners[2], additional,
  #      "\n ", DV, partners[2], additional, " ~ asame*", predictor, partners[2], additional, " + psame*", predictor, partners[1], additional)
  
  indistinguishable <- sem(indistinguishable_syn, df, missing="ML", estimator="MLR")
  cat("\n\n-------\nIndistinguishable dyads model:\n------\n")
  summary(indistinguishable, fit.measures=TRUE)
  #standardizedSolution(res, type="std.all")
  
  afree <- sem(afree_syn, df, missing="ML", estimator="MLR")
  
  pfree <- sem(pfree_syn, df, missing="ML", estimator="MLR")
  
  allfree <- sem(allfree_syn, df, missing="ML", estimator="MLR")
  
  if (printall) {
    cat("\n\n-------\nFree actor model:\n------\n")
    summary(afree, fit.measures=TRUE)
    
    cat("\n\n-------\nFree partner model:\n------\n")
    summary(pfree, fit.measures=TRUE)
    
    cat("\n\n-------\nFree actor and partner (saturated) model:\n------\n")
    summary(allfree, fit.measures=TRUE)
  }
  
  print(anova(indistinguishable, afree, pfree, allfree))
  cat(indistinguishable_syn)
  
  return(list(syntax=list(i=indistinguishable_syn, a=afree_syn, p=pfree_syn, all=allfree_syn), indistinguishable=indistinguishable, afree=afree, pfree=pfree, allfree=allfree))
}

mplusMLAPIM <- function(df, DV, predictors, partners=c("_0", "_1"), categorical=FALSE, count=FALSE) {
  require(MplusAutomation)
  free <- indistinguishable <- " %BETWEEN%\n "
  for (p in 1:length(predictors)) {
    free <- paste0(free, DV, partners[1], " ON ", predictors[p], partners[1], " ", predictors[p], partners[2], ";\n ",
        DV, partners[2], " ON ", predictors[p], partners[1], " ", predictors[p], partners[2], ";\n")
    indistinguishable <- paste0(indistinguishable, DV, partners[1], " ON ", predictors[p], partners[1], " (a", p, ")\n ", predictors[p], partners[2], " (p", p, ");\n ",
        DV, partners[2], " ON ", predictors[p], partners[1], " (p", p, ")\n ", predictors[p], partners[2], " (a", p, ");\n")
  }
  free <- paste0(free, " %WITHIN%\n")
  indistinguishable <- paste0(indistinguishable, " %WITHIN%\n")
#  free <- paste0(" %WITHIN%\n ", DV, partners[1], " ON ", predictor, partners[1], " ", predictor, partners[2], ";\n ",
#      DV, partners[2], " ON ", predictor, partners[1], " ", predictor, partners[2], ";\n %BETWEEN%\n")
#  indistinguishable <- paste0(" %WITHIN%\n ", DV, partners[1], " ON ", predictor, partners[1], " (a)\n ", predictor, partners[2], " (p);\n ",
#      DV, partners[2], " ON ", predictor, partners[1], " (p)\n ", predictor, partners[2], " (a);\n %BETWEEN%\n")
  
  #VARIABLE = paste("CLUSTER=PTNUM; \nWITHIN =", paste0(DV, partners, collapse=" "), paste(as.vector(outer(predictors, partners, paste0)), collapse=" "),  ";")
  VARIABLE = paste("CLUSTER=PTNUM; \nBETWEEN =", paste(as.vector(outer(predictors, partners, paste0)), collapse=" "),  ";") #assumes predictors are all between
  if (categorical) { VARIABLE <- paste(VARIABLE, "\n CATEGORICAL = ", paste0(DV, partners, collapse=" "), ";") }
  if (count) { VARIABLE <- paste(VARIABLE, "\n COUNT = ", paste0(DV, partners, collapse=" "), ";") }
  
  m <- mplusObject(TITLE=paste("MLSEM", DV, "APIM Free"),
      VARIABLE = VARIABLE,
      ANALYSIS = "TYPE=TWOLEVEL; ESTIMATOR=MLR;",
      MODEL = free,
      OUTPUT = "STANDARDIZED RESIDUAL SAMPSTAT;", #MODINDICES(ALL 1.0)
      rdata = df,
      usevariables=c("PTNUM", paste0(DV, partners), as.vector(outer(predictors, partners, paste0)))
  )
  
  ## estimate the model in Mplus and read results back into R
  freeout <- mplusModeler(m, "couples_mplusmodeler.dat", modelout = "couples_mplusmodeler.inp", run = 1L)$results
  
  m[["MODEL"]] <- indistinguishable
  m <- update(m, TITLE=~paste("MLSEM", DV, "APIM Indistinguishable")) #MODEL= indistinguishable, 
  indistout <- mplusModeler(m, "couples_mplusmodeler.dat", modelout = "couples_mplusmodeler.inp", run = 1L)$results
  
  compareModels(freeout, indistout, diffTest=TRUE, show=c("summaries", "equal", "diff"))
  
  return(list(free=freeout, indistinguishable=indistout))
  
  #unlink("couples_mplusmodeler.dat"); unlink("couples_mplusmodeler.inp"); unlink("couples_mplusmodeler.out")
}


#analogous to above, but not multilevel
mplusAPIM <- function(df, DV, predictors, partners=c("_0", "_1"), categorical=FALSE, count=FALSE, exogCorr=TRUE) {
  require(MplusAutomation)
  free <- indistinguishable <- ""
  for (p in 1:length(predictors)) {
    free <- paste0(free, DV, partners[1], " ON ", predictors[p], partners[1], " ", predictors[p], partners[2], ";\n ",
        DV, partners[2], " ON ", predictors[p], partners[1], " ", predictors[p], partners[2], ";\n")
    indistinguishable <- paste0(indistinguishable, DV, partners[1], " ON ", predictors[p], partners[1], " (a", p, ")\n ", predictors[p], partners[2], " (p", p, ");\n ",
        DV, partners[2], " ON ", predictors[p], partners[1], " (p", p, ")\n ", predictors[p], partners[2], " (a", p, ");\n")
    
    if (exogCorr) {
      free <- paste0(free, paste(as.vector(outer(predictors, partners, paste0)), collapse=" "), " WITH ", paste(as.vector(outer(predictors, partners, paste0)), collapse=" "), ";\n")
      indistinguishable <- paste0(indistinguishable, paste(as.vector(outer(predictors, partners, paste0)), collapse=" "), " WITH ", paste(as.vector(outer(predictors, partners, paste0)), collapse=" "), ";\n")      
    }
  }
  
  VARIABLE = "IDVARIABLE=PTNUM;"
  if (categorical) { VARIABLE <- paste(VARIABLE, "\n CATEGORICAL = ", paste0(DV, partners, collapse=" "), ";") }
  if (count) { VARIABLE <- paste(VARIABLE, "\n COUNT = ", paste0(DV, partners, collapse=" "), ";") }
  
  m <- mplusObject(TITLE=paste("ML", DV, "APIM Free"),
      VARIABLE = VARIABLE,
      ANALYSIS = "TYPE=GENERAL; ESTIMATOR=MLR;",
      #ANALYSIS = "TYPE=GENERAL; ESTIMATOR=BAYES; FBITERATIONS=50000;",
      MODEL = free,
      OUTPUT = "STANDARDIZED RESIDUAL SAMPSTAT;", #couples_mplusmodeler_single.out 
      rdata = df,
      usevariables=c("PTNUM", paste0(DV, partners), as.vector(outer(predictors, partners, paste0)))
  )
  
  ## estimate the model in Mplus and read results back into R
  freeout <- mplusModeler(m, "couples_mplusmodeler_single.dat", modelout = "couples_mplusmodeler_single.inp", run = 1L)$results
  
  m[["MODEL"]] <- indistinguishable
  m <- update(m, TITLE=~paste("MLSEM", DV, "APIM Indistinguishable")) #MODEL= indistinguishable, 
  indistout <- mplusModeler(m, "couples_mplusmodeler_single.dat", modelout = "couples_mplusmodeler_single.inp", run = 1L)$results
  
  compareModels(freeout, indistout, diffTest=TRUE, show=c("summaries", "equal", "diff"))
  
  return(list(free=freeout, indistinguishable=indistout))
  
  #unlink("couples_mplusmodeler.dat"); unlink("couples_mplusmodeler.inp"); unlink("couples_mplusmodeler.out")
}
