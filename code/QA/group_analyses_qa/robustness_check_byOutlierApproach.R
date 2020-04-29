#Test 4 main effects with the 10 corresponding data sets
#df1 <- all 110 people
#df2 <- univariate outliers cut
#df3 <- all 110 people + winsorizing for NA
#df4 <- univariate outliers cut + winsorizing for NA
#df5 <- all 110 people + winsoring for NA + add'l data
#df6 <- univariate outliers cut + winsorizing for NA + addl'l data
#df7 <- data currently presented in paper 
#df8 <- data currently presented in paper + winsorizing for NA 
#df9 <- data currently presented in paper + winsorizing for NA + add'l data
#df10 <- univaraite outliers cut + winsorizing for NA + add'l data + bivariate outliers cut

library(tidyverse)
library(haven)
library(dependlab)
library(MplusAutomation)
library(lavaan)
library(wle)
# Defined Functions -------------------------------------------------------


gbiv_gcd <- function(iv,
                     dv,
                     dd,
                     PTNUM_outliers,
                     source = 0,
                     data = NULL,
                     annotate = TRUE) {
  if (is.null(data)) {
    if (source == 0) {
      data = fulldata
    } else{
      data = resid_df
    }
  } else {
    data = data
  }
  if (annotate == TRUE) {
    min = min(data[[dv]], na.rm= TRUE)
    max = max(data[[dv]], na.rm = TRUE)
    xmax = max(data[[iv]], na.rm = TRUE)
    xmin = min(data[[iv]], na.rm = TRUE)
    data <- mutate(data, bad = if_else(PTNUM %in% PTNUM_outliers, "Bad", "Good"))
    data_good = dplyr::filter(data, bad == "Good")
    g <-
      ggplot(data, aes_string(x = iv, y = dv, color = "bad")) + geom_point() + stat_smooth(data = dplyr::filter(data, bad == "Good"), method = "lm") +   ylim(c(min, max)) + xlim(c(xmin, xmax)) + annotate(
        geom = "text",
        x = .75 * xmax,
        y = .75 * max,
        label = paste0("good r = ", round(cor(data_good[[iv]], data_good[[dv]], use = "complete.obs"), 3))
      ) + annotate(
        geom = "text",
        x = .25 * xmax,
        y = .25 * max,
        label = paste0("r = ", round(cor(data[[iv]], data[[dv]], use = "complete.obs"), 3))
      ) + annotate(geom = "text", x = .8*xmax + xmin, y = .2*max + min, label = paste0("N: ", nrow(data), "; GN: ", nrow(data_good)))
    g
  } else{
    g <-
      ggplot(data, aes_string(x = iv, y = dv)) + geom_point() + geom_smooth(method = "lm")+ annotate(geom = "text", x = .8*xmax + xmin, y = .2*max + min, label = paste0("All N: ", nrow(data)))
    g
  }
  return(g)
}

gbiv <- function(iv,
                 dv,
                 source = 0,
                 data = NULL,
                 annotate = TRUE) {
  if (is.null(data)) {
    if (source == 0) {
      data = fulldata
    } else{
      data = resid_df
    }
  } else {
    data = data
  }
  if (annotate == TRUE) {
    xlabel = iv
    min = min(data[[dv]], na.rm = TRUE)
    max = max(data[[dv]], na.rm = TRUE)
    xmax = max(data[[iv]], na.rm = TRUE)
    xmin = min(data[[iv]], na.rm = TRUE)
    g <-
      ggplot(data, aes_string(x = iv, y = dv)) + geom_point() + geom_smooth(method = "lm")  + labs(x = xlabel) +  ylim(c(min, max)) + xlim(c(xmin, xmax)) + annotate(
        geom = "text",
        x = .75 * xmax,
        y = .75 * max,
        label = paste0("r = ", round(cor(data[[iv]], data[[dv]], use = "complete.obs"), 3)) 
      ) + annotate(geom = "text", x = .8*xmax + xmin, y = .2*max + min, label = paste0("All N: ", nrow(data)))
    g
  } else{
    g <-
      ggplot(data, aes_string(x = iv, y = dv)) + geom_point() + geom_smooth(method = "lm") + + annotate(geom = "text", x = .8*xmax + xmin, y = .2*max + min, label = paste0("All N: ", nrow(data)))
    g
  }
}



wle_trim <- function(df, pairmat, wthresh = .5) {
  outliers <- c()
  # if (resid == TRUE) {
  # df <- dplyr::filter(df, !is.na(.[[pairmat))
  #
  # } else {
  # df <- dplyr::filter(df, !is.na(scpt))
  # }
  #length(df)
  for (i in 1:nrow(pairmat)) {
    char = pairmat[[i, 1]]
    char2 = pairmat[[i, 2]]
    df <- dplyr::filter(df, !is.na(df[[char]]), !is.na(df[[char2]]))
    n <- length(df[[1]])
    print(n)
    f <- as.formula(paste(pairmat[i, 1], "~", pairmat[i, 2]))
    
    m <- wle.lm(f, df)
    g_df <-
      dplyr::mutate(df, wle_weights = as.vector(m$weights))
    w <- as.vector(m$weights)
    outliers <- c(outliers, which(w < wthresh))
    
    
  }
  
  notoutliers <- 1:nrow(df)
  notoutliers <- notoutliers[!notoutliers %in% unique(outliers)]
  unique_outliers <- unique(outliers)
  df_outliers <- df[outliers,]
  outliers_list <- as.vector(df_outliers$PTNUM)
  df_trim <- df[notoutliers, ]
  numoutliers <- length(unique(outliers))
  return(list(df_trim, numoutliers, n, outliers_list, unique_outliers))
  
}


winsorize_vec <- function(v, cutoff, second_cutoff = NULL) {
  #detect whether cutoff is high or low
  x <- v
  q50 <- quantile(x, .50, na.rm = TRUE)
  if (cutoff > q50) {
    high <- TRUE
  } else {
    high <- FALSE
  }
  if (high) {
    x[x >= cutoff] <- max(x[x < cutoff], na.rm = TRUE)
  } else {
    x[x <= cutoff] <- min(x[x > cutoff], na.rm = TRUE)
  }
  
  if (is.null(second_cutoff)) {
    v <- x
    return(v)
  } else {
    if (second_cutoff > q50) {
      second_high <- TRUE
    } else {
      second_high <- FALSE
    }
    if (second_high) {
      x[x >= second_cutoff] <- max(x[x < second_cutoff], na.rm = TRUE)
    } else {
      x[x <= second_cutoff] <- min(x[x > second_cutoff], na.rm = TRUE)
    }
    v <- x
    return(v)
  }
}




modsyntax <- function(dat, model, bsem_dir = "/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/code") {
  div10 <- function(x) {
    return(x / 10)
  }
  mult10 <- function(x) {
    return(x * 10)
  }
  dat <-
    mutate_at(dat, vars(contains("scp")), div10)
  dat <-
    mutate_at(dat, vars(contains("ccp")), div10)
  mname = deparse(substitute(model))
  d <- sem(
    model,
    dat,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    mimic = "Mplus"
  )
  paramest <- parameterestimates(d)
  allvarnames = ""
  modsyntax = ""
  allvarsyntax = ""
  lavmodsyntax = ""
  for (i in 1:length(paramest$lhs)) {
    lhs = paramest$lhs[[i]]
    op = paramest$op[[i]]
    rhs = paramest$rhs[[i]]
    label = paramest$label[[i]]
    lavlinesyntax = ifelse((label == "" ||
                              op == ":=" ||
                              is.null(label)),
                           paste0(lhs, op, rhs, "\n"),
                           paste0(lhs, op, label, "*", rhs, "\n")
    )
    lavmodsyntax = paste0(lavmodsyntax, lavlinesyntax)
    #print(label)
    #print(op)
    if (op == "~~") {
      opr = " WITH "
    } else if (op == "~") {
      opr = " ON "
    } else if (op == "~1") {
      lhs = paste0("[", lhs, "]")
      
      opr = ""
      
    } else {
      opr = op
    }
    if (label == "" ||
        (op == ":=") || is.null(label)) {
      lab = ""
    } else {
      lab = paste0("* (", label, ")")
    }
    if (op == ":=") {
      linesyntax = ""
      newvarname = label
      allvarnames = paste(allvarnames, newvarname, sep = " ")
      newvarsyntax = paste0(lhs, " = ", rhs, lab, ";\n")
      allvarsyntax = paste0(allvarsyntax, newvarsyntax)
      modsyntax = paste0(modsyntax, linesyntax)
    } else {
      linesyntax = paste0(lhs, opr, rhs, lab, "; \n")
      modsyntax = paste0(modsyntax, linesyntax)
    }
  }
  if (allvarsyntax != "") {
    modsyntax = paste0(modsyntax,
                       "\n",
                       "MODEL CONSTRAINT: \n",
                       "NEW (",
                       allvarnames,
                       " ); \n",
                       allvarsyntax)
  }
  #  usevar_vec = c(as.vector(paramest$lhs), as.vector(paramest$rhs))
  #  usevarsyntax = ""
  # for (i in 1:length(usevar_vec)) {
  #    var = usevar_vec[[i]]
  #    usevarsyntax =paste(usevarsyntax, var, sep = " ")
  #    
  # }
  usevardf <- data.frame(var_lhs = as.vector(as.character(paramest$lhs)), opr = as.vector(paramest$op), var_rhs = as.vector(as.character(paramest$rhs)))
  usevardf <- dplyr::filter(usevardf,  opr != ":=", opr != "~1")
  usevardf_long <- tidyr::gather(usevardf, key = "var_side", value = "var", -opr) %>% dplyr::filter(var != "")
  
  usevar_asvector <- unique(as.vector(usevardf_long$var))
  fname <- paste0(bsem_dir, "/", mname, "_df.dat")
  print(fname)
  datObj <- prepareMplusData(dat, fname)
  return(list(usevar_asvector, modsyntax, datObj, dat, mname, d))
}

#pulls in modSyntax
runBSEM <- function(modelsyntaxobj) {
  mobj = mplusObject(TITLE = "Testing", 
                     usevariables = as.vector(modelsyntaxobj[[1]]),
                     ANALYSIS = "ESTIMATOR=BAYES; PROCESSORS = 2;BITERATIONS = (30000);",
                     MODEL = modelsyntaxobj[[2]],
                     rdata = modelsyntaxobj[[4]])
  mbin <-"/Users/alisonmarie526/Applications/Mplus/mplus"
  fitted<-
    mplusModeler(
      mobj,
      modelout = paste0(modelsyntaxobj[[5]], ".inp"),
      run = TRUE,
      Mplus_command = mbin,
      hashfilename = FALSE
    )              
}

BoxCox_extract <- function(data, lambdas, plot = TRUE){
  Box <- boxcox(data ~ 1, lambda = lambdas)
  Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
  
  Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
  Cox2[1,]    #display lambda with highest log-likelihood
  lambda = Cox2[1, "Box.x"] #extract lambda
  
  transformed <- (data ^lambda -1)/lambda
  
  if(plot) {
    before <- qplot(data)
    print(before)
    
    x <- qplot(transformed)
    print(x)
  }
  return(transformed)
}


univariateoutiers_min <- function(vec) {
  min <- as.numeric(quantile(vec, .25))
  max <- as.numeric(quantile(vec, .75))
  iqr = IQR(vec)
  min_cutoff <- min - 2*(iqr)
  return(min_cutoff)
  
  
  
}
univariateoutiers_max <- function(vec) {
  min <- as.numeric(quantile(vec, .25))
  max <- as.numeric(quantile(vec, .75))
  iqr = IQR(vec)
  max_cutoff <- max + 2*(iqr)
  return(max_cutoff)
  
  
  
}

bsem_fit_summary <- function(bsem_out) {
  bsem_out_summary <- bsem_out$results$summaries
  ppp <- bsem_out_summary[["PostPred_PValue"]]
  dic <-bsem_out_summary[["DIC"]]
  bic <- bsem_out_summary[["BIC"]]
  pd <- bsem_out_summary[["pD"]]
  vec <- c(dic, pd, bic, ppp)
  return(vec)
}

bsem_fit_table <- function(bsem_out_list, write = FALSE, csvname = NULL, outdir = NULL) {
  
  tmp <- plyr::ldply(bsem_out_list, .fun = function(x) {
    tmp_vec = bsem_fit_summary(x)
    data.frame(DIC = tmp_vec[[1]], pD = tmp_vec[[2]], BIC = tmp_vec[[3]], PPp = tmp_vec[[4]])
  })
  if(is.null(outdir)) {outdir = getwd()}
  if (write == TRUE) {
    tmp_towrite <-mutate_at(tmp, vars(-.id, -PPp), funs(round(.,2)))
    if (!is.null(csvname))
      write.csv(tmp_towrite, paste0(outdir,"/", csvname, ".csv"))
    
  } else{
    #best guess at name of csv
    outname = as.character(bsem_out_list[[1]]$results$input$data$file)
    outname <- gsub(".dat", "", outname)
    print(outname)
    write.csv(tmp_towrite, file = paste0(outdir,"/", outname, ".csv"))
    
  }
  return(tmp)
}

forfit <- function(model,
                   dat,
                   med = 0,
                   nogcd = FALSE,
                   exo = FALSE, 
                   estimator = "MLR") {
  div10 <- function(x) {
    return(x / 10)
  }
  mult10 <- function(x) {
    return(x * 10)
  }
  dat <-
    mutate_at(dat, vars(contains("scp")), div10)
  dat <-
    mutate_at(dat, vars(contains("ccp")), div10)
  #dat <-
  #mutate_at(dat, vars(starts_with("nasty")), mult10)
  if (med == 0) {
    d <- sem(
      model,
      dat,
      missing = "ML",
      estimator = "MLR",
      meanstructure = TRUE,
      mimic = "Mplus"
    )
    #g <- genCookDist(model, dat, missing = "ML", estimator = "MLR", meanstructure = TRUE, mimic = "Mplus")
  } else {
    d <- sem(
      model,
      dat,
      missing = "ML",
      estimator = "ML",
      meanstructure = TRUE,
      mimic = "Mplus",
      se = "bootstrap",
      bootstrap = 1000
    )
    #g <- genCookDist(model, dat, missing = "ML", estimator = "ML", meanstructure = TRUE, mimic = "Mplus", se = "bootstrap", bootstrap = 1000)
  }
  
  # plot(g)
  paramest <- parameterestimates(d)
  allvarnames = ""
  modsyntax = ""
  allvarsyntax = ""
  lavmodsyntax = ""
  for (i in 1:length(paramest$lhs)) {
    lhs = paramest$lhs[[i]]
    op = paramest$op[[i]]
    rhs = paramest$rhs[[i]]
    label = paramest$label[[i]]
    lavlinesyntax = ifelse((label == "" ||
                              op == ":=" ||
                              is.null(label)),
                           paste0(lhs, op, rhs, "\n"),
                           paste0(lhs, op, label, "*", rhs, "\n")
    )
    lavmodsyntax = paste0(lavmodsyntax, lavlinesyntax)
    #print(label)
    #print(op)
    if (op == "~~") {
      opr = " WITH "
    } else if (op == "~") {
      opr = " ON "
    } else if (op == "~1") {
      lhs = paste0("[", lhs, "]")
      
      opr = ""
      
    } else {
      opr = op
    }
    if (label == "" ||
        (op == ":=") || is.null(label)) {
      lab = ""
    } else {
      lab = paste0("* (", label, ")")
    }
    if (op == ":=") {
      linesyntax = ""
      newvarname = label
      allvarnames = paste(allvarnames, newvarname, sep = " ")
      newvarsyntax = paste0(lhs, " = ", rhs, lab, ";\n")
      allvarsyntax = paste0(allvarsyntax, newvarsyntax)
      modsyntax = paste0(modsyntax, linesyntax)
    } else {
      linesyntax = paste0(lhs, opr, rhs, lab, "; \n")
      modsyntax = paste0(modsyntax, linesyntax)
    }
  }
  if (allvarsyntax != "") {
    modsyntax = paste0(modsyntax,
                       "\n",
                       "MODEL CONSTRAINT: \n",
                       "NEW (",
                       allvarnames,
                       " ); \n",
                       allvarsyntax)
  }
  #  usevar_vec = c(as.vector(paramest$lhs), as.vector(paramest$rhs))
  #  usevarsyntax = ""
  # for (i in 1:length(usevar_vec)) {
  #    var = usevar_vec[[i]]
  #    usevarsyntax =paste(usevarsyntax, var, sep = " ")
  #    
  # }
  usevardf <- data.frame(var_lhs = as.vector(as.character(paramest$lhs)), opr = as.vector(paramest$op), var_rhs = as.vector(as.character(paramest$rhs)))
  usevardf <- dplyr::filter(usevardf,  opr != ":=", opr != "~1")
  usevardf_long <- tidyr::gather(usevardf, key = "var_side", value = "var", -opr) %>% dplyr::filter(var != "")
  
  usevar_asvector <- unique(as.vector(usevardf_long$var))
  print(usevar_asvector)
  print(modsyntax)
  print(lavmodsyntax)
  print(paste0("Number of Observations in this Data Set: ", length(dat$PTNUM)))
  if (estimator == "MLR") {
    analysisest = "TYPE = GENERAL; ESTIMATOR = MLR;"
  } else {
    analysisest = paste0("TYPE = GENERAL; ESTIMATOR = ", estimator, ";")
  }
  print(analysisest)
  mobj = mplusObject(
    TITLE = "Testing",
    usevariables = usevar_asvector,
    ANALYSIS = analysisest,
    MODEL = modsyntax,
    OUTPUT = "sampstat standardized tech1;",
    SAVEDATA = "file=cooksdtest.dat; save = COOKS INFLUENCE LOGLIKELIHOOD;",
    rdata = dat
  )
  mbin <-
    "/Users/alisonmarie526/Applications/Mplus/mplus"
  fitted<-
    mplusModeler(
      mobj,
      modelout = "apim_syntax.inp",
      run = TRUE,
      Mplus_command = mbin,
      hashfilename = FALSE
    )
  if (length(fitted$results$errors) == 0) {
    print("No issues estimating parameters.")
  } else {
    print(paste0("Error:"))
    for (i in 1:length(fitted$results$errors)) {
      print(fitted$results$errors[[i]])
    }
  }
  cooksd <- fitted$results$savedata$OUTCOOK
  plot(cooksd)
  infl <- fitted$results$savedata$OUTINFL
  lik <- fitted$results$savedata$OUTLOGL
  df <- dat %>% mutate_all(funs(as.vector(.)))
  df <-
    dplyr::mutate(df,
                  gcd = cooksd,
                  influence = infl,
                  llcont = lik)
  df_mod <- dplyr::filter(df, gcd < 2)
  if (med == 0) {
    if (exo) {
      e <- sem(
        model = model,
        data = df_mod,
        missing = "listwise",
        estimator = "ML",
        meanstructure = TRUE,
        mimic = "Mplus",
        conditional.x = TRUE
      )
      
    } else {
      e <- sem(
        model = lavmodsyntax,
        data = df_mod,
        missing = "ML",
        estimator = "MLR",
        meanstructure = TRUE,
        mimic = "Mplus"
      )
    }
  } else {
    e <- sem(
      model = model,
      data = df_mod,
      missing = "ML",
      estimator = "ML",
      meanstructure = TRUE,
      mimic = "Mplus",
      se = "bootstrap",
      bootstrap = 5000
    )
  }
  if (exo) {
    f <-  sem(
      model = model,
      data = dat,
      missing = "listwise",
      estimator = "ML",
      meanstructure = TRUE,
      mimic = "Mplus",
      conditional.x = TRUE
    )
  } else {
    f <-  sem(
      model = lavmodsyntax,
      data = dat,
      missing = "ML",
      estimator = "MLR",
      meanstructure = TRUE,
      mimic = "Mplus"
    )
  }
  d_pest <- parameterestimates(d, ci = FALSE)
  e_pest <- parameterestimates(e, ci = FALSE)
  d_pest <-
    mutate_if(d_pest, is.numeric, round, digits = 2)
  d_pest <-
    mutate_if(d_pest, is.character, abbreviate, 6)
  e_pest <-
    mutate_if(e_pest, is.numeric, round, digits = 2)
  e_pest <-
    mutate_if(e_pest, is.character, abbreviate, 6)
  g_2  <- cooksd>2
  # if(TRUE %in% g_2) {
  #   print("Current data set has at least one case with gCD greater than 2. A multivariate outlier is likely.")
  # }
  if (TRUE %in% g_2) {
    print(
      "First: Data with multivariate outliers. Second: Data without multivariate outliers"
    )
    e_l <- length(df_mod$PTNUM)
    d_l <- length(dat$PTNUM)
    print(
      paste0(
        "Number of participants in data set without multivariate outliers excluded: ",
        d_l
      )
    )
    print(
      paste0(
        "Number of participants in data set with multivariate outliers excluded: ",
        e_l
      )
    )
    print(cbind(d_pest, e_pest))
    
  }else {
    print("N.B. No GCD values greater than 2.")
  }
  
  
  df <-
    dplyr::mutate(df, bad = if_else(gcd < 2, "Good", "Bad"))
  parm_names <- parameterestimates(d, ci = FALSE)
  d_pest_only_regressions <-
    dplyr::filter(parm_names, op == "~")
  pairmat = as_tibble(dplyr::select(d_pest_only_regressions, lhs, rhs))
  print(pairmat)
  if (TRUE %in% g_2) {
    for (i in 1:nrow(pairmat)) {
      a <-
        gbiv_gcd(pairmat[[i, 2]], pairmat[[i, 1]], dd = "bad", data = df)
      plot(a)
    }} else {
      
      for (i in 1:nrow(pairmat)) {
        a <-
          gbiv(pairmat[[i, 2]], pairmat[[i, 1]], data = df)
        plot(a)
      }
    }
  if (estimator == "MLR") {
    if (nogcd) {
      out <- f
    } else {
      out <- e
    }
    return(out)
  } else {
    return(fitted)
  }
}


hmap <- function(df, npar) {
  outliers_df <- matrix(ncol = npar, nrow = npar)
  numtotal_df <- matrix(ncol = npar, nrow = npar)
  rawcor_df <- matrix(ncol = npar, nrow = npar) 
  modcor_df <- matrix(ncol = npar, nrow = npar) 
  for(i in 1:npar) {
    for (j in 1:npar) {
      tmp_dv = names(df)[[i]]
      tmp_iv = names(df)[[j]]
      tmp_tribble <- tribble(
        ~v1, ~v2,
        tmp_dv, tmp_iv)
      #print(tmp_iv)
      #print(tmp_dv)
      if (i != j) {
        tmp_outliers <-wle_trim(df, tmp_tribble)
        tmp_outliers_df = tmp_outliers[[1]]
        tmp_rawcor = cor(df[[tmp_iv]], df[[tmp_dv]], use = "complete.obs")
        tmp_modcor = cor(tmp_outliers_df[[tmp_iv]],tmp_outliers_df[[tmp_dv]], use = "complete.obs")
        rawcor_df[[i,j]] = tmp_rawcor
        modcor_df[[i, j]] = tmp_modcor
        num_total = as.numeric(tmp_outliers[[3]])
        num_outliers = as.numeric(tmp_outliers[[2]])
        outliers_df[[i,j]] = num_outliers
        numtotal_df[[i, j]] = num_total
        
      } else {
        tmp_rawcor = 1
        tmp_modcor = 1
        rawcor_df[[i, j]] = tmp_rawcor
        modcor_df[[i, j]] =tmp_modcor
        outliers_df[[i,j]] = 0
        numtotal_df[[i, j]] = length(df[[1]])
      }
    }
  }
  rawcor_hmap <- plot_ly(x= names(df), y = names(df), z =rawcor_df, type = "heatmap")
  modcor_hmap <- plot_ly(x= names(df), y = names(df), z =modcor_df, type = "heatmap")
  outliers_hmap <- plot_ly(x= names(df), y = names(df), z =outliers_df, type = "heatmap")
  numtotal_hmap <- plot_ly(x= names(df), y = names(df), z =numtotal_df, type = "heatmap")
  
  return(list(rawcor = rawcor_df, modcor = modcor_df, outliers = outliers_df, numtotal = numtotal_df,
              rawcor_heat = rawcor_hmap, modcor_heat = modcor_hmap, outliers_heat = outliers_hmap, numtotal_heat = numtotal_hmap))
}

visualize_paths <- function(lavsyntax = NULL, mlist = NULL) {
  if (!is.null(lavsyntax) | !is.null(mlist)) {
    div10 <- function(x) {
      return(x / 10)
    }
    mult10 <- function(x) {
      return(x * 10)
    }
    init_df <- mlist[[1]]
    init_df <-
      mutate_at(init_df, vars(contains("scp")), div10)
    init_df <-
      mutate_at(init_df, vars(contains("ccp")), div10)
    
    
    if(!"ECRanx_1" %in% names(init_df)) {
      init_df <- mutate(init_df, ECRanx_1 = ECR_anxiety_1,ECRavo_1 = ECR_avoidance_1, ECRanx_0 = ECR_anxiety_0, ECRavo_0 = ECR_avoidance_0)
    }
    d <- sem(
      model = lavsyntax,
      data = init_df,
      missing = "ML",
      estimator = "MLR",
      meanstructure = TRUE,
      mimic = "Mplus"
    )
    
    parm_names <- parameterestimates(d, ci = FALSE)
    d_pest_only_regressions <-
      dplyr::filter(parm_names, op == "~")
    pairmat = as_tibble(dplyr::select(d_pest_only_regressions, lhs, rhs))
    dim_pairmat = nrow(pairmat)
    graph_list <- array(list(), dim = c(length(mlist), dim_pairmat))
    for (i in 1:length(mlist)) {
      tmp_df <- mlist[[i]]
      if(!"ECRanx_1" %in% names(tmp_df)) {
        tmp_df <- mutate(tmp_df, ECRanx_1 = ECR_anxiety_1,ECRavo_1 = ECR_avoidance_1, ECRanx_0 = ECR_anxiety_0, ECRavo_0 = ECR_avoidance_0)
      }
      tmp_df <-
        mutate_at(tmp_df, vars(contains("scp")), div10)
      tmp_df <-
        mutate_at(tmp_df, vars(contains("ccp")), div10)
      
      d <- sem(
        model = lavsyntax,
        data = tmp_df,
        missing = "ML",
        estimator = "MLR",
        meanstructure = TRUE,
        mimic = "Mplus"
      )
      
      parm_names <- parameterestimates(d, ci = FALSE)
      d_pest_only_regressions <-
        dplyr::filter(parm_names, op == "~")
      pairmat = as_tibble(dplyr::select(d_pest_only_regressions, lhs, rhs))
      print(pairmat)
      if (i != 10) {
        for (k in 1:nrow(pairmat)) {
          tmp_graph <-
            gbiv(pairmat[[k, 2]], pairmat[[k, 1]], data = tmp_df)
          graph_list[[i, k]] <- tmp_graph
        }
      } else {
        outliers_obj <- wle_trim(df = tmp_df, pairmat = pairmat)
        outliers <- outliers_obj[[4]]
        dd = outliers_obj[[5]]
        for (k in 1:nrow(pairmat)) {
          tmp_graph <-
            gbiv_gcd(pairmat[[k, 2]], pairmat[[k, 1]], data = tmp_df, dd = dd, PTNUM_outliers = outliers)
          graph_list[[i, k]] <- tmp_graph
        }
        
      }
     
    }
    return(graph_list)
    
  } else {(print("Need to specify a list of models and a list of data frames to visualize"))}
  
  
  
}

# Define Models -----------------------------------------------------------


m1 <- "
scpt ~ elpt
scpr ~ elpr
ccpt ~ u*elpt
ccpr ~ elpr
prnapt ~ w*elpt
prnapr ~ j*elpr

ECRanx_1 ~h*pnapt
ECRanx_0 ~ k*pnapr
pnapt~~ pnapr
pnapt ~ prnapt +scpt + c*ccpt
pnapr ~ prnapr +scpr + d*ccpr
scpt ~ a1*prnapt
scpr~ a1*prnapr
ccpt ~b1*prnapt 
ccpr ~b1*prnapr
scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr
b1c := b1*c
b1d := b1*d
hb1c:=h*b1*c
whb1c := w*h*b1*c
uhc:=u*h*c
"

m2 <- "
scpt ~ cmpt
scpr ~ cmpr
ccpt ~ w*cmpt
ccpr ~ cmpr
prnapt ~ cmpt
prnapr ~ j*cmpr

ECRanx_1 ~h*pnapt
ECRanx_0 ~ k*pnapr
pnapt~~ pnapr
pnapt ~ prnapt +scpt + c*ccpt
pnapr ~ prnapr +scpr + d*ccpr
scpt ~ a1*prnapt
scpr~ a1*prnapr
ccpt ~b1*prnapt 
ccpr ~b1*prnapr
scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr
b1c := b1*c
b1d := b1*d
hb1c:=h*b1*c
whb1c := w*h*b1*c
whc := w*h*c
"

m3 <- "
scpt ~ cmpt
ccpt ~ z*cmpt
scpr ~ cmpr
ccpr ~ cmpr
prafpt ~ cmpt
prafpr ~ cmpr


ECRanx_0 ~ h*pafpt
ECRanx_1 ~ h*pafpr
ECRanx_1 ~ k*pafpt
ECRanx_0 ~ k*pafpr
pafpt~~ pafpr

pafpt ~ prafpt + a1*scpt + a3*ccpt + a5*scpr + a7*ccpr
pafpr ~ prafpr + a2*scpr + a4*ccpr + a5*scpt + a7*ccpt
scpr ~ b5*prafpt
scpt~ b5*prafpr
ccpt ~ b7*prafpr
ccpr ~ b7*prafpt 
scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr
ha3:=h*a3

ka3:=k*a3
a3b7:=a3*b7
a7b7:=a7*b7
a4b7:=a4*b7
zka3 := z*k*a3
"

m4 <- "
ECRavo_1 ~ h*pafpt
ECRavo_0 ~ k*pafpr
pafpt~~ pafpr
scpt ~ cmpt
ccpt ~ z*cmpt
scpr ~ cmpr
ccpr ~ cmpr
prafpt ~ cmpt
prafpr ~ cmpr
prafpt ~~ prafpr
pafpt ~ prafpt + a1*scpt + a3*ccpt + a5*scpr + a7*ccpr
pafpr ~ prafpr + a2*scpr + a4*ccpr + a5*scpt + a7*ccpt
scpr ~ b5*prafpt
scpt~ b5*prafpr
ccpt ~ b7*prafpr
ccpr ~ b7*prafpt 
scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr
ECRavo_0~cmpr
ha3:=h*a3
a3b7:=a3*b7
a7b7:=a7*b7
a4b7:=a4*b7
zha3 := z*h*a3
"

# Define data sets  -------------------------------------------------------


#Test 4 main effects with the 10 corresponding data sets
#df1 <- all 110 people
load('/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/data/couples_baseline_clinical_27Jul2017.RData')
df <- read.csv("/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv") %>% dplyr::filter(m1_R2 > .98, PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104) %>% dplyr::select(-starts_with("X"))

df <- mutate(df, agpt = iip_agency_patient, agpr = iip_agency_partner, cmpt = iip_communion_patient, cmpr = iip_communion_partner)%>%mutate(agpt.c = agpt - mean(agpt, na.rm = TRUE), agpr.c = agpr - mean(agpr, na.rm = TRUE),
                                                                                                                                            cmpt.c = cmpt - mean(cmpt, na.rm = TRUE), cmpr.c = cmpr - mean(cmpr, na.rm = TRUE)) %>%mutate(agcmpt = agpt.c*cmpt.c,
                                                                                                                                                                                                                                      agcmpr = agpr.c*cmpr.c)
idnames_df <- couples_clin_wide %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104) 
idnames <- as.vector(idnames_df$PTNUM)
das_all <- read_spss('/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/CombineSelfReports/INTAKE_20170223/DAS.sav')
das_wide <- dplyr::filter(das_all, mth== 12, PTNUM %in% idnames) %>% mutate(ptpr = if_else(DyadID == 0, "_partner", "_patient")) %>% dplyr::select(-ADate, -DyadID) 
das_wide <- score_das(das_wide)
das_wide_df <-dplyr::select(das_wide, PTNUM, ptpr, DASrelationship, DASCon, DASSat, DASCoh, DASAffExp, DASTotal) %>% mutate(PTNUM = as.character(PTNUM), DASrelationship = as.numeric(DASrelationship)) %>% gather(key = "DASType", value = "DASvalue", -PTNUM, -ptpr) %>% mutate(DASType = paste0(DASType, ptpr)) %>% dplyr::select(-ptpr) %>% spread(key = "DASType", value = "DASvalue")
paste_fu <- function(x) {x = paste0(x, "_fu")}
das_wide_df <- rename_at(das_wide_df,vars(starts_with("DAS")),  paste_fu)
das_ecr_data <- couples_clin_wide %>% dplyr::select(starts_with("DAS"), starts_with("ECR"), PTNUM)%>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104) 
forfit_df <- plyr::join_all(list(df, das_wide_df, das_ecr_data)) #plyr::join_all(list(df, das_ecr_data)) #
forfit_df <-transmute(forfit_df,scpt = scpt, ccpt = ccpt,scpr = scpr, ccpr = ccpr,  PTNUM = PTNUM, DASTot0 = DASTotal_0, DASTot1 = DASTotal_1, DASrel0 = DASrelationship_partner_fu, DASrel1 = DASrelationship_patient_fu, agcmpt = agcmpt, agcmpr = agcmpr, cmpt = cmpt,cmpr =  cmpr, elpt = iip_elevation_patient, elpr = iip_elevation_partner, agpt = agpt, agpr = agpr, pnapt = na_post_patient, pnapr = na_post_partner,prnapt = na_pre_patient, prnapr = na_pre_partner,pafpt = AFF_post_patient,pafpr= AFF_post_partner, prafpt = AFF_pre_patient, prafpr = AFF_pre_partner, DASfu_1 =DASTotal_patient_fu, DASfu_0 = DASTotal_partner_fu, ECRanx_1 = ECRanx_1, ECRanx_0 = ECRanx_0, ECRavo_1 = ECRavoid_1, ECRavo_0 = ECRavoid_0)
forfit_df <- mutate(forfit_df, DASrel = if_else(DASrel0 == 0 | DASrel1 == 0, 0, 1))
na_df <- dplyr::select(forfit_df, PTNUM, prnapt, prnapr, pnapt, pnapr)
na_df <- mutate(na_df,prnapt = log(prnapt), prnapr = log(prnapr), pnapt = log(pnapt), pnapr = log(pnapr))
foranalyses_df <- dplyr::select(forfit_df, -prnapt, -prnapr, -pnapt, -pnapr)%>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% mutate(DASTot0  = winsorize_vec(DASTot0, 60), DASTot1 = winsorize_vec(DASTot1, 60))
foranalyses_df <- left_join(foranalyses_df, na_df)
coreg_affap_df <- read.csv("/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/datasets_VAR/coreg_affap_df.csv")
elap_df <- read.csv("/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/datasets_VAR/elap_df.csv")
coreg_naap_df <- read.csv("/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/datasets_VAR/coreg_naap_df.csv")
newdf <- read.csv("/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/output/fulldata_VAR_mod10Dec2018.csv")
tomerge_newdf <- dplyr::select(newdf,PTNUM, ECR_avoidance_1, ECR_avoidance_0, ECR_anxiety_1,ECR_anxiety_0, elpt, elpr, agpt, agpr, cmpt, cmpr, DASTot0, DASTot1)
communionap <- tribble(
  ~v1, ~v2,
  "scpt", "iip_communion_partner",
  "scpr", "iip_communion_patient",
  "ccpt", "iip_communion_partner",
  "ccpr", "iip_communion_patient",
  "scpr", "iip_communion_partner",
  "scpt", "iip_communion_patient",
  "ccpr", "iip_communion_partner",
  "ccpt", "iip_communion_patient"
)

communionap_df <-wle_trim(df, communionap) 
communionap_df <- mutate(communionap_df[[1]], cmpt = iip_communion_patient, cmpr = iip_communion_partner)

#first define df1-df6 for m1-m4 (which is model invariant)
m4df1 <- m3df1 <-m2df1 <- m1df1 <- forfit_df 
m4df2 <- m3df2 <-m2df2 <- m1df2 <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr))
m1df3 <- dplyr::select(forfit_df, -prnapt, -prnapr, -pnapt, -pnapr) %>% mutate(DASTot0  = winsorize_vec(DASTot0, 60), DASTot1 = winsorize_vec(DASTot1, 60))
m1df3 <- left_join(m1df3, na_df)
m4df3 <- m3df3 <-m2df3 <-m1df3
m4df4 <- m3df4 <-m2df4 <- m1df4 <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-prnapt, -prnapr, -pnapt, -pnapr)%>% mutate(DASTot0  = winsorize_vec(DASTot0, 60), DASTot1 = winsorize_vec(DASTot1, 60))
m1df4 <- left_join(m1df4, na_df)
m4df4 <- m3df4 <-m2df4 <- m1df4
m1df5 <- forfit_df  %>% dplyr::select(-prnapt, -prnapr, -pnapt, -pnapr)%>%  mutate(DASTot0  = winsorize_vec(DASTot0, 60), DASTot1 = winsorize_vec(DASTot1, 60)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
m1df5 <- left_join(m1df5, na_df)
m4df5 <- m3df5 <-m2df5 <- m1df5 <- left_join(m1df5, tomerge_newdf)
m1df6 <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-prnapt, -prnapr, -pnapt, -pnapr)%>% mutate(DASTot0  = winsorize_vec(DASTot0, 60), DASTot1 = winsorize_vec(DASTot1, 60)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
m1df6 <- left_join(m1df6, na_df)
m4df6 <- m3df6 <-m2df6 <- m1df6 <- left_join(m1df6, tomerge_newdf)

#M1 for df7-10
#for dat
df_temp <- couples_clin_wide %>% dplyr::select(starts_with("DAS"), starts_with("ECR"), PTNUM)
dat <- left_join(coreg_affap_df, df_temp) %>% mutate(prafpt = AFF_pre_patient,prafpr = AFF_pre_partner, pafpt = AFF_post_patient, pafpr = AFF_post_partner, ECRavo_1 = ECRavoid_1, ECRavo_0 = ECRavoid_0) %>% dplyr::select(-ECRavoid_1, -ECRavoid_0)
df1 <- elap_df %>% mutate(elpt = iip_elevation_patient, elpr = iip_elevation_partner, agpt = iip_agency_patient, agpr = iip_agency_partner, cmpt = iip_communion_patient, cmpr = iip_communion_partner) %>% dplyr::select(PTNUM, elpt, elpr, agpt, agpr, cmpt, cmpr)
df2 <- coreg_naap_df %>% mutate(prnapt = na_pre_patient,prnapr = na_pre_partner, pnapt = na_post_patient, pnapr = na_post_partner) %>% dplyr::select(PTNUM, prnapt, prnapr, pnapt, pnapr, scpt, ccpt, scpr, ccpr)
dat2 <- inner_join(df1, df2) #N = 90
dat <- left_join(dat, dat2) %>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
m1df7 <- dat 
m1df8 <- dat %>% mutate(DASTot0  = winsorize_vec(DASTotal_0, 60), DASTot1 = winsorize_vec(DASTotal_1, 60))  %>% dplyr::select(-prnapt, -prnapr, -pnapt, -pnapr)
m1df8 <- left_join(m1df8, na_df)
m1df9 <- dat %>% mutate(DASTot0  = winsorize_vec(DASTotal_0, 60), DASTot1 = winsorize_vec(DASTotal_1, 60)) %>% dplyr::select(-prnapt, -prnapr, -pnapt, -pnapr, -elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1,  -elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1)#add in add'l data 
m1df9 <- left_join(m1df9, na_df)
m1df9 <- left_join(m1df9, tomerge_newdf)
m1df10 <- m1df6 #+ bivariate cuts



# M2 for df7-10
df1 <- mutate(communionap_df, cmpt = iip_communion_patient, cmpr = iip_communion_partner) %>% dplyr::select(PTNUM, cmpt, cmpr)
df2 <- coreg_naap_df %>% mutate(prnapt = na_pre_patient,prnapr = na_pre_partner, pnapt = na_post_patient, pnapr = na_post_partner) %>% dplyr::select(PTNUM, prnapt, prnapr, pnapt, pnapr, scpt, ccpt, scpr, ccpr)
dat2 <- inner_join(df1, df2) #N = 90
dat <- left_join(coreg_affap_df, df_temp) %>% mutate(prafpt = AFF_pre_patient,prafpr = AFF_pre_partner, pafpt = AFF_post_patient, pafpr = AFF_post_partner, ECRavo_1 = ECRavoid_1, ECRavo_0 = ECRavoid_0) %>% dplyr::select(-ECRavoid_1, -ECRavoid_0)
dat <- left_join(dat, dat2)%>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
m2df7 <- dat
m2df8 <- dat %>% mutate(DASTot0  = winsorize_vec(DASTotal_0, 60), DASTot1 = winsorize_vec(DASTotal_1, 60))%>% dplyr::select(-prnapt, -prnapr, -pnapt, -pnapr)
m2df8 <- left_join(m2df8, na_df)
m2df9 <- dat  %>% mutate(DASTot0  = winsorize_vec(DASTotal_0, 60), DASTot1 = winsorize_vec(DASTotal_1, 60)) %>% dplyr::select(-cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)#add in add'l data 
m2df9 <- left_join(m2df9, na_df)
m2df9 <- left_join(m1df9, tomerge_newdf)
m2df10 <- m2df6 #+ bivariate cuts


#M3 for df7-10
df1 <- mutate(communionap_df, cmpt = iip_communion_patient, cmpr = iip_communion_partner) %>% dplyr::select(PTNUM, cmpt, cmpr)
df2 <- coreg_affap_df %>% mutate(prafpt = AFF_pre_patient,prafpr = AFF_pre_partner, pafpt = AFF_post_patient, pafpr = AFF_post_partner) %>% dplyr::select(PTNUM, prafpt, prafpr, pafpt, pafpr, scpt, ccpt, scpr, ccpr)
dat2 <- inner_join(df1, df2) #N = 83
df <- couples_clin_wide %>% dplyr::select(starts_with("DAS"), starts_with("ECR"), PTNUM)
dat <- left_join(coreg_affap_df, df_temp) %>% mutate(prafpt = AFF_pre_patient,prafpr = AFF_pre_partner, pafpt = AFF_post_patient, pafpr = AFF_post_partner, ECRavo_1 = ECRavoid_1, ECRavo_0 = ECRavoid_0) %>% dplyr::select(-ECRavoid_1, -ECRavoid_0)
dat <- left_join(dat, dat2)%>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
m3df7 <- dat
m3df8 <- dat %>% mutate(DASTot0  = winsorize_vec(DASTotal_0, 60), DASTot1 = winsorize_vec(DASTotal_1, 60)) 
m3df8 <- left_join(m3df8, na_df)
m3df9 <- dat %>% mutate(DASTot0  = winsorize_vec(DASTotal_0, 60), DASTot1 = winsorize_vec(DASTotal_1, 60)) %>% dplyr::select(-cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)#add in add'l data 
m3df9 <- left_join(m3df9, na_df)
m3df9 <- left_join(m1df9, tomerge_newdf)
m3df10 <- m3df6 #+ bivariate cuts


#M4 for df7-10
df1 <- mutate(communionap_df, cmpt = iip_communion_patient, cmpr = iip_communion_partner) %>% dplyr::select(PTNUM, cmpt, cmpr)
df2 <- coreg_affap_df %>% mutate(prafpt = AFF_pre_patient,prafpr = AFF_pre_partner, pafpt = AFF_post_patient, pafpr = AFF_post_partner) %>% dplyr::select(PTNUM, prafpt, prafpr, pafpt, pafpr, scpt, ccpt, scpr, ccpr)
dat2 <- inner_join(df1, df2) #N = 83
df <- couples_clin_wide %>% dplyr::select(starts_with("DAS"), starts_with("ECR"), PTNUM)
dat <- left_join(coreg_affap_df, df_temp) %>% mutate(prafpt = AFF_pre_patient,prafpr = AFF_pre_partner, pafpt = AFF_post_patient, pafpr = AFF_post_partner, ECRavo_1 = ECRavoid_1, ECRavo_0 = ECRavoid_0) %>% dplyr::select(-ECRavoid_1, -ECRavoid_0)
dat <- left_join(dat, dat2)%>% dplyr::filter(PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104)
m4df7 <- dat
m4df8 <- dat %>% mutate(DASTot0  = winsorize_vec(DASTotal_0, 60), DASTot1 = winsorize_vec(DASTotal_1, 60)) 
m4df8 <- left_join(m4df8, na_df)
m4df9 <- dat  %>% mutate(DASTot0  = winsorize_vec(DASTotal_0, 60), DASTot1 = winsorize_vec(DASTotal_1, 60)) %>% dplyr::select(-cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)#add in add'l data 
m4df9 <- left_join(m4df9, na_df)
m4df9 <- left_join(m1df9, tomerge_newdf)
m4df10 <- m4df6 #+ bivariate cuts




# Graph Models Across Dataframes ------------------------------------------


m1_graphlist <- visualize_paths(lavsyntax = m1, mlist = list(m1df1, m1df2, m1df3, m1df4, m1df5, m1df6, m1df7, m1df8, m1df9, m1df10))
m2_graphlist <- visualize_paths(lavsyntax = m2, mlist = list(m2df1, m2df2, m2df3, m2df4, m2df5, m2df6, m2df7, m2df8, m2df9, m2df10))
m3_graphlist <- visualize_paths(lavsyntax = m3, mlist = list(m3df1, m3df2, m3df3, m3df4, m3df5, m3df6, m3df7, m3df8, m3df9, m3df10))
m4_graphlist <- visualize_paths(lavsyntax = m4, mlist = list(m4df1, m4df2, m4df3, m4df4, m4df5, m4df6, m4df7, m4df8, m4df9, m4df10))


# Print graphs ------------------------------------------------------------
print_graphs <- function(ll, name = "~/Desktop/tmp.pdf") {
  pdf(name, width = 20, height = 10)
  for(k in 1:dim(ll)[[2]]) {
  tmp <- cowplot::plot_grid(ll[[1,k]], ll[[2,k]], ll[[3,k]], ll[[4,k]], ll[[5, k]], ll[[6,k]], ll[[7,k]], ll[[8,k]], ll[[9,k]], ll[[10,k]], nrow = 2)
  print(tmp)
  }
  dev.off()
}

print_graphs(m1_graphlist, name = "~/Desktop/m1_graphlist.pdf")
print_graphs(m2_graphlist, name = "~/Desktop/m2_graphlist.pdf")
print_graphs(m3_graphlist, name = "~/Desktop/m3_graphlist.pdf")
print_graphs(m4_graphlist, name = "~/Desktop/m4_graphlist.pdf")




# SEM Out -----------------------------------------------------------------

m1_df6_out <- sem(model = m1, data = m1df6, missing = "ML",
              estimator = "MLR",
              meanstructure = TRUE,
              mimic = "Mplus")



# Now testing df6 vs df7 (manipulate NA + which cases) --------------------
m1df8_model <- m1df9_model <- m1df6_model <- m1df2_model <- m1df7_model <- m1
m1df6 <- dplyr::rename(m1df6, ECRanx_1 = ECR_anxiety_1, ECRanx_0 = ECR_anxiety_0)
m1df6_out <- runBSEM(modsyntax(dat = m1df6, model = m1df6_model, bsem_dir = "~/Desktop"))
m1df2_addldata <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
m1df2_addldata <-left_join(m1df2_addldata, tomerge_newdf)
m1df2_addldata <- dplyr::rename(m1df2_addldata, ECRanx_1 = ECR_anxiety_1, ECRanx_0 = ECR_anxiety_0)
m1df2_out <- runBSEM(modsyntax(dat = m1df2_addldata, model = m1df2_model, bsem_dir = "~/Desktop"))
m1df9 <- dplyr::rename(m1df9, ECRanx_1 = ECR_anxiety_1, ECRanx_0 = ECR_anxiety_0)
m1df9_out <- runBSEM(modsyntax(dat = m1df9, model = m1df9_model, bsem_dir = "~/Desktop"))
m1df8 <- dplyr::rename(m1df8, ECRanx_1 = ECR_anxiety_1, ECRanx_0 = ECR_anxiety_0)
m1df8_out <- runBSEM(modsyntax(dat = m1df8, model = m1df8_model, bsem_dir = "~/Desktop"))

m1df7_out <- runBSEM(modsyntax(dat = m1df7, model = m1df7_model, bsem_dir = "~/Desktop"))

m2df6_model <- m2df2_model <- m2df7_model <- m2
m2df6 <- dplyr::rename(m2df6, ECRanx_1 = ECR_anxiety_1, ECRanx_0 = ECR_anxiety_0) %>% mutate(scpt= 10*scpt, scpr = 10*scpr, ccpt = 10*ccpt, ccpr = 10*ccpr)
m2df6 <-  mutate(m1df6, scpt= 10*scpt, scpr = 10*scpr, ccpt = 10*ccpt, ccpr = 10*ccpr)

m2df6_out <- runBSEM(modsyntax(dat = m2df6, model = m2df6_model, bsem_dir = "~/Desktop"))
m2df2_addldata <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
m2df2_addldata <-left_join(m2df2_addldata, tomerge_newdf)
m2df2_addldata <- dplyr::rename(m2df2_addldata, ECRanx_1 = ECR_anxiety_1, ECRanx_0 = ECR_anxiety_0) %>% mutate(scpt= 10*scpt, scpr = 10*scpr, ccpt = 10*ccpt, ccpr = 10*ccpr)
m2df2_addldata <- dplyr::mutate(m2df2_addldata, scpt= scpt/10, scpr = scpr/10, ccpt = ccpt/10, ccpr = ccpr/10)
m2df2_out <- runBSEM(modsyntax(dat = m2df2_addldata, model = m2df2_model, bsem_dir = "~/Desktop"))
m2df7_out <- runBSEM(modsyntax(dat = m2df7, model = m2df7_model, bsem_dir = "~/Desktop"))


m1df6_model <- m1df2_model <- m1df7_model <- m1
m1df6 <- dplyr::rename(m1df6, ECRanx_1 = ECR_anxiety_1, ECRanx_0 = ECR_anxiety_0)
m1df6_out <- runBSEM(modsyntax(dat = m1df6, model = m1df6_model, bsem_dir = "~/Desktop"))
m1df2_addldata <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
m1df2_addldata <-left_join(m1df2_addldata, tomerge_newdf)
m1df2_addldata <- dplyr::rename(m1df2_addldata, ECRanx_1 = ECR_anxiety_1, ECRanx_0 = ECR_anxiety_0)
m1df2_out <- runBSEM(modsyntax(dat = m1df2_addldata, model = m1df2_model, bsem_dir = "~/Desktop"))
m1df7_out <- runBSEM(modsyntax(dat = m1df7, model = m1df7_model, bsem_dir = "~/Desktop"))

m3df6_model <- m3df2_model <- m3df7_model <- m3
m3df6 <- dplyr::rename(m3df6, ECRanx_1 = ECR_anxiety_1, ECRanx_0 = ECR_anxiety_0) %>% mutate(scpt= 10*scpt, scpr = 10*scpr, ccpt = 10*ccpt, ccpr = 10*ccpr)
m3df6_out <- runBSEM(modsyntax(dat = m3df6, model = m3df6_model, bsem_dir = "~/Desktop"))
m3df2_addldata <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
m3df2_addldata <-left_join(m3df2_addldata, tomerge_newdf)
m3df2_addldata <- dplyr::rename(m3df2_addldata, ECRanx_1 = ECR_anxiety_1, ECRanx_0 = ECR_anxiety_0) 
m3df2_addldata <- dplyr::mutate(m3df2_addldata, scpt= scpt*10, scpr = scpr*10, ccpt = ccpt*10, ccpr = ccpr*10)
m3df2_out <- runBSEM(modsyntax(dat = m3df2_addldata, model = m3df2_model, bsem_dir = "~/Desktop"))
m3df7_out <- runBSEM(modsyntax(dat = m3df7, model = m3df7_model, bsem_dir = "~/Desktop"))


m4df6_model <- m4df2_model <- m4df7_model <- m4
m4df6 <- dplyr::rename(m4df6, ECRavo_1 = ECR_avoidance_1, ECRavo_0 = ECR_avoidance_0) #%>% mutate(scpt= 10*scpt, scpr = 10*scpr, ccpt = 10*ccpt, ccpr = 10*ccpr)
m4df6_out <- runBSEM(modsyntax(dat = m4df6, model = m4df6_model, bsem_dir = "~/Desktop"))
m4df2_addldata <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
m4df2_addldata <-left_join(m4df2_addldata, tomerge_newdf)
m4df2_addldata <- dplyr::rename(m4df2_addldata, ECRavo_1 = ECR_avoidance_1, ECRavo_0 = ECR_avoidance_0) 
m4df2_addldata <- dplyr::mutate(m4df2_addldata, scpt= scpt*10, scpr = scpr*10, ccpt = ccpt*10, ccpr = ccpr*10)
m4df2_out <- runBSEM(modsyntax(dat = m4df2_addldata, model = m4df2_model, bsem_dir = "~/Desktop"))
m4df7_out <- runBSEM(modsyntax(dat = m4df7, model = m4df7_model, bsem_dir = "~/Desktop"))

# Comparing df6 and df7 ---------------------------------------------------
affna_df <- dplyr::select(forfit_df, PTNUM, prnapt, prnapr, pnapt, pnapr, prafpt, prafpr, pafpt, pafpr)
df6 <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-prnapt, -prnapr, -pnapt, -pnapr)%>% mutate(DASTot0  = winsorize_vec(DASTot0, 60), DASTot1 = winsorize_vec(DASTot1, 60)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
df6 <- left_join(df6, affna_df)
tomerge_newdf_renamed <- dplyr::rename(tomerge_newdf, ECRanx_0 = ECR_anxiety_0, ECRanx_1 = ECR_anxiety_1, ECRavo_1 = ECR_avoidance_1, ECRavo_0 = ECR_avoidance_0)
df6 <- left_join(df6, tomerge_newdf_renamed)
df6 <- mutate(df6, scpt = 10*scpt, ccpt = 10*ccpt, scpr = 10*scpr, ccpr = 10*ccpr)
df6$data_frame <- "df6"
m1df7$data_frame <- "df6"
m1df7$data_frame2 <- "df7"
a <- dplyr::select(df6, PTNUM, scpt, ccpt, scpr, ccpr, ECRanx_1, ECRanx_0, prnapt, prnapr, pnapt, pnapr, elpt, elpr,cmpt, cmpr, prafpt, prafpr, pafpt, pafpr, ECRavo_0, ECRavo_1, data_frame) %>% dplyr::mutate(PTNUM = as.numeric(PTNUM))
b <- dplyr::select(m1df7,  PTNUM, data_frame, data_frame2)
df67 <- full_join(a,b , by = c("PTNUM"))
df67 <- mutate(df67, whichdf = case_when(data_frame.x == "df6" & data_frame.y == "df6" ~ "both",
                               is.na(data_frame.x) ~ "df7", 
                               TRUE ~ "df6"))
df67_foranalyses <- dplyr::select(df67, -data_frame.x, -data_frame.y, -data_frame2) %>% dplyr::arrange(whichdf, PTNUM)
df67_foranalyses <- dplyr::mutate(df67_foranalyses, whichdf_num = case_when(whichdf == "both" ~ 0,
                                                                            whichdf == "df6" ~ 1, 
                                                                            whichdf == "df7" ~ 2))
df67_foranalyses <- dplyr::select(df67_foranalyses, -whichdf)
# df67_foranalyses <- df67_foranalyses %>% dplyr::mutate(scpt = ifelse(PTNUM == 8066, 29.68848, .$scpt),
#                                                        ccpt = ifelse(PTNUM == 8066, 31.6186, .$ccpt),
#                                                        scpr = ifelse(PTNUM == 8066, 66.12093, .$scpr),
#                                                        ccpr = ifelse(PTNUM == 8066, -116.4911, .$ccpr),
#                                                        ECRanx_1 = ifelse(PTNUM == 8066, NA, .$ECRanx_1),
#                                                        ECRanx_0 = ifelse(PTNUM == 8066, 3.833333, .$ECRanx_0),
#                                                        prnapt = ifelse(PTNUM == 8066, 39, .$prnapt),
#                                                        prnapr = ifelse(PTNUM == 8066, 26, .$prnapr),
#                                                        pnapt = ifelse(PTNUM == 8066, 30, .$pnapt),
#                                                        pnapr = ifelse(PTNUM == 8066, 22, .$pnapr),
#                                                        elpt = ifelse(PTNUM == 8066, 12.75, .$elpt),
#                                                        elpr = ifelse(PTNUM == 8066, 7.25, .$elpr),
#                                                        prafpt = ifelse(PTNUM == 8066, 0.207 , .$prafpt),
#                                                        prafpr = ifelse(PTNUM == 8066, -1.3965, .$prafpr),
#                                                        pafpt = ifelse(PTNUM == 8066, 1.635333,.$pafpt),
#                                                        pafpr = ifelse(PTNUM == 8066,0.957,.$pafpr),
#                                                        cmpt = ifelse(PTNUM == 8066, -1.164, .$cmpt),
#                                                        cmpr = ifelse(PTNUM == 8066, 2.03025, .$cmpr),
#                                                        ECRavo_1 = ifelse(PTNUM == 8066, 3.722222, .$ECRavo_1),
#                                                        ECRavo_0 = ifesle(PTNUM == 8066, 2.833333, .$ECRavo_0))
df67_foranalyses$id <- 1:nrow(df67_foranalyses)
prepareMplusData(df67_foranalyses, "~/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/mplus_modelcomparison_dec2018/df67.dat")


df67_DASFU_foranalyses <- df67_foranalyses
das_fu <- read.csv("~/Downloads/das_bu.csv") %>% dplyr::select(PTNUM, DASSatMEAN12.0, DASSatMEAN12.1, DAS_BL_Sat.0, DAS_BL_Sat.1, ANYBRK_CPL.1, ANYBRK_CPL.0)
das_fu <- das_fu %>% dplyr::mutate(rel = if_else((ANYBRK_CPL.1 == 0) |(ANYBRK_CPL.0 == 0), 0, 1))
das_fu <- das_fu %>% dplyr::mutate(DASSatMEAN12.0 = ifelse(rel == 0, NA, .$DASSatMEAN12.0),
                                   DASSatMEAN12.1 = ifelse(rel == 0, NA, .$DASSatMEAN12.1))

das_fu_mod<- transmute(das_fu, rel = rel, PTNUM = PTNUM, dasfu1 = DASSatMEAN12.1, dasfu0 = DASSatMEAN12.0,
                       dasbl1 = DAS_BL_Sat.1, dasbl0 = DAS_BL_Sat.0)

df67_DASFU_foranalyses <- left_join(df67_DASFU_foranalyses, das_fu_mod)

prepareMplusData(df67_DASFU_foranalyses, "~/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/mplus_modelcomparison_dec2018/df67_das.dat")




# IIP Robustness Check ----------------------------------------------------

affna_df <- dplyr::select(forfit_df, PTNUM, prnapt, prnapr, pnapt, pnapr, prafpt, prafpr, pafpt, pafpr)
df6 <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-prnapt, -prnapr, -pnapt, -pnapr)%>% mutate(DASTot0  = winsorize_vec(DASTot0, 60), DASTot1 = winsorize_vec(DASTot1, 60)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
df6 <- left_join(df6, affna_df)
tomerge_newdf <- dplyr::select(newdf,PTNUM, ECR_avoidance_1, ECR_avoidance_0, ECR_anxiety_1,ECR_anxiety_0, elpt.c, elpr.c, agpt.c, agpr.c, cmpt.c, cmpr.c,agcmpt, agcmpr, DASTot0, DASTot1)

tomerge_newdf_renamed <- dplyr::rename(tomerge_newdf, ECRanx_0 = ECR_anxiety_0, ECRanx_1 = ECR_anxiety_1, ECRavo_1 = ECR_avoidance_1, ECRavo_0 = ECR_avoidance_0)
df6 <- dplyr::select(df6, -agcmpt, -agcmpr)
df6 <- left_join(df6, tomerge_newdf_renamed)
df6 <- mutate(df6, scpt = 10*scpt, ccpt = 10*ccpt, scpr = 10*scpr, ccpr = 10*ccpr)
df6$data_frame <- "df6"
m1df7$data_frame <- "df6"
m1df7$data_frame2 <- "df7"
a <- dplyr::select(df6, PTNUM, scpt, ccpt, scpr, ccpr, ECRanx_1, ECRanx_0, prnapt, prnapr, pnapt, pnapr, elpt.c, elpr.c,cmpt.c, cmpr.c, agpt.c, agpr.c, agcmpt, agcmpr, prafpt, prafpr, pafpt, pafpr, ECRavo_0, ECRavo_1, data_frame) %>% dplyr::mutate(PTNUM = as.numeric(PTNUM))
b <- dplyr::select(m1df7,  PTNUM, data_frame, data_frame2)
df67 <- full_join(a,b , by = c("PTNUM"))
df67 <- mutate(df67, whichdf = case_when(data_frame.x == "df6" & data_frame.y == "df6" ~ "both",
                                         is.na(data_frame.x) ~ "df7", 
                                         TRUE ~ "df6"))
df67_foranalyses <- dplyr::select(df67, -data_frame.x, -data_frame.y, -data_frame2) %>% dplyr::arrange(whichdf, PTNUM)
df67_foranalyses <- dplyr::mutate(df67_foranalyses, whichdf_num = case_when(whichdf == "both" ~ 0,
                                                                            whichdf == "df6" ~ 1, 
                                                                            whichdf == "df7" ~ 2))
df67_foranalyses <- dplyr::select(df67_foranalyses, -whichdf)
# df67_foranalyses <- df67_foranalyses %>% dplyr::mutate(scpt = ifelse(PTNUM == 8066, 29.68848, .$scpt),
#                                                        ccpt = ifelse(PTNUM == 8066, 31.6186, .$ccpt),
#                                                        scpr = ifelse(PTNUM == 8066, 66.12093, .$scpr),
#                                                        ccpr = ifelse(PTNUM == 8066, -116.4911, .$ccpr),
#                                                        ECRanx_1 = ifelse(PTNUM == 8066, NA, .$ECRanx_1),
#                                                        ECRanx_0 = ifelse(PTNUM == 8066, 3.833333, .$ECRanx_0),
#                                                        prnapt = ifelse(PTNUM == 8066, 39, .$prnapt),
#                                                        prnapr = ifelse(PTNUM == 8066, 26, .$prnapr),
#                                                        pnapt = ifelse(PTNUM == 8066, 30, .$pnapt),
#                                                        pnapr = ifelse(PTNUM == 8066, 22, .$pnapr),
#                                                        elpt = ifelse(PTNUM == 8066, 12.75, .$elpt),
#                                                        elpr = ifelse(PTNUM == 8066, 7.25, .$elpr),
#                                                        prafpt = ifelse(PTNUM == 8066, 0.207 , .$prafpt),
#                                                        prafpr = ifelse(PTNUM == 8066, -1.3965, .$prafpr),
#                                                        pafpt = ifelse(PTNUM == 8066, 1.635333,.$pafpt),
#                                                        pafpr = ifelse(PTNUM == 8066,0.957,.$pafpr),
#                                                        cmpt = ifelse(PTNUM == 8066, -1.164, .$cmpt),
#                                                        cmpr = ifelse(PTNUM == 8066, 2.03025, .$cmpr),
#                                                        ECRavo_1 = ifelse(PTNUM == 8066, 3.722222, .$ECRavo_1),
#                                                        ECRavo_0 = ifesle(PTNUM == 8066, 2.833333, .$ECRavo_0))
df67_foranalyses$id <- 1:nrow(df67_foranalyses)

df67_DASFU_foranalyses <- df67_foranalyses
das_fu <- read.csv("~/Downloads/das_bu.csv") %>% dplyr::select(PTNUM, DASSatMEAN12.0, DASSatMEAN12.1, DAS_BL_Sat.0, DAS_BL_Sat.1, ANYBRK_CPL.1, ANYBRK_CPL.0)
das_fu <- das_fu %>% dplyr::mutate(rel = if_else((ANYBRK_CPL.1 == 0) |(ANYBRK_CPL.0 == 0), 0, 1))
das_fu <- das_fu %>% dplyr::mutate(DASSatMEAN12.0 = ifelse(rel == 0, NA, .$DASSatMEAN12.0),
                                   DASSatMEAN12.1 = ifelse(rel == 0, NA, .$DASSatMEAN12.1))

das_fu_mod<- transmute(das_fu, rel = rel, PTNUM = PTNUM, dasfu1 = DASSatMEAN12.1, dasfu0 = DASSatMEAN12.0,
                       dasbl1 = DAS_BL_Sat.1, dasbl0 = DAS_BL_Sat.0)


das_fu_110 <- das_fu %>% dplyr::filter(PTNUM %in% forfit_df$PTNUM) %>% dplyr::mutate(rel = if_else((ANYBRK_CPL.1 == 0) |(ANYBRK_CPL.0 == 0), 0, 1))
das_fu_110 <- das_fu_110 %>% dplyr::mutate(DASSatMEAN12.0 = ifelse(rel == 0, NA, .$DASSatMEAN12.0),
                                   DASSatMEAN12.1 = ifelse(rel == 0, NA, .$DASSatMEAN12.1))

das_fu_110_mod<- transmute(das_fu_110, rel = rel, PTNUM = PTNUM, dasfu1 = DASSatMEAN12.1, dasfu0 = DASSatMEAN12.0,
                       dasbl1 = DAS_BL_Sat.1, dasbl0 = DAS_BL_Sat.0)


df67_DASFU_foranalyses <- left_join(df67_DASFU_foranalyses, das_fu_mod)

prepareMplusData(df67_DASFU_foranalyses, "~/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/mplus_modelcomparison_dec2018/df67_das_allIIP.dat")


# Dem Differences in 121 vs 99? -------------------------------------------

dem_df <- couples_baseline_clin %>% dplyr::select(PTNUM,DyadID, race, p_sex,p_age) %>% tidyr::gather(key = "key", value = "value", race, p_sex, p_age) %>% dplyr::mutate(DyadID = paste0(key, "_",DyadID)) %>% dplyr::select(-key) %>% spread(key = DyadID,value = value) %>% dplyr::rename(r_0 = race_0, r_1 = race_1, sex_1 = p_sex_1, sex_0 = p_sex_0, age_1 = p_age_1, age_0 = p_age_0) 
dem_df <- dem_df %>% mutate(r_0 = if_else(r_0=="White                                                           ", 1,0), r_1 = if_else(r_1 == "White                                                           ", 1, 0),
                            female_0 = if_else(sex_0 =="male  ", 0, 1), female_1 = if_else(sex_1=="male  ", 0,1), 
                            age_0 = as.integer(age_0), age_1 = as.integer(age_1)) %>% dplyr::select(-sex_1, -sex_0)

dem_df_120 <- dplyr::filter(dem_df, PTNUM %in% df_121$PTNUM)
df_121 <- read.csv("/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv") %>% dplyr::filter( PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104) %>% dplyr::select(-starts_with("X"))
df_110 <- read.csv("/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv") %>% dplyr::filter(m1_R2 > .98, PTNUM != 8021, PTNUM !=8035, PTNUM != 8040, PTNUM != 8073, PTNUM !=8000, PTNUM != 8001, PTNUM != 8100, PTNUM != 8104) %>% dplyr::select(-starts_with("X"))
dem_df_120 <- dem_df_120 %>% mutate(R2_cut = if_else(PTNUM %in% df_110$PTNUM, 1,0),
                                    uni_cut = if_else(PTNUM %in% df67_foranalyses$PTNUM, 1, 0))

summary(glm(R2_cut ~ age_0, dem_df_120, family = binomial()))
summary(glm(uni_cut ~ age_0, dem_df_120, family = binomial()))
summary(glm(R2_cut ~ age_1, dem_df_120, family = binomial()))
summary(glm(uni_cut ~ age_1, dem_df_120, family = binomial()))
summary(glm(R2_cut ~ female_0, dem_df_120, family = binomial()))
summary(glm(uni_cut ~ female_0, dem_df_120, family = binomial()))
summary(glm(R2_cut ~ female_1, dem_df_120, family = binomial()))
summary(glm(uni_cut ~ female_1, dem_df_120, family = binomial())) #yes, female patients more likely to be cut
summary(glm(R2_cut ~ r_0, dem_df_120, family = binomial()))
summary(glm(uni_cut ~ r_0, dem_df_120, family = binomial()))
summary(glm(R2_cut ~ r_1, dem_df_120, family = binomial()))
summary(glm(uni_cut ~ r_1, dem_df_120, family = binomial()))
df67_DASFU_dem_foranalyses <- left_join(df67_DASFU_foranalyses, dem_df_120) 
prepareMplusData(df67_DASFU_dem_foranalyses, "~/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/mplus_modelcomparison_dec2018/df67_das_dem.dat")

# Testing whether m1-m5 is robust to baseline levels of coregulati --------

affna_df <- dplyr::select(forfit_df, PTNUM, prnapt, prnapr, pnapt, pnapr, prafpt, prafpr, pafpt, pafpr)
df6 <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-prnapt, -prnapr, -pnapt, -pnapr)%>% mutate(DASTot0  = winsorize_vec(DASTot0, 60), DASTot1 = winsorize_vec(DASTot1, 60)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
df6 <- left_join(df6, affna_df)
tomerge_newdf_renamed <- dplyr::rename(tomerge_newdf, ECRanx_0 = ECR_anxiety_0, ECRanx_1 = ECR_anxiety_1, ECRavo_1 = ECR_avoidance_1, ECRavo_0 = ECR_avoidance_0)
df6 <- left_join(df6, tomerge_newdf_renamed)
df6 <- mutate(df6, scpt = 10*scpt, ccpt = 10*ccpt, scpr = 10*scpr, ccpr = 10*ccpr)
df6$data_frame <- "df6"
m1df7$data_frame <- "df6"
m1df7$data_frame2 <- "df7"
a <- dplyr::select(df6, PTNUM, scpt, ccpt, scpr, ccpr, ECRanx_1, ECRanx_0, prnapt, prnapr, pnapt, pnapr, elpt, elpr,cmpt, cmpr, prafpt, prafpr, pafpt, pafpr, ECRavo_0, ECRavo_1, data_frame) %>% dplyr::mutate(PTNUM = as.numeric(PTNUM))
b <- dplyr::select(m1df7,  PTNUM, data_frame, data_frame2)
df67 <- full_join(a,b , by = c("PTNUM"))
df67 <- mutate(df67, whichdf = case_when(data_frame.x == "df6" & data_frame.y == "df6" ~ "both",
                                         is.na(data_frame.x) ~ "df7", 
                                         TRUE ~ "df6"))
df67_foranalyses <- dplyr::select(df67, -data_frame.x, -data_frame.y, -data_frame2) %>% dplyr::arrange(whichdf, PTNUM)
df67_foranalyses <- dplyr::mutate(df67_foranalyses, whichdf_num = case_when(whichdf == "both" ~ 0,
                                                                            whichdf == "df6" ~ 1, 
                                                                            whichdf == "df7" ~ 2))
df67_foranalyses <- dplyr::select(df67_foranalyses, -whichdf)
# df67_foranalyses <- df67_foranalyses %>% dplyr::mutate(scpt = ifelse(PTNUM == 8066, 29.68848, .$scpt),
#                                                        ccpt = ifelse(PTNUM == 8066, 31.6186, .$ccpt),
#                                                        scpr = ifelse(PTNUM == 8066, 66.12093, .$scpr),
#                                                        ccpr = ifelse(PTNUM == 8066, -116.4911, .$ccpr),
#                                                        ECRanx_1 = ifelse(PTNUM == 8066, NA, .$ECRanx_1),
#                                                        ECRanx_0 = ifelse(PTNUM == 8066, 3.833333, .$ECRanx_0),
#                                                        prnapt = ifelse(PTNUM == 8066, 39, .$prnapt),
#                                                        prnapr = ifelse(PTNUM == 8066, 26, .$prnapr),
#                                                        pnapt = ifelse(PTNUM == 8066, 30, .$pnapt),
#                                                        pnapr = ifelse(PTNUM == 8066, 22, .$pnapr),
#                                                        elpt = ifelse(PTNUM == 8066, 12.75, .$elpt),
#                                                        elpr = ifelse(PTNUM == 8066, 7.25, .$elpr),
#                                                        prafpt = ifelse(PTNUM == 8066, 0.207 , .$prafpt),
#                                                        prafpr = ifelse(PTNUM == 8066, -1.3965, .$prafpr),
#                                                        pafpt = ifelse(PTNUM == 8066, 1.635333,.$pafpt),
#                                                        pafpr = ifelse(PTNUM == 8066,0.957,.$pafpr),
#                                                        cmpt = ifelse(PTNUM == 8066, -1.164, .$cmpt),
#                                                        cmpr = ifelse(PTNUM == 8066, 2.03025, .$cmpr),
#                                                        ECRavo_1 = ifelse(PTNUM == 8066, 3.722222, .$ECRavo_1),
#                                                        ECRavo_0 = ifesle(PTNUM == 8066, 2.833333, .$ECRavo_0))
df67_foranalyses$id <- 1:nrow(df67_foranalyses)

df_resid <- read.csv("~/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/resid_df_VAR.csv") %>% dplyr::filter(!is.na(scpt), m1_R2 > .98, v_R2 > .98) %>% dplyr::filter(PTNUM != 8035, PTNUM != 8040, PTNUM!= 8073)
df_resid <- transmute(df_resid, scpt = resid_scpt, scpr = resid_scpr, ccpt = resid_ccpt, ccpr = resid_ccpr, PTNUM = PTNUM)
df67_foranalyses <- dplyr::select(df67_foranalyses, -scpt, -scpr, -ccpt, -ccpr) %>% left_join(df_resid)
prepareMplusData(df67_foranalyses, "~/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/mplus_modelcomparison_dec2018/df67_resid.dat")

df67_DASFU_foranalyses <- df67_foranalyses
das_fu <- read.csv("~/Downloads/das_bu.csv") %>% dplyr::select(PTNUM, DASSatMEAN12.0, DASSatMEAN12.1, DAS_BL_Sat.0, DAS_BL_Sat.1, ANYBRK_CPL.1, ANYBRK_CPL.0)
das_fu <- das_fu %>% dplyr::mutate(rel = if_else((ANYBRK_CPL.1 == 0) |(ANYBRK_CPL.0 == 0), 0, 1))
das_fu <- das_fu %>% dplyr::mutate(DASSatMEAN12.0 = ifelse(rel == 0, NA, .$DASSatMEAN12.0),
                                   DASSatMEAN12.1 = ifelse(rel == 0, NA, .$DASSatMEAN12.1))

das_fu_mod<- transmute(das_fu, rel = rel, PTNUM = PTNUM, dasfu1 = DASSatMEAN12.1, dasfu0 = DASSatMEAN12.0,
                       dasbl1 = DAS_BL_Sat.1, dasbl0 = DAS_BL_Sat.0)

df67_DASFU_foranalyses <- left_join(df67_DASFU_foranalyses, das_fu_mod)

prepareMplusData(df67_DASFU_foranalyses, "~/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/mplus_modelcomparison_dec2018/df67_das_resid.dat")


# add in spaff ------------------------------------------------------------
affna_df <- dplyr::select(forfit_df, PTNUM, prnapt, prnapr, pnapt, pnapr, prafpt, prafpr, pafpt, pafpr)
df6 <- forfit_df  %>% dplyr::filter(scpt > univariateoutiers_min(scpt), scpt < univariateoutiers_max(scpt),ccpt > univariateoutiers_min(ccpt), ccpt < univariateoutiers_max(ccpr), scpr > univariateoutiers_min(scpr), scpr < univariateoutiers_max(scpr),ccpr > univariateoutiers_min(ccpr), ccpr < univariateoutiers_max(ccpr)) %>% dplyr::select(-prnapt, -prnapr, -pnapt, -pnapr)%>% mutate(DASTot0  = winsorize_vec(DASTot0, 60), DASTot1 = winsorize_vec(DASTot1, 60)) %>% dplyr::select(-elpt, -elpr, -agpt, -agpr, -cmpt, -cmpr, -DASTot0, -DASTot1, -ECRanx_0, -ECRanx_1, -ECRavo_0, -ECRavo_1)
df6 <- left_join(df6, affna_df)
tomerge_newdf_renamed <- dplyr::rename(tomerge_newdf, ECRanx_0 = ECR_anxiety_0, ECRanx_1 = ECR_anxiety_1, ECRavo_1 = ECR_avoidance_1, ECRavo_0 = ECR_avoidance_0)
df6 <- left_join(df6, tomerge_newdf_renamed)
df6 <- mutate(df6, scpt = 10*scpt, ccpt = 10*ccpt, scpr = 10*scpr, ccpr = 10*ccpr)
df6$data_frame <- "df6"
m1df7$data_frame <- "df6"
m1df7$data_frame2 <- "df7"
a <- dplyr::select(df6, PTNUM, scpt, ccpt, scpr, ccpr, ECRanx_1, ECRanx_0, prnapt, prnapr, pnapt, pnapr, elpt, elpr,cmpt, cmpr, prafpt, prafpr, pafpt, pafpr, ECRavo_0, ECRavo_1, data_frame) %>% dplyr::mutate(PTNUM = as.numeric(PTNUM))
b <- dplyr::select(m1df7,  PTNUM, data_frame, data_frame2)
df67 <- full_join(a,b , by = c("PTNUM"))
df67 <- mutate(df67, whichdf = case_when(data_frame.x == "df6" & data_frame.y == "df6" ~ "both",
                                         is.na(data_frame.x) ~ "df7", 
                                         TRUE ~ "df6"))
df67_foranalyses <- dplyr::select(df67, -data_frame.x, -data_frame.y, -data_frame2) %>% dplyr::arrange(whichdf, PTNUM)
df67_foranalyses <- dplyr::mutate(df67_foranalyses, whichdf_num = case_when(whichdf == "both" ~ 0,
                                                                            whichdf == "df6" ~ 1, 
                                                                            whichdf == "df7" ~ 2))
df67_foranalyses <- dplyr::select(df67_foranalyses, -whichdf)
# df67_foranalyses <- df67_foranalyses %>% dplyr::mutate(scpt = ifelse(PTNUM == 8066, 29.68848, .$scpt),
#                                                        ccpt = ifelse(PTNUM == 8066, 31.6186, .$ccpt),
#                                                        scpr = ifelse(PTNUM == 8066, 66.12093, .$scpr),
#                                                        ccpr = ifelse(PTNUM == 8066, -116.4911, .$ccpr),
#                                                        ECRanx_1 = ifelse(PTNUM == 8066, NA, .$ECRanx_1),
#                                                        ECRanx_0 = ifelse(PTNUM == 8066, 3.833333, .$ECRanx_0),
#                                                        prnapt = ifelse(PTNUM == 8066, 39, .$prnapt),
#                                                        prnapr = ifelse(PTNUM == 8066, 26, .$prnapr),
#                                                        pnapt = ifelse(PTNUM == 8066, 30, .$pnapt),
#                                                        pnapr = ifelse(PTNUM == 8066, 22, .$pnapr),
#                                                        elpt = ifelse(PTNUM == 8066, 12.75, .$elpt),
#                                                        elpr = ifelse(PTNUM == 8066, 7.25, .$elpr),
#                                                        prafpt = ifelse(PTNUM == 8066, 0.207 , .$prafpt),
#                                                        prafpr = ifelse(PTNUM == 8066, -1.3965, .$prafpr),
#                                                        pafpt = ifelse(PTNUM == 8066, 1.635333,.$pafpt),
#                                                        pafpr = ifelse(PTNUM == 8066,0.957,.$pafpr),
#                                                        cmpt = ifelse(PTNUM == 8066, -1.164, .$cmpt),
#                                                        cmpr = ifelse(PTNUM == 8066, 2.03025, .$cmpr),
#                                                        ECRavo_1 = ifelse(PTNUM == 8066, 3.722222, .$ECRavo_1),
#                                                        ECRavo_0 = ifesle(PTNUM == 8066, 2.833333, .$ECRavo_0))
df67_foranalyses$id <- 1:nrow(df67_foranalyses)
spaff_df <- read.csv("/Users/alisonmarie526/Box/DEPENd/Projects/PD_Interaction_Dynamics/results/fulldata_VAR.csv") %>% dplyr::select(PTNUM, nastyproportion_patient, nastyproportion_partner)
df67_spaff <- left_join(df67_foranalyses, spaff_df)
df67_spaff <- rename(df67_spaff, npt = nastyproportion_patient, npr = nastyproportion_partner)
prepareMplusData(df67_spaff, "~/Box/DEPENd/Projects/PD_Interaction_Dynamics/code/mplus_modelcomparison_dec2018/df67_spaff.dat")



# Graph of 110 vs 99 uni distribution -------------------------------------



a <- ggplot(m1df1, aes(x = 10*scpt)) + geom_histogram(binwidth = 15) + xlim(0, 250) +     theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + annotate(geom = "text",x = 200,y =40,label = "N = 110")
b <- ggplot(m1df1, aes(x = 10*ccpt)) + geom_histogram(binwidth = 15)+ xlim(-150, 180)+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y = element_blank())
c <- ggplot(m1df1, aes(x = 10*scpr)) + geom_histogram(binwidth = 15)+ xlim(0, 110)  + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y = element_blank())
d <- ggplot(m1df1, aes(x = 10*ccpr)) + geom_histogram(binwidth = 15)+ xlim(-300, 100)+   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank())

e <- ggplot(df67_foranalyses, aes(x = scpt)) + geom_histogram(binwidth = 15)+ xlim(0, 250)+   xlab("Self-Coupling Pro") + annotate(geom = "text",x = 200,y =40,label = "N = 99") 
f <- ggplot(df67_foranalyses, aes(x = ccpt)) + geom_histogram(binwidth = 15)+ xlim(-150, 180) + xlab("Cross-Coupling Pro") + theme(axis.title.y = element_blank())
g <- ggplot(df67_foranalyses, aes(x = scpr)) + geom_histogram(binwidth = 15)+ xlim(0, 110) + xlab("Self-Coupling Par") + theme(axis.title.y = element_blank())
h <- ggplot(df67_foranalyses, aes(x = ccpr)) + geom_histogram(binwidth = 15)+ xlim(-300, 100)+   xlab("Cross-Coupling Par")+ theme(axis.title.y = element_blank())
pdf("~/Desktop/feb2019_figS1.pdf", width = 12, height =6)
cowplot::plot_grid(a,b,c,d,e,f,g,h, nrow = 2)
dev.off()